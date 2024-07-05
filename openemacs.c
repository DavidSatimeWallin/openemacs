// openemacs: A minimalistic emacs clone in less than 1 024 lines of code.
//
// openemacs is released under 2-clause BSD license and is based on kilo,
// a very small editor written by Salvatore "antirez" Sanfilippo.

#define _DEFAULT_SOURCE // Linux: ftruncate, getline, kill, strdup
#define _GNU_SOURCE     // Linux: strcasestr

#include <ctype.h>      // isdigit, isspace
#include <errno.h>      // errno, ENOENT/ENOTTY
#include <fcntl.h>      // open, O_CREAT/O_RDWR
#include <signal.h>     // kill
#include <stdarg.h>     // va_end, va_start
#include <stdbool.h>    // bool, false, true
#include <stdio.h>      // FILE, asprintf, fclose, fopen, getline, perror, sscanf, stderr, vasprintf
#include <stdlib.h>     // atexit, calloc, exit, free, realloc
#include <string.h>     // memcmp, memcpy, memmove, memset, strcasestr, strchr, strdup, strerror, strlen, strstr
#include <sys/ioctl.h>  // ioctl
#include <termios.h>    // struct termios, tcgetattr, tcsetattr, TCSAFLUSH/BRKINT/ICRNL/INPCK/ISTRIP/IXON/OPOST/CS8/ECHO/ICANON/IEXTEN/ISIG/VMIN/VTIME
#include <time.h>       // time
#include <unistd.h>     // close, getpid, ftruncate, isatty, read, STDIN_FILENO/STDOUT_FILENO, write

enum SYNTAX_HIGHLIGHT_MODE {
    SYNTAX_HIGHLIGHT_MODE_NORMAL,
    SYNTAX_HIGHLIGHT_MODE_SINGLE_LINE_COMMENT,
    SYNTAX_HIGHLIGHT_MODE_MULTI_LINE_COMMENT,
    SYNTAX_HIGHLIGHT_MODE_KEYWORD_GROUP_1,
    SYNTAX_HIGHLIGHT_MODE_KEYWORD_GROUP_2,
    SYNTAX_HIGHLIGHT_MODE_STRING,
    SYNTAX_HIGHLIGHT_MODE_NUMBER,
    SYNTAX_HIGHLIGHT_MODE_SEARCH_MATCH,
    SYNTAX_HIGHLIGHT_MODE_TRAILING_WHITESPACE,
};

struct editor_syntax {
    char **file_match;
    char **keywords;
    char single_line_comment_start[2];
    char multi_line_comment_start[3];
    char multi_line_comment_end[3];
};

// A single line of the file we are editing.
struct editor_row {
    int index_in_file;            // Row index in the file, zero-based.
    int size;                     // Size of the row, excluding the NULL term.
    int rendered_size;            // Size of the rendered row.
    char *chars;                  // Row content.
    char *rendered_chars;         // Row content "rendered" for screen (for TABs).
    char *rendered_chars_syntax_highlight_type; // Syntax highlight type for each character in render.
    bool has_open_comment;        // Row had open comment at end in last syntax highlight check.
};

struct editor_state {
    int cursor_x, cursor_y;       // Cursor x and y position in characters
    int desired_cursor_x;         // Cursor x which user wants if sufficient characters on the line
    int row_offset;               // Offset of row displayed.
    int column_offset;            // Offset of column displayed.
    int screen_rows;              // Number of rows that we can show
    int screen_columns;           // Number of columns that we can show
    int number_of_rows;           // Number of rows
    bool raw_mode;                // Is terminal raw mode enabled?
    struct editor_row *row;       // Rows
    bool dirty;                   // File modified but not saved.
    char *filename;               // Currently open filename
    char *status_message;
    time_t status_message_last_update;
    int number_of_cut_buffer_lines;
    char **cut_buffer_lines;
    bool has_open_cut_buffer;
    struct editor_syntax *syntax_highlight_mode; // Current syntax highlight, or NULL.
    struct termios original_termios;  // In order to restore at exit.
};

// We define a very simple "append buffer" structure, that is an heap
// allocated string where we can append to. This is useful in order to
// write all the escape sequences in a buffer and flush them to the standard
// output in a single call, to avoid flickering effects.
struct append_buffer {
    char *buffer;
    size_t length;
};

static struct editor_state E;

enum KEY_ACTION {
    CTRL_A = 1, CTRL_C = 3, CTRL_D = 4, CTRL_E = 5, CTRL_F = 6, BACKSPACE = 8, TAB = 9,
    CTRL_K = 11, CTRL_L = 12, ENTER = 13, CTRL_N = 14, CTRL_P = 16, CTRL_Q = 17, CTRL_R = 18,
    CTRL_S = 19, CTRL_U = 21, CTRL_X = 24, CTRL_Y = 25, CTRL_Z = 26, ESC = 27, FORWARD_DELETE =  127,
    // The following are just soft codes, not really reported by the
    // terminal directly.
    ARROW_LEFT = 1000, ARROW_RIGHT, ARROW_UP, ARROW_DOWN, DEL_KEY, HOME_KEY,
    END_KEY, PAGE_UP, PAGE_DOWN
};

// C/C++ ("class" being C++ only)
char *C_SYNTAX_HIGHLIGHT_FILE_EXTENSIONS[] = { ".c", ".cpp", ".h", ".hpp", NULL };
char *C_SYNTAX_HIGHLIGHT_KEYWORDS[] = {
    // C/C++ keywords
    "auto", "break", "case", "class", "const", "continue", "default", "do", "else", "enum", "extern", "for", "goto", "if", "register", "return", "sizeof", "static", "struct", "switch", "typedef", "union", "volatile", "while",
    // C types
    "bool|", "char|", "double|", "float|", "int|", "long|", "short|", "signed|", "unsigned|", "void|",
    // C preprocessor directives
    "#define|", "#endif|", "#error|", "#if|", "#ifdef|", "#ifndef|", "#include|", "#undef|", NULL
};

// Go
char *GO_SYNTAX_HIGHLIGHT_FILE_EXTENSIONS[] = { ".go", NULL };
char *GO_SYNTAX_HIGHLIGHT_KEYWORDS[] = {
    // Go keywords
    "if", "for", "range", "while", "defer", "switch", "case", "else", "func", "package", "import", "type", "struct", "import", "const", "var",
    // Go types
    "nil|", "true|", "false|", "error|", "err|", "int|", "int32|", "int64|", "uint|", "uint32|", "uint64|", "string|", "bool|", NULL
};

// V
char *V_SYNTAX_HIGHLIGHT_FILE_EXTENSIONS[] = { ".v", NULL };
char *V_SYNTAX_HIGHLIGHT_KEYWORDS[] = {
    // V keywords
    "if", "for", "match", "while", "switch", "defer", "else", "fn", "module", "import", "type", "const", "struct", "assert", "in", "chan", "goto",
    // V types
    "nil|", "true|", "false|", "error|", "err|", "int|", "i32|", "i64|", "u32|", "u64|", "string|", "array|", "i8|", "u8|", "i16|", "u16|", "i128|", "u128|", "rune|", "isize|", "usize|", "voidptr|", "any|", "f32|", "f64|", "map|", "shared|", "thread|", NULL
};
// END V

// Python
char *PYTHON_SYNTAX_HIGHLIGHT_FILE_EXTENSIONS[] = { ".py", NULL };
char *PYTHON_SYNTAX_HIGHLIGHT_KEYWORDS[] = {
    // Python keywords
    "and", "as", "assert", "break", "class", "continue", "def", "del", "elif", "else", "except", "exec", "finally", "for", "from", "global", "if", "import", "in", "is", "lambda", "not", "or", "pass", "print", "raise", "return", "try", "while", "with", "yield",
    // Python types
    "buffer|", "bytearray|", "complex|", "False|", "float|", "frozenset|", "int|", "list|", "long|", "None|", "set|", "str|", "tuple|", "True|", "type|", "unicode|", "xrange|", NULL
};

struct editor_syntax SYNTAX_HIGHLIGHT_DATABASE[] = {
    { .file_match = C_SYNTAX_HIGHLIGHT_FILE_EXTENSIONS, .keywords = C_SYNTAX_HIGHLIGHT_KEYWORDS, .single_line_comment_start = "//", .multi_line_comment_start = "/*", .multi_line_comment_end = "*/" },
    { .file_match = PYTHON_SYNTAX_HIGHLIGHT_FILE_EXTENSIONS, .keywords = PYTHON_SYNTAX_HIGHLIGHT_KEYWORDS, .single_line_comment_start = "# ", .multi_line_comment_start = "", .multi_line_comment_end = "" },
    { .file_match = GO_SYNTAX_HIGHLIGHT_FILE_EXTENSIONS, .keywords = GO_SYNTAX_HIGHLIGHT_KEYWORDS, .single_line_comment_start = "//", .multi_line_comment_start = "", .multi_line_comment_end = "" },
    { .file_match = V_SYNTAX_HIGHLIGHT_FILE_EXTENSIONS, .keywords = V_SYNTAX_HIGHLIGHT_KEYWORDS, .single_line_comment_start = "//", .multi_line_comment_start = "", .multi_line_comment_end = "" }
};

__attribute__((format(printf, 1, 2)))
static void editor_set_status_message(char const *format, ...) {
    va_list ap;
    va_start(ap, format);
    free(E.status_message);
    if (vasprintf(&E.status_message, format, ap) == -1) { perror("vasprintf failed."); exit(EXIT_FAILURE); }
    va_end(ap);
    E.status_message_last_update = time(NULL);
}

static void editor_disable_raw_mode(void) {
    if (E.raw_mode) {
        tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.original_termios);
        E.raw_mode = false;
    }
}

static void editor_free_row(struct editor_row *row) {
    free(row->chars);
    row->chars = NULL;
    free(row->rendered_chars);
    row->rendered_chars = NULL;
    free(row->rendered_chars_syntax_highlight_type);
    row->rendered_chars_syntax_highlight_type = NULL;
}

static void editor_free_cut_buffer(void) {
    for (int i = 0; i < E.number_of_cut_buffer_lines; i++) {
        free(E.cut_buffer_lines[i]);
    }
    free(E.cut_buffer_lines);
    E.cut_buffer_lines = NULL;
    E.number_of_cut_buffer_lines = 0;
}

static void editor_at_exit(void) {
    editor_disable_raw_mode();
    // Clean up allocations. Make sure valgrind reports:
    // "All heap blocks were freed -- no leaks are possible"
    //
    // Allocations used:
    // - asprintf (implicit malloc)
    // - calloc
    // - realloc
    // - strdup
    // - vasprintf (implicit realloc)
    for (int i = 0; i < E.number_of_rows; i++) {
        editor_free_row(&E.row[i]);
    }
    editor_free_cut_buffer();
    free(E.filename);
    free(E.row);
    free(E.status_message);
    // Assert: "All heap blocks were freed -- no leaks are possible"
}

static void console_buffer_open(void) {
    // Switch to another buffer in order to be able to restore state at exit
    // by calling console_buffer_close(void).
    if (write(STDOUT_FILENO, "\x1b[?47h", 6) == -1) { perror("Write to stdout failed"); exit(EXIT_FAILURE); }
}

static int editor_enable_raw_mode(void) {
    if (E.raw_mode) { return 0; }
    if (!isatty(STDIN_FILENO)) { goto fatal; }
    if (tcgetattr(STDIN_FILENO, &E.original_termios) == -1) { goto fatal; }
    struct termios raw = E.original_termios; // Modify the original mode
    // input modes: no break, no CR to NL, no parity check, no strip char,
    // no start/stop output control.
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    // output modes - disable post processing
    raw.c_oflag &= ~OPOST;
    // control modes - set 8 bit chars
    raw.c_cflag |= CS8;
    // local modes - echoing off, canonical off, no extended functions,
    // no signal chars (^Z,^C)
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    // control chars - set return condition: min number of bytes and timer.
    raw.c_cc[VMIN] = 0; // Return each byte, or zero for timeout.
    raw.c_cc[VTIME] = 1; // 100 ms timeout (unit is tens of second).
    // Put terminal in raw mode after flushing
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) < 0) { goto fatal; }
    E.raw_mode = true;
    console_buffer_open();
    return 0;
fatal:
    errno = ENOTTY;
    return -1;
}

// Read a key from the terminal put in raw mode, trying to handle
// escape sequences.
static int editor_read_key(void) {
    int n_read;
    char key, sequence[3] = { 0 };
    while ((n_read = read(STDIN_FILENO, &key, 1)) == 0);
    if (n_read == -1) { exit(EXIT_FAILURE); }
    while (true) {
        if (key == ESC) { // Escape sequence
            // If this is just an ESC, we'll timeout here.
            if (read(STDIN_FILENO, sequence, 1) == 0 || read(STDIN_FILENO, sequence + 1, 1) == 0) { return ESC; }
            // ESC [ sequences.
            if (sequence[0] == '[') {
                if (sequence[1] >= '0' && sequence[1] <= '9') {
                    // Extended escape, read additional byte.
                    if (read(STDIN_FILENO, sequence + 2, 1) == 0) { return ESC; }
                    if (sequence[2] == '~') {
                        if (sequence[1] == '3') {
                            return DEL_KEY;
                        } else if (sequence[1] == '5') {
                            return PAGE_UP;
                        } else if (sequence[1] == '6') {
                            return PAGE_DOWN;
                        }
                    }
                } else {
                    if (sequence[1] == 'A') {
                        return ARROW_UP;
                    } else if (sequence[1] == 'B') {
                        return ARROW_DOWN;
                    } else if (sequence[1] == 'C') {
                        return ARROW_RIGHT;
                    } else if (sequence[1] == 'D') {
                        return ARROW_LEFT;
                    } else if (sequence[1] == 'H') {
                        return HOME_KEY;
                    } else if (sequence[1] == 'F') {
                        return END_KEY;
                    }
                }
            } else if (sequence[0] == 'O') { // ESC O sequences.
                if (sequence[1] == 'H') {
                    return HOME_KEY;
                } else if (sequence[1] == 'F') {
                    return END_KEY;
                }
            }
        } else {
            return key;
        }
    }
}

static int get_window_size(int *rows, int *columns) {
    struct winsize window_size;
    if (ioctl(STDIN_FILENO, TIOCGWINSZ, &window_size) == -1 || window_size.ws_col == 0) {
        return -1;
    }
    *columns = window_size.ws_col;
    *rows = window_size.ws_row;
    return 0;
}

static bool is_separator(int c) {
    return c == '\0' || isspace(c) || strchr(",.()+-/*=~%[];:{}", c);
}

// Return true if the specified row last char is part of a multi line comment
// that starts at this row or at one before, and does not end at the end
// of the row but spawns to the next row.
static bool editor_row_has_open_comment(struct editor_row const *row) {
    if (row->rendered_chars_syntax_highlight_type && row->rendered_size &&
            row->rendered_chars_syntax_highlight_type[row->rendered_size - 1] == SYNTAX_HIGHLIGHT_MODE_MULTI_LINE_COMMENT &&
            (row->rendered_size < 2 || (row->rendered_chars[row->rendered_size - 2] != '*' || row->rendered_chars[row->rendered_size - 1] != '/'))) { return true; }
    return false;
}

// Set every byte of row->rendered_chars_syntax_highlight_type (that corresponds to
// every character in the line) to the right syntax highlight type
// (SYNTAX_HIGHLIGHT_MODE_* defines).
static void editor_update_syntax(struct editor_row *row) {
    row->rendered_chars_syntax_highlight_type = realloc(row->rendered_chars_syntax_highlight_type, row->rendered_size);
    memset(row->rendered_chars_syntax_highlight_type, SYNTAX_HIGHLIGHT_MODE_NORMAL, row->rendered_size);
    if (!E.syntax_highlight_mode) { return; } // No syntax, everything is SYNTAX_HIGHLIGHT_MODE_NORMAL.
    char **keywords = E.syntax_highlight_mode->keywords;
    char *single_line_comment_start = E.syntax_highlight_mode->single_line_comment_start;
    char *multi_line_comment_start = E.syntax_highlight_mode->multi_line_comment_start;
    char *multi_line_comment_end = E.syntax_highlight_mode->multi_line_comment_end;
    // Point to the first non-space char.
    char *p = row->rendered_chars;
    int i = 0; // Current char offset
    while (*p && isspace(*p)) {
        p++;
        i++;
    }
    bool prev_sep = true; // Tell the parser if 'i' points to start of word.
    bool in_comment = false; // Are we inside multi-line comment?
    char in_string_char = 0; // 0, " or '
    // If the previous line has an open comment, this line starts
    // with an open comment state.
    if (row->index_in_file > 0 && editor_row_has_open_comment(&E.row[row->index_in_file - 1])) {
        in_comment = true;
    }
    while (*p) {
        // Handle // comments.
        if (prev_sep && *p == single_line_comment_start[0] && *(p + 1) == single_line_comment_start[1]) {
            // From here to end is a comment
            memset(row->rendered_chars_syntax_highlight_type + i, SYNTAX_HIGHLIGHT_MODE_SINGLE_LINE_COMMENT, row->size - i);
            break;
        }
        // Handle multi line comments.
        if (in_comment) {
            row->rendered_chars_syntax_highlight_type[i] = SYNTAX_HIGHLIGHT_MODE_MULTI_LINE_COMMENT;
            if (*p == multi_line_comment_end[0] && *(p + 1) == multi_line_comment_end[1]) {
                row->rendered_chars_syntax_highlight_type[i + 1] = SYNTAX_HIGHLIGHT_MODE_MULTI_LINE_COMMENT;
                p += 2;
                i += 2;
                in_comment = false;
                prev_sep = true;
                continue;
            } else {
                prev_sep = false;
                p++;
                i++;
                continue;
            }
        } else if (!in_string_char && *p == multi_line_comment_start[0] && *(p + 1) == multi_line_comment_start[1]) {
            row->rendered_chars_syntax_highlight_type[i] = SYNTAX_HIGHLIGHT_MODE_MULTI_LINE_COMMENT;
            row->rendered_chars_syntax_highlight_type[i + 1] = SYNTAX_HIGHLIGHT_MODE_MULTI_LINE_COMMENT;
            p += 2;
            i += 2;
            in_comment = true;
            prev_sep = false;
            continue;
        }
        // Handle "" and ''
        if (in_string_char) {
            row->rendered_chars_syntax_highlight_type[i] = SYNTAX_HIGHLIGHT_MODE_STRING;
            if (*p == '\\') {
                row->rendered_chars_syntax_highlight_type[i + 1] = SYNTAX_HIGHLIGHT_MODE_STRING;
                p += 2;
                i += 2;
                prev_sep = false;
                continue;
            }
            if (*p == in_string_char) { in_string_char = 0; }
            p++;
            i++;
            continue;
        } else if (*p == '"' || *p == '\'') {
            in_string_char = *p;
            row->rendered_chars_syntax_highlight_type[i] = SYNTAX_HIGHLIGHT_MODE_STRING;
            p++;
            i++;
            prev_sep = false;
            continue;
        }
        // Handle numbers
        if ((isdigit(*p) && (prev_sep || row->rendered_chars_syntax_highlight_type[i - 1] == SYNTAX_HIGHLIGHT_MODE_NUMBER)) ||
                (*p == '.' && i > 0 && row->rendered_chars_syntax_highlight_type[i - 1] == SYNTAX_HIGHLIGHT_MODE_NUMBER)) {
            row->rendered_chars_syntax_highlight_type[i] = SYNTAX_HIGHLIGHT_MODE_NUMBER;
            p++;
            i++;
            prev_sep = false;
            continue;
        }
        // Handle keywords and lib calls
        if (prev_sep) {
            size_t j;
            for (j = 0; keywords[j]; j++) {
                size_t keyword_length = strlen(keywords[j]);
                bool keyword_type_2 = keywords[j][keyword_length - 1] == '|';
                if (keyword_type_2) { keyword_length--; }
                if (strlen(p) >= keyword_length && !memcmp(p, keywords[j], keyword_length) && is_separator(*(p + keyword_length))) {
                    // Keyword
                    memset(row->rendered_chars_syntax_highlight_type + i, keyword_type_2 ? SYNTAX_HIGHLIGHT_MODE_KEYWORD_GROUP_2 : SYNTAX_HIGHLIGHT_MODE_KEYWORD_GROUP_1,
                           keyword_length);
                    p += keyword_length;
                    i += keyword_length;
                    break;
                }
            }
            if (keywords[j]) {
                prev_sep = false;
                continue; // We had a keyword match
            }
        }
        // Not special chars
        prev_sep = is_separator(*p);
        p++;
        i++;
    }
    for (int i = row->rendered_size - 1; i >= 0; i--) {
        if (row->rendered_chars_syntax_highlight_type[i] == SYNTAX_HIGHLIGHT_MODE_MULTI_LINE_COMMENT) { break; }
        if (isspace(row->rendered_chars[i]) || row->rendered_chars[i] == '\0' || row->rendered_chars[i] == '\n' || row->rendered_chars[i] == '\r') {
            row->rendered_chars_syntax_highlight_type[i] = SYNTAX_HIGHLIGHT_MODE_TRAILING_WHITESPACE;
        } else {
            break;
        }
    }
    // Propagate syntax change to the next row if the open comment
    // state changed. This may recursively affect all the following rows
    // in the file.
    bool open_comment = editor_row_has_open_comment(row);
    if (row->has_open_comment != open_comment && row->index_in_file + 1 < E.number_of_rows) {
        editor_update_syntax(&E.row[row->index_in_file + 1]);
    }
    row->has_open_comment = open_comment;
}

static int editor_syntax_to_color(enum SYNTAX_HIGHLIGHT_MODE hl) {
    if (hl == SYNTAX_HIGHLIGHT_MODE_SINGLE_LINE_COMMENT || hl == SYNTAX_HIGHLIGHT_MODE_MULTI_LINE_COMMENT) {
        return 31;    // normal red foreground
    } else if (hl == SYNTAX_HIGHLIGHT_MODE_KEYWORD_GROUP_1) {
        return 35;    // normal magenta foreground
    } else if (hl == SYNTAX_HIGHLIGHT_MODE_KEYWORD_GROUP_2) {
        return 32;    // normal green foreground
    } else if (hl == SYNTAX_HIGHLIGHT_MODE_STRING) {
        return 95;    // bright magenta foreground
    } else if (hl == SYNTAX_HIGHLIGHT_MODE_NUMBER) {
        return 97;    // bright white foreground
    } else if (hl == SYNTAX_HIGHLIGHT_MODE_SEARCH_MATCH) {
        return 96;    // bright cyan foreground
    } else if (hl == SYNTAX_HIGHLIGHT_MODE_TRAILING_WHITESPACE) {
        return 41;    // normal red background
    } else {
        return 37;    // normal white foreground
    }
}

static void editor_select_syntax_highlight_based_on_filename_suffix(char const *filename) {
    for (size_t j = 0; j < sizeof(SYNTAX_HIGHLIGHT_DATABASE) / sizeof(SYNTAX_HIGHLIGHT_DATABASE[0]); j++) {
        struct editor_syntax *s = SYNTAX_HIGHLIGHT_DATABASE + j;
        int i = 0;
        while (s->file_match[i]) {
            char *p = strstr(filename, s->file_match[i]);
            if (p && (s->file_match[i][0] != '.' || p[strlen(s->file_match[i])] == '\0')) {
                E.syntax_highlight_mode = s;
                return;
            }
            i++;
        }
    }
}

// Update the rendered version and the syntax highlight of a row.
static void editor_update_row(struct editor_row *row) {
    int tabs = 0;
    // Create a version of the row we can directly print on the screen,
    // respecting tabs, substituting non printable characters with '?'.
    free(row->rendered_chars);
    for (int i = 0; i < row->size; i++) {
        if (row->chars[i] == TAB) { tabs++; }
    }
    row->rendered_chars = calloc(row->size + tabs * 8 + 1, sizeof(char));
    int local_index = 0;
    for (int i = 0; i < row->size; i++) {
        if (row->chars[i] == TAB) {
            row->rendered_chars[local_index++] = ' ';
            while ((local_index + 1) % 8 != 0) { row->rendered_chars[local_index++] = ' '; }
        } else if (!isprint(row->chars[i])) {
            row->rendered_chars[local_index++] = '?';
        } else {
            row->rendered_chars[local_index++] = row->chars[i];
        }
    }
    row->rendered_size = local_index;
    row->rendered_chars[local_index] = '\0';
    editor_update_syntax(row);
}

// Insert a row at the specified position, shifting the other rows on the bottom
// if required.
static void editor_insert_row(int at, char const *s, size_t len) {
    if (at > E.number_of_rows) { return; }
    E.row = realloc(E.row, sizeof(struct editor_row) * (E.number_of_rows + 1));
    if (at != E.number_of_rows) {
        memmove(E.row + at + 1, E.row + at, sizeof(E.row[0]) * (E.number_of_rows - at));
        for (int i = at + 1; i <= E.number_of_rows; i++) { E.row[i].index_in_file++; }
    }
    E.row[at].size = len;
    E.row[at].chars = calloc(len + 1, sizeof(char));
    memcpy(E.row[at].chars, s, len + 1);
    E.row[at].rendered_chars_syntax_highlight_type = NULL;
    E.row[at].has_open_comment = false;
    E.row[at].rendered_chars = NULL;
    E.row[at].rendered_size = 0;
    E.row[at].index_in_file = at;
    editor_update_row(E.row + at);
    E.number_of_rows++;
    E.dirty = true;
}

// Remove the row at the specified position, shifting the remaining on the
// top.
static void editor_delete_row(int at) {
    if (at >= E.number_of_rows) { return; }
    struct editor_row *row = E.row + at;
    editor_free_row(row);
    memmove(E.row + at, E.row + at + 1, sizeof(E.row[0]) * (E.number_of_rows - at - 1));
    for (int i = at; i < E.number_of_rows - 1; i++) { E.row[i].index_in_file++; }
    E.number_of_rows--;
    E.dirty = true;
}

// Turn the editor rows into a single heap-allocated string.
// Returns the pointer to the heap-allocated string and populate the
// integer pointed by 'buflen' with the size of the string, excluding
// the final nulterm.
static char *editor_rows_to_string(int *buflen) {
    char *buf = NULL, *p = NULL;
    int total_length = 0;
    // Compute count of bytes
    for (int i = 0; i < E.number_of_rows; i++) {
        total_length += E.row[i].size + 1; // +1 is for "\n" at end of every row
    }
    *buflen = total_length;
    total_length++; // Also make space for nulterm
    p = buf = calloc(total_length, sizeof(char));
    for (int i = 0; i < E.number_of_rows; i++) {
        memcpy(p, E.row[i].chars, E.row[i].size);
        p += E.row[i].size;
        *p = '\n';
        p++;
    }
    *p = '\0';
    return buf;
}

// Insert a character at the specified position in a row, moving the remaining
// chars on the right if needed.
static void editor_row_insert_char(struct editor_row *row, int at, int c) {
    if (at > row->size) {
        // Pad the string with spaces if the insert location is outside the
        // current length by more than a single character.
        int pad_length = at - row->size;
        // In the next line +2 means: new char and NULL term.
        row->chars = realloc(row->chars, row->size + pad_length + 2);
        memset(row->chars + row->size, ' ', pad_length);
        row->chars[row->size + pad_length + 1] = '\0';
        row->size += pad_length + 1;
    } else {
        // If we are in the middle of the string just make space for 1 new
        // char plus the (already existing) NULL term.
        row->chars = realloc(row->chars, row->size + 2);
        memmove(row->chars + at + 1, row->chars + at, row->size - at + 1);
        row->size++;
    }
    row->chars[at] = c;
    editor_update_row(row);
    E.dirty = true;
}

// Append the string 's' at the end of a row
static void editor_row_append_string(struct editor_row *row, char const *s, size_t len) {
    row->chars = realloc(row->chars, row->size + len + 1);
    memcpy(row->chars + row->size, s, len);
    row->size += len;
    row->chars[row->size] = '\0';
    editor_update_row(row);
    E.dirty = true;
}

// Delete the character at offset 'at' from the specified row.
static void editor_row_delete_char(struct editor_row *row, int at) {
    if (row->size <= at) { return; }
    memmove(row->chars + at, row->chars + at + 1, row->size - at);
    editor_update_row(row);
    row->size--;
    E.dirty = true;
}

// Insert the specified char at the current prompt position.
static void editor_insert_char(int c) {
    int file_row = E.row_offset + E.cursor_y;
    int file_column = E.column_offset + E.cursor_x;
    struct editor_row *row = (file_row >= E.number_of_rows) ? NULL : &E.row[file_row];
    // If the row where the cursor is currently located does not exist in our
    // logical representation of the file, add enough empty rows as needed.
    if (!row) {
        while (E.number_of_rows <= file_row) {
            editor_insert_row(E.number_of_rows, "", 0);
        }
    }
    row = &E.row[file_row];
    editor_row_insert_char(row, file_column, c);
    if (E.cursor_x == E.screen_columns - 1) {
        E.column_offset++;
    } else {
        E.cursor_x++;
    }
    E.dirty = true;
}

// Inserting a newline is slightly complex as we have to handle inserting a
// newline in the middle of a line, splitting the line as needed.
static void editor_insert_newline(void) {
    int file_row = E.row_offset + E.cursor_y;
    int file_column = E.column_offset + E.cursor_x;
    struct editor_row *row = (file_row >= E.number_of_rows) ? NULL : &E.row[file_row];
    if (!row) {
        if (file_row == E.number_of_rows) {
            editor_insert_row(file_row, "", 0);
            goto fix_cursor;
        }
        return;
    }
    // If the cursor is over the current line size, we want to conceptually
    // think it's just over the last character.
    if (file_column >= row->size) { file_column = row->size; }
    if (file_column == 0) {
        editor_insert_row(file_row, "", 0);
    } else {
        // We are in the middle of a line. Split it between two rows.
        editor_insert_row(file_row + 1, row->chars + file_column, row->size - file_column);
        row = &E.row[file_row];
        row->chars[file_column] = '\0';
        row->size = file_column;
        editor_update_row(row);
    }
fix_cursor:
    if (E.cursor_y == E.screen_rows - 1) {
        E.row_offset++;
    } else {
        E.cursor_y++;
    }
    E.cursor_x = 0;
    E.column_offset = 0;
}

// Delete the char at the current prompt position.
static void editor_delete_char(void) {
    int file_row = E.row_offset + E.cursor_y;
    int file_column = E.column_offset + E.cursor_x;
    struct editor_row *row = (file_row >= E.number_of_rows) ? NULL : &E.row[file_row];
    if (!row || (file_column == 0 && file_row == 0)) { return; }
    if (file_column == 0) {
        // Handle the case of column 0, we need to move the current line
        // on the right of the previous one.
        file_column = E.row[file_row - 1].size;
        editor_row_append_string(&E.row[file_row - 1], row->chars, row->size);
        editor_delete_row(file_row);
        row = NULL;
        if (E.cursor_y == 0) {
            E.row_offset--;
        } else {
            E.cursor_y--;
        }
        E.cursor_x = file_column;
        if (E.cursor_x >= E.screen_columns) {
            int shift = (E.screen_columns - E.cursor_x) + 1;
            E.cursor_x -= shift;
            E.column_offset += shift;
        }
    } else {
        editor_row_delete_char(row, file_column - 1);
        if (E.cursor_x == 0 && E.column_offset) {
            E.column_offset--;
        } else {
            E.cursor_x--;
        }
    }
    if (row) { editor_update_row(row); }
    E.dirty = true;
}

// Load the specified program in the editor memory and returns 0 on success
// or 1 on error.
static int editor_open(char const *filename) {
    editor_select_syntax_highlight_based_on_filename_suffix(filename);
    E.dirty = false;
    free(E.filename);
    E.filename = strdup(filename);
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        if (errno != ENOENT) {
            perror("Opening file");
            exit(EXIT_FAILURE);
        }
        return 1;
    }
    char *line = NULL;
    size_t line_capacity = 0;
    ssize_t line_length;
    while ((line_length = getline(&line, &line_capacity, fp)) != -1) {
        if (line_length && (line[line_length - 1] == '\n' || line[line_length - 1] == '\r')) { line[--line_length] = '\0'; }
        editor_insert_row(E.number_of_rows, line, line_length);
    }
    free(line);
    fclose(fp);
    E.dirty = false;
    return 0;
}

// Save the current file to disk. Return 0 on success, 1 on error.
static int editor_save(void) {
    int len;
    char *buf = editor_rows_to_string(&len);
    int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
    // Use truncate + a single write(2) call in order to make saving
    // a bit safer, under the limits of what we can do in a small editor.
    if (fd == -1 || ftruncate(fd, len) == -1 || write(fd, buf, len) != len) { goto write_error; }
    close(fd);
    free(buf);
    E.dirty = false;
    editor_set_status_message("Wrote %s (%d bytes)", E.filename, len);
    return 0;
write_error:
    free(buf);
    if (fd != -1) { close(fd); }
    editor_set_status_message("Can't save! I/O error: %s", strerror(errno));
    return 1;
}

static void abuf_append(struct append_buffer *ab, const char *s, int len) {
    char *new = realloc(ab->buffer, ab->length + len);
    if (!new) { return; }
    memcpy(new + ab->length, s, len);
    ab->buffer = new;
    ab->length += len;
}

static void abuf_free(struct append_buffer *ab) {
    free(ab->buffer);
    ab->buffer = NULL;
}

// This function writes the whole screen using VT100 escape characters
// starting from the logical state of the editor in the global state 'E'.
static void editor_refresh_screen(void) {
    struct editor_row *r = NULL;
    struct append_buffer ab = { .buffer = NULL, .length = 0 };
    abuf_append(&ab, "\x1b[?25l", 6); // Hide cursor.
    abuf_append(&ab, "\x1b[H", 3); // Go home.
    for (int y = 0; y < E.screen_rows; y++) {
        int file_row = E.row_offset + y;
        if (file_row >= E.number_of_rows) {
            abuf_append(&ab, "~\x1b[0K\r\n", 7);
            continue;
        }
        r = &E.row[file_row];
        int len = r->rendered_size - E.column_offset;
        if (len > 0) {
            if (len > E.screen_columns) { len = E.screen_columns; }
            char *c = r->rendered_chars + E.column_offset;
            char *rendered_chars_syntax_highlight_type = r->rendered_chars_syntax_highlight_type + E.column_offset;
            int current_color = -1;
            for (int i = 0; i < len; i++) {
                if (rendered_chars_syntax_highlight_type[i] == SYNTAX_HIGHLIGHT_MODE_NORMAL) {
                    if (current_color != -1) {
                        abuf_append(&ab, "\x1b[39m", 5);
                        current_color = -1;
                    }
                    abuf_append(&ab, c + i, 1);
                } else {
                    int color = editor_syntax_to_color(rendered_chars_syntax_highlight_type[i]);
                    if (color != current_color) {
                        char *buffer = NULL;
                        int color_length = asprintf(&buffer, "\x1b[%dm", color);
                        if (color_length == -1) { perror("asprintf failed"); exit(EXIT_FAILURE); }
                        current_color = color;
                        abuf_append(&ab, buffer, color_length);
                        free(buffer);
                    }
                    abuf_append(&ab, c + i, 1);
                }
            }
            abuf_append(&ab, "\x1b[0m", 4); // Reset to white on black
        }
        abuf_append(&ab, "\x1b[39m", 5);
        abuf_append(&ab, "\x1b[0K", 4);
        abuf_append(&ab, "\r\n", 2);
    }
    // Create a two rows status. First row:
    abuf_append(&ab, "\x1b[0K", 4);
    abuf_append(&ab, "\x1b[7m", 4);
    char *status = NULL;
    int len = asprintf(&status, "Editing: %.20s%s | Line: %d/%d (%d %%) | Column: %d", E.filename, E.dirty ? " (modified)" : "",
                       E.row_offset + E.cursor_y + 1 <= E.number_of_rows ? E.row_offset + E.cursor_y + 1 : E.number_of_rows, E.number_of_rows, E.number_of_rows > 0
                       && E.row_offset + E.cursor_y + 1 < E.number_of_rows ? 100 * (E.row_offset + E.cursor_y + 1) / E.number_of_rows : 100, E.cursor_x + 1);
    if (len == -1) { perror("asprintf failed"); exit(EXIT_FAILURE); }
    if (len > E.screen_columns) { len = E.screen_columns; }
    abuf_append(&ab, status, len);
    free(status);
    while (len++ < E.screen_columns) {
        abuf_append(&ab, " ", 1);
    }
    abuf_append(&ab, "\x1b[0m\r\n", 6);
    // Second row depends on E.status_message and the status message update time.
    abuf_append(&ab, "\x1b[0K", 4);
    int message_length = strlen(E.status_message);
    if (message_length && time(NULL) - E.status_message_last_update < 5) {
        abuf_append(&ab, E.status_message, message_length <= E.screen_columns ? message_length : E.screen_columns);
    }
    // Put cursor at its current position. Note that the horizontal position
    // at which the cursor is displayed may be different compared to 'E.cursor_x'
    // because of TABs.
    int cursor_x_including_expanded_tabs = 1;
    int file_row = E.row_offset + E.cursor_y;
    struct editor_row *row = (file_row >= E.number_of_rows) ? NULL : &E.row[file_row];
    if (row) {
        for (int i = E.column_offset; i < (E.cursor_x + E.column_offset); i++) {
            if (i < row->size && row->chars[i] == TAB) { cursor_x_including_expanded_tabs += 7 - ((cursor_x_including_expanded_tabs) % 8); }
            cursor_x_including_expanded_tabs++;
        }
    }
    char *buffer = NULL;
    if (asprintf(&buffer, "\x1b[%d;%dH", E.cursor_y + 1, cursor_x_including_expanded_tabs) == -1) { perror("asprintf failed"); exit(EXIT_FAILURE); }
    abuf_append(&ab, buffer, strlen(buffer));
    free(buffer);
    abuf_append(&ab, "\x1b[?25h", 6); // Show cursor.
    if (write(STDOUT_FILENO, ab.buffer, ab.length) == -1) { perror("Write to stdout failed"); exit(EXIT_FAILURE); }
    abuf_free(&ab);
}

// Move cursor to X position (0: start of line, -1 == end of line)
static void editor_move_cursor_to_x_position(int x) {
    int file_row = E.row_offset + E.cursor_y;
    struct editor_row *row = (file_row >= E.number_of_rows) ? NULL : &E.row[file_row];
    if (row) { E.cursor_x = x == -1 ? row->size : x; }
    E.desired_cursor_x = x;
}

static void editor_move_cursor_by_arrow_key_input(int key) {
    int file_row = E.row_offset + E.cursor_y;
    int file_column = E.column_offset + E.cursor_x;
    int row_length;
    struct editor_row *row = (file_row >= E.number_of_rows) ? NULL : &E.row[file_row];
    bool vertical_move = false;
    if (key == ARROW_LEFT) {
        if (E.cursor_x == 0) {
            if (E.column_offset) {
                E.column_offset--;
            } else if (file_row > 0) {
                E.cursor_y--;
                E.cursor_x = E.row[file_row - 1].size;
                if (E.cursor_x > E.screen_columns - 1) {
                    E.column_offset = E.cursor_x - E.screen_columns + 1;
                    E.cursor_x = E.screen_columns - 1;
                }
            }
        } else {
            E.cursor_x -= 1;
        }
    } else if (key == ARROW_RIGHT) {
        if (row && file_column < row->size) {
            if (E.cursor_x == E.screen_columns - 1) {
                E.column_offset++;
            } else {
                E.cursor_x += 1;
            }
        } else if (row && file_column == row->size) {
            E.cursor_x = 0;
            E.column_offset = 0;
            if (E.cursor_y == E.screen_rows - 1) {
                E.row_offset++;
            } else {
                E.cursor_y += 1;
            }
        }
    } else if (key == ARROW_UP || key == CTRL_P) {
        if (E.cursor_y == 0) {
            if (E.row_offset) { E.row_offset--; }
        } else {
            E.cursor_y -= 1;
        }
        vertical_move = true;
    } else if (key == ARROW_DOWN || key == CTRL_N) {
        if (file_row < E.number_of_rows) {
            if (E.cursor_y == E.screen_rows - 1) {
                E.row_offset++;
            } else {
                E.cursor_y += 1;
            }
        }
        vertical_move = true;
    }
    // Fix cx if the current line has not enough chars.
    file_row = E.row_offset + E.cursor_y;
    file_column = E.column_offset + E.cursor_x;
    row = (file_row >= E.number_of_rows) ? NULL : &E.row[file_row];
    row_length = row ? row->size : 0;
    if (file_column > row_length) {
        if (vertical_move && (E.desired_cursor_x == -1 || E.cursor_x > E.desired_cursor_x)) {
            E.desired_cursor_x = E.cursor_x;
        }
        E.cursor_x -= file_column - row_length;
        if (E.cursor_x < 0) {
            E.column_offset += E.cursor_x;
            E.cursor_x = 0;
        }
    } else if (vertical_move) {
        if (E.desired_cursor_x != -1) {
            E.cursor_x = E.desired_cursor_x;
            if (E.cursor_x > row_length) {
                E.cursor_x = row_length;
            } else {
                E.desired_cursor_x = -1;
            }
        }
    }
    if (!vertical_move) {
        E.desired_cursor_x = -1;
    }
}

static void editor_recenter_vertically(void) {
    if (E.cursor_y - E.screen_rows / 2 != 0 && E.row_offset + E.cursor_y - E.screen_rows / 2 > 0 &&
            E.row_offset + E.cursor_y + E.screen_rows / 2 < E.number_of_rows) {
        for (int i = 0; i < E.screen_rows / 2; i++) {
            editor_move_cursor_by_arrow_key_input(E.cursor_y - E.screen_rows / 2 < 0 ? ARROW_UP : ARROW_DOWN);
        }
        for (int i = 0; i < E.screen_rows / 2; i++) {
            editor_move_cursor_by_arrow_key_input(E.cursor_y - E.screen_rows / 2 < 0 ? ARROW_DOWN : ARROW_UP);
        }
    }
}

#define SEARCH_QUERY_MAX_LENGTH 256
static void editor_search(void) {
    char query[SEARCH_QUERY_MAX_LENGTH + 1] = { 0 };
    int query_length = 0;
    int last_match = -1; // Last line where a match was found. -1 for none.
    int search_next = 0; // If 1 search next, if -1 search prev.
    int saved_hl_line = -1; // No saved HL
    char *saved_hl = NULL;
#define SEARCH_AND_RESTORE_SYNTAX_HIGHLIGHT_MODE do { \
    if (saved_hl) { \
        memcpy(E.row[saved_hl_line].rendered_chars_syntax_highlight_type, saved_hl, E.row[saved_hl_line].rendered_size); \
        free(saved_hl); \
        saved_hl = NULL; \
    } \
} while (0)
    // Save the cursor position in order to restore it later.
    int saved_cursor_x = E.cursor_x, saved_cursor_y = E.cursor_y;
    int saved_column_offset = E.column_offset, saved_row_offset = E.row_offset;
    while (true) {
        editor_set_status_message("Search (use ESC/Arrows/Enter): %s", query);
        editor_refresh_screen();
        int key = editor_read_key();
        if (key == DEL_KEY || key == BACKSPACE || key == FORWARD_DELETE) {
            if (query_length != 0) { query[--query_length] = '\0'; }
            last_match = -1;
        } else if (key == ESC || key == CTRL_C || key == ENTER) {
            if (key == ESC || key == CTRL_C) {
                E.cursor_x = saved_cursor_x;
                E.cursor_y = saved_cursor_y;
                E.column_offset = saved_column_offset;
                E.row_offset = saved_row_offset;
            }
            SEARCH_AND_RESTORE_SYNTAX_HIGHLIGHT_MODE;
            // Redundant %s to suppress gcc's format-zero-length warning
            editor_set_status_message("%s", "");
            free(saved_hl);
            return;
        } else if (key == ARROW_RIGHT || key == ARROW_DOWN || key == CTRL_S) {
            search_next = 1;
        } else if (key == ARROW_LEFT || key == ARROW_UP || key == CTRL_R) {
            search_next = -1;
        } else if (isprint(key) && query_length < SEARCH_QUERY_MAX_LENGTH) {
            query[query_length++] = key;
            query[query_length] = '\0';
            last_match = -1;
        }
        // Search occurrence.
        if (last_match == -1) { search_next = 1; }
        if (search_next) {
            char *match = NULL;
            int match_offset = 0;
            int current = last_match;
            for (int i = 0; i < E.number_of_rows; i++) {
                current += search_next;
                if (current == -1) {
                    current = E.number_of_rows - 1;
                } else if (current == E.number_of_rows) {
                    current = 0;
                }
                match = strcasestr(E.row[current].rendered_chars, query);
                if (match) {
                    match_offset = match - E.row[current].rendered_chars;
                    break;
                }
            }
            search_next = 0;
            // Highlight
            SEARCH_AND_RESTORE_SYNTAX_HIGHLIGHT_MODE;
            if (match) {
                struct editor_row *row = &E.row[current];
                last_match = current;
                if (row->rendered_chars_syntax_highlight_type) {
                    saved_hl_line = current;
                    free(saved_hl);
                    saved_hl = calloc(row->rendered_size, sizeof(char));
                    memcpy(saved_hl, row->rendered_chars_syntax_highlight_type, row->rendered_size);
                    memset(row->rendered_chars_syntax_highlight_type + match_offset, SYNTAX_HIGHLIGHT_MODE_SEARCH_MATCH, query_length);
                }
                E.cursor_y = 0;
                E.cursor_x = match_offset;
                E.row_offset = current;
                E.column_offset = 0;
                // Scroll horizontally as needed.
                if (E.cursor_x > E.screen_columns) {
                    int diff = E.cursor_x - E.screen_columns;
                    E.cursor_x -= diff;
                    E.column_offset += diff;
                }
                editor_recenter_vertically();
            }
        }
    }
    free(saved_hl);
}

static void console_buffer_close(void) {
    // Restore console to the state before program started
    if (write(STDOUT_FILENO, "\x1b[?9l", 5) == -1) { perror("Write to stdout failed"); exit(EXIT_FAILURE); }
    if (write(STDOUT_FILENO, "\x1b[?47l", 6) == -1) { perror("Write to stdout failed"); exit(EXIT_FAILURE); }
    struct append_buffer ab = { .buffer = NULL, .length = 0 };
    char *buffer = NULL;
    if (asprintf(&buffer, "\x1b[%d;%dH\r\n", E.screen_rows + 1, 1) == -1) { perror("asprintf failed"); exit(EXIT_FAILURE); }
    abuf_append(&ab, buffer, strlen(buffer));
    free(buffer);
    if (write(STDOUT_FILENO, ab.buffer, ab.length) == -1) { perror("Write to stdout failed"); exit(EXIT_FAILURE); }
    abuf_free(&ab);
}

static void editor_process_keypress(void) {
    static int quit_confirmations_left = 1;
    static int previous_key = -1;
    int key = editor_read_key();
    if (previous_key == CTRL_Q) {
        // Quoted insert - insert character as-is
        editor_insert_char(key);
    } else if (key == ENTER) {
        editor_insert_newline();
    } else if (key == BACKSPACE || key == DEL_KEY || key == FORWARD_DELETE) {
        editor_delete_char();
    } else if (key == TAB) {
        for (size_t i = 0; i < 4; i++) { editor_insert_char(' '); }
    } else if (key == PAGE_DOWN || key == PAGE_UP) {
        E.has_open_cut_buffer = false;
        if (key == PAGE_UP && E.cursor_y != 0) {
            E.cursor_y = 0;
        } else if (key == PAGE_DOWN && E.cursor_y != E.screen_rows - 1) {
            E.cursor_y = E.screen_rows - 1;
        }
        int times = E.screen_rows - 2;
        while (times--) {
            editor_move_cursor_by_arrow_key_input(key == PAGE_UP ? ARROW_UP : ARROW_DOWN);
        }
    } else if (key == ARROW_DOWN || key == ARROW_LEFT || key == ARROW_RIGHT || key == ARROW_UP || key == CTRL_N || key == CTRL_P) {
        E.has_open_cut_buffer = false;
        editor_move_cursor_by_arrow_key_input(key);
    } else if (key == CTRL_A) {
        editor_move_cursor_to_x_position(0);
    } else if (key == CTRL_C) {
        if (previous_key != CTRL_X) { return; }
        if (E.dirty && quit_confirmations_left--) {
            editor_set_status_message("WARNING! Unsaved changes. Press ctrl-x + ctrl-c one more time to confirm.");
            previous_key = key;
            return;
        } else {
            console_buffer_close();
            exit(EXIT_SUCCESS);
        }
    } else if (key == CTRL_E) {
        editor_move_cursor_to_x_position(-1);
    } else if (key == CTRL_K) {
        if (E.row_offset + E.cursor_y >= E.number_of_rows) { return; }
        struct editor_row *row = E.row + E.row_offset + E.cursor_y;
        if (!E.has_open_cut_buffer) {
            editor_free_cut_buffer();
        }
        E.has_open_cut_buffer = true;
        E.cut_buffer_lines = realloc(E.cut_buffer_lines, sizeof(char *) * (E.number_of_cut_buffer_lines + 1));
        E.cut_buffer_lines[E.number_of_cut_buffer_lines] = strdup(row->chars);
        E.number_of_cut_buffer_lines += 1;
        editor_delete_row(E.row_offset + E.cursor_y);
    } else if (key == CTRL_L) {
        editor_recenter_vertically();
    } else if (key == CTRL_X || key == ESC) {
        previous_key = key;
        return;
    } else if (key == CTRL_S || (previous_key == CTRL_X && (key == 's' || key == 'S'))) {
        if (previous_key == CTRL_X) {
            editor_save();
        } else {
            editor_search();
        }
    } else if (key == CTRL_Y) {
        for (int i = 0; i < E.number_of_cut_buffer_lines; i++) {
            editor_insert_row(E.row_offset + E.cursor_y, E.cut_buffer_lines[i], strlen(E.cut_buffer_lines[i]));
            editor_move_cursor_by_arrow_key_input(ARROW_DOWN);
        }
    } else if (key == CTRL_Z) {
        console_buffer_close();
        kill(0, SIGTSTP);
    } else {
        if (key >= 0 && key <= 31) {
            editor_set_status_message("Unrecognized command: ASCII %d", key);
        } else {
            editor_insert_char(key);
        }
    }
    quit_confirmations_left = 1; // Reset quit confirmations.
    previous_key = key;
}

static void editor_update_window_size(void) {
    if (get_window_size(&E.screen_rows, &E.screen_columns) == -1) {
        perror("Unable to query the screen for size (columns/rows)");
        exit(EXIT_FAILURE);
    }
    E.screen_rows -= 2; // Get room for status bar.
}

static void editor_handle_sigwinch(int unused __attribute__((unused))) {
    editor_update_window_size();
    if (E.cursor_y > E.screen_rows) {
        E.cursor_y = E.screen_rows - 1;
    }
    if (E.cursor_x > E.screen_columns) {
        E.cursor_x = E.screen_columns - 1;
    }
    editor_refresh_screen();
}

static void editor_handle_sigcont(int unused __attribute__((unused))) {
    editor_disable_raw_mode();
    console_buffer_open();
    editor_enable_raw_mode();
    editor_refresh_screen();
}

static void editor_init(void) {
    E.column_offset = 0;
    E.cursor_x = 0;
    E.cursor_y = 0;
    E.cut_buffer_lines = NULL;
    E.dirty = false;
    E.filename = NULL;
    E.has_open_cut_buffer = false;
    E.number_of_cut_buffer_lines = 0;
    E.number_of_rows = 0;
    E.row = NULL;
    E.row_offset = 0;
    E.status_message = NULL;
    E.syntax_highlight_mode = NULL;
    editor_update_window_size();
    signal(SIGWINCH, editor_handle_sigwinch);
    signal(SIGCONT, editor_handle_sigcont);
    atexit(editor_at_exit);
    editor_enable_raw_mode();
    editor_set_status_message("Commands: ctrl-s = Search | ctrl-x + ctrl-s = Save | ctrl-x + ctrl-c = Quit");
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: openemacs <filename>\n");
        exit(EXIT_FAILURE);
    }
    editor_init();
    editor_open(argv[1]);
    while (true) {
        editor_refresh_screen();
        editor_process_keypress();
    }
}
