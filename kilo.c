/*
 * kilo: A simple editor in less than 1 000 lines of code.
 *
 * The original version of kilo which this work is a fork of was released by
 * Salvatore "antirez" Sanfilippo under the BSD 2-clause license.
 */

#define _DEFAULT_SOURCE /* Linux: ftruncate, getline, kill, strdup */

#include <ctype.h>      /* isdigit, isspace */
#include <errno.h>      /* errno, ENOENT, ENOTTY */
#include <fcntl.h>      /* open, O_CREAT, O_RDWR */
#include <signal.h>     /* kill */
#include <stdarg.h>     /* va_end, va_start */
#include <stdio.h>      /* FILE, fclose, fopen, getline, perror, snprintf, sscanf, stderr, vsnprintf */
#include <stdlib.h>     /* atexit, exit, free, malloc, realloc */
#include <string.h>     /* memcmp, memcpy, memmove, memset, strchr, strlen, strdup, strerror, strstr */
#include <sys/ioctl.h>  /* ioctl */
#include <termios.h>    /* struct termios, tcgetattr, tcsetattr, TCSAFLUSH/BRKINT/ICRNL/INPCK/ISTRIP/IXON/OPOST/CS8/ECHO/ICANON/IEXTEN/ISIG/VMIN/VTIME */
#include <time.h>       /* time */
#include <unistd.h>     /* close, getpid, ftruncate, isatty, read, STDIN_FILENO, STDOUT_FILENO, write */

#define SYNTAX_HIGHLIGHT_TYPE_NORMAL 0
#define SYNTAX_HIGHLIGHT_TYPE_SINGLE_LINE_COMMENT 1
#define SYNTAX_HIGHLIGHT_TYPE_MULTI_LINE_COMMENT 2
#define SYNTAX_HIGHLIGHT_TYPE_KEYWORD_1 3
#define SYNTAX_HIGHLIGHT_TYPE_KEYWORD_2 4
#define SYNTAX_HIGHLIGHT_TYPE_STRING 5
#define SYNTAX_HIGHLIGHT_TYPE_NUMBER 6
#define SYNTAX_HIGHLIGHT_TYPE_SEARCH_MATCH 7

struct editor_syntax {
    char **file_match;
    char **keywords;
    char single_line_comment_start[2];
    char multi_line_comment_start[3];
    char multi_line_comment_end[3];
};

/* This structure represents a single line of the file we are editing. */
typedef struct editor_row {
    int index_in_file;            /* Row index in the file, zero-based. */
    int size;                     /* Size of the row, excluding the null term. */
    int rendered_size;            /* Size of the rendered row. */
    char *chars;                  /* Row content. */
    char *rendered_chars;         /* Row content "rendered" for screen (for TABs). */
    unsigned char *rendered_chars_syntax_highlight_type; /* Syntax highlight type for each character in render.*/
    int has_open_comment;         /* Row had open comment at end in last syntax highlight check. */
} editor_row;

struct editor_config {
    int cursor_x, cursor_y;       /* Cursor x and y position in characters */
    int row_offset;               /* Offset of row displayed. */
    int column_offset;            /* Offset of column displayed. */
    int screen_rows;              /* Number of rows that we can show */
    int screen_columns;           /* Number of columns that we can show */
    int number_of_rows;           /* Number of rows */
    int raw_mode;                 /* Is terminal raw mode enabled? */
    editor_row *row;              /* Rows */
    int dirty;                    /* File modified but not saved. */
    char *filename;               /* Currently open filename */
    char status_message[80];
    time_t status_message_time;
    struct editor_syntax *syntax; /* Current syntax highlight, or NULL. */
};

static struct editor_config E;

enum KEY_ACTION {
    KEY_NULL = 0, CTRL_A = 1, CTRL_C = 3, CTRL_D = 4, CTRL_E = 5, CTRL_F = 6,
    BACKSPACE = 8, TAB = 9, CTRL_L = 12, ENTER = 13, CTRL_N = 14, CTRL_P = 16, CTRL_R = 18,
    CTRL_S = 19, CTRL_U = 21, CTRL_X = 24, CTRL_Z = 26, ESC = 27, FORWARD_DELETE =  127,
    /* The following are just soft codes, not really reported by the
     * terminal directly. */
    ARROW_LEFT = 1000, ARROW_RIGHT, ARROW_UP, ARROW_DOWN, DEL_KEY, HOME_KEY,
    END_KEY, PAGE_UP, PAGE_DOWN
};

/* Set an editor status message for the second line of the status, at the
 * end of the screen. */
void editor_set_status_message(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(E.status_message, sizeof(E.status_message), fmt, ap);
    va_end(ap);
    E.status_message_time = time(NULL);
}

/* =========================== Syntax highlights DB =========================
 *
 * In order to add a new syntax, define two arrays with a list of file name
 * matches and keywords. The file name matches are used in order to match
 * a given syntax with a given file name: if a match pattern starts with a
 * dot, it is matched as the last past of the filename, for example ".c".
 * Otherwise the pattern is just searched inside the filename, like "Makefile").
 *
 * The list of keywords to highlight is just a list of words, however if they
 * a trailing '|' character is added at the end, they are highlighted in
 * a different color, so that you can have two different sets of keywords.
 *
 * Finally add a stanza in the SYNTAX_HIGHLIGHT_DATABASE global variable with two arrays
 * of strings, and a set of flags in order to enable highlighting of
 * comments and numbers.
 *
 * The characters for single and multi line comments must be exactly two
 * and must be provided as well (see the C language example).
 *
 * There is no support to highlight patterns currently. */

/* C/C++ ("class" being C++ only) */
char *C_SYNTAX_HIGHLIGHT_FILE_EXTENSIONS[] = { ".c", ".cpp", NULL };
char *C_SYNTAX_HIGHLIGHT_KEYWORDS[] = {
    /* C/C++ keywords */
    "auto", "break", "case", "class", "const", "continue", "default", "do", "else", "enum", "extern", "for", "goto", "if", "register", "return", "sizeof", "static", "struct", "switch", "typedef", "union", "volatile", "while",
    /* C types */
    "char|", "double|", "float|", "int|", "long|", "short|", "signed|", "unsigned|", "void|",
    /* C preprocessor directives */
    "#define|", "#endif|", "#error|", "#ifdef|", "#ifndef|", "#if|", "#include|", "#undef|", NULL
};

/* Python */
char *PYTHON_SYNTAX_HIGHLIGHT_FILE_EXTENSIONS[] = { ".py", NULL };
char *PYTHON_SYNTAX_HIGHLIGHT_KEYWORDS[] = {
    /* Python keywords */
    "and", "as", "assert", "break", "class", "continue", "def", "del", "elif", "else", "except", "exec", "finally", "for", "from", "global", "if", "import", "in", "is", "lambda", "not", "or", "pass", "print", "raise", "return", "try", "while", "with", "yield",
    /* Python types */
    "buffer|", "bytearray|", "complex|", "False|", "float|", "frozenset|", "int|", "list|", "long|", "None|", "set|", "str|", "tuple|", "True|", "type|", "unicode|", "xrange|", NULL
};

/* Here we define an array of syntax highlights by extensions, keywords,
 * comments delimiters and flags. */
struct editor_syntax SYNTAX_HIGHLIGHT_DATABASE[] = {
    { C_SYNTAX_HIGHLIGHT_FILE_EXTENSIONS,      C_SYNTAX_HIGHLIGHT_KEYWORDS,      "//", "/*", "*/" },
    { PYTHON_SYNTAX_HIGHLIGHT_FILE_EXTENSIONS, PYTHON_SYNTAX_HIGHLIGHT_KEYWORDS, "# ", "",   ""   }
};

#define SYNTAX_HIGHLIGHT_DATABASE_ENTRIES (sizeof(SYNTAX_HIGHLIGHT_DATABASE) / sizeof(SYNTAX_HIGHLIGHT_DATABASE[0]))

/* ======================= Low level terminal handling ====================== */

static struct termios orig_termios; /* In order to restore at exit.*/

void disable_raw_mode(void) {
    /* Don't even check the return value as it's too late. */
    if (E.raw_mode) {
        tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
        E.raw_mode = 0;
    }
}

/* Called at exit to avoid remaining in raw mode. */
void editor_at_exit(void) {
    disable_raw_mode();
}

void console_buffer_open(void) {
    /* switch to another buffer in order to be able to restore state at exit
     * by calling console_buffer_close(void).
     */
    if (write(STDOUT_FILENO, "\x1b[?47h", 6) == -1) perror("Write to stdout failed");
}

/* Raw mode: 1960 magic shit. */
int enable_raw_mode(void) {
    struct termios raw;
    if (E.raw_mode) return 0; /* Already enabled. */
    if (!isatty(STDIN_FILENO)) goto fatal;
    atexit(editor_at_exit);
    if (tcgetattr(STDIN_FILENO, &orig_termios) == -1) goto fatal;
    raw = orig_termios;  /* modify the original mode */
    /* input modes: no break, no CR to NL, no parity check, no strip char,
     * no start/stop output control. */
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    /* output modes - disable post processing */
    raw.c_oflag &= ~OPOST;
    /* control modes - set 8 bit chars */
    raw.c_cflag |= CS8;
    /* local modes - echoing off, canonical off, no extended functions,
     * no signal chars (^Z,^C) */
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    /* control chars - set return condition: min number of bytes and timer. */
    raw.c_cc[VMIN] = 0; /* Return each byte, or zero for timeout. */
    raw.c_cc[VTIME] = 1; /* 100 ms timeout (unit is tens of second). */
    /* put terminal in raw mode after flushing */
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) < 0) goto fatal;
    E.raw_mode = 1;
    console_buffer_open();
    return 0;
fatal:
    errno = ENOTTY;
    return -1;
}

/* Read a key from the terminal put in raw mode, trying to handle
 * escape sequences. */
int editor_read_key(void) {
    int nread;
    char key, seq[3];
    while ((nread = read(STDIN_FILENO, &key, 1)) == 0);
    if (nread == -1) exit(1);
    while (1) {
        switch (key) {
        case ESC: /* escape sequence */
            /* If this is just an ESC, we'll timeout here. */
            if (read(STDIN_FILENO, seq, 1) == 0) return ESC;
            if (read(STDIN_FILENO, seq + 1, 1) == 0) return ESC;
            /* ESC [ sequences. */
            if (seq[0] == '[') {
                if (seq[1] >= '0' && seq[1] <= '9') {
                    /* Extended escape, read additional byte. */
                    if (read(STDIN_FILENO, seq + 2, 1) == 0) return ESC;
                    if (seq[2] == '~') {
                        switch (seq[1]) {
                        case '3':
                            return DEL_KEY;
                        case '5':
                            return PAGE_UP;
                        case '6':
                            return PAGE_DOWN;
                        }
                    }
                } else {
                    switch (seq[1]) {
                    case 'A':
                        return ARROW_UP;
                    case 'B':
                        return ARROW_DOWN;
                    case 'C':
                        return ARROW_RIGHT;
                    case 'D':
                        return ARROW_LEFT;
                    case 'H':
                        return HOME_KEY;
                    case 'F':
                        return END_KEY;
                    }
                }
            }
            /* ESC O sequences. */
            else if (seq[0] == 'O') {
                switch (seq[1]) {
                case 'H':
                    return HOME_KEY;
                case 'F':
                    return END_KEY;
                }
            }
            break;
        default:
            return key;
        }
    }
}

/* Try to get the number of columns in the current terminal.
 * Returns 0 on success, -1 on error. */
int get_window_size(int *rows, int *columns) {
    struct winsize ws;
    if (ioctl(1, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0)
        return -1;
    *columns = ws.ws_col;
    *rows = ws.ws_row;
    return 0;
}

/* ====================== Syntax highlight color scheme  ==================== */

int is_separator(int c) {
    return c == '\0' || isspace(c) || strchr(",.()+-/*=~%[];:", c) != NULL;
}

/* Return true if the specified row last char is part of a multi line comment
 * that starts at this row or at one before, and does not end at the end
 * of the row but spawns to the next row. */
int editor_row_has_open_comment(editor_row *row) {
    if (row->rendered_chars_syntax_highlight_type && row->rendered_size &&
            row->rendered_chars_syntax_highlight_type[row->rendered_size - 1] == SYNTAX_HIGHLIGHT_TYPE_MULTI_LINE_COMMENT &&
            (row->rendered_size < 2 || (row->rendered_chars[row->rendered_size - 2] != '*' || row->rendered_chars[row->rendered_size - 1] != '/'))) return 1;
    return 0;
}

/* Set every byte of row->rendered_chars_syntax_highlight_type (that corresponds to
 * every character in the line) to the right syntax highlight type
 * (SYNTAX_HIGHLIGHT_TYPE_* defines). */
void editor_update_syntax(editor_row *row) {
    row->rendered_chars_syntax_highlight_type = realloc(row->rendered_chars_syntax_highlight_type, row->rendered_size);
    memset(row->rendered_chars_syntax_highlight_type, SYNTAX_HIGHLIGHT_TYPE_NORMAL, row->rendered_size);
    if (E.syntax == NULL) return; /* No syntax, everything is SYNTAX_HIGHLIGHT_TYPE_NORMAL. */
    char *p;
    char **keywords = E.syntax->keywords;
    char *scs = E.syntax->single_line_comment_start;
    char *mcs = E.syntax->multi_line_comment_start;
    char *mce = E.syntax->multi_line_comment_end;
    /* Point to the first non-space char. */
    p = row->rendered_chars;
    int i = 0; /* Current char offset */
    while (*p && isspace(*p)) {
        p++;
        i++;
    }
    int prev_sep = 1; /* Tell the parser if 'i' points to start of word. */
    int in_string = 0; /* Are we inside "" or '' ? */
    int in_comment = 0; /* Are we inside multi-line comment? */
    /* If the previous line has an open comment, this line starts
     * with an open comment state. */
    if (row->index_in_file > 0 && editor_row_has_open_comment(&E.row[row->index_in_file - 1]))
        in_comment = 1;
    while (*p) {
        /* Handle // comments. */
        if (prev_sep && *p == scs[0] && *(p + 1) == scs[1]) {
            /* From here to end is a comment */
            memset(row->rendered_chars_syntax_highlight_type + i, SYNTAX_HIGHLIGHT_TYPE_SINGLE_LINE_COMMENT, row->size - i);
            return;
        }
        /* Handle multi line comments. */
        if (in_comment) {
            row->rendered_chars_syntax_highlight_type[i] = SYNTAX_HIGHLIGHT_TYPE_MULTI_LINE_COMMENT;
            if (*p == mce[0] && *(p + 1) == mce[1]) {
                row->rendered_chars_syntax_highlight_type[i + 1] = SYNTAX_HIGHLIGHT_TYPE_MULTI_LINE_COMMENT;
                p += 2;
                i += 2;
                in_comment = 0;
                prev_sep = 1;
                continue;
            } else {
                prev_sep = 0;
                p++;
                i++;
                continue;
            }
        } else if (*p == mcs[0] && *(p + 1) == mcs[1]) {
            row->rendered_chars_syntax_highlight_type[i] = SYNTAX_HIGHLIGHT_TYPE_MULTI_LINE_COMMENT;
            row->rendered_chars_syntax_highlight_type[i + 1] = SYNTAX_HIGHLIGHT_TYPE_MULTI_LINE_COMMENT;
            p += 2;
            i += 2;
            in_comment = 1;
            prev_sep = 0;
            continue;
        }
        /* Handle "" and '' */
        if (in_string) {
            row->rendered_chars_syntax_highlight_type[i] = SYNTAX_HIGHLIGHT_TYPE_STRING;
            if (*p == '\\') {
                row->rendered_chars_syntax_highlight_type[i + 1] = SYNTAX_HIGHLIGHT_TYPE_STRING;
                p += 2;
                i += 2;
                prev_sep = 0;
                continue;
            }
            if (*p == in_string) in_string = 0;
            p++;
            i++;
            continue;
        } else {
            if (*p == '"' || *p == '\'') {
                in_string = *p;
                row->rendered_chars_syntax_highlight_type[i] = SYNTAX_HIGHLIGHT_TYPE_STRING;
                p++;
                i++;
                prev_sep = 0;
                continue;
            }
        }
        /* Handle numbers */
        if ((isdigit(*p) && (prev_sep || row->rendered_chars_syntax_highlight_type[i - 1] == SYNTAX_HIGHLIGHT_TYPE_NUMBER)) ||
                (*p == '.' && i > 0 && row->rendered_chars_syntax_highlight_type[i - 1] == SYNTAX_HIGHLIGHT_TYPE_NUMBER)) {
            row->rendered_chars_syntax_highlight_type[i] = SYNTAX_HIGHLIGHT_TYPE_NUMBER;
            p++;
            i++;
            prev_sep = 0;
            continue;
        }
        /* Handle keywords and lib calls */
        if (prev_sep) {
            int j;
            for (j = 0; keywords[j]; j++) {
                int klen = strlen(keywords[j]);
                int kw2 = keywords[j][klen - 1] == '|';
                if (kw2) klen--;
                if (!memcmp(p, keywords[j], klen) && is_separator(*(p + klen))) {
                    /* Keyword */
                    memset(row->rendered_chars_syntax_highlight_type + i, kw2 ? SYNTAX_HIGHLIGHT_TYPE_KEYWORD_2 : SYNTAX_HIGHLIGHT_TYPE_KEYWORD_1, klen);
                    p += klen;
                    i += klen;
                    break;
                }
            }
            if (keywords[j] != NULL) {
                prev_sep = 0;
                continue; /* We had a keyword match */
            }
        }
        /* Not special chars */
        prev_sep = is_separator(*p);
        p++;
        i++;
    }
    /* Propagate syntax change to the next row if the open comment
     * state changed. This may recursively affect all the following rows
     * in the file. */
    int oc = editor_row_has_open_comment(row);
    if (row->has_open_comment != oc && row->index_in_file + 1 < E.number_of_rows)
        editor_update_syntax(&E.row[row->index_in_file + 1]);
    row->has_open_comment = oc;
}

/* Maps syntax highlight token types to terminal colors. */
int editor_syntax_to_color(int hl) {
    switch (hl) {
    case SYNTAX_HIGHLIGHT_TYPE_SINGLE_LINE_COMMENT:
    case SYNTAX_HIGHLIGHT_TYPE_MULTI_LINE_COMMENT:
        return 31; /* normal red */
    case SYNTAX_HIGHLIGHT_TYPE_KEYWORD_1:
        return 35; /* normal magenta */
    case SYNTAX_HIGHLIGHT_TYPE_KEYWORD_2:
        return 32; /* normal green */
    case SYNTAX_HIGHLIGHT_TYPE_STRING:
        return 95; /* bright magenta */
    case SYNTAX_HIGHLIGHT_TYPE_NUMBER:
        return 97; /* bright white */
    case SYNTAX_HIGHLIGHT_TYPE_SEARCH_MATCH:
        return 96; /* bright cyan */
    default:
        return 37; /* normal white */
    }
}

/* Select the syntax highlight scheme depending on the filename,
 * setting it in the global state E.syntax. */
void editor_select_syntax_highlight(char *filename) {
    for (unsigned int j = 0; j < SYNTAX_HIGHLIGHT_DATABASE_ENTRIES; j++) {
        struct editor_syntax *s = SYNTAX_HIGHLIGHT_DATABASE + j;
        unsigned int i = 0;
        while (s->file_match[i]) {
            char *p;
            int patlen = strlen(s->file_match[i]);
            if ((p = strstr(filename, s->file_match[i])) != NULL)
                if (s->file_match[i][0] != '.' || p[patlen] == '\0') {
                    E.syntax = s;
                    return;
                }
            i++;
        }
    }
}

/* ======================= Editor rows implementation ======================= */

/* Update the rendered version and the syntax highlight of a row. */
void editor_update_row(editor_row *row) {
    int tabs = 0;
    /* Create a version of the row we can directly print on the screen,
      * respecting tabs, substituting non printable characters with '?'. */
    free(row->rendered_chars);
    for (int i = 0; i < row->size; i++)
        if (row->chars[i] == TAB) tabs++;
    int nonprint = 0;
    row->rendered_chars = malloc(row->size + tabs * 8 + nonprint * 9 + 1);
    int local_index = 0;
    for (int j = 0; j < row->size; j++) {
        if (row->chars[j] == TAB) {
            row->rendered_chars[local_index++] = ' ';
            while ((local_index + 1) % 8 != 0) row->rendered_chars[local_index++] = ' ';
        } else if (!isprint(row->chars[j])) {
            row->rendered_chars[local_index++] = '?';
        } else {
            row->rendered_chars[local_index++] = row->chars[j];
        }
    }
    row->rendered_size = local_index;
    row->rendered_chars[local_index] = '\0';
    /* Update the syntax highlighting attributes of the row. */
    editor_update_syntax(row);
}

/* Insert a row at the specified position, shifting the other rows on the bottom
 * if required. */
void editor_insert_row(int at, char *s, size_t len) {
    if (at > E.number_of_rows) return;
    E.row = realloc(E.row, sizeof(editor_row) * (E.number_of_rows + 1));
    if (at != E.number_of_rows) {
        memmove(E.row + at + 1, E.row + at, sizeof(E.row[0]) * (E.number_of_rows - at));
        for (int j = at + 1; j <= E.number_of_rows; j++) E.row[j].index_in_file++;
    }
    E.row[at].size = len;
    E.row[at].chars = malloc(len + 1);
    memcpy(E.row[at].chars, s, len + 1);
    E.row[at].rendered_chars_syntax_highlight_type = NULL;
    E.row[at].has_open_comment = 0;
    E.row[at].rendered_chars = NULL;
    E.row[at].rendered_size = 0;
    E.row[at].index_in_file = at;
    editor_update_row(E.row + at);
    E.number_of_rows++;
    E.dirty++;
}

/* Free row's heap allocated stuff. */
void editor_free_row(editor_row *row) {
    free(row->rendered_chars);
    free(row->chars);
    free(row->rendered_chars_syntax_highlight_type);
}

/* Remove the row at the specified position, shifting the remaining on the
 * top. */
void editor_delete_row(int at) {
    editor_row *row;
    if (at >= E.number_of_rows) return;
    row = E.row + at;
    editor_free_row(row);
    memmove(E.row + at, E.row + at + 1, sizeof(E.row[0]) * (E.number_of_rows - at - 1));
    for (int j = at; j < E.number_of_rows - 1; j++) E.row[j].index_in_file++;
    E.number_of_rows--;
    E.dirty++;
}

/* Turn the editor rows into a single heap-allocated string.
 * Returns the pointer to the heap-allocated string and populate the
 * integer pointed by 'buflen' with the size of the string, excluding
 * the final nulterm. */
char *editor_rows_to_string(int *buflen) {
    char *buf = NULL, *p;
    int totlen = 0;
    /* Compute count of bytes */
    for (int j = 0; j < E.number_of_rows; j++)
        totlen += E.row[j].size + 1; /* +1 is for "\n" at end of every row */
    *buflen = totlen;
    totlen++; /* Also make space for nulterm */
    p = buf = malloc(totlen);
    for (int j = 0; j < E.number_of_rows; j++) {
        memcpy(p, E.row[j].chars, E.row[j].size);
        p += E.row[j].size;
        *p = '\n';
        p++;
    }
    *p = '\0';
    return buf;
}

/* Insert a character at the specified position in a row, moving the remaining
 * chars on the right if needed. */
void editor_row_insert_char(editor_row *row, int at, int c) {
    if (at > row->size) {
        /* Pad the string with spaces if the insert location is outside the
         * current length by more than a single character. */
        int padlen = at - row->size;
        /* In the next line +2 means: new char and null term. */
        row->chars = realloc(row->chars, row->size + padlen + 2);
        memset(row->chars + row->size, ' ', padlen);
        row->chars[row->size + padlen + 1] = '\0';
        row->size += padlen + 1;
    } else {
        /* If we are in the middle of the string just make space for 1 new
         * char plus the (already existing) null term. */
        row->chars = realloc(row->chars, row->size + 2);
        memmove(row->chars + at + 1, row->chars + at, row->size - at + 1);
        row->size++;
    }
    row->chars[at] = c;
    editor_update_row(row);
    E.dirty++;
}

/* Append the string 's' at the end of a row */
void editor_row_append_string(editor_row *row, char *s, size_t len) {
    row->chars = realloc(row->chars, row->size + len + 1);
    memcpy(row->chars + row->size, s, len);
    row->size += len;
    row->chars[row->size] = '\0';
    editor_update_row(row);
    E.dirty++;
}

/* Delete the character at offset 'at' from the specified row. */
void editor_row_delete_char(editor_row *row, int at) {
    if (row->size <= at) return;
    memmove(row->chars + at, row->chars + at + 1, row->size - at);
    editor_update_row(row);
    row->size--;
    E.dirty++;
}

/* Insert the specified char at the current prompt position. */
void editor_insert_char(int c) {
    int file_row = E.row_offset + E.cursor_y;
    int file_column = E.column_offset + E.cursor_x;
    editor_row *row = (file_row >= E.number_of_rows) ? NULL : &E.row[file_row];
    /* If the row where the cursor is currently located does not exist in our
     * logical representation of the file, add enough empty rows as needed. */
    if (!row)
        while (E.number_of_rows <= file_row)
            editor_insert_row(E.number_of_rows, "", 0);
    row = &E.row[file_row];
    editor_row_insert_char(row, file_column, c);
    if (E.cursor_x == E.screen_columns - 1)
        E.column_offset++;
    else
        E.cursor_x++;
    E.dirty++;
}

/* Inserting a newline is slightly complex as we have to handle inserting a
 * newline in the middle of a line, splitting the line as needed. */
void editor_insert_newline(void) {
    int file_row = E.row_offset + E.cursor_y;
    int file_column = E.column_offset + E.cursor_x;
    editor_row *row = (file_row >= E.number_of_rows) ? NULL : &E.row[file_row];
    if (!row) {
        if (file_row == E.number_of_rows) {
            editor_insert_row(file_row, "", 0);
            goto fix_cursor;
        }
        return;
    }
    /* If the cursor is over the current line size, we want to conceptually
     * think it's just over the last character. */
    if (file_column >= row->size) file_column = row->size;
    if (file_column == 0) {
        editor_insert_row(file_row, "", 0);
    } else {
        /* We are in the middle of a line. Split it between two rows. */
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

/* Delete the char at the current prompt position. */
void editor_delete_char(void) {
    int file_row = E.row_offset + E.cursor_y;
    int file_column = E.column_offset + E.cursor_x;
    editor_row *row = (file_row >= E.number_of_rows) ? NULL : &E.row[file_row];
    if (!row || (file_column == 0 && file_row == 0)) return;
    if (file_column == 0) {
        /* Handle the case of column 0, we need to move the current line
         * on the right of the previous one. */
        file_column = E.row[file_row - 1].size;
        editor_row_append_string(&E.row[file_row - 1], row->chars, row->size);
        editor_delete_row(file_row);
        row = NULL;
        if (E.cursor_y == 0)
            E.row_offset--;
        else
            E.cursor_y--;
        E.cursor_x = file_column;
        if (E.cursor_x >= E.screen_columns) {
            int shift = (E.screen_columns - E.cursor_x) + 1;
            E.cursor_x -= shift;
            E.column_offset += shift;
        }
    } else {
        editor_row_delete_char(row, file_column - 1);
        if (E.cursor_x == 0 && E.column_offset)
            E.column_offset--;
        else
            E.cursor_x--;
    }
    if (row) editor_update_row(row);
    E.dirty++;
}

/* Load the specified program in the editor memory and returns 0 on success
 * or 1 on error. */
int editor_open(char *filename) {
    FILE *fp;
    E.dirty = 0;
    free(E.filename);
    E.filename = strdup(filename);
    fp = fopen(filename, "r");
    if (!fp) {
        if (errno != ENOENT) {
            perror("Opening file");
            exit(1);
        }
        return 1;
    }
    char *line = NULL;
    size_t linecap = 0;
    ssize_t linelen;
    while ((linelen = getline(&line, &linecap, fp)) != -1) {
        if (linelen && (line[linelen - 1] == '\n' || line[linelen - 1] == '\r')) line[--linelen] = '\0';
        editor_insert_row(E.number_of_rows, line, linelen);
    }
    free(line);
    fclose(fp);
    E.dirty = 0;
    return 0;
}

/* Save the current file on disk. Return 0 on success, 1 on error. */
int editor_save(void) {
    int len;
    char *buf = editor_rows_to_string(&len);
    int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
    if (fd == -1) goto write_error;
    /* Use truncate + a single write(2) call in order to make saving
     * a bit safer, under the limits of what we can do in a small editor. */
    if (ftruncate(fd, len) == -1) goto write_error;
    if (write(fd, buf, len) != len) goto write_error;
    close(fd);
    free(buf);
    E.dirty = 0;
    editor_set_status_message("Wrote %s (%d bytes)", E.filename, len);
    return 0;
write_error:
    free(buf);
    if (fd != -1) close(fd);
    editor_set_status_message("Can't save! I/O error: %s", strerror(errno));
    return 1;
}

/* ============================= Terminal update ============================ */

/* We define a very simple "append buffer" structure, that is an heap
 * allocated string where we can append to. This is useful in order to
 * write all the escape sequences in a buffer and flush them to the standard
 * output in a single call, to avoid flickering effects. */
struct append_buffer {
    char *b;
    int len;
};

#define APPEND_BUFFER_INIT { NULL, 0 }

void abuf_append(struct append_buffer *ab, const char *s, int len) {
    char *new = realloc(ab->b, ab->len + len);
    if (new == NULL) return;
    memcpy(new + ab->len, s, len);
    ab->b = new;
    ab->len += len;
}

void abuf_free(struct append_buffer *ab) {
    free(ab->b);
}

/* This function writes the whole screen using VT100 escape characters
 * starting from the logical state of the editor in the global state 'E'. */
void editor_refresh_screen(void) {
    editor_row *r;
    char buf[32];
    struct append_buffer ab = APPEND_BUFFER_INIT;
    abuf_append(&ab, "\x1b[?25l", 6); /* Hide cursor. */
    abuf_append(&ab, "\x1b[H", 3);    /* Go home. */
    for (int y = 0; y < E.screen_rows; y++) {
        int file_row = E.row_offset + y;
        if (file_row >= E.number_of_rows) {
            abuf_append(&ab, "~\x1b[0K\r\n", 7);
            continue;
        }
        r = &E.row[file_row];
        int len = r->rendered_size - E.column_offset;
        int current_color = -1;
        if (len > 0) {
            if (len > E.screen_columns) len = E.screen_columns;
            char *c = r->rendered_chars + E.column_offset;
            unsigned char *rendered_chars_syntax_highlight_type = r->rendered_chars_syntax_highlight_type + E.column_offset;
            for (int j = 0; j < len; j++) {
                if (rendered_chars_syntax_highlight_type[j] == SYNTAX_HIGHLIGHT_TYPE_NORMAL) {
                    if (current_color != -1) {
                        abuf_append(&ab, "\x1b[39m", 5);
                        current_color = -1;
                    }
                    abuf_append(&ab, c + j, 1);
                } else {
                    int color = editor_syntax_to_color(rendered_chars_syntax_highlight_type[j]);
                    if (color != current_color) {
                        char buf[16];
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
                        current_color = color;
                        abuf_append(&ab, buf, clen);
                    }
                    abuf_append(&ab, c + j, 1);
                }
            }
        }
        abuf_append(&ab, "\x1b[39m", 5);
        abuf_append(&ab, "\x1b[0K", 4);
        abuf_append(&ab, "\r\n", 2);
    }
    /* Create a two rows status. First row: */
    abuf_append(&ab, "\x1b[0K", 4);
    abuf_append(&ab, "\x1b[7m", 4);
    char status[80];
    int len = snprintf(status, sizeof(status), "Editing: %.20s%s | Line: %d/%d (%d %%) | Column: %d", E.filename, E.dirty ? " (modified)" : "",
                       E.row_offset + E.cursor_y + 1 <= E.number_of_rows ? E.row_offset + E.cursor_y + 1 : E.number_of_rows, E.number_of_rows, E.number_of_rows > 0
                       && E.row_offset + E.cursor_y + 1 < E.number_of_rows ? 100 * (E.row_offset + E.cursor_y + 1) / E.number_of_rows : 100, E.cursor_x + 1);
    if (len > E.screen_columns) len = E.screen_columns;
    abuf_append(&ab, status, len);
    while (len++ < E.screen_columns)
        abuf_append(&ab, " ", 1);
    abuf_append(&ab, "\x1b[0m\r\n", 6);
    /* Second row depends on E.status_message and the status message update time. */
    abuf_append(&ab, "\x1b[0K", 4);
    int msglen = strlen(E.status_message);
    if (msglen && time(NULL) - E.status_message_time < 5)
        abuf_append(&ab, E.status_message, msglen <= E.screen_columns ? msglen : E.screen_columns);
    /* Put cursor at its current position. Note that the horizontal position
     * at which the cursor is displayed may be different compared to 'E.cursor_x'
     * because of TABs. */
    int cx = 1;
    int file_row = E.row_offset + E.cursor_y;
    editor_row *row = (file_row >= E.number_of_rows) ? NULL : &E.row[file_row];
    if (row)
        for (int j = E.column_offset; j < (E.cursor_x + E.column_offset); j++) {
            if (j < row->size && row->chars[j] == TAB) cx += 7 - ((cx) % 8);
            cx++;
        }
    snprintf(buf, sizeof(buf), "\x1b[%d;%dH", E.cursor_y + 1, cx);
    abuf_append(&ab, buf, strlen(buf));
    abuf_append(&ab, "\x1b[?25h", 6); /* Show cursor. */
    if (write(STDOUT_FILENO, ab.b, ab.len) == -1) perror("Write to stdout failed");
    abuf_free(&ab);
}

/* =============================== Search mode ================================ */

#define SEARCH_QUERY_LENGTH 256

/* Move cursor to X position (0: start of line, -1 == end of line) */
void editor_move_cursor_to_x_position(int i) {
    int file_row = E.row_offset + E.cursor_y;
    editor_row *row = (file_row >= E.number_of_rows) ? NULL : &E.row[file_row];
    if (row) E.cursor_x = i == -1 ? row->size : 0;
}

void editor_search(void) {
    char query[SEARCH_QUERY_LENGTH + 1] = { 0 };
    int qlen = 0;
    int last_match = -1; /* Last line where a match was found. -1 for none. */
    int search_next = 0; /* if 1 search next, if -1 search prev. */
    int saved_hl_line = -1;  /* No saved HL */
    char *saved_hl = NULL;
#define SEARCH_AND_RESTORE_SYNTAX_HIGHLIGHT_TYPE do { \
    if (saved_hl) { \
        memcpy(E.row[saved_hl_line].rendered_chars_syntax_highlight_type, saved_hl, E.row[saved_hl_line].rendered_size); \
        saved_hl = NULL; \
    } \
} while (0)
    /* Save the cursor position in order to restore it later. */
    int saved_cx = E.cursor_x, saved_cy = E.cursor_y;
    int saved_column_offset = E.column_offset, saved_row_offset = E.row_offset;
    while (1) {
        editor_set_status_message("Search: %s (Use ESC/Arrows/Enter)", query);
        editor_refresh_screen();
        int c = editor_read_key();
        if (c == DEL_KEY || c == BACKSPACE || c == FORWARD_DELETE) {
            if (qlen != 0) query[--qlen] = '\0';
            last_match = -1;
        } else if (c == ESC || c == CTRL_C || c == ENTER) {
            if (c == ESC || c == CTRL_C) {
                E.cursor_x = saved_cx;
                E.cursor_y = saved_cy;
                E.column_offset = saved_column_offset;
                E.row_offset = saved_row_offset;
            }
            SEARCH_AND_RESTORE_SYNTAX_HIGHLIGHT_TYPE;
            editor_set_status_message("");
            return;
        } else if (c == ARROW_RIGHT || c == ARROW_DOWN || c == CTRL_S) {
            search_next = 1;
        } else if (c == ARROW_LEFT || c == ARROW_UP || c == CTRL_R) {
            search_next = -1;
        } else if (isprint(c)) {
            if (qlen < SEARCH_QUERY_LENGTH) {
                query[qlen++] = c;
                query[qlen] = '\0';
                last_match = -1;
            }
        }
        /* Search occurrence. */
        if (last_match == -1) search_next = 1;
        if (search_next) {
            char *match = NULL;
            int match_offset = 0;
            int current = last_match;
            for (int i = 0; i < E.number_of_rows; i++) {
                current += search_next;
                if (current == -1) current = E.number_of_rows - 1;
                else if (current == E.number_of_rows) current = 0;
                match = strstr(E.row[current].rendered_chars, query);
                if (match) {
                    match_offset = match - E.row[current].rendered_chars;
                    break;
                }
            }
            search_next = 0;
            /* Highlight */
            SEARCH_AND_RESTORE_SYNTAX_HIGHLIGHT_TYPE;
            if (match) {
                editor_row *row = &E.row[current];
                last_match = current;
                if (row->rendered_chars_syntax_highlight_type) {
                    saved_hl_line = current;
                    saved_hl = malloc(row->rendered_size);
                    memcpy(saved_hl, row->rendered_chars_syntax_highlight_type, row->rendered_size);
                    memset(row->rendered_chars_syntax_highlight_type + match_offset, SYNTAX_HIGHLIGHT_TYPE_SEARCH_MATCH, qlen);
                }
                E.cursor_y = 0;
                E.cursor_x = match_offset;
                E.row_offset = current;
                E.column_offset = 0;
                /* Scroll horizontally as needed. */
                if (E.cursor_x > E.screen_columns) {
                    int diff = E.cursor_x - E.screen_columns;
                    E.cursor_x -= diff;
                    E.column_offset += diff;
                }
            }
        }
    }
}

/* ========================= Editor events handling  ======================== */

/* Handle cursor position change because arrow keys were pressed. */
void editor_move_cursor_to_y_position_by_arrow_key_input(int key) {
    int file_row = E.row_offset + E.cursor_y;
    int file_column = E.column_offset + E.cursor_x;
    int rowlen;
    editor_row *row = (file_row >= E.number_of_rows) ? NULL : &E.row[file_row];
    switch (key) {
    case ARROW_LEFT:
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
        break;
    case ARROW_RIGHT:
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
        break;
    case ARROW_UP:
    case CTRL_P:
        if (E.cursor_y == 0) {
            if (E.row_offset) E.row_offset--;
        } else {
            E.cursor_y -= 1;
        }
        break;
    case ARROW_DOWN:
    case CTRL_N:
        if (file_row < E.number_of_rows) {
            if (E.cursor_y == E.screen_rows - 1) {
                E.row_offset++;
            } else {
                E.cursor_y += 1;
            }
        }
        break;
    }
    /* Fix cx if the current line has not enough chars. */
    file_row = E.row_offset + E.cursor_y;
    file_column = E.column_offset + E.cursor_x;
    row = (file_row >= E.number_of_rows) ? NULL : &E.row[file_row];
    rowlen = row ? row->size : 0;
    if (file_column > rowlen) {
        E.cursor_x -= file_column - rowlen;
        if (E.cursor_x < 0) {
            E.column_offset += E.cursor_x;
            E.cursor_x = 0;
        }
    }
}

void console_buffer_close(void) {
    /* restore console to the state before program started */
    if (write(STDOUT_FILENO, "\x1b[?9l", 5) == -1) perror("Write to stdout failed");
    if (write(STDOUT_FILENO, "\x1b[?47l", 6) == -1) perror("Write to stdout failed");
    char buf[32];
    struct append_buffer ab = APPEND_BUFFER_INIT;
    snprintf(buf, sizeof(buf), "\x1b[%d;%dH\r\n", E.screen_rows + 1, 1);
    abuf_append(&ab, buf, strlen(buf));
    if (write(STDOUT_FILENO, ab.b, ab.len) == -1) perror("Write to stdout failed");
    abuf_free(&ab);
}

/* Process events arriving from the standard input, which is, the user
 * is typing stuff on the terminal. */
#define QUIT_CONFIRMATIONS 3
void editor_process_keypress(void) {
    /* When the file is modified, requires ctrl-c to be pressed N times
     * before actually quitting. */
    static int quit_times = QUIT_CONFIRMATIONS;
    static int previous_key = -1;
    int key = editor_read_key();
    switch (key) {
    case ENTER:
        editor_insert_newline();
        break;
    case CTRL_A:
        /* Go to start of line */
        editor_move_cursor_to_x_position(0);
        break;
    case CTRL_C:
        if (previous_key != CTRL_X) break;
        /* Quit if the file was already saved. */
        if (E.dirty && quit_times) {
            editor_set_status_message("WARNING! File has unsaved changes. Press ctrl-c %d more times to quit.", quit_times);
            quit_times--;
            return;
        } else {
            console_buffer_close();
            exit(0);
        }
        break;
    case CTRL_S:
        if (previous_key == CTRL_X)
            editor_save();
        else
            editor_search();
        break;
    case CTRL_E:
        /* Go to end of line */
        editor_move_cursor_to_x_position(-1);
        break;
    case BACKSPACE:
    case DEL_KEY:
    case FORWARD_DELETE:
        editor_delete_char();
        break;
    case PAGE_DOWN:
    case PAGE_UP:
        if (key == PAGE_UP && E.cursor_y != 0)
            E.cursor_y = 0;
        else if (key == PAGE_DOWN && E.cursor_y != E.screen_rows - 1)
            E.cursor_y = E.screen_rows - 1;
        int times = E.screen_rows - 2;
        while (times--)
            editor_move_cursor_to_y_position_by_arrow_key_input(key == PAGE_UP ? ARROW_UP : ARROW_DOWN);
        break;
    case ARROW_DOWN:
    case ARROW_LEFT:
    case ARROW_RIGHT:
    case ARROW_UP:
    case CTRL_N:
    case CTRL_P:
        editor_move_cursor_to_y_position_by_arrow_key_input(key);
        break;
    case CTRL_L:
    case CTRL_X:
        /* Just refresh the line as side effect. */
        break;
    case CTRL_Z:
        /* Suspend process. */
        console_buffer_close();
        kill(getpid(), SIGTSTP);
        break;
    case ESC:
        /* Nothing to do for ESC in this mode. */
        break;
    case TAB:
        for (int i = 0; i < 4; i++) editor_insert_char(' ');
        break;
    default:
        if (key >= 0 && key <= 31) {
            editor_set_status_message("Unrecognized command: ASCII %d", key);
            break;
        }
        editor_insert_char(key);
        break;
    }
    quit_times = QUIT_CONFIRMATIONS; /* Reset it to the original value. */
    previous_key = key;
}

void update_window_size(void) {
    if (get_window_size(&E.screen_rows, &E.screen_columns) == -1) {
        perror("Unable to query the screen for size (columns/rows)");
        exit(1);
    }
    E.screen_rows -= 2; /* Get room for status bar. */
}

void handle_sigwinch(int unused __attribute__((unused))) {
    update_window_size();
    if (E.cursor_y > E.screen_rows)
        E.cursor_y = E.screen_rows - 1;
    if (E.cursor_x > E.screen_columns)
        E.cursor_x = E.screen_columns - 1;
    editor_refresh_screen();
}

void handle_sigcont(int unused __attribute__((unused))) {
    disable_raw_mode();
    console_buffer_open();
    enable_raw_mode();
    editor_refresh_screen();
}

void init_editor(void) {
    E.cursor_x = 0;
    E.cursor_y = 0;
    E.row_offset = 0;
    E.column_offset = 0;
    E.number_of_rows = 0;
    E.row = NULL;
    E.dirty = 0;
    E.filename = NULL;
    E.syntax = NULL;
    update_window_size();
    signal(SIGWINCH, handle_sigwinch);
    signal(SIGCONT, handle_sigcont);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: kilo <filename>\n");
        exit(1);
    }
    init_editor();
    editor_select_syntax_highlight(argv[1]);
    editor_open(argv[1]);
    enable_raw_mode();
    editor_set_status_message("Commands: ctrl-s = Search | ctrl-x + ctrl-s = Save | ctrl-x + ctrl-c = Quit");
    while (1) {
        editor_refresh_screen();
        editor_process_keypress();
    }
    return 0;
}
