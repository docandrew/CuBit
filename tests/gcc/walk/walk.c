/* archive-walk (tests/gcc): read an ar archive's member headers three ways
   (pread, lseek + read, mmap of the pages holding each header) and report
   the first header a method reads wrongly. A diagnostic for ld's
   "malformed archive" on CuBit; prints to stderr, exits 0 when all agree. */
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

enum { MAGIC_BYTES = 8, HEADER_BYTES = 60, SIZE_AT = 48, SIZE_BYTES = 10,
       END_AT = 58, PAGE = 4096 };

static long member_size(const char *h) {
    char text[SIZE_BYTES + 1];
    memcpy(text, h + SIZE_AT, SIZE_BYTES);
    text[SIZE_BYTES] = 0;
    return strtol(text, NULL, 10);
}

/* Bytes as text, unprintable ones as '.'. */
static void show(const char *what, long at, const char *b, int n) {
    char text[128];
    for (int k = 0; k < n && k < (int)sizeof text - 1; k++)
        text[k] = (b[k] >= ' ' && b[k] < 127) ? b[k] : '.';
    text[n < (int)sizeof text - 1 ? n : (int)sizeof text - 1] = 0;
    fprintf(stderr, "walk: %s at %ld: [%s]\n", what, at, text);
}

static int header_ok(const char *h) { return h[END_AT] == '`' && h[END_AT + 1] == '\n'; }

enum method { BY_PREAD, BY_SEEK, BY_MMAP };
static const char *names[] = { "pread", "lseek+read", "mmap" };

static int read_header(int fd, enum method m, long at, char *h) {
    if (m == BY_PREAD)
        return pread(fd, h, HEADER_BYTES, at) == HEADER_BYTES;
    if (m == BY_SEEK)
        return lseek(fd, at, SEEK_SET) == at && read(fd, h, HEADER_BYTES) == HEADER_BYTES;
    long base = at / PAGE * PAGE;
    size_t length = (size_t)(at - base + HEADER_BYTES + PAGE - 1) / PAGE * PAGE;
    char *p = mmap(NULL, length, PROT_READ, MAP_PRIVATE, fd, base);
    if (p == MAP_FAILED) return 0;
    memcpy(h, p + (at - base), HEADER_BYTES);
    munmap(p, length);
    return 1;
}

int main(int argc, char **argv) {
    if (argc != 2) return 2;
    int fd = open(argv[1], O_RDONLY);
    struct stat st;
    if (fd < 0 || fstat(fd, &st) != 0) { fprintf(stderr, "walk: cannot open %s\n", argv[1]); return 2; }
    fprintf(stderr, "walk: %s, %ld bytes\n", argv[1], (long)st.st_size);
    int failed = 0;
    for (int m = BY_PREAD; m <= BY_MMAP; m++) {
        long at = MAGIC_BYTES, count = 0;
        char h[HEADER_BYTES];
        while (at < st.st_size) {
            if (!read_header(fd, m, at, h) || !header_ok(h)) {
                fprintf(stderr, "walk: %s: bad header %ld at offset %ld\n", names[m], count, at);
                show("read", at, h, HEADER_BYTES);
                if (m == BY_PREAD) {
                    /* The two blocks the header spans, each read alone. */
                    char b[64];
                    long first = at / PAGE * PAGE;
                    for (long block = first; block <= first + PAGE; block += PAGE) {
                        if (pread(fd, b, sizeof b, block) == (ssize_t)sizeof b)
                            show("block start", block, b, sizeof b);
                    }
                    if (pread(fd, b, sizeof b, first + PAGE - sizeof b) == (ssize_t)sizeof b)
                        show("block end", first + PAGE - (long)sizeof b, b, sizeof b);
                }
                failed = 1;
                break;
            }
            long size = member_size(h);
            at += HEADER_BYTES + size + (size & 1);
            count++;
        }
        if (at >= st.st_size)
            fprintf(stderr, "walk: %s: %ld headers OK\n", names[m], count);
    }
    return failed;
}
