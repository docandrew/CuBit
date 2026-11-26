/* The unix.stdin inlet (CuBit.Inlet_Rings): read descriptor 0 to its end.
 * Built twice: stdin-check.app declares the inlet on descriptor 0 and must
 * read the launcher's lines, "line 0" .. "line <N-1>" in order, then the
 * end of the stream; stdin-none.app (NO_INLET) declares none, so
 * descriptor 0 is not open (EBADF, never an implicit console). Verdicts on
 * the debug console. */
#define _GNU_SOURCE
#include <errno.h>
#include <poll.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <cubit/debug.h>

static int failures;

static void say(const char *name, int ok)
{
	char line[200];
	int n = snprintf(line, sizeof line, "%s: %s %s\n",
#ifdef NO_INLET
	                 "stdin-none",
#else
	                 "stdin-check",
#endif
	                 name, ok ? "PASS" : "FAIL");
	cubit_debug_write(line, (size_t)n);
	if (!ok) failures++;
}

int main(void)
{
#ifdef NO_INLET
	char byte;
	errno = 0;
	say("read(0) without a declared inlet is EBADF", read(0, &byte, 1) == -1 && errno == EBADF);
	struct pollfd p = { .fd = 0, .events = POLLIN };
	say("poll reports descriptor 0 not open", poll(&p, 1, 0) == 1 && (p.revents & POLLNVAL));
#else
	/* The launcher writes LINES lines; the ring is far smaller, so the
	 * stream only arrives whole if the launcher waits for this reader. */
	enum { LINES = 3000 };
	char buffer[777], expected[32], line[32];
	size_t used = 0;
	long lines = 0, bytes = 0;
	int in_order = 1;
	for (;;) {
		ssize_t got = read(0, buffer, sizeof buffer);
		if (got < 0) {
			say("read(0) never fails", 0);
			break;
		}
		if (got == 0)
			break;
		bytes += got;
		for (ssize_t i = 0; i < got; i++) {
			if (buffer[i] != '\n') {
				if (used < sizeof line - 1)
					line[used++] = buffer[i];
				continue;
			}
			line[used] = 0;
			snprintf(expected, sizeof expected, "line %ld", lines);
			if (strcmp(line, expected) != 0)
				in_order = 0;
			lines++;
			used = 0;
		}
	}
	say("every line arrives, in order", in_order && lines == LINES && used == 0);
	say("read(0) at the end of the stream is 0", read(0, buffer, 1) == 0);
	struct pollfd p = { .fd = 0, .events = POLLIN };
	say("poll reports the end of the stream", poll(&p, 1, 0) == 1 && (p.revents & POLLHUP));
	char total[64];
	int n = snprintf(total, sizeof total, "stdin-check: %ld bytes, %ld lines\n", bytes, lines);
	cubit_debug_write(total, (size_t)n);
	errno = 0;
	say("write to descriptor 0 is EBADF", write(0, "x", 1) == -1 && errno == EBADF);
#endif
	return failures ? 1 : 0;
}
