/* CuBit side of the native ext3 interoperability check (native.sh).
 * The volume's journal was left dirty by a crashed Linux guest; the
 * filesystem service replayed it at admission. Verify what Linux wrote,
 * then unlink, mkdir, rmdir, create and write through CuBit's journal. */
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <cubit/debug.h>

#define ROOT "@nvme:0/journal"
static char buffer[16384];
static int failures;

/* Output goes to the debug console (the serial log), as fs-bench's. */
static void say(const char *text)
{
	cubit_debug_write(text, strlen(text));
}

static void fail(const char *what)
{
	char line[160];
	snprintf(line, sizeof line, "journal-check: FAIL %s (errno %d)\n", what, errno);
	say(line);
	failures++;
}

static void expect_file(const char *path, char fill, size_t size)
{
	int fd = open(path, O_RDONLY);
	size_t got = 0;
	if (fd < 0) { fail(path); return; }
	for (;;) {
		ssize_t n = read(fd, buffer, sizeof buffer);
		if (n < 0) { fail(path); break; }
		if (n == 0) break;
		for (ssize_t i = 0; i < n; i++)
			if (buffer[i] != fill) { fail(path); close(fd); return; }
		got += (size_t)n;
	}
	close(fd);
	if (got != size) fail(path);
}

static void write_file(const char *path, int flags, off_t at, char fill, size_t size)
{
	int fd = open(path, O_WRONLY | flags, 0644);
	if (fd < 0) { fail(path); return; }
	memset(buffer, fill, sizeof buffer);
	for (size_t done = 0; done < size; ) {
		size_t part = size - done < sizeof buffer ? size - done : sizeof buffer;
		if (pwrite(fd, buffer, part, at + (off_t)done) != (ssize_t)part) { fail(path); break; }
		done += part;
	}
	if (fsync(fd) != 0) fail(path);
	close(fd);
}

int main(void)
{
	struct stat st;
	say("journal-check: start\n");
	expect_file(ROOT "/linux-a", 'L', 65536);
	expect_file(ROOT "/linux-dir/linux-b", 'M', 5000);
	expect_file(ROOT "/linux-victim", 'V', 20000);
	if (stat(ROOT "/linux-gone", &st) == 0 || errno != ENOENT) fail("linux-gone present");
	say("journal-check: linux replay verified\n");

	if (unlink(ROOT "/linux-victim") != 0) fail("unlink");
	if (mkdir(ROOT "/cubit-dir", 0755) != 0) fail("mkdir");
	if (mkdir(ROOT "/cubit-empty", 0755) != 0) fail("mkdir empty");
	if (rmdir(ROOT "/cubit-empty") != 0) fail("rmdir");
	if (rmdir(ROOT "/linux-dir") == 0 || errno != ENOTEMPTY) fail("rmdir non-empty");
	write_file(ROOT "/cubit-dir/cubit-file", O_CREAT, 0, 'C', 300000);
	write_file(ROOT "/linux-a", 0, 65536, 'N', 4096);
	expect_file(ROOT "/cubit-dir/cubit-file", 'C', 300000);
	say(failures ? "JOURNAL-CHECK: FAIL\n" : "JOURNAL-CHECK: PASS\n");
	return failures != 0;
}
