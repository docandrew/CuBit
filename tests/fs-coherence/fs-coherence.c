/*
 * Cross-process filesystem coherence, one case per program (FS-022).
 *
 * The program prepares files, starts its "other process" (CuBit: itself
 * again through posix_spawn, with OTHER_ARGUMENT; Linux: fork), which
 * changes them and exits without closing anything (_exit: on CuBit the
 * libc then closes nothing, so its buffered pages and parked handles are
 * left to the filesystem service). Only once waitpid says the other
 * process has exited, and SETTLE_MS later, does the program check what
 * the names now hold. The pause lets the service finish the other
 * process's teardown first (on CuBit its channels' grant events and
 * procmgr's OP_RELEASE_OWNER, which waitpid does not wait for): the
 * checks then read what a later reader would. The results must not depend
 * on it; it only makes the teardown-first order the one tested. Markers go to the kernel console (CuBit) or
 * stdout (Linux):
 *   fs-coherence: start CASE
 *   fs-coherence: CASE PASS | FAIL [what]
 *   fs-coherence: end
 *
 * CuBit: built once per case (-DCASE="name" -DPROGRAM="fs-coherence-name.app",
 * tests/fs-coherence/build-cubit.sh); files under @nvme:0/fs-coherence.
 * Linux: `fs-coherence CASE DIR` (tests/fs-coherence/host.sh).
 *
 * Cases:
 *   exit-left-open        written and never closed by the other process
 *   exit-closed           written and closed (CuBit: parked, still buffered)
 *   reopen-after-other-rename           our parked name renamed away and
 *                                       created again by the other process
 *   reopen-after-other-unlink-recreate  our parked name unlinked and
 *                                       created again by the other process
 *   other-truncate        a file we cached, truncated and rewritten shorter
 *   rename-over-open      a file we hold open, replaced by a rename: our
 *                         descriptor keeps the old file, the name the new one
 *   readdir-after-other-create  a directory we listed, then the other
 *                         process created files in it
 */
#define _GNU_SOURCE
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#ifdef CUBIT
#include <cubit/debug.h>
#include <spawn.h>
#define DIRECTORY "@nvme:0/fs-coherence"
#endif

#define OTHER_ARGUMENT "other-process"
#define FILE_BYTES 3000                 /* not a whole page */
#define SHORT_BYTES 1000                /* after the other process's truncate */
#define CREATED_FILES 8                 /* readdir-after-other-create */
#define FILE_MODE 0644
#define SETTLE_MS 500                   /* the other process's teardown */
#define NS_PER_MS 1000000L
#define DIR_MODE 0755
#define PATH_BYTES 256                 /* a directory and a case's name */
#define NAME_BYTES (PATH_BYTES + 16)  /* and an entry in it */

static const char *directory;
static const char *case_name;

static void report(const char *fmt, ...)
{
	char line[256];
	va_list ap;
	va_start(ap, fmt);
	int n = vsnprintf(line, sizeof line, fmt, ap);
	va_end(ap);
	if (n < 0)
		return;
	if ((size_t)n >= sizeof line)
		n = sizeof line - 1;
#ifdef CUBIT
	cubit_debug_write(line, (size_t)n);
#else
	fputs(line, stdout);
	fflush(stdout);
#endif
}

/* directory/CASE-suffix */
static void name_of(char *out, const char *suffix)
{
	snprintf(out, PATH_BYTES, "%s/%s-%s", directory, case_name, suffix);
}

static int write_all(int fd, const unsigned char *p, size_t n)
{
	while (n > 0) {
		ssize_t w = write(fd, p, n);
		if (w <= 0) return -1;
		p += w;
		n -= (size_t)w;
	}
	return 0;
}

/* Create (or truncate) path with bytes of byte, then close it. */
static int put_file(const char *path, int byte, size_t bytes)
{
	unsigned char data[FILE_BYTES];
	memset(data, byte, sizeof data);
	int fd = open(path, O_CREAT | O_TRUNC | O_WRONLY, FILE_MODE);
	if (fd < 0) return -1;
	int ok = write_all(fd, data, bytes) == 0;
	return close(fd) == 0 && ok ? 0 : -1;
}

/* fd holds exactly bytes of byte from offset 0. */
static int holds(int fd, int byte, size_t bytes)
{
	unsigned char data[FILE_BYTES + 1];
	ssize_t n = pread(fd, data, sizeof data, 0);
	if (n != (ssize_t)bytes) return 0;
	for (size_t i = 0; i < bytes; i++)
		if (data[i] != byte) return 0;
	return 1;
}

/* path holds exactly bytes of byte (opened read-only, read, closed). */
static int file_is(const char *path, int byte, size_t bytes)
{
	int fd = open(path, O_RDONLY);
	if (fd < 0) return 0;
	int ok = holds(fd, byte, bytes);
	close(fd);
	return ok;
}

/* Entries of path other than . and ..; -1 if it cannot be listed. With
 * expect, each must be one of the CREATED_FILES names. */
static int entries(const char *path, int expect)
{
	DIR *d = opendir(path);
	if (!d) return -1;
	int count = 0, seen[CREATED_FILES] = { 0 };
	struct dirent *e;
	while ((e = readdir(d))) {
		if (!strcmp(e->d_name, ".") || !strcmp(e->d_name, "..")) continue;
		count++;
		int k;
		if (expect && (sscanf(e->d_name, "n%d", &k) != 1 || k < 0 ||
			       k >= CREATED_FILES || seen[k]++)) {
			closedir(d);
			return -1;
		}
	}
	closedir(d);
	return count;
}

static void created_name(char *out, const char *sub, int k)
{
	snprintf(out, NAME_BYTES, "%s/n%d", sub, k);
}

/* --- the cases: prepare (this process), act (the other), check ----------- */

static int held_fd = -1;                /* rename-over-open */

static int prepare(void)
{
	char a[PATH_BYTES], b[PATH_BYTES];
	name_of(a, "a");
	name_of(b, "b");
	unlink(a);
	unlink(b);
	if (!strcmp(case_name, "exit-left-open") || !strcmp(case_name, "exit-closed"))
		return 0;
	if (!strcmp(case_name, "reopen-after-other-rename") ||
	    !strcmp(case_name, "reopen-after-other-unlink-recreate") ||
	    !strcmp(case_name, "other-truncate"))
		/* Read back, then closed: on CuBit parked, its pages cached. */
		return put_file(a, 'A', FILE_BYTES) == 0 && file_is(a, 'A', FILE_BYTES) ? 0 : -1;
	if (!strcmp(case_name, "rename-over-open")) {
		if (put_file(a, 'A', FILE_BYTES) < 0 || put_file(b, 'B', FILE_BYTES) < 0) return -1;
		held_fd = open(a, O_RDONLY);
		return held_fd >= 0 && holds(held_fd, 'A', FILE_BYTES) ? 0 : -1;
	}
	if (!strcmp(case_name, "readdir-after-other-create")) {
		char sub[PATH_BYTES], n[NAME_BYTES];
		name_of(sub, "dir");
		for (int k = 0; k < CREATED_FILES; k++) {
			created_name(n, sub, k);
			unlink(n);
		}
		rmdir(sub);
		return mkdir(sub, DIR_MODE) == 0 && entries(sub, 0) == 0 ? 0 : -1;
	}
	return -1;
}

static int act(void)
{
	char a[PATH_BYTES], b[PATH_BYTES];
	name_of(a, "a");
	name_of(b, "b");
	if (!strcmp(case_name, "exit-left-open")) {
		unsigned char data[FILE_BYTES];
		memset(data, 'k', sizeof data);
		int fd = open(a, O_CREAT | O_TRUNC | O_WRONLY, FILE_MODE);
		return fd >= 0 && write_all(fd, data, FILE_BYTES) == 0 ? 0 : -1;  /* left open */
	}
	if (!strcmp(case_name, "exit-closed"))
		return put_file(a, 'c', FILE_BYTES);
	if (!strcmp(case_name, "reopen-after-other-rename"))
		return rename(a, b) == 0 && put_file(a, 'a', FILE_BYTES) == 0 ? 0 : -1;
	if (!strcmp(case_name, "reopen-after-other-unlink-recreate"))
		return unlink(a) == 0 && put_file(a, 'a', FILE_BYTES) == 0 ? 0 : -1;
	if (!strcmp(case_name, "other-truncate")) {
		unsigned char data[SHORT_BYTES];
		memset(data, 't', sizeof data);
		int fd = open(a, O_TRUNC | O_WRONLY);
		return fd >= 0 && write_all(fd, data, SHORT_BYTES) == 0 && close(fd) == 0 ? 0 : -1;
	}
	if (!strcmp(case_name, "rename-over-open"))
		return rename(b, a);
	if (!strcmp(case_name, "readdir-after-other-create")) {
		char sub[PATH_BYTES], n[NAME_BYTES];
		name_of(sub, "dir");
		for (int k = 0; k < CREATED_FILES; k++) {
			created_name(n, sub, k);
			if (put_file(n, 'n', FILE_BYTES) < 0) return -1;
		}
		return 0;
	}
	return -1;
}

/* Checked once the other process has exited; cleans up after itself. */
static const char *check(void)
{
	char a[PATH_BYTES], b[PATH_BYTES];
	const char *wrong = 0;
	struct stat st;
	name_of(a, "a");
	name_of(b, "b");
	if (!strcmp(case_name, "exit-left-open")) {
		if (!file_is(a, 'k', FILE_BYTES)) wrong = "left-open bytes lost";
	} else if (!strcmp(case_name, "exit-closed")) {
		if (!file_is(a, 'c', FILE_BYTES)) wrong = "closed bytes lost";
	} else if (!strcmp(case_name, "reopen-after-other-rename")) {
		if (!file_is(a, 'a', FILE_BYTES)) wrong = "name does not hold the new file";
		else if (!file_is(b, 'A', FILE_BYTES)) wrong = "renamed file changed";
	} else if (!strcmp(case_name, "reopen-after-other-unlink-recreate")) {
		if (!file_is(a, 'a', FILE_BYTES)) wrong = "name does not hold the new file";
	} else if (!strcmp(case_name, "other-truncate")) {
		if (stat(a, &st) != 0 || st.st_size != SHORT_BYTES) wrong = "size not the truncated one";
		else if (!file_is(a, 't', SHORT_BYTES)) wrong = "bytes not the rewritten ones";
	} else if (!strcmp(case_name, "rename-over-open")) {
		if (!holds(held_fd, 'A', FILE_BYTES)) wrong = "open descriptor lost its file";
		else if (!file_is(a, 'B', FILE_BYTES)) wrong = "name does not hold the renamed file";
		else if (open(b, O_RDONLY) >= 0 || errno != ENOENT) wrong = "old name still there";
		close(held_fd);
	} else if (!strcmp(case_name, "readdir-after-other-create")) {
		char sub[PATH_BYTES], n[NAME_BYTES];
		name_of(sub, "dir");
		if (entries(sub, 1) != CREATED_FILES) wrong = "listing misses created files";
		for (int k = 0; k < CREATED_FILES; k++) {
			created_name(n, sub, k);
			if (!wrong && !file_is(n, 'n', FILE_BYTES)) wrong = "created file's bytes lost";
			unlink(n);
		}
		rmdir(sub);
	}
	unlink(a);
	unlink(b);
	return wrong;
}

static int finish(const char *wrong)
{
	if (wrong)
		report("fs-coherence: %s FAIL %s\n", case_name, wrong);
	else
		report("fs-coherence: %s PASS\n", case_name);
	report("fs-coherence: end\n");
	return wrong != 0;
}

int main(int argc, char **argv)
{
#ifdef CUBIT
	directory = DIRECTORY;
	case_name = CASE;
	if (argc > 1 && !strcmp(argv[1], OTHER_ARGUMENT))
		_exit(act() < 0);               /* no libc cleanup: the service's must do */
#else
	if (argc < 3) {
		fprintf(stderr, "usage: fs-coherence CASE DIRECTORY\n");
		return 2;
	}
	case_name = argv[1];
	directory = argv[2];
#endif
	report("fs-coherence: start %s\n", case_name);
	if (prepare() < 0)
		return finish("setup");
#ifdef CUBIT
	char *other_argv[] = { PROGRAM, OTHER_ARGUMENT, 0 };
	char *other_envp[] = { 0 };
	pid_t child = -1;
	if (posix_spawn(&child, PROGRAM, 0, 0, other_argv, other_envp) != 0)
		return finish("other process not started");
#else
	pid_t child = fork();
	if (child == 0) _exit(act() < 0);
	if (child < 0)
		return finish("other process not started");
#endif
	int status = 0;
	if (waitpid(child, &status, 0) != child || !WIFEXITED(status) || WEXITSTATUS(status) != 0)
		return finish("other process failed");
	struct timespec settle = { 0, SETTLE_MS * NS_PER_MS };
	nanosleep(&settle, 0);
	return finish(check());
}
