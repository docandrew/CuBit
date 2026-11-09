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
 *   hard-links            one file with two names (FS-031), held open by
 *                         one: the other process unlinks the other name.
 *                         The file, its bytes and one link stay; a write
 *                         through the open name keeps that count; then the
 *                         last name, unlinked while open, stays readable
 *                         until closed. (CuBit has no link operation: the
 *                         headless runner makes both names on its disk
 *                         copy, and checks it with e2fsck afterwards.)
 *
 * Advisory locks (flock), released by the filesystem service when their
 * handle closes or their process exits, however it ends:
 *   lock-holder-exits     the other process holds LOCK_EX and exits without
 *                         closing: our LOCK_NB fails while it lives, our
 *                         blocking LOCK_EX is granted once it is gone
 *   lock-holder-killed    the same, but the holder is killed (it traps:
 *                         the kernel ends it, the libc closes nothing)
 *   lock-shared-exclusive the other process holds LOCK_SH: our LOCK_SH
 *                         is granted, LOCK_EX conflicts with both, and is
 *                         granted (blocking) once the other has exited
 *   lock-deadline         a blocking LOCK_EX with a 200 ms limit gives up
 *                         with ETIMEDOUT while the other holds the lock
 *                         (CuBit: cubit_flock_timeout; Linux: a LOCK_NB
 *                         loop, the reference behaviour)
 *   lock-close            one process: a second descriptor conflicts with
 *                         the first (flock locks belong to the open file);
 *                         close and LOCK_UN release; a closed descriptor
 *                         reopened read-only (CuBit: its parked handle
 *                         reused), locked and closed again leaves no lock
 *   fsync-directory       fsync of a directory descriptor and of a
 *                         read-only file descriptor succeed (CuBit: both
 *                         commit the volume's journal)
 */
#define _GNU_SOURCE
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#ifdef CUBIT
#include <cubit/debug.h>
#include <cubit/files.h>
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
#define HOLD_MS 600                     /* the other process holds its lock */
#define LIMIT_MS 200                    /* lock-deadline's limit */
/* The limit is a deadline on the filesystem service's clock (GET_TIME's
 * milliseconds); CLOCK_MONOTONIC, which measures it here, is the HPET when
 * the TSC is not published (QEMU): the two were seen 1 % apart (198 ms). */
#define CLOCK_SLACK_MS 5
#define READY_WAIT_MS 10000             /* the other process takes its lock */
#define POLL_MS 5
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

static long now_ms(void)
{
	struct timespec t;
	clock_gettime(CLOCK_MONOTONIC, &t);
	return t.tv_sec * 1000L + t.tv_nsec / NS_PER_MS;
}

static void sleep_ms(long ms)
{
	struct timespec t = { ms / 1000, (ms % 1000) * NS_PER_MS };
	nanosleep(&t, 0);
}

/* The cases with a lock-holding other process. */
static int is_lock_case(void)
{
	return !strncmp(case_name, "lock-", 5) && strcmp(case_name, "lock-close");
}

/* flock with a limit: 0, or -1 with errno (ETIMEDOUT when it gave up). */
static int lock_within(int fd, int operation, long limit_ms)
{
#ifdef CUBIT
	long r = cubit_flock_timeout(fd, operation, limit_ms);
	if (r < 0) {
		errno = (int)-r;
		return -1;
	}
	return 0;
#else
	long until = now_ms() + limit_ms;
	if (limit_ms < 0)
		return flock(fd, operation);
	for (;;) {
		if (flock(fd, operation | LOCK_NB) == 0) return 0;
		if (errno != EWOULDBLOCK) return -1;
		if (now_ms() >= until) {
			errno = ETIMEDOUT;
			return -1;
		}
		sleep_ms(POLL_MS);
	}
#endif
}

/* Wait (with a limit) until the other process has made name. */
static int wait_for(const char *name)
{
	struct stat st;
	long until = now_ms() + READY_WAIT_MS;
	while (stat(name, &st) != 0) {
		if (now_ms() >= until) return -1;
		sleep_ms(POLL_MS);
	}
	return 0;
}

/* --- the cases: prepare (this process), act (the other), check ----------- */

static int held_fd = -1;                /* rename-over-open, hard-links */

/* hard-links: a and b name one file of FILE_BYTES 'H'; b held open. */
static int prepare_links(const char *a, const char *b)
{
	struct stat sa, sb;
#ifndef CUBIT
	unlink(a);
	unlink(b);
	if (put_file(a, 'H', FILE_BYTES) < 0 || link(a, b) != 0) return -1;
#endif
	if (stat(a, &sa) != 0 || stat(b, &sb) != 0 || sa.st_ino != sb.st_ino ||
	    sa.st_nlink != 2 || sb.st_nlink != 2)
		return -1;
	held_fd = open(b, O_RDWR);
	return held_fd >= 0 && holds(held_fd, 'H', FILE_BYTES) ? 0 : -1;
}

/* hard-links, once the other process unlinked a. */
static const char *check_links(const char *a, const char *b)
{
	struct stat st;
	unsigned char data[FILE_BYTES];
	const char *wrong = 0;
	memset(data, 'W', sizeof data);
	if (!holds(held_fd, 'H', FILE_BYTES)) wrong = "open name lost its file";
	else if (fstat(held_fd, &st) != 0 || st.st_nlink != 1) wrong = "link count not 1";
	else if (stat(a, &st) == 0 || errno != ENOENT) wrong = "unlinked name still there";
	else if (!file_is(b, 'H', FILE_BYTES)) wrong = "other name lost its bytes";
	else if (pwrite(held_fd, data, FILE_BYTES, 0) != FILE_BYTES || fsync(held_fd) != 0)
		wrong = "write through the other name";
	if (close(held_fd) != 0 && !wrong) wrong = "close";
	held_fd = -1;
	if (wrong) return wrong;
	/* A write must not bring the old count back. */
	if (stat(b, &st) != 0 || st.st_nlink != 1) return "link count changed by a write";
	if (!file_is(b, 'W', FILE_BYTES)) return "written bytes lost";
	/* The last name, unlinked while open: readable until closed. */
	int fd = open(b, O_RDONLY);
	if (fd < 0) return "other name not reopened";
	if (unlink(b) != 0) wrong = "last name not unlinked";
	else if (!holds(fd, 'W', FILE_BYTES)) wrong = "unlinked open file unreadable";
	else if (open(b, O_RDONLY) >= 0 || errno != ENOENT) wrong = "last name still there";
	close(fd);
	/* Commit the volume (CuBit: its journal) for the host's e2fsck. */
	int dir = open(directory, O_RDONLY | O_DIRECTORY);
	if ((dir < 0 || fsync(dir) != 0) && !wrong) wrong = "directory fsync";
	if (dir >= 0) close(dir);
	return wrong;
}

static int prepare(void)
{
	char a[PATH_BYTES], b[PATH_BYTES];
	name_of(a, "a");
	name_of(b, "b");
	if (!strcmp(case_name, "hard-links"))
		return prepare_links(a, b);
	unlink(a);
	unlink(b);
	if (!strcmp(case_name, "exit-left-open") || !strcmp(case_name, "exit-closed"))
		return 0;
	if (is_lock_case())                     /* a: the lock file, b: ready */
		return put_file(a, 'L', 1);
	if (!strcmp(case_name, "lock-close")) {
		if (put_file(a, 'L', 1) < 0) return -1;
		int one = open(a, O_RDWR), two = open(a, O_RDWR), ok = one >= 0 && two >= 0;
		ok = ok && flock(one, LOCK_EX | LOCK_NB) == 0;
		ok = ok && flock(two, LOCK_EX | LOCK_NB) < 0 && errno == EWOULDBLOCK;
		ok = ok && flock(two, LOCK_SH | LOCK_NB) < 0 && errno == EWOULDBLOCK;
		ok = ok && close(one) == 0;                         /* close releases */
		ok = ok && flock(two, LOCK_EX | LOCK_NB) == 0;
		int three = open(a, O_RDONLY);
		ok = ok && three >= 0 && flock(three, LOCK_SH | LOCK_NB) < 0 && errno == EWOULDBLOCK;
		ok = ok && flock(two, LOCK_UN) == 0;                /* LOCK_UN releases */
		ok = ok && flock(three, LOCK_SH | LOCK_NB) == 0;
		ok = ok && flock(two, LOCK_SH | LOCK_NB) == 0;      /* shared with shared */
		ok = ok && close(three) == 0 && close(two) == 0;
		/* Read-only again (CuBit: the parked handle reused), locked and
		 * closed: nothing may stay locked. */
		int four = open(a, O_RDONLY);
		ok = ok && four >= 0 && flock(four, LOCK_EX | LOCK_NB) == 0 && close(four) == 0;
		int five = open(a, O_RDWR);
		ok = ok && five >= 0 && flock(five, LOCK_EX | LOCK_NB) == 0 && close(five) == 0;
		return ok ? 0 : -1;
	}
	if (!strcmp(case_name, "fsync-directory")) {
		if (put_file(a, 'F', FILE_BYTES) < 0) return -1;
		int file = open(a, O_RDONLY), dir = open(directory, O_RDONLY | O_DIRECTORY);
		int ok = file >= 0 && dir >= 0 && fsync(file) == 0 && fsync(dir) == 0;
		if (rename(a, b) != 0 || fsync(dir) != 0) ok = 0;
		if (file >= 0) close(file);
		if (dir >= 0) close(dir);
		return ok ? 0 : -1;
	}
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
	if (!strcmp(case_name, "hard-links"))
		return unlink(a);                   /* our open name keeps the file */
	if (!strcmp(case_name, "readdir-after-other-create")) {
		char sub[PATH_BYTES], n[NAME_BYTES];
		name_of(sub, "dir");
		for (int k = 0; k < CREATED_FILES; k++) {
			created_name(n, sub, k);
			if (put_file(n, 'n', FILE_BYTES) < 0) return -1;
		}
		return 0;
	}
	if (!strcmp(case_name, "lock-close") || !strcmp(case_name, "fsync-directory"))
		return 0;                           /* all in this process */
	if (is_lock_case()) {
		int fd = open(a, O_RDWR);
		int shared = !strcmp(case_name, "lock-shared-exclusive");
		if (fd < 0 || flock(fd, shared ? LOCK_SH : LOCK_EX) != 0) return -1;
		if (put_file(b, 'R', 1) < 0) return -1;     /* ready: it holds */
		sleep_ms(!strcmp(case_name, "lock-deadline") ? 3 * HOLD_MS : HOLD_MS);
		if (!strcmp(case_name, "lock-holder-killed"))
			__builtin_trap();               /* killed holding it */
		return 0;                           /* exits holding it, fd open */
	}
	return -1;
}

/* This process, while the other runs (lock cases): what conflicts while
 * it holds its lock, and what is granted once it is gone. */
static const char *during(void)
{
	char a[PATH_BYTES], b[PATH_BYTES];
	name_of(a, "a");
	name_of(b, "b");
	if (!is_lock_case()) return 0;
	if (wait_for(b) < 0) return "the other process never took its lock";
	int one = open(a, O_RDWR), two = open(a, O_RDWR);
	if (one < 0 || two < 0) return "lock file not opened";
	const char *wrong = 0;
	if (!strcmp(case_name, "lock-shared-exclusive")) {
		if (flock(one, LOCK_SH | LOCK_NB) != 0) wrong = "shared refused beside shared";
		else if (flock(two, LOCK_EX | LOCK_NB) == 0 || errno != EWOULDBLOCK)
			wrong = "exclusive granted beside shared";
		else if (close(one) != 0) wrong = "close";
		else if (one = -1, flock(two, LOCK_EX | LOCK_NB) == 0 || errno != EWOULDBLOCK)
			wrong = "exclusive granted beside the other's shared";
		else if (flock(two, LOCK_EX) != 0) wrong = "blocking exclusive not granted";
	} else if (!strcmp(case_name, "lock-deadline")) {
		long started = now_ms();
		if (lock_within(one, LOCK_EX, LIMIT_MS) == 0) wrong = "granted while held";
		else if (errno != ETIMEDOUT) wrong = "not ETIMEDOUT";
		else if (now_ms() - started < LIMIT_MS - CLOCK_SLACK_MS) {
			static char early[64];
			snprintf(early, sizeof early, "gave up before the limit (%ld ms)", now_ms() - started);
			wrong = early;
		}
		else if (now_ms() - started >= 3 * HOLD_MS) wrong = "gave up long after the limit";
		else if (lock_within(two, LOCK_EX, -1) != 0) wrong = "not granted after the holder";
	} else {                                    /* holder exits or is killed */
		if (flock(one, LOCK_EX | LOCK_NB) == 0 || errno != EWOULDBLOCK)
			wrong = "granted while the other holds it";
		else if (flock(one, LOCK_EX) != 0) wrong = "blocking request not granted";
		else if (flock(two, LOCK_EX | LOCK_NB) == 0) wrong = "two exclusive holders";
	}
	if (one >= 0) close(one);
	close(two);
	return wrong;
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
	} else if (!strcmp(case_name, "fsync-directory")) {
		if (!file_is(b, 'F', FILE_BYTES)) wrong = "renamed file's bytes lost";
	} else if (!strcmp(case_name, "hard-links")) {
		wrong = check_links(a, b);
	} else if (!strncmp(case_name, "lock-", 5)) {
		/* Nobody holds it now. */
		int fd = open(a, O_RDWR);
		if (fd < 0 || flock(fd, LOCK_EX | LOCK_NB) != 0) wrong = "still locked after everyone let go";
		if (fd >= 0) close(fd);
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
	const char *wrong_during = during();
	int status = 0;
	int killed = !strcmp(case_name, "lock-holder-killed");
	if (waitpid(child, &status, 0) != child ||
	    (killed ? WIFEXITED(status) && WEXITSTATUS(status) == 0
		    : !WIFEXITED(status) || WEXITSTATUS(status) != 0))
		return finish("other process failed");
	if (wrong_during)
		return finish(wrong_during);
	struct timespec settle = { 0, SETTLE_MS * NS_PER_MS };
	nanosleep(&settle, 0);
	return finish(check());
}
