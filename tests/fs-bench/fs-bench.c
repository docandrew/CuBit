/*
 * CuBit filesystem benchmark. The same source runs on CuBit (musl over
 * filesystem.svc, on the NVMe ext2 volume) and on Linux (ext2 on an NVMe
 * device), each in the same QEMU configuration. POSIX calls only.
 *
 * Workloads, each repeated ROUNDS times, on files under one directory:
 *   seq-write     FILE_MIB MiB in CHUNK_BYTES writes to a new file, then fsync
 *   seq-read      the file in CHUNK_BYTES reads, warm (right after writing)
 *                 and cold (after dropping the page cache: Linux only;
 *                 CuBit cannot drop its caches, so it reads warm again)
 *   rand-read     RANDOM_OPS positioned BLOCK_BYTES reads at random blocks
 *   rand-write    RANDOM_OPS positioned BLOCK_BYTES writes, no fsync
 *   rand-write-fsync  FSYNC_OPS positioned writes, each followed by fsync
 *   create / open-read / list / unlink   SMALL_FILES files of BLOCK_BYTES
 *                 in a directory of their own per round
 *
 * The directory is argv[1], else FS_BENCH_DIR, else DEFAULT_DIR; FILE_MIB
 * may be overridden by FS_BENCH_FILE_MIB (or -DFILE_MIB=N at build time).
 * Where unlink is missing (ENOSYS), it is reported as unsupported.
 */
#define _GNU_SOURCE
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#ifdef CUBIT
#include <cubit/debug.h>
#define DEFAULT_DIR "@nvme:0/fs-bench"
#else
#define DEFAULT_DIR "/mnt/fs-bench"
#endif

#define ROUNDS 3
#ifndef FILE_MIB
#define FILE_MIB 64
#endif
#define MIB (1024ull * 1024)
#define CHUNK_BYTES MIB
#define BLOCK_BYTES 4096
#define RANDOM_OPS 2048
#define FSYNC_OPS 256
#define SMALL_FILES 1000
#define RANDOM_SEED 0x2545f4914f6cdd1dull
#define PERCENT_MEDIAN 50
#define PERCENT_TAIL 99
#define US_PER_S 1e6
#define NS_PER_US 1e3
#define MS_PER_S 1e3
#define FILE_MODE 0644
#define DIR_MODE 0755
#define PATH_BYTES 256
#define NAME_BYTES (PATH_BYTES + 16)  /* a directory path and a file name */
#define DROP_CACHES "/proc/sys/vm/drop_caches"
#define DROP_ALL "3"                    /* page cache, dentries and inodes */

static unsigned char chunk[CHUNK_BYTES] __attribute__((aligned(BLOCK_BYTES)));
static double samples[RANDOM_OPS];
static uint64_t file_bytes;

/* Results go to the serial console: on CuBit the debug console (stdout is
 * a stream with no subscriber here), on Linux stdout (the console). */
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

/* --- time ------------------------------------------------------------------ */

static double monotonic_us(void)
{
	struct timespec t;
	clock_gettime(CLOCK_MONOTONIC, &t);
	return t.tv_sec * US_PER_S + t.tv_nsec / NS_PER_US;
}

#ifdef CUBIT
/* CuBit's clock_gettime counts milliseconds; per-operation latencies use
 * the time-stamp counter, calibrated against it over CALIBRATE_MS. */
#define CALIBRATE_MS 200
static double tsc_per_us;

static uint64_t tsc(void)
{
	uint32_t lo, hi;
	__asm__ __volatile__ ("lfence; rdtsc; lfence" : "=a"(lo), "=d"(hi) :: "memory");
	return (uint64_t)hi << 32 | lo;
}

static void calibrate(void)
{
	double start = monotonic_us(), t0;
	while ((t0 = monotonic_us()) == start) {}  /* align to a tick edge */
	uint64_t c0 = tsc();
	double t1;
	while ((t1 = monotonic_us()) - t0 < CALIBRATE_MS * NS_PER_US) {}
	tsc_per_us = (double)(tsc() - c0) / (t1 - t0);
	report("fs-bench: clock tsc ticks_per_us=%.1f\n", tsc_per_us);
}

static double now_us(void)
{
	return (double)tsc() / tsc_per_us;
}
#else
static void calibrate(void)
{
	report("fs-bench: clock clock_gettime(CLOCK_MONOTONIC)\n");
}

static double now_us(void)
{
	return monotonic_us();
}
#endif

/* --- helpers --------------------------------------------------------------- */

static uint64_t random_state = RANDOM_SEED;

static uint64_t next_random(void)
{
	uint64_t x = random_state;      /* xorshift64 */
	x ^= x << 13;
	x ^= x >> 7;
	x ^= x << 17;
	return random_state = x;
}

static uint64_t random_block(void)
{
	return next_random() % (file_bytes / BLOCK_BYTES);
}

/* Each block starts with its number and the round, so reads can be checked. */
static void stamp(unsigned char *block, uint64_t number, uint64_t round)
{
	memcpy(block, &number, sizeof number);
	memcpy(block + sizeof number, &round, sizeof round);
}

static int stamped(const unsigned char *block, uint64_t number)
{
	uint64_t got;
	memcpy(&got, block, sizeof got);
	return got == number;
}

static int compare(const void *a, const void *b)
{
	double x = *(const double *)a, y = *(const double *)b;
	return x < y ? -1 : x > y;
}

/* Percentile p of n sorted samples (nearest rank). */
static double percentile(const double *sorted, int n, int p)
{
	int rank = (n * p + 99) / 100;
	return sorted[rank < 1 ? 0 : rank - 1];
}

static void report_latency(const char *op, int round, int n, double total_us)
{
	qsort(samples, (size_t)n, sizeof samples[0], compare);
	report("fs-bench: %s round=%d ops=%d p50_us=%.1f p99_us=%.1f max_us=%.1f iops=%.0f\n",
	       op, round, n, percentile(samples, n, PERCENT_MEDIAN),
	       percentile(samples, n, PERCENT_TAIL), samples[n - 1],
	       n * US_PER_S / total_us);
}

static int fail(const char *op, const char *what)
{
	report("fs-bench: %s FAIL %s errno=%d\n", op, what, errno);
	return -1;
}

static int write_all(int fd, const void *p, size_t n)
{
	const unsigned char *c = p;
	while (n) {
		ssize_t k = write(fd, c, n);
		if (k <= 0)
			return -1;
		c += k;
		n -= (size_t)k;
	}
	return 0;
}

static int read_all(int fd, void *p, size_t n)
{
	unsigned char *c = p;
	while (n) {
		ssize_t k = read(fd, c, n);
		if (k <= 0)
			return -1;
		c += k;
		n -= (size_t)k;
	}
	return 0;
}

/* Directories: mkdir, or one that already exists. */
static int ensure_directory(const char *path)
{
	struct stat st;
	if (mkdir(path, DIR_MODE) == 0)
		return 0;
	if (errno != EEXIST && errno != ENOSYS)
		return -1;
	return stat(path, &st) == 0 && S_ISDIR(st.st_mode) ? 0 : -1;
}

/* Linux: write back and drop the page cache so the next read is from the
 * device. CuBit has no way to drop its caches. */
static void drop_caches(void)
{
#ifndef CUBIT
	sync();
	int fd = open(DROP_CACHES, O_WRONLY);
	if (fd >= 0) {
		if (write(fd, DROP_ALL, sizeof DROP_ALL - 1) < 0)
			report("fs-bench: drop_caches errno=%d\n", errno);
		close(fd);
	}
#endif
}

/* --- workloads --------------------------------------------------------------- */

static int seq_write(const char *path, int round)
{
	int fd = open(path, O_CREAT | O_TRUNC | O_WRONLY, FILE_MODE);
	if (fd < 0)
		return fail("seq-write", "open");
	uint64_t blocks_per_chunk = CHUNK_BYTES / BLOCK_BYTES;
	double t0 = now_us();
	for (uint64_t at = 0; at < file_bytes; at += CHUNK_BYTES) {
		for (uint64_t b = 0; b < blocks_per_chunk; b++)
			stamp(chunk + b * BLOCK_BYTES, at / BLOCK_BYTES + b, (uint64_t)round);
		if (write_all(fd, chunk, CHUNK_BYTES) < 0) {
			close(fd);
			return fail("seq-write", "write");
		}
	}
	double t1 = now_us();
	if (fsync(fd) < 0) {
		close(fd);
		return fail("seq-write", "fsync");
	}
	double t2 = now_us();
	close(fd);
	report("fs-bench: seq-write round=%d bytes=%llu write_ms=%.1f fsync_ms=%.1f MBps=%.1f\n",
	       round, (unsigned long long)file_bytes, (t1 - t0) / MS_PER_S,
	       (t2 - t1) / MS_PER_S, file_bytes / (t2 - t0));
	return 0;
}

static int seq_read(const char *path, const char *op, int round)
{
	int fd = open(path, O_RDONLY);
	if (fd < 0)
		return fail(op, "open");
	uint64_t blocks_per_chunk = CHUNK_BYTES / BLOCK_BYTES;
	double t0 = now_us();
	for (uint64_t at = 0; at < file_bytes; at += CHUNK_BYTES) {
		if (read_all(fd, chunk, CHUNK_BYTES) < 0) {
			close(fd);
			return fail(op, "read");
		}
		for (uint64_t b = 0; b < blocks_per_chunk; b++)
			if (!stamped(chunk + b * BLOCK_BYTES, at / BLOCK_BYTES + b)) {
				close(fd);
				errno = 0;
				return fail(op, "content");
			}
	}
	double ms = (now_us() - t0) / MS_PER_S;
	close(fd);
	report("fs-bench: %s round=%d bytes=%llu ms=%.1f MBps=%.1f\n", op, round,
	       (unsigned long long)file_bytes, ms, file_bytes / (ms * MS_PER_S));
	return 0;
}

static int rand_read(const char *path, int round)
{
	unsigned char *block = chunk;
	int fd = open(path, O_RDONLY);
	if (fd < 0)
		return fail("rand-read", "open");
	double t0 = now_us();
	for (int i = 0; i < RANDOM_OPS; i++) {
		uint64_t b = random_block();
		double s = now_us();
		ssize_t k = pread(fd, block, BLOCK_BYTES, (off_t)(b * BLOCK_BYTES));
		samples[i] = now_us() - s;
		if (k != BLOCK_BYTES) {
			close(fd);
			return fail("rand-read", "pread");
		}
		if (!stamped(block, b)) {
			close(fd);
			errno = 0;
			return fail("rand-read", "content");
		}
	}
	double total = now_us() - t0;
	close(fd);
	report_latency("rand-read", round, RANDOM_OPS, total);
	return 0;
}

static int rand_write(const char *path, const char *op, int round, int ops, int sync_each)
{
	unsigned char *block = chunk;
	int fd = open(path, O_RDWR);
	if (fd < 0)
		return fail(op, "open");
	memset(block, round, BLOCK_BYTES);
	double t0 = now_us();
	for (int i = 0; i < ops; i++) {
		uint64_t b = random_block();
		stamp(block, b, (uint64_t)round);
		double s = now_us();
		ssize_t k = pwrite(fd, block, BLOCK_BYTES, (off_t)(b * BLOCK_BYTES));
		if (k != BLOCK_BYTES || (sync_each && fsync(fd) < 0)) {
			close(fd);
			return fail(op, k != BLOCK_BYTES ? "pwrite" : "fsync");
		}
		samples[i] = now_us() - s;
	}
	double total = now_us() - t0;
	close(fd);
	report_latency(op, round, ops, total);
	return 0;
}

static void small_name(char *out, const char *dir, int i)
{
	snprintf(out, NAME_BYTES, "%s/f%04d", dir, i);
}

static void report_rate(const char *op, int round, double us)
{
	report("fs-bench: %s round=%d files=%d ms=%.1f per_second=%.0f\n", op, round,
	       SMALL_FILES, us / MS_PER_S, SMALL_FILES * US_PER_S / us);
}

static int small_files(const char *base, int round)
{
	char dir[PATH_BYTES], name[NAME_BYTES];
	snprintf(dir, sizeof dir, "%s/r%d", base, round);
	if (ensure_directory(dir) < 0)
		return fail("create", "directory");
	memset(chunk, 'a' + round, BLOCK_BYTES);

	double t0 = now_us();
	for (int i = 0; i < SMALL_FILES; i++) {
		small_name(name, dir, i);
		int fd = open(name, O_CREAT | O_EXCL | O_WRONLY, FILE_MODE);
		if (fd < 0)
			return fail("create", "open");
		stamp(chunk, (uint64_t)i, (uint64_t)round);
		int ok = write_all(fd, chunk, BLOCK_BYTES);
		if (close(fd) < 0 || ok < 0)
			return fail("create", "write");
	}
	report_rate("create", round, now_us() - t0);

	t0 = now_us();
	for (int i = 0; i < SMALL_FILES; i++) {
		small_name(name, dir, i);
		int fd = open(name, O_RDONLY);
		if (fd < 0)
			return fail("open-read", "open");
		int ok = read_all(fd, chunk, BLOCK_BYTES);
		close(fd);
		if (ok < 0)
			return fail("open-read", "read");
		if (!stamped(chunk, (uint64_t)i)) {
			errno = 0;
			return fail("open-read", "content");
		}
	}
	report_rate("open-read", round, now_us() - t0);

	t0 = now_us();
	DIR *d = opendir(dir);
	int found = 0;
	struct dirent *e;
	while (d && (e = readdir(d)))
		if (e->d_name[0] == 'f')
			found++;
	if (!d || closedir(d) < 0 || found != SMALL_FILES) {
		report("fs-bench: list found=%d\n", found);
		return fail("list", "readdir");
	}
	report_rate("list", round, now_us() - t0);

	t0 = now_us();
	for (int i = 0; i < SMALL_FILES; i++) {
		small_name(name, dir, i);
		if (unlink(name) < 0) {
			if (errno == ENOSYS && i == 0) {
				report("fs-bench: unlink round=%d unsupported errno=%d\n", round, errno);
				return 0;
			}
			return fail("unlink", "unlink");
		}
	}
	report_rate("unlink", round, now_us() - t0);
	rmdir(dir);
	return 0;
}

/* Coherence between handles of one file (run once, before the timing
 * rounds; on CuBit these exercise the service's delegations: a reader's
 * cache is revoked by a writer, and a writer's buffered pages are
 * harvested before another handle opens the file). Each check prints
 * PASS or FAIL. */
#define COHERENCE_BYTES 8192
static int coherence_check(int ok, const char *name)
{
	report("fs-bench: coherence %s %s\n", name, ok ? "PASS" : "FAIL");
	return ok ? 0 : -1;
}

static int parked(const char *dir);

static int coherence(const char *dir)
{
	char path[PATH_BYTES];
	unsigned char a[COHERENCE_BYTES], b[COHERENCE_BYTES];
	int failures = 0;
	snprintf(path, sizeof path, "%s/coherence", dir);

	/* 1. A reader's cached pages give way to another handle's write. */
	memset(a, 'A', sizeof a);
	int w = open(path, O_CREAT | O_TRUNC | O_WRONLY, FILE_MODE);
	if (w < 0 || write_all(w, a, sizeof a) < 0 || close(w) < 0)
		return coherence_check(0, "setup");
	int r = open(path, O_RDONLY);
	if (r < 0 || pread(r, b, sizeof b, 0) != (ssize_t)sizeof b) return coherence_check(0, "read");
	failures += coherence_check(memcmp(a, b, sizeof a) == 0, "first-read") < 0;
	w = open(path, O_WRONLY);
	memset(a, 'B', BLOCK_BYTES);
	if (w < 0 || pwrite(w, a, BLOCK_BYTES, 0) != BLOCK_BYTES) return coherence_check(0, "write");
	failures += coherence_check(pread(r, b, BLOCK_BYTES, 0) == BLOCK_BYTES &&
				    memcmp(a, b, BLOCK_BYTES) == 0, "read-after-other-write") < 0;
	close(w);
	close(r);

	/* 2. A sole writer's buffered pages reach a later opener. */
	w = open(path, O_RDWR);
	memset(a, 'C', sizeof a);
	if (w < 0 || pwrite(w, a, sizeof a, 0) != (ssize_t)sizeof a) return coherence_check(0, "buffered-write");
	failures += coherence_check(pread(w, b, sizeof b, 0) == (ssize_t)sizeof b &&
				    memcmp(a, b, sizeof a) == 0, "read-own-write") < 0;
	r = open(path, O_RDONLY);
	failures += coherence_check(r >= 0 && pread(r, b, sizeof b, 0) == (ssize_t)sizeof b &&
				    memcmp(a, b, sizeof a) == 0, "other-open-sees-buffered-write") < 0;
	close(r);
	/* Extending past the end, then a fresh handle sees the new size. */
	memset(a, 'D', BLOCK_BYTES);
	if (pwrite(w, a, BLOCK_BYTES, COHERENCE_BYTES) != BLOCK_BYTES) failures++;
	failures += coherence_check(fsync(w) == 0, "fsync") < 0;
	close(w);
	struct stat st;
	failures += coherence_check(stat(path, &st) == 0 &&
				    st.st_size == COHERENCE_BYTES + BLOCK_BYTES, "size-after-extend") < 0;
	r = open(path, O_RDONLY);
	failures += coherence_check(r >= 0 && pread(r, b, BLOCK_BYTES, COHERENCE_BYTES) == BLOCK_BYTES &&
				    memcmp(a, b, BLOCK_BYTES) == 0, "extended-bytes") < 0;
	close(r);

	/* 3. Namespace: unlink, mkdir, rmdir. */
	failures += coherence_check(unlink(path) == 0 && open(path, O_RDONLY) < 0 &&
				    errno == ENOENT, "unlink") < 0;
	char sub[PATH_BYTES];
	snprintf(sub, sizeof sub, "%s/coherence-dir", dir);
	failures += coherence_check(mkdir(sub, 0755) == 0 && stat(sub, &st) == 0 &&
				    S_ISDIR(st.st_mode), "mkdir") < 0;
	failures += coherence_check(rmdir(sub) == 0 && stat(sub, &st) < 0, "rmdir") < 0;
	if (parked(dir) < 0) failures++;
	return failures ? -1 : 0;
}

/* --- reopening by name after the name or the file changed -----------------
 * On CuBit a closed read handle is kept ("parked") and a read-only reopen
 * of its name reuses it with no request, while the namespace and the
 * file are unchanged (file.c). These reopen names another process or this
 * one changed meanwhile: each must see the file the name now holds. The
 * other process is a second fs-bench (CuBit: the startup runs two; the one
 * that creates the claim file first is the benchmark) or a fork (Linux). */
#define PARK_BYTES 3000                 /* not a whole page */
#define HOLD_OPENS 40                   /* more than a file's handle limit */
#define WAIT_MS 20000
#define POLL_NS 1000000L

static int put_file(const char *path, int byte)
{
	unsigned char data[PARK_BYTES];
	memset(data, byte, sizeof data);
	int fd = open(path, O_CREAT | O_TRUNC | O_WRONLY, FILE_MODE);
	if (fd < 0) return -1;
	int ok = write_all(fd, data, sizeof data) == 0;
	return close(fd) == 0 && ok ? 0 : -1;
}

/* The file at path holds PARK_BYTES of byte (read, then closed). */
static int file_is(const char *path, int byte)
{
	unsigned char data[PARK_BYTES + 1];
	int fd = open(path, O_RDONLY);
	if (fd < 0) return 0;
	ssize_t n = pread(fd, data, sizeof data, 0);
	close(fd);
	if (n != PARK_BYTES) return 0;
	for (size_t i = 0; i < PARK_BYTES; i++)
		if (data[i] != byte) return 0;
	return 1;
}

static int wait_for(const char *path)
{
	struct stat st;
	struct timespec pause = { 0, POLL_NS };
	for (int waited = 0; waited < WAIT_MS; waited++) {
		if (stat(path, &st) == 0) return 0;
		nanosleep(&pause, 0);
	}
	return -1;
}

static void name_in(char *out, const char *dir, const char *name)
{
	snprintf(out, NAME_BYTES, "%s/%s", dir, name);
}

/* The other process: once told to, change what the benchmark parked. */
static int other_process(const char *dir)
{
	char go[NAME_BYTES], done[NAME_BYTES], w[NAME_BYTES], x[NAME_BYTES],
	     y[NAME_BYTES], y2[NAME_BYTES];
	name_in(go, dir, "park-go");
	name_in(done, dir, "park-done");
	name_in(w, dir, "park-written");
	name_in(x, dir, "park-unlinked");
	name_in(y, dir, "park-renamed");
	name_in(y2, dir, "park-renamed-away");
	char kept[NAME_BYTES], stale[NAME_BYTES], dropped[NAME_BYTES],
	     dropped2[NAME_BYTES], held[NAME_BYTES], go2[NAME_BYTES];
	name_in(kept, dir, "park-left-open");
	name_in(stale, dir, "park-stale");
	name_in(dropped, dir, "park-dropped");
	name_in(dropped2, dir, "park-dropped-away");
	name_in(held, dir, "park-held");
	name_in(go2, dir, "park-go2");
	if (wait_for(go) < 0) return -1;
	int failed = 0;
	unsigned char data[PARK_BYTES];
	memset(data, 'w', sizeof data);
	int fd = open(w, O_WRONLY);        /* same size, new bytes */
	if (fd < 0 || pwrite(fd, data, sizeof data, 0) != (ssize_t)sizeof data) failed = 1;
	if (fd >= 0) close(fd);
	if (unlink(x) < 0 || put_file(x, 'x') < 0) failed = 1;
	if (rename(y, y2) < 0 || put_file(y, 'y') < 0) failed = 1;
	/* A file the benchmark has pages of, written here as its only
	 * handle (on CuBit, under a write delegation). */
	memset(data, 's', sizeof data);
	fd = open(stale, O_WRONLY);
	if (fd < 0 || pwrite(fd, data, sizeof data, 0) != (ssize_t)sizeof data) failed = 1;
	if (fd >= 0) close(fd);
	/* The benchmark's parked name moves away; a new file takes it. */
	if (rename(dropped, dropped2) < 0 || put_file(dropped, 'N') < 0) failed = 1;
	/* Many handles of one file: the benchmark must still open it. */
	int holds[HOLD_OPENS], held_count = 0;
	for (int k = 0; k < HOLD_OPENS; k++) {
		holds[k] = open(held, O_RDONLY);
		if (holds[k] >= 0) held_count++;
	}
	if (held_count == 0) failed = 1;
	/* Written and left open: the bytes must outlive this process, which
	 * exits without closing (CuBit: _exit, so the libc closes nothing). */
	memset(data, 'k', sizeof data);
	fd = open(kept, O_CREAT | O_TRUNC | O_WRONLY, FILE_MODE);
	if (fd < 0 || write_all(fd, data, sizeof data) < 0) failed = 1;
	if (put_file(done, failed ? 'F' : 'D') < 0) return -1;
	wait_for(go2);
	for (int k = 0; k < HOLD_OPENS; k++)
		if (holds[k] >= 0) close(holds[k]);
	return failed ? -1 : 0;
}

static int parked(const char *dir)
{
	char a[NAME_BYTES], b[NAME_BYTES], go[NAME_BYTES], done[NAME_BYTES],
	     w[NAME_BYTES], x[NAME_BYTES], y[NAME_BYTES], y2[NAME_BYTES];
	int failures = 0;
	name_in(a, dir, "park-a");
	name_in(b, dir, "park-b");

	/* This process: unlink and create again, rename. */
	failures += coherence_check(put_file(a, '1') == 0 && file_is(a, '1') && file_is(a, '1') &&
				    unlink(a) == 0 && put_file(a, '2') == 0 && file_is(a, '2'),
				    "reopen-after-unlink-recreate") < 0;
	failures += coherence_check(put_file(b, '3') == 0 && file_is(b, '3') &&
				    unlink(b) == 0 && rename(a, b) == 0 &&
				    !file_is(a, '2') && errno == ENOENT && file_is(b, '2'),
				    "reopen-after-rename") < 0;
	unlink(b);

	/* Another process: a write, an unlink and create, a rename and create. */
	name_in(go, dir, "park-go");
	name_in(done, dir, "park-done");
	name_in(w, dir, "park-written");
	name_in(x, dir, "park-unlinked");
	name_in(y, dir, "park-renamed");
	name_in(y2, dir, "park-renamed-away");
	char stale[NAME_BYTES], stale2[NAME_BYTES], dropped[NAME_BYTES],
	     dropped2[NAME_BYTES], held[NAME_BYTES], go2[NAME_BYTES], ro[NAME_BYTES];
	name_in(stale, dir, "park-stale");
	name_in(stale2, dir, "park-stale-away");
	name_in(dropped, dir, "park-dropped");
	name_in(dropped2, dir, "park-dropped-away");
	name_in(held, dir, "park-held");
	name_in(go2, dir, "park-go2");
	name_in(ro, dir, "park-read-only-sync");
	/* fsync on a read-only descriptor of a just-written file. */
	int rofd = put_file(ro, 'r') == 0 ? open(ro, O_RDONLY) : -1;
	failures += coherence_check(rofd >= 0 && fsync(rofd) == 0 && file_is(ro, 'r'),
				    "fsync-read-only") < 0;
	if (rofd >= 0) close(rofd);
	unlink(ro);
	if (put_file(w, 'W') < 0 || put_file(x, 'X') < 0 || put_file(y, 'Y') < 0 ||
	    !file_is(w, 'W') || !file_is(x, 'X') || !file_is(y, 'Y'))
		return coherence_check(0, "other-process-setup");
	/* Pages of stale cached here; its name moves away and back (this
	 * process then holds no handle of it, only cached pages). A file
	 * written here and left parked with its buffered bytes. A file the
	 * other process will hold many times. */
	if (put_file(stale, 'S') < 0 || !file_is(stale, 'S') ||
	    rename(stale, stale2) < 0 || rename(stale2, stale) < 0 ||
	    put_file(dropped, 'U') < 0 || put_file(held, 'H') < 0)
		return coherence_check(0, "other-process-setup");
#ifndef CUBIT
	pid_t child = fork();
	if (child == 0) _exit(other_process(dir) < 0);
#endif
	int told = put_file(go, 'G') == 0;
	int answered = told && wait_for(done) == 0 && file_is(done, 'D');
	int held_open = open(held, O_RDONLY);
	failures += coherence_check(answered && held_open >= 0, "open-while-other-holds-many") < 0;
	if (held_open >= 0) close(held_open);
	put_file(go2, 'G');
#ifndef CUBIT
	if (child > 0) waitpid(child, 0, 0);
#endif
	failures += coherence_check(answered && file_is(stale, 's'),
				    "reopen-after-other-delegated-write") < 0;
	/* Unlinking our parked name, now another file's: the file it moved to
	 * keeps our buffered bytes, the new one goes. */
	failures += coherence_check(answered && unlink(dropped) == 0 && !file_is(dropped, 'N') &&
				    file_is(dropped2, 'U'), "unlink-keeps-moved-files-data") < 0;
	failures += coherence_check(answered, "other-process") < 0;
	failures += coherence_check(answered && file_is(w, 'w'), "reopen-after-other-write") < 0;
	failures += coherence_check(answered && file_is(x, 'x'), "reopen-after-other-unlink-recreate") < 0;
	failures += coherence_check(answered && file_is(y, 'y') && file_is(y2, 'Y'),
				    "reopen-after-other-rename") < 0;
	char kept[NAME_BYTES];
	name_in(kept, dir, "park-left-open");
	failures += coherence_check(answered && file_is(kept, 'k'), "other-exit-left-open") < 0;
	unlink(go);
	unlink(done);
	unlink(w);
	unlink(x);
	unlink(y);
	unlink(y2);
	unlink(kept);
	unlink(stale);
	unlink(dropped2);
	unlink(held);
	unlink(go2);
	return failures ? -1 : 0;
}

int main(int argc, char **argv)
{
	const char *dir = argc > 1 ? argv[1] : getenv("FS_BENCH_DIR");
	const char *mib = getenv("FS_BENCH_FILE_MIB");
	char path[PATH_BYTES];
	int failures = 0;
	if (!dir || !*dir)
		dir = DEFAULT_DIR;
	file_bytes = (mib && atoi(mib) > 0 ? (uint64_t)atoi(mib) : FILE_MIB) * MIB;
	if (ensure_directory(dir) < 0) {
		fail("start", "directory");
		report("fs-bench: FAIL\n");
		return 1;
	}
	snprintf(path, sizeof path, "%s/data", dir);
#ifdef CUBIT
	/* Two instances start: the first to claim is the benchmark, the other
	 * the other process of the parked-handle checks. */
	char claim[NAME_BYTES];
	name_in(claim, dir, "claim");
	int claimed = open(claim, O_CREAT | O_EXCL | O_WRONLY, FILE_MODE);
	if (claimed < 0 && errno == EEXIST) {
		int r = other_process(dir);
		report("fs-bench: other process %s\n", r < 0 ? "FAIL" : "done");
		_exit(r < 0);           /* no libc cleanup: the service's must do */
	}
	if (claimed >= 0) close(claimed);
#endif
	report("fs-bench: start dir=%s file_mib=%llu chunk=%d block=%d random_ops=%d fsync_ops=%d small_files=%d\n",
	       dir, (unsigned long long)(file_bytes / MIB), (int)CHUNK_BYTES, BLOCK_BYTES,
	       RANDOM_OPS, FSYNC_OPS, SMALL_FILES);
	calibrate();
	if (coherence(dir) < 0) failures++;
	for (int r = 1; r <= ROUNDS; r++) {
		if (seq_write(path, r) < 0) { failures++; continue; }
		if (seq_read(path, "seq-read-warm", r) < 0) failures++;
		drop_caches();
		if (seq_read(path, "seq-read-cold", r) < 0) failures++;
		if (rand_read(path, r) < 0) failures++;
		if (rand_write(path, "rand-write", r, RANDOM_OPS, 0) < 0) failures++;
		if (rand_write(path, "rand-write-fsync", r, FSYNC_OPS, 1) < 0) failures++;
		if (small_files(dir, r) < 0) failures++;
	}
	if (unlink(path) < 0 && errno != ENOSYS)
		failures++;
#ifdef CUBIT
	unlink(claim);
#endif
	report(failures ? "fs-bench: FAIL\n" : "fs-bench: done\n");
	return failures != 0;
}
