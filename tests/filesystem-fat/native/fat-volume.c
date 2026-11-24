/*
 * CuBit
 * Copyright (C) 2026 Jon Andrew
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * FAT32 on a real CuBit (FS-030, headless test fat32-volume): the files a
 * Linux host put on the stick are read, then a tree is written, read back,
 * renamed, removed and listed, all through the libc and filesystem.svc on
 * a second NVMe disk (@nvme:1). The host then checks the image with
 * fsck.fat -n and compares every file (check-image.py, the same patterns).
 *
 * Linux: fat-volume DIRECTORY (a mounted copy, to check the program).
 * Markers: "fat-volume: start", "fat-volume: PASS" or "fat-volume: FAIL
 * <what>", then "fat-volume: end" (QEMU stops there).
 *
 * With REMOVAL_VOLUME (headless usb-disk, the same suite on a USB stick,
 * @usb:0): the program first waits for @usb:0 to attach, then asks for
 * the second stick ("fat-volume: plug removal stick", the runner hot-plugs
 * it) and waits for REMOVAL_VOLUME. After the suite the second stick is
 * written to without pause, "fat-volume: unplug" asks the runner to
 * remove it (device_del), and every write and fsync after that must fail
 * within REMOVAL_LIMIT_S seconds, nothing hanging: "fat-volume: removal
 * seen ms=N", or FAIL.
 */
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>
#ifdef CUBIT
#include <cubit/debug.h>
#ifndef VOLUME
#define VOLUME "@nvme:1"
#endif
#endif

#define PATH_BYTES 512
#define CHUNK 50000          /* not a sector or cluster multiple */
#define DIR_MODE 0755
#define FILE_MODE 0644

static const char *volume;

static void report(const char *fmt, ...)
{
	char line[512];
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

/* The bytes of file number k (check-image.py makes the same). */
static unsigned char pattern(unsigned k, unsigned long i)
{
	return (unsigned char)(i * 31u + k * 7u + (i >> 8));
}

static void path_of(char *out, const char *name)
{
	snprintf(out, PATH_BYTES, "%s/%s", volume, name);
}

static const char *wrong;
static void fail(const char *what, const char *name)
{
	static char why[PATH_BYTES + 64];
	if (wrong)
		return;
	snprintf(why, sizeof why, "%s %s (errno %d)", what, name, errno);
	wrong = why;
}

static void write_file(const char *name, unsigned k, unsigned long bytes)
{
	char path[PATH_BYTES];
	static unsigned char buffer[CHUNK];
	path_of(path, name);
	int fd = open(path, O_CREAT | O_TRUNC | O_WRONLY, FILE_MODE);
	if (fd < 0)
		return fail("create", name);
	for (unsigned long at = 0; at < bytes;) {
		unsigned long n = bytes - at < CHUNK ? bytes - at : CHUNK;
		for (unsigned long i = 0; i < n; i++)
			buffer[i] = pattern(k, at + i);
		if (write(fd, buffer, n) != (ssize_t)n) {
			close(fd);
			return fail("write", name);
		}
		at += n;
	}
	if (fsync(fd) != 0)
		fail("fsync", name);
	if (close(fd) != 0)
		fail("close", name);
}

static void check_file(const char *name, unsigned k, unsigned long bytes)
{
	char path[PATH_BYTES];
	static unsigned char buffer[CHUNK + 1];
	struct stat st;
	path_of(path, name);
	if (stat(path, &st) != 0 || (unsigned long)st.st_size != bytes || !S_ISREG(st.st_mode))
		return fail("stat", name);
	int fd = open(path, O_RDONLY);
	if (fd < 0)
		return fail("open", name);
	unsigned long at = 0;
	for (;;) {
		ssize_t n = read(fd, buffer, CHUNK + 1);
		if (n < 0) {
			close(fd);
			return fail("read", name);
		}
		if (n == 0)
			break;
		for (ssize_t i = 0; i < n; i++)
			if (buffer[i] != pattern(k, at + (unsigned long)i)) {
				close(fd);
				return fail("contents", name);
			}
		at += (unsigned long)n;
	}
	close(fd);
	if (at != bytes)
		fail("length", name);
}

/* The names a directory lists, one count; each name must be there. */
static void check_listing(const char *dir, const char *const *names, int count)
{
	char path[PATH_BYTES];
	path_of(path, dir);
	DIR *d = opendir(path);
	if (!d)
		return fail("opendir", dir);
	int seen = 0, listed = 0;
	struct dirent *e;
	while ((e = readdir(d))) {
		if (!strcmp(e->d_name, ".") || !strcmp(e->d_name, ".."))
			continue;
		listed++;
		for (int i = 0; i < count; i++)
			if (!strcmp(e->d_name, names[i]))
				seen++;
	}
	closedir(d);
	if (seen != count || listed != count)
		fail("listing", dir);
}

static void make_dir(const char *name)
{
	char path[PATH_BYTES];
	path_of(path, name);
	if (mkdir(path, DIR_MODE) != 0)
		fail("mkdir", name);
}

#ifdef LIVE_VOLUME
/* The boot medium beside the stick (headless usb-disk: an ISO 9660 image
 * on a USB stick on stick 1's controller): read as the optical volume
 * (whose root is the medium's apps/ directory), never writable. */
static void live_medium(void)
{
	static const char expected[] = "usb-disk live medium\n";
	char path[PATH_BYTES], got[64];
	snprintf(path, sizeof path, "%s/readme.txt", LIVE_VOLUME);
	int fd = open(path, O_RDONLY);
	ssize_t n = fd >= 0 ? read(fd, got, sizeof got) : -1;
	if (fd >= 0)
		close(fd);
	if (n != (ssize_t)(sizeof expected - 1) || memcmp(got, expected, sizeof expected - 1) != 0) {
		fail("read", "live medium readme");
		return;
	}
	snprintf(path, sizeof path, "%s/written.txt", LIVE_VOLUME);
	fd = open(path, O_CREAT | O_WRONLY, FILE_MODE);
	if (fd >= 0) {
		close(fd);
		fail("created a file on", "the live medium");
		return;
	}
	report("fat-volume: live medium read only ok\n");
}
#endif

#ifdef REMOVAL_VOLUME
#define ATTACH_LIMIT_S 60

/* Until the volume's root can be listed (it is attached and mounted). */
static void wait_for_volume(const char *name)
{
	char root[PATH_BYTES];
	snprintf(root, sizeof root, "%s/", name);
	for (int tenth = 0; tenth < ATTACH_LIMIT_S * 10; tenth++) {
		DIR *d = opendir(root);
		if (d) {
			closedir(d);
			report("fat-volume: %s attached\n", name);
			return;
		}
		struct timespec pause = { 0, 100000000 };
		nanosleep(&pause, NULL);
	}
	fail("never attached", name);
}

#define REMOVAL_LIMIT_S 30
#define REMOVAL_CHUNK 65536

static long elapsed_ms(const struct timespec *since)
{
	struct timespec now;
	clock_gettime(CLOCK_MONOTONIC, &now);
	return (now.tv_sec - since->tv_sec) * 1000 + (now.tv_nsec - since->tv_nsec) / 1000000;
}

/* Writes to REMOVAL_VOLUME until the stick is gone; every failure must be
 * prompt. */
static void removal(void)
{
	static unsigned char chunk[REMOVAL_CHUNK];
	char path[PATH_BYTES];
	struct timespec start;
	int fd, asked = 0;
	unsigned long written = 0;

	for (unsigned long i = 0; i < sizeof chunk; i++)
		chunk[i] = pattern(99, i);
	snprintf(path, sizeof path, "%s/stream.bin", REMOVAL_VOLUME);
	fd = open(path, O_CREAT | O_TRUNC | O_WRONLY, FILE_MODE);
	if (fd < 0) {
		fail("open", "removal stream");
		return;
	}
	clock_gettime(CLOCK_MONOTONIC, &start);
	for (;;) {
		struct timespec before;
		clock_gettime(CLOCK_MONOTONIC, &before);
		int ok = write(fd, chunk, sizeof chunk) == (ssize_t)sizeof chunk && fsync(fd) == 0;
		long took = elapsed_ms(&before);
		if (!ok) {
			if (!asked) {
				fail("write before unplug", "removal stream");
				break;
			}
			report("fat-volume: removal seen ms=%ld written=%lu last-call-ms=%ld\n",
			       elapsed_ms(&start), written, took);
			if (took > REMOVAL_LIMIT_S * 1000L)
				fail("slow failure", "removal stream");
			break;
		}
		written += sizeof chunk;
		if (!asked) {
			report("fat-volume: unplug\n");
			asked = 1;
			clock_gettime(CLOCK_MONOTONIC, &start);
		}
		if (elapsed_ms(&start) > REMOVAL_LIMIT_S * 1000L) {
			fail("never failed after unplug", "removal stream");
			break;
		}
		/* Wrap before the stick fills: keep rewriting the start. */
		if (written >= 8UL * 1024 * 1024 && lseek(fd, 0, SEEK_SET) == 0)
			written = 0;
	}
	close(fd);
	/* A new file on the gone disk is refused at once. */
	snprintf(path, sizeof path, "%s/after.txt", REMOVAL_VOLUME);
	fd = open(path, O_CREAT | O_WRONLY, FILE_MODE);
	if (fd >= 0) {
		int durable = write(fd, chunk, 512) == 512 && fsync(fd) == 0;
		close(fd);
		if (durable)
			fail("wrote after unplug", "after.txt");
	}
}
#endif

int main(int argc, char **argv)
{
#ifdef CUBIT
	(void)argc;
	(void)argv;
	volume = VOLUME;
#else
	if (argc < 2) {
		fprintf(stderr, "usage: fat-volume DIRECTORY\n");
		return 2;
	}
	volume = argv[1];
#endif
	report("fat-volume: start\n");
#ifdef REMOVAL_VOLUME
	/* USB sticks attach as devmgr hears of them (FS-034), after the
	 * programs at startup may run: wait for the volumes to appear. */
	wait_for_volume(VOLUME);
	report("fat-volume: plug removal stick\n");
	wait_for_volume(REMOVAL_VOLUME);
#endif
	/* What the Linux host wrote (check-image.py): a long name, Unicode,
	 * a subdirectory, an empty file. */
	check_file("from linux/hello from linux.txt", 1, 1234);
	check_file("from linux/Gr\xc3\xbc\xc3\x9f" "e.bin", 2, 70000);
	check_file("from linux/empty", 3, 0);
	{
		const char *names[] = { "hello from linux.txt", "Gr\xc3\xbc\xc3\x9f" "e.bin", "empty", "nested" };
		check_listing("from linux", names, 4);
	}
	/* A tree written here. */
	make_dir("cubit");
	make_dir("cubit/tree");
	make_dir("cubit/tree/deeper and deeper");
	write_file("cubit/README.TXT", 10, 600);
	write_file("cubit/tree/a file with a long name.dat", 11, 300001);
	write_file("cubit/tree/deeper and deeper/big.bin", 12, 2100000);
	write_file("cubit/tree/\xce\xb1\xce\xb2\xce\xb3 \xe6\x97\xa5\xe6\x9c\xac.txt", 13, 4096);
	write_file("cubit/tree/zero", 14, 0);
	write_file("cubit/to rename.txt", 15, 5000);
	write_file("cubit/to remove.txt", 16, 7000);
	make_dir("cubit/empty dir");
	for (int i = 0; i < 40; i++) {
		char name[64];
		snprintf(name, sizeof name, "cubit/many/entry %02d of the crowded directory.txt", i);
		if (i == 0)
			make_dir("cubit/many");
		write_file(name, 20 + (unsigned)i, (unsigned long)(i * 101));
	}
	check_file("cubit/tree/a file with a long name.dat", 11, 300001);
	check_file("cubit/tree/deeper and deeper/big.bin", 12, 2100000);
	{
		char a[PATH_BYTES], b[PATH_BYTES];
		path_of(a, "cubit/to rename.txt");
		path_of(b, "cubit/tree/renamed across directories.txt");
		if (rename(a, b) != 0)
			fail("rename", "to rename.txt");
		path_of(a, "cubit/to remove.txt");
		if (unlink(a) != 0)
			fail("unlink", "to remove.txt");
		path_of(a, "cubit/empty dir");
		if (rmdir(a) != 0)
			fail("rmdir", "empty dir");
#ifdef CUBIT
		/* Windows cannot hold these names: refused (FAT32 only). */
		path_of(a, "cubit/bad:name");
		if (open(a, O_CREAT | O_WRONLY, FILE_MODE) >= 0)
			fail("accepted", "bad:name");
		path_of(a, "cubit/con.txt");
		if (open(a, O_CREAT | O_WRONLY, FILE_MODE) >= 0)
			fail("accepted", "con.txt");
#endif
	}
	check_file("cubit/tree/renamed across directories.txt", 15, 5000);
	check_file("cubit/README.TXT", 10, 600);
#ifdef CUBIT
	check_file("cubit/readme.txt", 10, 600);   /* case-insensitive, as FAT */
#endif
	{
		const char *names[] = { "a file with a long name.dat", "deeper and deeper",
					"\xce\xb1\xce\xb2\xce\xb3 \xe6\x97\xa5\xe6\x9c\xac.txt", "zero",
					"renamed across directories.txt" };
		check_listing("cubit/tree", names, 5);
	}
	{
		/* Every write is durable and the volume clean (FAT32: the flush). */
		char path[PATH_BYTES];
		path_of(path, "cubit");
		int dir = open(path, O_RDONLY | O_DIRECTORY);
		if (dir < 0 || fsync(dir) != 0)
			fail("fsync", "cubit");
		if (dir >= 0)
			close(dir);
	}
#ifdef LIVE_VOLUME
	if (!wrong)
		live_medium();
#endif
#ifdef REMOVAL_VOLUME
	if (!wrong)
		removal();
#endif
	if (wrong)
		report("fat-volume: FAIL %s\n", wrong);
	else
		report("fat-volume: PASS\n");
	report("fat-volume: end\n");
	return wrong != 0;
}
