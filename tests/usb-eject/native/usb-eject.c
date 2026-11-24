/*
 * CuBit
 * Copyright (C) 2026 Jon Andrew
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * USB sticks plugged in after boot, ejected, pulled out and plugged in
 * again (FS-034, headless test usb-eject). The runner (drive.py) plugs and
 * pulls the sticks on this program's markers; one xHCI controller, empty
 * at boot.
 *
 *  1. "usb-eject: plug A": stick A is plugged in; it must attach as
 *     @usb:0. Files are written and read back.
 *  2. Eject with a file open is refused (Open_Handles, count 1); after the
 *     close it is Safe_To_Remove, and @usb:0 is gone at once.
 *     "usb-eject: pull A" (the runner unplugs it), then "usb-eject: plug A
 *     again": the files are there, byte for byte.
 *  3. "usb-eject: plug B": stick B beside A on the same controller
 *     (@usb:1): both are written and read.
 *  4. "usb-eject: surprise A": A is pulled out while a stream is written to
 *     it; writes must fail within REMOVAL_LIMIT_S (nothing hangs), and a
 *     handle opened before is refused by the service too (fsync).
 *  5. "usb-eject: plug A dirty": A comes back; it was cut while dirty, so
 *     FAT32's mount policy mounts it read only (a write is EROFS) and its
 *     earlier files still read back.
 *  6. B is ejected (Safe_To_Remove), A too; "usb-eject: end".
 * The runner then checks B with fsck.fat -n (clean) and A (its dirty mark).
 * Markers: "usb-eject: PASS" or "usb-eject: FAIL <what>".
 */
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>
#include <cubit/debug.h>

/* The libc's eject (CuBit.Libc_Files.Volume_Eject, CuBit.Volume_Ejection):
 * the outcome's code, or -errno. */
extern long __cubit_volume_eject(const char *locator, int close_open, unsigned long long *open);
enum { SAFE_TO_REMOVE = 1, OPEN_HANDLES = 2, NOT_REMOVABLE = 3, NOT_ATTACHED = 4,
       FLUSH_FAILED = 5, ACCESS_DENIED = 6 };

#define STICK_A "@usb:0"
#define STICK_B "@usb:1"
#define PATH_BYTES 256
#define ATTACH_LIMIT_S 60
#define REMOVAL_LIMIT_S 30
#define CHUNK 65536
#define FILE_MODE 0644

static int failed;

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
	cubit_debug_write(line, (size_t)n);
}

static void fail(const char *what)
{
	if (!failed)
		report("usb-eject: FAIL %s (errno %d)\n", what, errno);
	failed = 1;
}

static unsigned char pattern(unsigned k, unsigned long i)
{
	return (unsigned char)(i * 29u + k * 13u + (i >> 9));
}

static long elapsed_ms(const struct timespec *since)
{
	struct timespec now;
	clock_gettime(CLOCK_MONOTONIC, &now);
	return (now.tv_sec - since->tv_sec) * 1000 + (now.tv_nsec - since->tv_nsec) / 1000000;
}

static void pause_ms(long ms)
{
	struct timespec p = { ms / 1000, (ms % 1000) * 1000000 };
	nanosleep(&p, NULL);
}

static int present(const char *volume)
{
	char root[PATH_BYTES];
	snprintf(root, sizeof root, "%s/", volume);
	DIR *d = opendir(root);
	if (d)
		closedir(d);
	return d != NULL;
}

static int wait_present(const char *volume, int want)
{
	for (int tenth = 0; tenth < ATTACH_LIMIT_S * 10; tenth++) {
		if (present(volume) == want)
			return 1;
		pause_ms(100);
	}
	return 0;
}

static int write_file(const char *volume, const char *name, unsigned k, unsigned long size)
{
	static unsigned char buf[CHUNK];
	char path[PATH_BYTES];
	snprintf(path, sizeof path, "%s/%s", volume, name);
	int fd = open(path, O_CREAT | O_TRUNC | O_WRONLY, FILE_MODE);
	if (fd < 0)
		return 0;
	for (unsigned long done = 0; done < size;) {
		unsigned long n = size - done < CHUNK ? size - done : CHUNK;
		for (unsigned long i = 0; i < n; i++)
			buf[i] = pattern(k, done + i);
		if (write(fd, buf, n) != (ssize_t)n) {
			close(fd);
			return 0;
		}
		done += n;
	}
	int ok = fsync(fd) == 0;
	return close(fd) == 0 && ok;
}

static int check_file(const char *volume, const char *name, unsigned k, unsigned long size)
{
	static unsigned char buf[CHUNK];
	char path[PATH_BYTES];
	snprintf(path, sizeof path, "%s/%s", volume, name);
	int fd = open(path, O_RDONLY);
	if (fd < 0)
		return 0;
	unsigned long done = 0;
	for (;;) {
		ssize_t n = read(fd, buf, sizeof buf);
		if (n < 0) {
			close(fd);
			return 0;
		}
		if (n == 0)
			break;
		for (ssize_t i = 0; i < n; i++)
			if (buf[i] != pattern(k, done + (unsigned long)i)) {
				close(fd);
				return 0;
			}
		done += (unsigned long)n;
	}
	close(fd);
	return done == size;
}

static long eject(const char *volume, int close_open, unsigned long long *open_count)
{
	unsigned long long ignored;
	return __cubit_volume_eject(volume, close_open, open_count ? open_count : &ignored);
}

/* Writes a stream to the stick until it is pulled out; every failure must
 * come within REMOVAL_LIMIT_S. */
static void surprise(void)
{
	static unsigned char chunk[CHUNK];
	char path[PATH_BYTES];
	struct timespec start;
	int asked = 0;
	unsigned long written = 0;

	for (unsigned long i = 0; i < sizeof chunk; i++)
		chunk[i] = pattern(77, i);
	/* A handle opened before the removal, never written: its next use
	 * must fail too (the service closed it with its volume). */
	snprintf(path, sizeof path, "%s/kept.bin", STICK_A);
	int kept = open(path, O_RDONLY);
	snprintf(path, sizeof path, "%s/stream.bin", STICK_A);
	int fd = open(path, O_CREAT | O_TRUNC | O_WRONLY, FILE_MODE);
	if (fd < 0 || kept < 0) {
		fail("open before surprise removal");
		return;
	}
	clock_gettime(CLOCK_MONOTONIC, &start);
	for (;;) {
		struct timespec before;
		clock_gettime(CLOCK_MONOTONIC, &before);
		ssize_t n = write(fd, chunk, sizeof chunk);
		if (n != (ssize_t)sizeof chunk || (written % (8 * CHUNK) == 0 && fsync(fd) != 0)) {
			if (!asked) {
				fail("write before the surprise removal");
				return;
			}
			if (elapsed_ms(&before) > REMOVAL_LIMIT_S * 1000L) {
				fail("a write hung after the removal");
				return;
			}
			report("usb-eject: removal seen ms=%ld errno=%d\n", elapsed_ms(&start), errno);
			break;
		}
		written += (unsigned long)n;
		if (!asked && written >= 4 * CHUNK) {
			report("usb-eject: surprise A\n");
			asked = 1;
			clock_gettime(CLOCK_MONOTONIC, &start);
		}
		if (asked && elapsed_ms(&start) > REMOVAL_LIMIT_S * 1000L) {
			fail("writes never failed after the removal");
			return;
		}
	}
	close(fd);
	/* Wait for devmgr's detach: then the kept handle is closed too. */
	if (!wait_present(STICK_A, 0)) {
		fail("@usb:0 still present after the removal");
		return;
	}
	/* Pages it already holds may still be read from the libc's cache
	 * (as from Linux's page cache after a stick is yanked); anything that
	 * reaches the service is refused: the handle was closed with its
	 * volume (REPLY_VOLUME_GONE, EIO). */
	if (fsync(kept) == 0) {
		fail("a handle on the removed stick still synced");
		return;
	}
	report("usb-eject: kept handle refused errno=%d\n", errno);
	close(kept);
}

int main(void)
{
	unsigned long long open_count = 0;
	long outcome;
	char path[PATH_BYTES];

	report("usb-eject: start\n");
	if (present(STICK_A))
		fail("@usb:0 before any stick was plugged in");

	/* 1. Hot-plug: nothing at boot, then A. */
	report("usb-eject: plug A\n");
	if (!failed && !wait_present(STICK_A, 1))
		fail("stick A never attached as @usb:0");
	if (!failed && (!write_file(STICK_A, "first.bin", 1, 300000) ||
			!write_file(STICK_A, "kept.bin", 2, 5000) ||
			!check_file(STICK_A, "first.bin", 1, 300000)))
		fail("write and read back on stick A");
	if (!failed)
		report("usb-eject: A written\n");

	/* 2. Eject: refused while a file is open, then safe. */
	if (!failed) {
		/* A file opened afresh (a reopen of a closed file can come from
		 * the libc's park of closed handles, which the service cannot
		 * tell from closed: Eject counts it as closed and closes it). */
		snprintf(path, sizeof path, "%s/held.txt", STICK_A);
		int fd = open(path, O_CREAT | O_EXCL | O_WRONLY, FILE_MODE);
		outcome = eject(STICK_A, 0, &open_count);
		if (fd < 0 || outcome != OPEN_HANDLES || open_count != 1)
			fail("eject with a file open was not refused (Open_Handles, 1)");
		else
			report("usb-eject: eject refused open=%llu\n", open_count);
		if (fd >= 0)
			close(fd);
	}
	if (!failed) {
		outcome = eject("@nvme:0", 0, NULL);
		if (outcome != NOT_REMOVABLE && outcome != ACCESS_DENIED)
			fail("a fixed disk was not refused");
	}
	if (!failed) {
		outcome = eject(STICK_A, 0, NULL);
		if (outcome != SAFE_TO_REMOVE)
			fail("eject of the idle stick");
		else if (present(STICK_A))
			fail("@usb:0 still listed after its eject");
		else if (eject(STICK_A, 0, NULL) != NOT_ATTACHED)
			fail("a second eject was not Not_Attached");
		else
			report("usb-eject: A safe to remove\n");
	}
	if (!failed) {
		report("usb-eject: pull A\n");
		pause_ms(500);
		report("usb-eject: plug A again\n");
		if (!wait_present(STICK_A, 1))
			fail("stick A never attached again");
		else if (!check_file(STICK_A, "first.bin", 1, 300000) ||
			 !check_file(STICK_A, "kept.bin", 2, 5000))
			fail("stick A's files after Eject and re-plug");
		else
			report("usb-eject: A data intact\n");
	}

	/* 3. Two sticks on one controller. */
	if (!failed) {
		report("usb-eject: plug B\n");
		if (!wait_present(STICK_B, 1))
			fail("stick B never attached as @usb:1");
		else if (!write_file(STICK_B, "b.bin", 3, 200000) ||
			 !write_file(STICK_A, "second.bin", 4, 100000) ||
			 !check_file(STICK_B, "b.bin", 3, 200000) ||
			 !check_file(STICK_A, "second.bin", 4, 100000))
			fail("two sticks at once");
		else
			report("usb-eject: two sticks ok\n");
	}

	/* 4. Pulled out without Eject while written. */
	if (!failed)
		surprise();

	/* 5. Back, dirty: mounted read only by FAT32's policy. */
	if (!failed) {
		report("usb-eject: plug A dirty\n");
		if (!wait_present(STICK_A, 1)) {
			fail("stick A never attached after the surprise removal");
		} else {
			snprintf(path, sizeof path, "%s/after.bin", STICK_A);
			int fd = open(path, O_CREAT | O_WRONLY, FILE_MODE);
			int why = errno;
			if (fd >= 0) {
				close(fd);
				fail("a dirty stick was mounted writable");
			} else if (!check_file(STICK_A, "first.bin", 1, 300000) ||
				   !check_file(STICK_B, "b.bin", 3, 200000)) {
				fail("files after the surprise removal");
			} else {
				report("usb-eject: dirty A read only errno=%d\n", why);
			}
		}
	}

	/* 6. Eject both (B is clean afterwards). */
	if (!failed) {
		if (eject(STICK_B, 1, NULL) != SAFE_TO_REMOVE)
			fail("eject of stick B");
		else if (eject(STICK_A, 1, NULL) != SAFE_TO_REMOVE)
			fail("eject of the read-only stick A");
		else
			report("usb-eject: both ejected\n");
	}
	if (!failed)
		report("usb-eject: PASS\n");
	report("usb-eject: end\n");
	return failed;
}
