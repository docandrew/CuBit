/*
 * CuBit libc: files through the filesystem service (docs/servo-port.md).
 *
 * A client of filesystem.svc's typed protocol
 * (userspace/runtime/gnat/cubit-filesystems.ads), over the capability in
 * the fixed filesystem slot. The service checks every path against the
 * program's filesystem scopes (its manifest's `filesystem-scope` entries);
 * this library adds no authority and no policy of its own.
 *
 * Names: a CuBit path ("@nvme:0/fonts/a.ttf") is used as is. A POSIX
 * absolute path names the same place on the system volume
 * ("/fonts/a.ttf" -> "@nvme:0/fonts/a.ttf"); relative paths start at the
 * system volume's root (there is no working directory). The system
 * volume is named by its device today; CuBit is volume-first with
 * machine-config aliases (@nvme0 = @mydisk), so this should become an
 * alias, "@system", once aliases are resolved (not implemented yet).
 *
 * Transfers go through one bounce buffer lent to the service once, as a
 * shared-memory grant; requests are serialized on it. Read-only for now.
 */
#define _GNU_SOURCE
#include <errno.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>
#include "lock.h"
#include "cubit_fd.h"
#include <cubit/debug.h>

#define SLOT_FILESYSTEM 1
#define SYSTEM_VOLUME "@nvme:0"        /* TODO: "@system" once aliases resolve */

enum {
	SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY = 41,
	SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY = 106,
	SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION = 108,
};

enum {
	OP_OPEN = 0x0001, OP_CLOSE = 0x0002, OP_OPEN_DIRECTORY = 0x0005,
	OP_SEEK = 0x0006, OP_READ_DIRECTORY_PAGE = 0x0007,
	OP_CLOSE_DIRECTORY = 0x0009, OP_READ_AT = 0x000D,
};

enum {
	REPLY_OK = 0xF000, REPLY_NO_SPACE = 0xF002, REPLY_READ_ONLY = 0xF003,
	REPLY_OUT_OF_RANGE = 0xF004, REPLY_ACCESS_DENIED = 0xF007,
	REPLY_WRONG_OBJECT_TYPE = 0xF009, REPLY_ALREADY_EXISTS = 0xF00A,
	REPLY_NOT_FOUND = 0xF00B,
};

#define PROTOCOL_VERSION 1
#define SEEK_FROM_END 2
#define BOUNCE_PAGES 64
#define BOUNCE_BYTES (BOUNCE_PAGES * 4096UL)
#define MAX_PATH 1024

/* CuBit.Messages.Message: 48 bytes. */
struct message {
	uint32_t label;
	uint8_t length, flags;
	uint16_t reserved;
	uint64_t authority;
	uint64_t words[4];
};

static inline unsigned long cubit(unsigned long n, unsigned long a,
	unsigned long b, unsigned long c, unsigned long d)
{
	unsigned long ret;
	register unsigned long r10 __asm__("r10") = d;
	__asm__ __volatile__ ("syscall" : "=a"(ret)
		: "a"(n), "D"(a), "S"(b), "d"(c), "r"(r10)
		: "rcx", "r11", "memory");
	return ret;
}

static volatile int fs_lock[1];
static unsigned char *bounce;
static uint64_t grant_slot, grant_generation;

/* Caller holds fs_lock. */
static int lend_bounce(void)
{
	if (bounce) return 0;
	void *p = mmap(0, BOUNCE_BYTES, PROT_READ | PROT_WRITE,
		MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	if (p == MAP_FAILED) return -ENOMEM;
	unsigned long slot = cubit(SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY,
		SLOT_FILESYSTEM, (unsigned long)p, BOUNCE_PAGES, 1);
	if (slot == (unsigned long)-1) return -EACCES;  /* no filesystem capability */
	unsigned long gen = cubit(SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION,
		slot, 0, 0, 0);
	if (gen == (unsigned long)-1 || gen == 0) return -EACCES;
	grant_slot = slot;
	grant_generation = gen;
	bounce = p;
	return 0;
}

static uint32_t call(uint32_t label, uint8_t length, uint64_t w0, uint64_t w1,
	uint64_t w2, uint64_t w3, uint64_t *reply0)
{
	struct message m = { label, length, 0, 0, 0, { w0, w1, w2, w3 } };
	unsigned long tag = cubit(SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY,
		SLOT_FILESYSTEM, (unsigned long)&m, 0, 0);
	if (reply0) *reply0 = m.words[0];
	return (uint32_t)tag;
}

static long to_errno(uint32_t label)
{
	switch (label) {
	case REPLY_NOT_FOUND:         return -ENOENT;
	case REPLY_ACCESS_DENIED:     return -EACCES;
	case REPLY_WRONG_OBJECT_TYPE: return -ENOTDIR;
	case REPLY_ALREADY_EXISTS:    return -EEXIST;
	case REPLY_READ_ONLY:         return -EROFS;
	case REPLY_NO_SPACE:          return -ENOSPC;
	case REPLY_OUT_OF_RANGE:      return -EINVAL;
	default:                      return -EIO;
	}
}

/* The CuBit name for a path; its length, or -errno. */
static long cubit_name(const char *path, char *out)
{
	size_t n = 0;
	if (!path || !*path) return -ENOENT;
	if (path[0] != '@') {
		memcpy(out, SYSTEM_VOLUME, sizeof SYSTEM_VOLUME - 1);
		n = sizeof SYSTEM_VOLUME - 1;
		out[n++] = '/';
	}
	for (const char *p = path; *p; ) {
		/* Drop empty and "." components; the service rejects "..". */
		if (*p == '/') { p++; continue; }
		const char *end = strchrnul(p, '/');
		size_t len = (size_t)(end - p);
		if (!(len == 1 && p[0] == '.')) {
			if (out[n - 1] != '/' && p != path) out[n++] = '/';
			if (n + len >= MAX_PATH) return -ENAMETOOLONG;
			memcpy(out + n, p, len);
			n += len;
		}
		p = end;
	}
	return (long)n;
}

hidden long __cubit_file_open(const char *path, int directory,
	uint64_t *handle, uint64_t *size)
{
	char name[MAX_PATH];
	long len = cubit_name(path, name);
	if (len < 0) return len;
	LOCK(fs_lock);
	long r = lend_bounce();
	if (r) { UNLOCK(fs_lock); return r; }
	memcpy(bounce, name, (size_t)len);
	uint64_t h = 0, sz = 0;
	uint32_t label = directory
		? call(OP_OPEN_DIRECTORY, 3, grant_slot, (uint64_t)len, grant_generation, 0, &h)
		: call(OP_OPEN, 4, grant_slot, (uint64_t)len, 0 /* read-only */, grant_generation, &h);
	if (label != REPLY_OK) {
		UNLOCK(fs_lock);
		if (label == REPLY_ACCESS_DENIED) {
			/* A request for authority the program lacks: say which
			 * (diagnostics console). */
			static const char pre[] = "cubit-libc: denied: ";
			cubit_debug_write(pre, sizeof pre - 1);
			cubit_debug_write(name, (size_t)len);
			cubit_debug_write("\n", 1);
		}
		return to_errno(label);
	}
	if (!directory) {
		label = call(OP_SEEK, 3, h, 0, SEEK_FROM_END, 0, &sz);
		if (label != REPLY_OK) {
			call(OP_CLOSE, 1, h, 0, 0, 0, 0);
			UNLOCK(fs_lock);
			return to_errno(label);
		}
	}
	UNLOCK(fs_lock);
	*handle = h;
	if (size) *size = sz;
	return 0;
}

hidden void __cubit_file_close(uint64_t handle, int directory)
{
	LOCK(fs_lock);
	call(directory ? OP_CLOSE_DIRECTORY : OP_CLOSE, 1, handle, 0, 0, 0, 0);
	UNLOCK(fs_lock);
}

hidden long __cubit_file_read_at(uint64_t handle, void *buf, size_t count,
	uint64_t offset)
{
	size_t done = 0;
	LOCK(fs_lock);
	long r = lend_bounce();
	if (r) { UNLOCK(fs_lock); return r; }
	while (done < count) {
		size_t want = count - done > BOUNCE_BYTES ? BOUNCE_BYTES : count - done;
		uint64_t got = 0;
		uint64_t ref = (grant_generation << 32) | grant_slot;
		uint32_t label = call(OP_READ_AT, 4, handle, ref, want, offset + done, &got);
		if (label != REPLY_OK) {
			if (got && got <= want) {
				memcpy((char *)buf + done, bounce, got);
				done += got;
			}
			UNLOCK(fs_lock);
			return done ? (long)done : to_errno(label);
		}
		if (got > want) got = want;
		memcpy((char *)buf + done, bounce, got);
		done += got;
		if (got < want) break;          /* end of file */
	}
	UNLOCK(fs_lock);
	return (long)done;
}

/* One Directory.Page.V1 into page (4096 bytes). */
hidden long __cubit_dir_read_page(uint64_t handle, void *page)
{
	LOCK(fs_lock);
	long r = lend_bounce();
	if (r) { UNLOCK(fs_lock); return r; }
	uint32_t label = call(OP_READ_DIRECTORY_PAGE, 4, handle, grant_slot,
		PROTOCOL_VERSION, grant_generation, 0);
	if (label == REPLY_OK) memcpy(page, bounce, 4096);
	UNLOCK(fs_lock);
	return label == REPLY_OK ? 0 : to_errno(label);
}
