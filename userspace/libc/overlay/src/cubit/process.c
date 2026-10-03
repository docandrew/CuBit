/*
 * CuBit libc: starting programs and collecting their exit status
 * (docs/process-arguments.md). There is no fork and no exec: procmgr starts
 * a program by name with OP_LAUNCH, given its argument vector and
 * environment as a launch block, and the kernel tells the launcher when it
 * ends (EVENT_CHILD_EXIT). posix_spawn and waitpid/wait4 are built on that.
 *
 * Authority: the program needs procmgr's endpoint (manifest:
 * (request-service process-manager read-write process-manager), slot 12).
 * Arguments and environment are data; they never change what authority the
 * child gets, which comes from its own manifest and procmgr's policy.
 *
 * Not supported: file actions (no descriptors cross processes yet: ENOTSUP),
 * spawn attributes (accepted, without effect: no signals, groups or
 * scheduler classes), PATH search (names are procmgr's; a leading '/' is
 * dropped), and waiting for processes this program did not start.
 */
#define _GNU_SOURCE
#include <errno.h>
#include <spawn.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include "lock.h"
#include <cubit/launch.h>

/* Fixed binding of the process-manager service
 * (userspace/ccl/catalogs/native-runtime-services.ccl). */
#define SLOT_PROCESS_MANAGER 12

enum {
	SYSCALL_SLEEP_UNTIL_MONOTONIC_MICROSECOND = 119,
	SYSCALL_READ_MONOTONIC_MICROSECONDS = 114,
	SYSCALL_RECEIVE_EVENT_NB = 26,
	SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY = 41,
	SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY = 106,
	SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION = 108,
	SYSCALL_WAIT_FOR_IPC_OR_COMPLETION_UNTIL_MONOTONIC_MILLISECOND = 113,
};

enum { REPLY_OK = 0xF000, REPLY_ERR = 0xF001 };
#define REQUEST_WORDS 4
#define GRANT_READ_ONLY 0
#define WAIT_FOREVER (~0UL)
/* A wakeup that brought no exit event (other traffic is queued for other
 * parts of the program): pause this long before looking again. */
#define IDLE_PAUSE_MICROSECONDS 1000UL

#define PAGE_BYTES 4096UL
#define REQUEST_BYTES (CUBIT_LAUNCH_MAXIMUM_NAME_BYTES + CUBIT_LAUNCH_MAXIMUM_BYTES)
#define REQUEST_PAGES ((REQUEST_BYTES + PAGE_BYTES - 1) / PAGE_BYTES)

/* Kernel PIDs are 1 .. 255 (kernel/src/process.ads). */
#define PID_LIMIT 256
#define SIGNAL_KILLED 9
#define STATUS_CODE_SHIFT 8
#define EXIT_CODE_MASK 0xff

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

/* --- children ---------------------------------------------------------- */

/* Children started and not yet seen to end, by PID and generation: the
 * kernel reuses a PID as soon as its process retires, and a launch procmgr
 * abandons still reports an exit, so the PID alone is ambiguous. Ended
 * children not yet waited for, oldest first. */
static volatile int child_lock[1];
static struct { int pid; uint64_t generation; } live[PID_LIMIT];
static unsigned live_count;
static struct { int pid, status; } ended[PID_LIMIT];
static unsigned ended_count;

static void put_le16(unsigned char *p, uint16_t v)
{
	p[0] = v & 0xff; p[1] = v >> 8;
}

static void put_le32(unsigned char *p, uint32_t v)
{
	for (int k = 0; k < 4; k++) p[k] = (v >> (8 * k)) & 0xff;
}

/* Encode argv and envp as a launch block at out; its length, or 0 if it
 * would exceed the launch limits. */
static size_t encode(unsigned char *out, char *const argv[], char *const envp[])
{
	size_t used = CUBIT_LAUNCH_HEADER_BYTES;
	uint32_t counts[2] = { 0, 0 };
	char *const *lists[2] = { argv, envp };
	for (int l = 0; l < 2; l++) {
		if (!lists[l]) continue;
		for (char *const *s = lists[l]; *s; s++) {
			size_t n = strlen(*s) + 1;
			if (counts[0] + counts[1] >= CUBIT_LAUNCH_MAXIMUM_STRINGS ||
			    n > CUBIT_LAUNCH_MAXIMUM_BYTES - used)
				return 0;
			memcpy(out + used, *s, n);
			used += n;
			counts[l]++;
		}
	}
	put_le16(out + CUBIT_LAUNCH_VERSION_AT, CUBIT_LAUNCH_FORMAT_VERSION);
	put_le16(out + CUBIT_LAUNCH_RESERVED_AT, 0);
	put_le32(out + CUBIT_LAUNCH_ARGUMENTS_AT, counts[0]);
	put_le32(out + CUBIT_LAUNCH_ENVIRONMENT_AT, counts[1]);
	put_le32(out + CUBIT_LAUNCH_STRING_BYTES_AT,
		(uint32_t)(used - CUBIT_LAUNCH_HEADER_BYTES));
	return used;
}

/* The request buffer, lent read-only to procmgr once (child_lock). */
static unsigned char *request;
static uint64_t request_grant;

static int lend_request(void)
{
	if (request) return 0;
	void *p = mmap(0, REQUEST_PAGES * PAGE_BYTES, PROT_READ | PROT_WRITE,
		MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	if (p == MAP_FAILED) return ENOMEM;
	unsigned long slot = cubit(SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY,
		SLOT_PROCESS_MANAGER, (unsigned long)p, REQUEST_PAGES, GRANT_READ_ONLY);
	if (slot == (unsigned long)-1) {
		munmap(p, REQUEST_PAGES * PAGE_BYTES);
		return EPERM;                   /* no process-manager authority */
	}
	unsigned long gen = cubit(SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION,
		slot, 0, 0, 0);
	if (gen == (unsigned long)-1 || gen == 0) return EPERM;
	request_grant = ((uint64_t)gen << 32) | slot;
	request = p;
	return 0;
}

static int launch_error(uint64_t failure)
{
	switch (failure) {
	case CUBIT_LAUNCH_MALFORMED_REQUEST: return EINVAL;
	case CUBIT_LAUNCH_GRANT_UNAVAILABLE: return ENOMEM;
	case CUBIT_LAUNCH_ARGUMENTS_REJECTED: return E2BIG;
	case CUBIT_LAUNCH_SPAWN_FAILED: return ENOENT;
	case CUBIT_LAUNCH_NOT_GRANTED: return EACCES;
	default: return EIO;
	}
}

int __cubit_spawn(pid_t *res, const char *path, char *const argv[],
	char *const envp[])
{
	while (*path == '/') path++;            /* procmgr names: no root */
	size_t name_bytes = strlen(path);
	if (name_bytes == 0) return ENOENT;
	if (name_bytes > CUBIT_LAUNCH_MAXIMUM_NAME_BYTES) return ENAMETOOLONG;

	LOCK(child_lock);
	int error = lend_request();
	if (error) {
		UNLOCK(child_lock);
		return error;
	}
	memcpy(request, path, name_bytes);
	unsigned char *block = request + name_bytes;
	size_t block_bytes = encode(block, argv, envp);
	uint32_t argc, envc;
	if (block_bytes == 0 ||
	    !__cubit_launch_arguments_validate(block, (uint32_t)block_bytes,
	                                       &argc, &envc)) {
		UNLOCK(child_lock);
		return E2BIG;
	}
	struct message m = { CUBIT_OP_LAUNCH, REQUEST_WORDS, 0, 0, 0,
		{ request_grant, name_bytes, block_bytes, 0 } };
	/* Children are registered under the lock that waiting drains exit
	 * events under, so an early exit event cannot be missed. */
	uint32_t label = (uint32_t)cubit(SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY,
		SLOT_PROCESS_MANAGER, (unsigned long)&m, 0, 0);
	if (label == REPLY_OK && m.words[0] > 0 && m.words[0] < PID_LIMIT) {
		if (live_count < PID_LIMIT) {
			live[live_count].pid = (int)m.words[0];
			live[live_count].generation = m.words[1];
			live_count++;
		}
		if (res) *res = (pid_t)m.words[0];
		error = 0;
	} else if (label == REPLY_ERR) {
		error = launch_error(m.words[0]);
	} else {
		error = EPERM;
	}
	UNLOCK(child_lock);
	return error;
}

/* Move every queued exit event of our children into ended (child_lock).
 * Other events are dropped: nothing else in the libc consumes events. */
static void drain_events(void)
{
	struct message m;
	while (cubit(SYSCALL_RECEIVE_EVENT_NB, (unsigned long)&m, 0, 0, 0) == 1) {
		uint64_t pid = m.words[0], kind = m.words[1], code = m.words[2];
		if (m.label != CUBIT_EVENT_CHILD_EXIT ||
		    m.length != CUBIT_CHILD_EXIT_WORDS ||
		    pid == 0 || pid >= PID_LIMIT || ended_count >= PID_LIMIT)
			continue;
		unsigned k = 0;
		while (k < live_count && !(live[k].pid == (int)pid &&
		                           live[k].generation == m.words[3]))
			k++;
		if (k == live_count) continue;     /* not a child we started */
		int status;
		if (kind == CUBIT_TERMINATION_EXITED && code <= EXIT_CODE_MASK)
			status = (int)code << STATUS_CODE_SHIFT;
		else if (kind == CUBIT_TERMINATION_STOPPED)
			status = SIGNAL_KILLED;
		else
			continue;
		live[k] = live[live_count - 1];
		live_count--;
		ended[ended_count].pid = (int)pid;
		ended[ended_count].status = status;
		ended_count++;
	}
}

/* Take the oldest ended child matching pid (-1 or below, 0: any); its PID,
 * or 0. child_lock. */
static int take_ended(pid_t pid, int *status)
{
	for (unsigned k = 0; k < ended_count; k++) {
		if (pid > 0 && ended[k].pid != pid) continue;
		int found = ended[k].pid;
		if (status) *status = ended[k].status;
		memmove(&ended[k], &ended[k + 1],
			(ended_count - k - 1) * sizeof ended[0]);
		ended_count--;
		return found;
	}
	return 0;
}

static int has_child(pid_t pid)
{
	if (pid <= 0) return live_count > 0;
	for (unsigned k = 0; k < live_count; k++)
		if (live[k].pid == pid) return 1;
	return 0;
}

pid_t __cubit_wait4(pid_t pid, int *status, int options, struct rusage *ru)
{
	/* No process groups on CuBit: 0 and -group mean any child. */
	if (options & ~(WNOHANG | WUNTRACED | WCONTINUED)) {
		errno = EINVAL;
		return -1;
	}
	if (ru) memset(ru, 0, sizeof *ru);
	for (;;) {
		LOCK(child_lock);
		drain_events();
		int found = take_ended(pid, status);
		int waiting = has_child(pid);
		UNLOCK(child_lock);
		if (found) return found;
		if (!waiting) {
			errno = ECHILD;
			return -1;
		}
		if (options & WNOHANG) return 0;
		/* Woken by any traffic, not only events: when no exit event
		 * came, pause briefly rather than spin on someone else's. */
		cubit(SYSCALL_WAIT_FOR_IPC_OR_COMPLETION_UNTIL_MONOTONIC_MILLISECOND,
			WAIT_FOREVER, 0, 0, 0);
		LOCK(child_lock);
		drain_events();
		int ready = ended_count > 0;
		UNLOCK(child_lock);
		if (!ready) {
			unsigned long now = cubit(SYSCALL_READ_MONOTONIC_MICROSECONDS,
				0, 0, 0, 0);
			cubit(SYSCALL_SLEEP_UNTIL_MONOTONIC_MICROSECOND,
				now + IDLE_PAUSE_MICROSECONDS, 0, 0, 0);
		}
	}
}
