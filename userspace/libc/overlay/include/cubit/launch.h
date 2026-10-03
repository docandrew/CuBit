/*
 * CuBit libc: launch arguments and child exits (docs/process-arguments.md).
 *
 * The kernel maps a process's launch block read-only at
 * CUBIT_LAUNCH_ARGUMENTS_ADDRESS and starts it with its length in RDI; the
 * start code (crt/crt1.c) builds argv and the environment from it. The
 * layout and limits are CuBit.Launch_Arguments
 * (userspace/runtime/gnat/cubit-launch_arguments.ads); blocks are checked
 * with its proved validator, exported here.
 */
#ifndef _CUBIT_LAUNCH_H
#define _CUBIT_LAUNCH_H
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif

#define CUBIT_LAUNCH_ARGUMENTS_ADDRESS 0x00005A0000000000UL
#define CUBIT_LAUNCH_FORMAT_VERSION 1
#define CUBIT_LAUNCH_HEADER_BYTES 16
#define CUBIT_LAUNCH_MAXIMUM_BYTES (64 * 1024)
#define CUBIT_LAUNCH_MAXIMUM_STRINGS 4096
#define CUBIT_LAUNCH_MAXIMUM_NAME_BYTES 255
/* Header field offsets. */
#define CUBIT_LAUNCH_VERSION_AT 0
#define CUBIT_LAUNCH_RESERVED_AT 2
#define CUBIT_LAUNCH_ARGUMENTS_AT 4
#define CUBIT_LAUNCH_ENVIRONMENT_AT 8
#define CUBIT_LAUNCH_STRING_BYTES_AT 12

/* 1 if the length bytes at block are a well-formed launch block (then its
 * argument and environment counts are stored), 0 otherwise. */
int __cubit_launch_arguments_validate(const void *block, uint32_t length,
	uint32_t *arguments, uint32_t *environment);

/* OP_LAUNCH (procmgr) and its failure codes (CuBit.Launch_Arguments). */
#define CUBIT_OP_LAUNCH 0x0106
enum cubit_launch_failure {
	CUBIT_LAUNCH_MALFORMED_REQUEST = 1,
	CUBIT_LAUNCH_GRANT_UNAVAILABLE = 2,
	CUBIT_LAUNCH_ARGUMENTS_REJECTED = 3,
	CUBIT_LAUNCH_SPAWN_FAILED = 4,
	/* The launcher's manifest does not name the program (no may-launch
	 * entry), or the program asks for authority the launcher lacks. */
	CUBIT_LAUNCH_NOT_GRANTED = 5,
};

/* EVENT_CHILD_EXIT (CuBit.Child_Exits): words PID, kind, code and the
 * process's generation; OP_LAUNCH replies with PID and generation. PIDs
 * are reused, so (PID, generation) names one process. */
#define CUBIT_EVENT_CHILD_EXIT 0x0103
#define CUBIT_CHILD_EXIT_WORDS 4
enum cubit_termination_kind {
	CUBIT_TERMINATION_EXITED = 1,   /* SYSCALL_EXIT with a code */
	CUBIT_TERMINATION_STOPPED = 2,  /* killed, faulted, main thread ended */
};

#ifdef __cplusplus
}
#endif
#endif
