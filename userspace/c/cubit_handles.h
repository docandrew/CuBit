/*
 * CuBit OS - capability handles in C (CAP-002, docs/capability-handles.md)
 * Copyright (C) 2026 Jon Andrew
 *
 * A capability handle names one entry of this process's handle table. C code
 * holds it as an opaque one-word struct of its kind, never as an integer:
 * it receives handles from the runtime (its startup table, a reply that
 * carried one) and passes them back unchanged. Mirrors the Ada
 * CuBit.Handles.C_ABI types (Convention C_Pass_By_Copy).
 */
#ifndef CUBIT_HANDLES_H
#define CUBIT_HANDLES_H

#if !defined(_CUBIT_TYPES_DEFINED)
#include <stdint.h>
#endif

/* An endpoint: call, send or submit to the process it names. */
typedef struct cubit_endpoint { uint64_t opaque; } cubit_endpoint;

/* No endpoint: refused by the kernel wherever an endpoint is needed. */
#define CUBIT_NO_ENDPOINT ((cubit_endpoint){0})

/* Whether e names a handle at all (the kernel still checks it is current). */
static inline int cubit_endpoint_present(cubit_endpoint e)
{
    return (e.opaque >> 32) != 0;
}

/* Whether a and b are the same handle of this process. */
static inline int cubit_endpoint_same(cubit_endpoint a, cubit_endpoint b)
{
    return a.opaque == b.opaque;
}

/* The kernel trace object (shared/capabilities/trace_abi.ads): benchmarks
 * reset the kernel's scheduling and system-call trace before a measured run
 * and print its summary after. */
typedef struct cubit_trace { uint64_t opaque; } cubit_trace;

/* Trace_ABI.Operation codes. */
#define CUBIT_TRACE_RESET        1u
#define CUBIT_TRACE_SUMMARY      2u
#define CUBIT_TRACE_START_LOCAL  3u  /* LATENCY_TRACE kernels only */
#define CUBIT_TRACE_FREEZE_LOCAL 4u  /* LATENCY_TRACE kernels only */
#define CUBIT_TRACE_DUMP_LOCAL   5u  /* LATENCY_TRACE kernels only */

/* Trace_ABI.Outcome codes; 0: no trace object answered. */
#define CUBIT_TRACE_DONE               1u
#define CUBIT_TRACE_NOT_IN_THIS_KERNEL 5u

/* One operation on the trace object (the libc's CuBit.Libc_Startup). */
unsigned cubit_trace_perform(cubit_trace t, unsigned operation);

/* A program's startup endpoints come from its generated startup header
 * (ccl-manifest --c-output startup.h, CAP-003): cubit_startup_<binding>()
 * for each binding its manifest declares, so a misspelt name is a compile
 * error. The libc provides the lookup underneath (CuBit.Libc_Startup);
 * programs without the libc read their Ada Startup package instead. */

#endif
