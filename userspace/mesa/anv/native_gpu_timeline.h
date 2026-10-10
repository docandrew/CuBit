#pragma once
/* Mesa's GPU timeline logic, in Ada (native_gpu_timeline.ads, proved):
 * a reached value plus the points the session queue signals later, each a
 * (context, value) on the GPU's timeline. C stores the state as opaque bytes
 * and changes it only through these calls, under its own lock. No IPC. */
#include <stdint.h>

#define NATIVE_GPU_CONTEXTS 4u
#define NATIVE_GPU_TIMELINE_BYTES 1552u   /* Native_GPU_Timeline.Timeline_Bytes */

struct native_gpu_timeline {
   _Alignas(8) unsigned char opaque[NATIVE_GPU_TIMELINE_BYTES];
};
/* Highest value per context: completed values, or a descriptor's waits. */
struct native_gpu_values {
   uint64_t value[NATIVE_GPU_CONTEXTS];
};

enum native_gpu_add_result {
   NATIVE_GPU_ADDED = 0,
   NATIVE_GPU_NOT_INCREASING = 1,   /* at or below a value signalled or pending */
   NATIVE_GPU_FULL = 2,             /* more points than jobs the queue holds */
   NATIVE_GPU_BAD_CONTEXT = 3,
};
enum native_gpu_wait_kind {
   NATIVE_GPU_WAIT_REACHED = 0,
   NATIVE_GPU_WAIT_ON_GPU = 1,      /* the first point at or above the value */
   NATIVE_GPU_WAIT_UNSUBMITTED = 2, /* nothing signals it yet */
};

void native_gpu_timeline_initialize(struct native_gpu_timeline *t, uint64_t initial);
/* ok: 1 signalled, 0 below the reached value (nothing changes). */
void native_gpu_timeline_signal(struct native_gpu_timeline *t, uint64_t value, uint32_t *ok);
void native_gpu_timeline_add(struct native_gpu_timeline *t, uint64_t value, uint32_t context,
                             uint64_t gpu, enum native_gpu_add_result *result);
void native_gpu_timeline_resolve(struct native_gpu_timeline *t,
                                 const struct native_gpu_values *completed);
/* value: reached; last: the highest signalled or pending. */
void native_gpu_timeline_value(const struct native_gpu_timeline *t, uint64_t *value,
                               uint64_t *last);
void native_gpu_timeline_find(const struct native_gpu_timeline *t, uint64_t wait,
                              enum native_gpu_wait_kind *kind, uint32_t *context, uint64_t *gpu);
/* Binary payload move: target takes source; source unsignalled at
 * *new_source_next. ok 0: source_next was neither signalled nor pending. */
void native_gpu_timeline_move(struct native_gpu_timeline *target,
                              struct native_gpu_timeline *source, uint64_t source_next,
                              uint64_t *target_next, uint64_t *new_source_next, uint32_t *ok);
uint32_t native_gpu_timeline_bytes(void);

/* A descriptor's waits: same-context waits drop (ring order covers them),
 * others keep the highest value per context. */
void native_gpu_wait_set_clear(struct native_gpu_values *set);
void native_gpu_wait_set_merge(struct native_gpu_values *set, uint32_t job_context,
                               uint32_t context, uint64_t gpu, uint32_t *ok);
/* fits 0: more contexts than a descriptor's two waits. */
void native_gpu_wait_set_select(const struct native_gpu_values *set,
                                uint32_t *first_context, uint64_t *first_target,
                                uint32_t *second_context, uint64_t *second_target,
                                uint32_t *fits);

_Static_assert(sizeof(struct native_gpu_timeline) == NATIVE_GPU_TIMELINE_BYTES, "timeline");
_Static_assert(sizeof(struct native_gpu_values) == NATIVE_GPU_CONTEXTS * 8, "values");
