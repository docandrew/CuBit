#!/usr/bin/env python3
"""Actual ANV queue entry, mock backend, host threads; not GPU execution."""
from pathlib import Path
import subprocess
import sys
import tempfile

source = Path(sys.argv[1])
text = (source / "src/intel/vulkan/anv_batch_chain.c").read_text()
start = text.index("VkResult\nanv_queue_submit(struct vk_queue")
end = text.index("\nvoid\nanv_cmd_buffer_clflush", start)
function = text[start:end]
fixture = r'''
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <pthread.h>
#include <stdio.h>
#define container_of(ptr,type,field) ((type *)((char *)(ptr)-offsetof(type,field)))
typedef int VkResult;
enum { VK_SUCCESS=0, VK_TIMEOUT=2 };
struct vk_sync_wait { int value; };
struct signal { void *sync; uint64_t signal_value; };
struct cmd { int base; };
struct vk_queue { int unused; };
struct vk_queue_submit {
 int command_buffer_count; struct cmd **command_buffers;
 uint32_t wait_count, signal_count; struct vk_sync_wait *waits;
 struct signal *signals;
 int buffer_bind_count, image_opaque_bind_count, image_bind_count;
};
struct anv_device;
struct backend { VkResult (*wait_queue_dependencies)(struct anv_device *,uint32_t,const struct vk_sync_wait *,uint64_t); };
struct info { bool no_hw; };
struct anv_device { int vk; struct { int trace_context; } ds;
 struct info *info; struct backend *kmd_backend; pthread_mutex_t mutex; };
struct anv_queue { struct vk_queue vk; struct anv_device *device; int ds; };
struct anv_utrace_submit { int unused; };
struct anv_cmd_buffer;
static pthread_mutex_t gate = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t condition = PTHREAD_COND_INITIALIZER;
static bool waiting, completed, fail_wait;
static unsigned executions, flushes;
static void anv_queue_free_initial_submission(struct anv_queue *q) {(void)q;}
static bool u_trace_should_process(void *p) {(void)p;return false;}
static void intel_ds_perfetto_refresh_debug_utils_object_name(void *p,void *q) {(void)p;(void)q;}
static int vk_sync_signal(void *p,void *q,uint64_t v) {(void)p;(void)q;(void)v;return 0;}
static int vk_queue_set_lost(void *p,const char *s) {(void)p;(void)s;return -1;}
static int anv_device_utrace_flush_cmd_buffers(struct anv_queue *q,int n,struct anv_cmd_buffer **c,struct anv_utrace_submit **u)
{(void)q;(void)n;(void)c;(void)u;pthread_mutex_lock(&gate);flushes++;pthread_mutex_unlock(&gate);return 0;}
static uint64_t intel_ds_begin_submit(void *p) {(void)p;return 0;}
static void intel_ds_end_submit(void *p,uint64_t v) {(void)p;(void)v;}
static void intel_ds_device_process(void *p,bool v) {(void)p;(void)v;}
static int anv_queue_submit_sparse_bind(struct anv_queue *q,struct vk_queue_submit *s) {(void)q;(void)s;assert(false);return -1;}
static int anv_queue_submit_cmd_buffers_locked(struct anv_queue *q,struct vk_queue_submit *s,struct anv_utrace_submit *u) {
 (void)q;(void)u; pthread_mutex_lock(&gate);
 if(s->wait_count) assert(completed);
 else {completed=true;pthread_cond_broadcast(&condition);}
 executions++;pthread_mutex_unlock(&gate);return 0;
}
static int wait_dependencies(struct anv_device *d,uint32_t n,const struct vk_sync_wait *w,uint64_t deadline) {
 (void)d;(void)w;assert(deadline==UINT64_MAX);
 if(!n)return 0;
 if(fail_wait)return VK_TIMEOUT;
 pthread_mutex_lock(&gate);waiting=true;pthread_cond_broadcast(&condition);
 while(!completed)pthread_cond_wait(&condition,&gate);
 pthread_mutex_unlock(&gate);return 0;
}
FUNCTION
static struct info info;
static struct backend backend={wait_dependencies};
static struct anv_device device={.info=&info,.kmd_backend=&backend,.mutex=PTHREAD_MUTEX_INITIALIZER};
static struct anv_queue consumer={.device=&device}, producer={.device=&device};
static struct vk_sync_wait dependency;
static struct vk_queue_submit dependent={.wait_count=1,.waits=&dependency}, independent;
static void *run(void *unused) {(void)unused;assert(anv_queue_submit(&consumer.vk,&dependent)==0);return NULL;}
int main(void) {
 pthread_t thread;assert(!pthread_create(&thread,NULL,run,NULL));
 pthread_mutex_lock(&gate);while(!waiting)pthread_cond_wait(&condition,&gate);pthread_mutex_unlock(&gate);
 assert(anv_queue_submit(&producer.vk,&independent)==0);
 assert(!pthread_join(thread,NULL));assert(executions==2 && flushes==2);
 fail_wait=true;assert(anv_queue_submit(&consumer.vk,&dependent)==VK_TIMEOUT);
 assert(executions==2 && flushes==2);
 backend.wait_queue_dependencies=NULL;
 assert(anv_queue_submit(&producer.vk,&independent)==0);assert(executions==3);
 puts("Actual ANV queue entry PASS: producer progress, wait failure, optional hook");
}
'''
output = Path(tempfile.mkdtemp(prefix="queue-dependencies."))
# Relocate the exact callback block under the device lock. This must deadlock.
begin = function.index("   if (device->kmd_backend->wait_queue_dependencies)")
finish = function.index("\n   }", begin) + len("\n   }")
block = function[begin:finish]
mutation = function[:begin] + function[finish:]
lock = "      pthread_mutex_lock(&device->mutex);"
mutation = mutation.replace(lock, lock + "\n" + block, 1)
for name, body in (("current", function), ("locked-wait", mutation)):
    unit, binary = output / (name + ".c"), output / name
    unit.write_text(fixture.replace("FUNCTION", body))
    subprocess.run(["cc", "-std=c11", "-Wall", "-Wextra", "-Werror",
                    "-pthread", str(unit), "-o", str(binary)], check=True)
    try:
        subprocess.run([str(binary)], check=True, timeout=5)
    except subprocess.TimeoutExpired:
        if name != "locked-wait":
            raise
        print("Misplaced-wait mutation caught: producer blocked by device mutex")
    else:
        if name == "locked-wait":
            raise AssertionError("regression did not detect lock inversion")
print(f"Evidence: {output}")
