/* Actual linked ANV CPU allocator on CuBit; no provider or GPU commands.
 * NULL device is supported by Mesa's error-reporting path. State-table
 * CuBit storage uses a stable CPU reservation, not device BOs.
 */
#include "anv_private.h"
#include <cubit/debug.h>
#include <stdio.h>
#include <pthread.h>
#include <sched.h>

#define TABLE_WORKERS 4
#define TABLE_ALLOCS 1024
static unsigned workers_go;
struct table_worker {
   struct anv_state_table *table;
   unsigned number;
   bool passed;
   uint32_t indexes[TABLE_ALLOCS];
   struct anv_free_entry *saved[TABLE_ALLOCS];
};

static uint32_t entry_tag(unsigned worker, unsigned allocation)
{
   return ((worker + 1) << 24) | (allocation + 1);
}

static void *grow_table(void *opaque)
{
   struct table_worker *worker = opaque;
   worker->passed = false;
   while (!__atomic_load_n(&workers_go, __ATOMIC_ACQUIRE))
      sched_yield();
   for (unsigned i = 0; i < TABLE_ALLOCS; ++i) {
      if (anv_state_table_add(worker->table, &worker->indexes[i], 1) != VK_SUCCESS)
         return NULL;
      worker->saved[i] = &worker->table->map[worker->indexes[i]];
      worker->saved[i]->next = entry_tag(worker->number, i);
      if ((i & 15) == 0)
         sched_yield();
   }
   worker->passed = true;
   return NULL;
}

static bool concurrent_growth(struct anv_state_table *table)
{
   static struct table_worker workers[TABLE_WORKERS];
   pthread_t threads[TABLE_WORKERS];
   unsigned started = 0;
   bool valid = true;
   __atomic_store_n(&workers_go, 0, __ATOMIC_RELEASE);
   for (; started < TABLE_WORKERS; ++started) {
      workers[started].table = table;
      workers[started].number = started;
      if (pthread_create(&threads[started], NULL, grow_table, &workers[started])) {
         valid = false;
         break;
      }
   }
   __atomic_store_n(&workers_go, 1, __ATOMIC_RELEASE);
   for (unsigned t = 0; t < started; ++t) {
      if (pthread_join(threads[t], NULL)) {
         /* Do not retire the table while a worker may still use it. */
         static const char failure[] = "TEST: FAIL native Mesa table worker join\n";
         cubit_debug_write(failure, sizeof(failure) - 1);
         for (;;) { sched_yield(); }
      }
      valid = valid && workers[t].passed;
   }
   if (!valid)
      return false;
   for (unsigned t = 0; t < TABLE_WORKERS; ++t)
      for (unsigned i = 0; i < TABLE_ALLOCS; ++i) {
         uint32_t index = workers[t].indexes[i];
         if (index >= table->size / sizeof(*table->map) ||
             workers[t].saved[i] != &table->map[index] ||
             workers[t].saved[i]->next != entry_tag(t, i))
            return false;
      }
   return true;
}

int cubit_test_state_table(void);

int cubit_test_state_table(void)
{
   struct anv_state_table table = {0};
   char message[160];
   VkResult result = anv_state_table_init(&table, NULL, 64);
   int length = snprintf(message, sizeof(message),
      "MESA-CPU-TABLE init=%d bytes=%u (ACTUAL ANV; NO GPU)\n",
      result, table.size);
   if (length > 0)
      cubit_debug_write(message, (size_t)length < sizeof(message) ?
                        (size_t)length : sizeof(message) - 1);
   if (result != VK_SUCCESS) {
      static const char failure[] = "TEST: FAIL native Mesa CPU state-table init\n";
      cubit_debug_write(failure, sizeof(failure) - 1);
      return 1;
   }

   uint32_t index = UINT32_MAX;
   result = anv_state_table_add(&table, &index, 1);
   bool valid = result == VK_SUCCESS && index < table.size / sizeof(*table.map);
   if (valid) {
      uint32_t first = index;
      struct anv_free_entry *old = &table.map[first];
      old->next = 0x43554249;
      uint32_t old_size = table.size;
      uint32_t count = old_size / sizeof(*table.map) + 1;
      result = anv_state_table_add(&table, &index, count);
      valid = result == VK_SUCCESS && table.size > old_size &&
              index > first && (uint64_t)index + count <=
                 table.size / sizeof(*table.map) &&
              &table.map[first] == old &&
              table.map[first].next == 0x43554249 && old->next == 0x43554249;
      if (valid) {
         table.map[first].next = 0x4d455341;
         valid = old->next == 0x4d455341;
         old->next = 0x53544154;
         valid = valid && table.map[first].next == 0x53544154;
      }
   }
   if (valid)
      valid = concurrent_growth(&table);
   if (valid) {
      static const char pass[] =
         "TEST: PASS native Mesa CPU table 4 workers 4096 retained entries (NO GPU)\n";
      cubit_debug_write(pass, sizeof(pass) - 1);
   }
   anv_state_table_finish(&table);
   const char *outcome = valid ?
      "TEST: PASS native Mesa CPU state-table growth and aliases (NO GPU)\n" :
      "TEST: FAIL native Mesa CPU state-table growth or aliases\n";
   cubit_debug_write(outcome, strlen(outcome));
   return valid ? 0 : 1;
}
