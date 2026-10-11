/* Mesa's actual vk_sync dispatcher and binary-on-timeline wrapper, for
 * hosted fixtures that link the CuBit adapter with the GPU timeline sync
 * type (anv_cubit_sync.c). Only the debug-option source is replaced. */
#include "vk_sync.c"
#include "vk_sync_binary.c"
#include "util/os_time.c"
#include "c11/impl/time.c"
int64_t debug_get_num_option(const char *name, int64_t fallback)
{ (void)name; return fallback; }
