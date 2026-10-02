#ifndef ANV_CUBIT_STATE_TABLE_H
#define ANV_CUBIT_STATE_TABLE_H
#include <stdint.h>
#include <vulkan/vulkan_core.h>
struct anv_state_table;
struct anv_device;
VkResult anv_cubit_state_table_init(struct anv_state_table *, struct anv_device *, uint32_t);
VkResult anv_cubit_state_table_expand(struct anv_state_table *, uint32_t);
void anv_cubit_state_table_finish(struct anv_state_table *);
#endif
