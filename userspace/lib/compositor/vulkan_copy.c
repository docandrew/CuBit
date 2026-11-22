#include "vulkan_copy.h"
#include <limits.h>
#include <stddef.h>
_Static_assert(sizeof(struct cubit_vulkan_copy_plan) == 24, "Ada plan ABI");
_Static_assert(offsetof(struct cubit_vulkan_copy_plan, width) == 16, "Ada width ABI");
uint32_t cubit_vulkan_record_copy(void *borrowed,
                                const struct cubit_vulkan_copy_plan *p)
{
    const struct cubit_vulkan_copy *b = borrowed;
    if (!b || !p || !b->record || !b->command || !b->target || !b->source ||
        b->source == b->target || !p->width || !p->height ||
        p->target_x > INT32_MAX || p->target_y > INT32_MAX ||
        p->source_x > INT32_MAX || p->source_y > INT32_MAX ||
        p->width > INT32_MAX || p->height > INT32_MAX ||
        p->width > INT32_MAX - p->target_x ||
        p->height > INT32_MAX - p->target_y ||
        p->width > INT32_MAX - p->source_x ||
        p->height > INT32_MAX - p->source_y)
        return 1;
    const VkImageCopy region = {
        .srcSubresource = { VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1 },
        .srcOffset = { (int32_t)p->source_x, (int32_t)p->source_y, 0 },
        .dstSubresource = { VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1 },
        .dstOffset = { (int32_t)p->target_x, (int32_t)p->target_y, 0 },
        .extent = { p->width, p->height, 1 }
    };
    b->record(b->command, b->source, VK_IMAGE_LAYOUT_GENERAL,
              b->target, VK_IMAGE_LAYOUT_GENERAL, 1, &region);
    return 0;
}
