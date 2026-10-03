#ifndef CUBIT_VULKAN_CHECKER_REQUEST_H
#define CUBIT_VULKAN_CHECKER_REQUEST_H
#include <stdint.h>
struct cubit_vulkan_checker_request {
    int32_t left, top, right, bottom, origin_x, origin_y;
    uint32_t numerator, denominator, width, height, rotation;
    uint32_t clip_x, clip_y, clip_w, clip_h, rgb;
};
#endif
