/* Link-only stub for Vulkan_Scene's preview layers, which the submission
   mock tests never record (desktop_vulkan_link_mock.c has the same). */
#include <stdint.h>
uint32_t cubit_vulkan_record_preview(void *a,const void *b){(void)a;(void)b;return 1;}
