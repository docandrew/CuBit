/* Hosted fault injection of the extracted real Mesa open function.
 * Mock transport only; real ANV structure compatibility is compile-tested
 * separately by test-lifecycle-compile.py.
 */
#define _GNU_SOURCE
#include <assert.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

typedef int VkResult;
enum { VK_SUCCESS = 0, VK_ERROR_INITIALIZATION_FAILED = -3,
       VK_ERROR_INCOMPATIBLE_DRIVER = -9 };
struct vk_device { void (*copy_sync_payloads)(void); void *sync; };
struct physical { const char *path; };
struct info { bool is_virtio; };
struct anv_device {
   int fd;
   struct physical *physical;
   struct info *info;
   struct vk_device vk;
};
static int opened_fd, init_result, provider;
static char calls[32];
static unsigned count;
static void event(char c) { assert(count + 1 < sizeof calls); calls[count++] = c; }
static void vk_drm_syncobj_copy_payloads(void) {}
static int mock_open(const char *path, int flags)
{
   assert(!strcmp(path, "test-device"));
   assert(flags == (O_RDWR | O_CLOEXEC));
   event('O');
   return opened_fd;
}
static int intel_virtio_init_fd(int fd)
{ assert(fd == opened_fd); event('I'); return init_result; }
static void intel_virtio_unref_fd(int fd)
{ assert(fd == opened_fd); event('U'); }
static int mock_close(int fd)
{ assert(fd == opened_fd); event('C'); return 0; }
static void *intel_virtio_sync_provider(int fd)
{ assert(fd == opened_fd); event('V'); return &provider; }
static void vk_device_set_drm_fd(struct vk_device *vk, int fd)
{ assert(fd == opened_fd); event('D'); vk->sync = &provider; }
static VkResult vk_error(struct anv_device *device, VkResult result)
{ (void)device; return result; }
#define open mock_open
#define close mock_close
#include "device-open-under-test.h"

int main(void)
{
   struct physical physical = {"test-device"};
   struct info info;
   /* Include fd=0: a valid descriptor must never be treated as failure. */
   const int fds[] = {-1, 0, 7, 65535};
   unsigned cases = 0;
   for (unsigned f = 0; f < sizeof fds / sizeof fds[0]; f++) {
      for (int init = -1; init <= 1; init++) {
         for (int virtio = 0; virtio <= 1; virtio++) {
            memset(calls, 0, sizeof calls);
            count = 0;
            opened_fd = fds[f];
            init_result = init;
            info.is_virtio = virtio;
            struct anv_device device = {
               .fd = -99, .physical = &physical, .info = &info,
            };
            VkResult result = anv_drm_open_device(&device);
            if (opened_fd == -1) {
               assert(result == VK_ERROR_INITIALIZATION_FAILED);
               assert(!strcmp(calls, "O"));
            } else if (init < 0) {
               assert(result == VK_ERROR_INCOMPATIBLE_DRIVER);
               assert(!strcmp(calls, "OIUC"));
            } else {
               assert(result == VK_SUCCESS && device.fd == opened_fd);
               assert(!strcmp(calls, virtio ? "OIV" : "OID"));
               assert(device.vk.sync == &provider);
               assert(device.vk.copy_sync_payloads == vk_drm_syncobj_copy_payloads);
               memset(calls, 0, sizeof calls);
               count = 0;
               /* Reuse success cases to cover both cleanup contracts. */
               if (init == 0) {
                  anv_drm_abort_device(&device);
                  assert(!strcmp(calls, "UC"));
               } else {
                  anv_drm_close_device(&device);
                  assert(!strcmp(calls, "C"));
               }
               assert(device.fd == -1);
            }
            if (result != VK_SUCCESS) {
               assert(device.fd == -1);
               assert(device.vk.sync == NULL);
               assert(device.vk.copy_sync_payloads == NULL);
            }
            cases++;
         }
      }
   }
   printf("Mesa device-open fault injection PASS: %u cases (mock transport)\n", cases);
}
