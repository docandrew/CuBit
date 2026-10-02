#include "../../userspace/mesa/anv/native_gpu_presenter.h"
#include "../../userspace/mesa/anv/native_gpu_buffers.h"
#include "../../userspace/mesa/anv/native_gpu_memory.h"
#include "../../userspace/mesa/anv/native_gpu_presentation.h"
#include <assert.h>
#include <stdio.h>

static unsigned fault, maps, acquires, forwards, returns, attaches, children, roots;
static bool child_pending, root_pending;
static const uint64_t root = UINT64_C(0x700000008), child = UINT64_C(0x900000009);
uint32_t cubit_intel_map_presentation(uint64_t s, uint32_t h, uint64_t o,
   uint64_t b, uint32_t *m, uint64_t *r)
{
   assert(s == 7 && h == 3 && o == 4096 && b == 4096);
   maps++;
   *m = fault == 1 ? 0 : 2; *r = fault == 1 ? 0 : root;
   return fault == 1 || fault == 9 ? 5 : 0;
}
uint32_t cubit_intel_acquire_view(uint64_t s, uint64_t r, uint64_t o,
   uint64_t b, uint64_t w, uint64_t *a)
{
   assert(s == 7 && r == root && o == 0 && b == 4096 && w == 0);
   acquires++; *a = fault == 2 ? 0 : 0x100000;
   return fault == 2;
}
uint32_t cubit_intel_forward_presentation(uint64_t s, uint64_t r, uint64_t o,
   uint64_t b, uint64_t *c)
{
   assert(s == 12 && r == root && o == 0 && b == 4096);
   forwards++; *c = fault == 3 ? 0 : child;
   return fault == 3;
}
uint32_t cubit_intel_return_view(uint64_t r)
{
   assert(r == root); returns++; return fault == 4;
}
uint32_t cubit_intel_attach_linear(uint64_t s, uint64_t surf, uint64_t c,
   uint64_t w, uint64_t h, uint64_t p)
{
   assert(s == 12 && surf == 15 && c == child && w == 4 && h == 2 && p == 16);
   assert(returns == 1); attaches++;
   return fault == 5 ? 7 : fault == 8 ? 1 : 0;
}
uint32_t cubit_intel_retire_presentation(uint64_t c)
{
   assert(c == child); children++;
   return fault == 6 ? 2 : child_pending ? 1 : 0;
}
uint32_t cubit_intel_retire_mapping(uint64_t s, uint32_t m)
{
   assert(s == 7 && m == 2); roots++;
   return fault == 7 ? 5 : root_pending ? 4 : 0;
}
static uint32_t attach(struct cubit_presenter *r)
{
   return cubit_presenter_attach_completed_linear(r, 7, 12, 3, 15, 4096, 4, 2, 16);
}
int main(void)
{
   for (fault = 0; fault <= 9; fault++) {
      struct cubit_presenter r = {0};
      maps = acquires = forwards = returns = attaches = children = roots = 0;
      child_pending = root_pending = true;
      uint32_t status = attach(&r);
      assert(status == ((fault >= 1 && fault <= 5) || fault >= 8 ? 5 : 0));
      assert(maps == 1);
      assert(attach(&r) == 5 && maps == 1); /* no replay */
      if (fault == 1 || fault == 4 || fault == 9) {
         assert(r.state == CUBIT_PRESENTER_FAILED);
         assert(cubit_presenter_release(&r) == 5);
         assert(cubit_presenter_release(&r) == 5);
         assert(returns == (fault == 4 ? 1u : 0u));
         assert(children == 0 && roots == 0);
         if (fault == 9) {
            /* A failed map reply with partially populated outputs is still
             * uncertain, not the no-resource local rejection case. */
            assert(r.mapping == 2 && r.root == root);
            assert(acquires == 0 && forwards == 0);
         }
         continue;
      }
      if (fault == 6) {
         assert(cubit_presenter_release(&r) == 5 && roots == 0);
         assert(r.state == CUBIT_PRESENTER_FAILED);
         continue;
      }
      assert(cubit_presenter_release(&r) == 4);
      if (fault != 2 && fault != 3) assert(roots == 0);
      child_pending = false;
      status = cubit_presenter_release(&r);
      if (fault == 7) {
         assert(status == 5 && r.state == CUBIT_PRESENTER_FAILED);
         continue;
      }
      assert(status == 4 && r.state == CUBIT_PRESENTER_RETIRING);
      unsigned child_calls = children;
      root_pending = false;
      assert(cubit_presenter_release(&r) == 0);
      assert(r.state == CUBIT_PRESENTER_RETIRED && children == child_calls);
      assert(returns == (fault == 2 ? 0u : 1u));
      unsigned root_calls = roots;
      assert(cubit_presenter_release(&r) == 0 && roots == root_calls);
   }
   for (unsigned bad = 0; bad < 13; bad++) {
      struct cubit_presenter r = {0};
      unsigned before = maps;
      unsigned operations = acquires + forwards + returns + attaches + children + roots;
      assert(cubit_presenter_attach_completed_linear(&r,
         bad == 0 ? 64 : 7, bad == 7 ? 64 : 12, bad == 1 ? 0 : 3,
         bad == 8 ? 0 : 15,
         bad == 2 ? UINT64_MAX : bad == 12 ? 16*1024*1024 : 4096,
         bad == 3 ? 0 : bad == 9 ? 65536 : 4,
         bad == 4 ? 65536 : bad == 10 ? 0 : bad == 11 ? 65535 : 2,
         bad == 5 ? 15 : bad == 6 ? UINT32_MAX : bad == 11 ? 512 : 16) == 5);
      /* Case 11 needs to exceed the mapped byte limit rather than only
       * checking height, which is independently legal at 65535. */
      assert(maps == before);
      assert(r.state == CUBIT_PRESENTER_RETIRED);
      assert(cubit_presenter_release(&r) == 0);
      assert(cubit_presenter_release(&r) == 0);
      assert(attach(&r) == 5 && maps == before);
      assert(acquires + forwards + returns + attaches + children + roots == operations);
   }
   puts("Presenter lifetime PASS: opt-in, page rounding, no replay, retained failures, release ordering");
}
