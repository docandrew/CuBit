#include "native_gpu_presenter.h"
#include "native_gpu_buffers.h"
#include "native_gpu_memory.h"
#include "native_gpu_presentation.h"

static uint32_t
fail(struct cubit_presenter *r)
{
   r->state = CUBIT_PRESENTER_FAILED;
   return 5;
}

static uint32_t
reject_before_mapping(struct cubit_presenter *r)
{
   /* Local validation has issued no IPC and acquired no authority. Consume
    * the single-use record, but do not turn a harmless rejection into an
    * unretirable BO. Release can now confirm that there is nothing to drain.
    * Never use this path after map_presentation: even a failed reply there
    * can hide a server-side grant. */
   r->state = CUBIT_PRESENTER_RETIRED;
   return 5;
}

uint32_t
cubit_presenter_release(struct cubit_presenter *r)
{
   if (!r || r->state == CUBIT_PRESENTER_EMPTY ||
       r->state == CUBIT_PRESENTER_FAILED)
      return 5;
   if (r->state == CUBIT_PRESENTER_RETIRED)
      return 0;
   r->state = CUBIT_PRESENTER_RETIRING;
   if (r->borrowed) {
      if (cubit_intel_return_view(r->root) != 0)
         return fail(r); /* Uncertain return must never be retried. */
      r->borrowed = false;
   }
   if (r->child) {
      uint32_t status = cubit_intel_retire_presentation(r->child);
      if (status == 1)
         return 4;
      if (status != 0)
         return fail(r);
      r->child = 0;
   }
   if (r->mapping) {
      uint32_t status = cubit_intel_retire_mapping(r->render_slot, r->mapping);
      if (status == 4)
         return 4;
      if (status != 0)
         return fail(r);
      r->mapping = 0;
      r->root = 0;
   }
   r->state = CUBIT_PRESENTER_RETIRED;
   return 0;
}

uint32_t
cubit_presenter_attach_completed_linear(struct cubit_presenter *r,
   uint64_t render_slot, uint64_t desktop_slot, uint32_t handle,
   uint64_t surface, uint64_t offset, uint32_t width, uint32_t height,
   uint32_t pitch)
{
   const uint64_t limit = 16 * 1024 * 1024;
   if (!r || r->state != CUBIT_PRESENTER_EMPTY)
      return 5;
   if (render_slot > 63 || desktop_slot > 63 || !handle || !surface ||
       !width || width > 65535 || !height || height > 65535 ||
       pitch < (uint64_t)width * 4 || (uint64_t)pitch * height > limit ||
       offset % 4096)
      return reject_before_mapping(r);
   uint64_t bytes = ((uint64_t)pitch * height + 4095) & ~UINT64_C(4095);
   if (offset > limit - bytes)
      return reject_before_mapping(r);
   r->render_slot = render_slot;
   r->desktop_slot = desktop_slot;
   r->state = CUBIT_PRESENTER_RETIRING;
   if (cubit_intel_map_presentation(render_slot, handle, offset, bytes,
                                    &r->mapping, &r->root) != 0)
      return fail(r); /* An uncertain reply can hide a server-side grant. */
   uint64_t address = 0;
   if (cubit_intel_acquire_view(render_slot, r->root, 0, bytes, 0, &address) != 0) {
      (void)cubit_presenter_release(r);
      return 5;
   }
   r->borrowed = true;
   if (cubit_intel_forward_presentation(desktop_slot, r->root, 0, bytes,
                                        &r->child) != 0) {
      (void)cubit_presenter_release(r);
      return 5;
   }
   /* The independently pinned child now retains the backing. Do not hold an
    * unnecessary CPU borrow for the entire surface lifetime. */
   if (cubit_intel_return_view(r->root) != 0)
      return fail(r);
   r->borrowed = false;
   if (cubit_intel_attach_linear(desktop_slot, surface, r->child,
                                 width, height, pitch) != 0) {
      (void)cubit_presenter_release(r);
      return 5;
   }
   r->state = CUBIT_PRESENTER_ATTACHED;
   return 0;
}
