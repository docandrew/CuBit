#pragma once
#include <stdbool.h>
#include <stdint.h>

/* Mesa-side lifecycle bookkeeping, not GPU synchronization or authority.
 * Zero-initialize once, serialize, never copy/reset a used record. Keep both
 * endpoint slots and BO backing alive until cleanup completes. This record
 * must outlive the surface/ANV BO wrapper if cleanup is pending or uncertain.
 */
enum cubit_presenter_state {
   CUBIT_PRESENTER_EMPTY, CUBIT_PRESENTER_ATTACHED,
   CUBIT_PRESENTER_RETIRING, CUBIT_PRESENTER_RETIRED, CUBIT_PRESENTER_FAILED
};
struct cubit_presenter {
   enum cubit_presenter_state state;
   uint64_t render_slot, desktop_slot, root, child;
   uint32_t mapping;
   bool borrowed;
};
/* Caller has already waited for GPU completion AND established coherent CPU
 * visibility of LINEAR BGRA8888 pixels. No tiled/compressed/in-flight images.
 * Offset is page-aligned; rounded padding must belong to this BO allocation.
 * 0 attached, 5 failure. Failed calls can leave RETIRING/FAILED records; retain
 * them, do not replay. Nothing here creates/closes BOs or reuses GPU addresses.
 * A purely local validation failure consumes the record as RETIRED: release
 * returns 0 because no IPC or borrowing occurred. Still do not reuse it.
 */
uint32_t cubit_presenter_attach_completed_linear(struct cubit_presenter *record,
   uint64_t render_slot, uint64_t desktop_slot, uint32_t handle,
   uint64_t surface, uint64_t offset, uint32_t width, uint32_t height,
   uint32_t pitch);
/* Call after requesting surface replacement/destruction, then poll if needed.
 * 0 all tracked CPU grants retired, 4 pending, 5 uncertain/unrecoverable.
 * This does not detach/destroy a surface or establish GPU/DMA completion.
 * ATTACH/PRESENT acknowledgments alone never imply source-buffer release.
 */
uint32_t cubit_presenter_release(struct cubit_presenter *record);
