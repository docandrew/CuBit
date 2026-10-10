#ifndef CUBIT_VULKAN_DEVICE_STORAGE_H
#define CUBIT_VULKAN_DEVICE_STORAGE_H
#include "vulkan_context.h"
#include "vulkan_owned_targets.h"
#include "vulkan_upload_buffer.h"
/* Private process-static request storage. No allocation, command submission,
 * export or authority acquisition. Must be externally serialized with owner
 * startup/close. Context/target preparation has one lifetime; source metadata
 * follows the guarded reuse contract below. */
struct cubit_vulkan_device_targets {
    void *description;
    void *images[3];
    uint32_t allowed_types;
};
void *cubit_vulkan_device_context_request(const struct cubit_mesa_service_device *);
uint32_t cubit_vulkan_device_targets_prepare(uint32_t width, uint32_t height,
                                           struct cubit_vulkan_device_targets *out);
/* Only the SPARK context-child owner may invoke these. Creation returns
 * 0 live, 1 known-clean rejection, 2 repeated/uncertain. Close requires all
 * source tickets and GPU work retired, and returns 0 only on confirmed release. */
uint32_t cubit_vulkan_device_pipeline_create(void);
uint32_t cubit_vulkan_device_pipeline_close(void);
struct cubit_vulkan_checker_request;
/* Same owned context child as the textured pipeline; valid only during the
 * SPARK submission's active pass. Zero means recorded, never completed. */
uint32_t cubit_vulkan_device_checker_record(void *borrowed,const struct cubit_vulkan_checker_request *request);
/* Private live owned-image record only, from this admitted device. Does not
 * validate external capabilities, perform upload, or establish image layout. */
void *cubit_vulkan_device_source_request(uint32_t slot,void *owned_image);
/* All 148 backing slots share the 153-entry GPU ledger with three targets
 * and one upload buffer, under the same aggregate byte limit.
 * Caller must serialize and own this slot with a Fresh/confirmed-Closed SPARK
 * source owner; native stage zero can also mean a clean pre-creation failure.
 * This constructs metadata only. No allocation, upload or descriptor import. */
#define CUBIT_VULKAN_OWNED_SOURCE_CAPACITY 148
struct cubit_vulkan_device_source { void *image; uint32_t allowed_types; };
uint32_t cubit_vulkan_device_source_prepare(uint32_t slot,uint32_t width,
    uint32_t height,uint32_t mask,struct cubit_vulkan_device_source *out);

/* Metadata for the single coherent upload buffer on the admitted device.
 * Caller owns a Fresh/confirmed-Closed upload owner and serializes all access.
 * Stage zero can be a clean pre-create failure; it is not reset authority.
 * No allocation, mapping, command recording or queue operation occurs here. */
void *cubit_vulkan_device_upload_prepare(void);
/* Separate stable metadata; same Fresh/confirmed-Closed ownership rule. */
void *cubit_vulkan_device_readback_prepare(void);

#endif
