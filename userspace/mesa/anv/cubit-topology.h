#pragma once
#include <stdbool.h>
#include <stdint.h>
struct intel_device_info;
/* Internal Mesa adapter, not an IPC wire structure or authority check.
 * Caller authenticates a stable driver snapshot and initializes ADL-N device
 * defaults first. This replaces masks and updates counts, pixel pipes and L3
 * banks together. The device provider must still finalize scratch limits and
 * workarounds before device exposure; success here does not expose a device.
 */
bool cubit_mesa_adln_topology_masks(struct intel_device_info *info,
                                    uint8_t dss_mask, uint16_t eu_mask);
