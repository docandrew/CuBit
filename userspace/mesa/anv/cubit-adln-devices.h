#pragma once
#include <stdbool.h>
#include <stdint.h>
/* The Alder Lake-N graphics family the CuBit Intel backend drives (HW-002):
 * the same range as intel-gpu's Intel_GPU_ADLN_Devices and its manifest
 * (drives PCI_IDs 8086:46D0..46D4). Mesa's own PCI table supplies each
 * device's defaults (46d2 adl_gt05, the others adl_gt1); topology comes from
 * the driver's measured fuse masks. */
#define CUBIT_INTEL_VENDOR 0x8086u
#define CUBIT_ADLN_FIRST_DEVICE 0x46d0u
#define CUBIT_ADLN_LAST_DEVICE 0x46d4u

static inline bool
cubit_adln_supported_device(uint32_t device)
{
   return device >= CUBIT_ADLN_FIRST_DEVICE && device <= CUBIT_ADLN_LAST_DEVICE;
}
