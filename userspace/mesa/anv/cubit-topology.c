#include "cubit-topology.h"
#include <string.h>
#include "intel/dev/intel_device_info.h"

bool
cubit_mesa_adln_topology_masks(struct intel_device_info *info,
                               uint8_t dss_mask, uint16_t eu_mask)
{
   if (!info || info->pci_device_id != 0x46d2 ||
       info->platform != INTEL_PLATFORM_ADL || info->ver != 12 ||
       info->verx10 != 120 || !dss_mask || (dss_mask & ~0x3fu) || !eu_mask)
      return false;
   /* ADL-N fuses enable whole EU pairs, not individual members. */
   if ((eu_mask & 0x5555u) != ((eu_mask >> 1) & 0x5555u))
      return false;

   memset(info->subslice_masks, 0, sizeof(info->subslice_masks));
   memset(info->eu_masks, 0, sizeof(info->eu_masks));
   memset(info->num_subslices, 0, sizeof(info->num_subslices));
   memset(info->ppipe_subslices, 0, sizeof(info->ppipe_subslices));
   info->num_slices = 0;
   info->subslice_total = 0;
   info->slice_masks = 1;
   info->max_slices = 1;
   info->max_subslices_per_slice = 6;
   info->max_eus_per_subslice = 16;
   info->subslice_slice_stride = 1;
   info->eu_subslice_stride = 2;
   info->eu_slice_stride = 12;
   info->subslice_masks[0] = dss_mask;
   for (unsigned dss = 0; dss < 6; dss++) {
      if (!(dss_mask & (1u << dss)))
         continue;
      info->eu_masks[dss * 2] = eu_mask & 0xffu;
      info->eu_masks[dss * 2 + 1] = eu_mask >> 8;
   }
   /* Keep derived topology coherent with the measured masks. These are the
    * pinned Mesa routines also used by native Linux device discovery. */
   intel_device_info_topology_update_counts(info);
   intel_device_info_update_pixel_pipes(info, info->subslice_masks);
   intel_device_info_update_l3_banks(info);
   return true;
}
