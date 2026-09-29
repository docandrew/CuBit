with Interfaces;
with System;
with Intel_GPU_GuC_CT_Setup;
with Intel_GPU_Submission_Backing;
package Intel_GPU_Firmware_Buffer is
   use type Interfaces.Unsigned_64;
   -- One retained allocation, disjoint firmware/log/CT regions. Preparation
   -- does not GPU-publish any region.
   -- Keep firmware growth from silently consuming the writable log storage.
   Firmware_Region_Bytes : constant Interfaces.Unsigned_64 := 512 * 1024;
   Log_Region_Offset : constant Interfaces.Unsigned_64 := Firmware_Region_Bytes;
   Log_Region_Bytes : constant Interfaces.Unsigned_64 := 16 * 1024;
   CT_Region_Offset : constant Interfaces.Unsigned_64 := Log_Region_Offset + Log_Region_Bytes;
   CT_Region_Bytes : constant Interfaces.Unsigned_64 := 32 * 1024;
   pragma Compile_Time_Error
     (CT_Region_Offset + CT_Region_Bytes > Intel_GPU_Submission_Backing.First,
      "submission backing overlaps firmware/log/CT regions");
   pragma Compile_Time_Error
     (Firmware_Region_Bytes > Log_Region_Offset or else
      Log_Region_Offset + Log_Region_Bytes > 1024 * 1024 or else
      Log_Region_Offset mod 4096 /= 0 or else Log_Region_Bytes mod 4096 /= 0,
      "firmware/log regions must be disjoint aligned slices of retained allocation");
   pragma Compile_Time_Error
     (CT_Region_Offset < Log_Region_Offset + Log_Region_Bytes or else
      CT_Region_Offset + CT_Region_Bytes > 1024 * 1024 or else
      CT_Region_Bytes < Intel_GPU_GuC_CT_Setup.Required_Bytes or else
      CT_Region_Offset mod 4096 /= 0 or else CT_Region_Bytes mod 4096 /= 0,
      "CT region must be disjoint, page aligned and fit the retained allocation");
   type Prepared_Buffer (Ready : Boolean := False) is record
      case Ready is
         when True =>
            DMA_Address, CPU_Address, Allocation_Bytes, Content_Bytes : Interfaces.Unsigned_64;
         when False => null;
      end case;
   end record;
   function Prepared return Prepared_Buffer;
   type Prepared_Log_Buffer (Ready : Boolean := False) is record
      case Ready is
         when True =>
            DMA_Address, CPU_Address, Region_Bytes : Interfaces.Unsigned_64;
         when False => null;
      end case;
   end record;
   function Prepared_Log return Prepared_Log_Buffer;
   type Prepared_CT_Buffer (Ready : Boolean := False) is record
      case Ready is
         when True =>
            DMA_Address, CPU_Address, Region_Bytes : Interfaces.Unsigned_64;
         when False => null;
      end case;
   end record;
   function Prepared_CT return Prepared_CT_Buffer;
   -- Non-overlapping, initially zeroed slices of the same retained allocation.
   -- These are CPU/DMA backing views, never GPU VAs or publication permission.
   -- No slice may be independently freed or reinitialized after device use.
   type Prepared_Submission_Buffer (Ready : Boolean := False) is record
      case Ready is
         when True =>
            DMA_Address, CPU_Address, Region_Bytes : Interfaces.Unsigned_64;
         when False => null;
      end case;
   end record;
   function Prepared_Submission (Part : Intel_GPU_Submission_Backing.Region)
     return Prepared_Submission_Buffer;
   -- Both descriptors and rings are initially zero/readback-checked/flushed
   -- by Prepare's whole-allocation pass. This is a retained backing view,
   -- NOT permission to reinitialize after firmware registration/publication.
   -- Zeroed/readback-checked/flushed storage for a state page and three 4KiB
   -- log sections. Addresses are CPU/DMA, NOT GGTT addresses. Shares the
   -- retained allocation's lifetime and must never be freed independently.
   -- Only published after complete copy/padding/readback and x86 cache flush.
   -- Addresses refer to
   -- retained backing, not GGTT placement. This does not establish device cache
   -- visibility, authentication, GPU ownership or permission to free memory.
   -- One-shot retained CPU buffer preparation. No GPU mapping,
   -- authentication, upload or submission is performed.
   function Prepare (Source : System.Address; Bytes : Interfaces.Unsigned_64)
     return String;
end Intel_GPU_Firmware_Buffer;
