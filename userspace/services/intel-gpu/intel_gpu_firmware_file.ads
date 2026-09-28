with Interfaces;
with System;
with CuBit.Messages;
with Intel_GPU_Firmware;
package Intel_GPU_Firmware_File is
   Path : constant String := "firmware/intel/tgl_guc_70.bin";
   type Load_Status is
     (Loaded, Already_Attempted, Allocation_Failed, Grant_Failed,
      Transport_Failed, Open_Failed, Access_Denied, Contents_Rejected,
      Metadata_Rejected, Close_Failed);
   function Name (Status : Load_Status) return String is
     (case Status is
        when Loaded => "LOADED",
        when Already_Attempted => "already-attempted",
        when Allocation_Failed => "allocation-failed",
        when Grant_Failed => "grant-failed",
        when Transport_Failed => "transport-failed",
        when Open_Failed => "open-failed",
        when Access_Denied => "access-denied",
        when Contents_Rejected => "contents-rejected",
        when Metadata_Rejected => "metadata-rejected",
        when Close_Failed => "close-failed");
   --  One-shot bring-up loader; invoke before other asynchronous producers.
   --  Slot must already name the filesystem with this read scope installed.
   --  On timeout, process-lived scratch/loan storage is quarantined, never
   --  reused. Loaded bytes are private, immutable by convention, not DMA-ready
   --  or authenticated. No GPU registers are touched.
   procedure Load
     (Slot : CuBit.Messages.CapabilitySlot;
      Address : out System.Address; Bytes : out Interfaces.Unsigned_64;
      Plan : out Intel_GPU_Firmware.Layout; Status : out Load_Status);
end Intel_GPU_Firmware_File;
