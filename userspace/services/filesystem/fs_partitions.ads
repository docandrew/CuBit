------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A GPT disk's root volume: the first Linux filesystem partition (the
--  installer's root, SYS-001). Read through the device's Block.Device.V1
--  endpoint and the volume's grant, decoded by Partition_Tables (the same
--  decoder the UFS driver logs the table with). A disk without a GPT is
--  one volume, as before.
------------------------------------------------------------------------------
with CuBit.Handles;
with CuBit.Memory_Grants;
with System;
with Interfaces; use Interfaces;
with Ext2;

package Fs_Partitions is
   type Window_Result is (Whole_Device, Partition, Device_Error, No_Root_Partition);

   procedure Find_Root
     (Endpoint : CuBit.Handles.Endpoint; Grant : CuBit.Memory_Grants.Grant_Reference;
      Buffer : System.Address; Buffer_Bytes : Unsigned_32;
      Window : out Ext2.Volume_Window; Result : out Window_Result);
end Fs_Partitions;
