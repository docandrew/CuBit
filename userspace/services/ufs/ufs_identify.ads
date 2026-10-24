------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Device bring-up after link startup, one logged stage at a time: NOP
--  OUT/IN, fDeviceInit, device and geometry descriptors, unit descriptors,
--  then SCSI identification of the first enabled unit (TEST UNIT READY,
--  INQUIRY, READ CAPACITY(16)) and a read-only look at its partition table.
--  Reads only: no command here writes the medium.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with CuBit.Block_Devices;
with UFS_UPIU;

package UFS_Identify is
   type Identity is record
      Unit       : UFS_UPIU.Logical_Unit := 0;
      Blocks     : Unsigned_64 := 0;
      Block_Size : CuBit.Block_Devices.Logical_Block_Size :=
        CuBit.Block_Devices.Logical_Block_Size'First;
   end record;

   --  Ready: the unit answered and its capacity is usable.
   procedure Identify (Result : out Identity; Ready : out Boolean);

   --  Logs the protective MBR and the GPT header and entries.
   procedure Report_Partitions (Device : Identity);
end UFS_Identify;
