------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Typed protocol shared by block-device drivers and filesystem services.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with CuBit.Memory_Grants;

package CuBit.Block_Devices is
   PROTOCOL_VERSION : constant Unsigned_16 := 1;

   type Block_Operation is
     (Read_Blocks, Write_Blocks, Describe_Device, Flush_Device);
   for Block_Operation use
     (Read_Blocks     => 16#0210#,
      Write_Blocks    => 16#0211#,
      Describe_Device => 16#0212#,
      Flush_Device    => 16#0213#);

   --  Wire labels remain named constants because Ada case choices require
   --  static expressions.  This is the only numeric definition site.
   OP_READ_BLOCKS     : constant Unsigned_32 := 16#0210#;
   OP_WRITE_BLOCKS    : constant Unsigned_32 := 16#0211#;
   OP_DESCRIBE_DEVICE : constant Unsigned_32 := 16#0212#;
   OP_FLUSH_DEVICE    : constant Unsigned_32 := 16#0213#;

   REPLY_OK    : constant Unsigned_32 := 16#F000#;
   REPLY_ERROR : constant Unsigned_32 := 16#F001#;

   type Media_Kind is
     (Fixed_Media, Removable_Media, Optical_Media, Memory_Media);
   for Media_Kind use
     (Fixed_Media     => 0,
      Removable_Media => 1,
      Optical_Media   => 2,
      Memory_Media    => 3);

   type Device_Features is mod 2 ** 64;
   FEATURE_READ_ONLY : constant Device_Features := 2#0001#;
   FEATURE_REMOVABLE : constant Device_Features := 2#0010#;
   FEATURE_FLUSH     : constant Device_Features := 2#0100#;
   FEATURE_SCATTER_GATHER : constant Device_Features := 2#1000#;

   subtype Logical_Block_Size is Unsigned_32 range 512 .. 65_536;

   type Device_Description is record
      blockCount        : Unsigned_64 := 0;
      logicalBlockSize  : Logical_Block_Size := 512;
      physicalBlockSize : Logical_Block_Size := 512;
      maxTransferBlocks : Unsigned_32 := 0;
      features          : Device_Features := 0;
      media             : Media_Kind := Fixed_Media;
   end record;

   --  OP_DESCRIBE_DEVICE reply words:
   --    0 = logical block count
   --    1 = logical size (low 32), physical size (high 32)
   --    2 = maximum blocks per transfer
   --    3 = features (low 32), media kind (bits 39:32), version (bits 63:48)
   function Pack_Sizes
     (logicalSize, physicalSize : Logical_Block_Size) return Unsigned_64;
   function Pack_Properties
     (features : Device_Features;
      media    : Media_Kind) return Unsigned_64;
   function Valid_Block_Size (value : Unsigned_64) return Boolean;
   function Decode_Description
     (word0, word1, word2, word3 : Unsigned_64;
      description : out Device_Description) return Boolean;

   function Is_Read_Only (description : Device_Description) return Boolean;

   --  OP_READ_BLOCKS and OP_WRITE_BLOCKS request words:
   --    0 = starting logical block address
   --    1 = shared-memory grant slot
   --    2 = logical block count
   --    3 = shared-memory grant generation
   --  Drivers must resolve the complete reference through the kernel before
   --  touching the mapped buffer.  A device read requires Write_Access; a
   --  device write requires Read_Access.

   --  A local binding to one already-authorized endpoint.  The endpoint slot
   --  is transport identity, not device identity.  The buffer is transitional
   --  until kernel-tracked derived loans can carry client pages end to end.
   type Device_Session is record
      endpointSlot : Unsigned_64 := 0;
      grant        : CuBit.Memory_Grants.Grant_Reference;
      grantBuffer  : System.Address := System.Null_Address;
      grantBytes   : Unsigned_32 := 0;
      description  : Device_Description;
   end record;
end CuBit.Block_Devices;
