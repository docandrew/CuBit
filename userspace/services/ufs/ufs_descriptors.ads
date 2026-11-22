------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Decoders for the UFS descriptors the driver reads at bring-up (JESD220
--  section 14.1): device, geometry, unit and string. Fields are big-endian;
--  every decoder accepts any array origin and checks bLength and the
--  descriptor identifier before reading a field.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with SCSI;

package UFS_Descriptors with SPARK_Mode, Pure is
   subtype Bytes is SCSI.Bytes;

   type Device_Information is record
      --  wSpecVersion as BCD, e.g. 16#0310# for UFS 3.1.
      Specification   : Unsigned_16 := 0;
      Logical_Units   : Unsigned_8 := 0;
      Manufacturer    : Unsigned_16 := 0;
      --  String descriptor indexes.
      Product_Name    : Unsigned_8 := 0;
      Manufacturer_Name : Unsigned_8 := 0;
      Serial_Number     : Unsigned_8 := 0;
   end record;

   procedure Decode_Device
     (Data : Bytes; Result : out Device_Information; Valid : out Boolean);

   type Geometry_Information is record
      --  qTotalRawDeviceCapacity, in 512-byte units.
      Raw_Capacity_Sectors : Unsigned_64 := 0;
      Maximum_Units        : Unsigned_8 := 0;
   end record;

   procedure Decode_Geometry
     (Data : Bytes; Result : out Geometry_Information; Valid : out Boolean);

   --  bLogicalBlockSize is a power of two: 2 ** 12 = 4 KiB.
   subtype Block_Size_Exponent is Unsigned_8 range 9 .. 16;

   type Unit_Information is record
      Enabled        : Boolean := False;
      Write_Protect  : Unsigned_8 := 0;
      Block_Exponent : Block_Size_Exponent := Block_Size_Exponent'First;
      Block_Count    : Unsigned_64 := 0;
   end record;

   --  Valid also requires a block size CuBit's block protocol carries.
   procedure Decode_Unit
     (Data : Bytes; Index : Unsigned_8; Result : out Unit_Information;
      Valid : out Boolean);

   --  String descriptors carry UTF-16 (big-endian code units).
   Maximum_Text : constant := 126;
   subtype Text_Length is Natural range 0 .. Maximum_Text;
   type Text is record
      Length : Text_Length := 0;
      Value  : String (1 .. Maximum_Text) := [others => ' '];
   end record;

   --  Code units outside printable ASCII become '?'; a NUL ends the text.
   procedure Decode_String
     (Data : Bytes; Result : out Text; Valid : out Boolean)
     with Post => (if not Valid then Result.Length = 0);
end UFS_Descriptors;
