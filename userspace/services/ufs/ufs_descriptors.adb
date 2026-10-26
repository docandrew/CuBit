------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body UFS_Descriptors with SPARK_Mode is
   Byte_Bits : constant := 8;

   --  Common header: bLength, bDescriptorIDN (zero-based offsets).
   Length_Offset     : constant := 16#00#;
   Identifier_Offset : constant := 16#01#;
   Header_Bytes      : constant := 2;

   --  bDescriptorIDN values (UFS 14.1.1).
   Device_Identifier   : constant Unsigned_8 := 16#00#;
   Unit_Identifier     : constant Unsigned_8 := 16#02#;
   String_Identifier   : constant Unsigned_8 := 16#05#;
   Geometry_Identifier : constant Unsigned_8 := 16#07#;

   --  Device descriptor (14.1.4.1).
   Device_Number_LU    : constant := 16#06#;
   Device_Spec_Version : constant := 16#10#;
   Device_Manufacturer_Name : constant := 16#14#;
   Device_Product_Name : constant := 16#15#;
   Device_Serial_Number : constant := 16#16#;
   Device_Manufacturer : constant := 16#18#;
   Device_Required     : constant := Device_Manufacturer + 2;

   --  Geometry descriptor (14.1.4.4).
   Geometry_Raw_Capacity : constant := 16#04#;
   Geometry_Max_LU       : constant := 16#0C#;
   Geometry_Required     : constant := Geometry_Max_LU + 1;

   --  Unit descriptor (14.1.4.5).
   Unit_Index_Offset   : constant := 16#02#;
   Unit_Enable         : constant := 16#03#;
   Unit_Write_Protect  : constant := 16#05#;
   Unit_Block_Size     : constant := 16#0A#;
   Unit_Block_Count    : constant := 16#0B#;
   Unit_Required       : constant := Unit_Block_Count + 8;
   --  bLUEnable: 01h enabled (02h: enabled for a replay-protected unit).
   Unit_Enabled_Value  : constant Unsigned_8 := 16#01#;

   Printable_First : constant := 16#20#;
   Printable_Last  : constant := 16#7E#;

   --  Data holds a descriptor of kind Identifier with at least Required
   --  bytes, all inside Data.
   function Holds
     (Data : Bytes; Identifier : Unsigned_8; Required : Positive)
      return Boolean is
     (Data'Length >= Header_Bytes
      and then Natural (Data (Data'First + Length_Offset)) >= Required
      and then Natural (Data (Data'First + Length_Offset)) <= Data'Length
      and then Data (Data'First + Identifier_Offset) = Identifier);

   function Field_8 (Data : Bytes; Offset : Natural) return Unsigned_8 is
     (Data (Data'First + Offset))
     with Pre => Offset < Data'Length;

   function Field_16 (Data : Bytes; Offset : Natural) return Unsigned_16 is
     (Shift_Left (Unsigned_16 (Data (Data'First + Offset)), Byte_Bits)
      or Unsigned_16 (Data (Data'First + Offset + 1)))
     with Pre => Data'Length >= 2 and then Offset <= Data'Length - 2;

   function Field_64 (Data : Bytes; Offset : Natural) return Unsigned_64
     with Pre => Data'Length >= 8 and then Offset <= Data'Length - 8;

   function Field_64 (Data : Bytes; Offset : Natural) return Unsigned_64 is
      Result : Unsigned_64 := 0;
   begin
      for Index in 0 .. 7 loop
         Result := Shift_Left (Result, Byte_Bits)
           or Unsigned_64 (Data (Data'First + Offset + Index));
      end loop;
      return Result;
   end Field_64;

   procedure Decode_Device
     (Data : Bytes; Result : out Device_Information; Valid : out Boolean) is
   begin
      Result := (others => <>);
      Valid := Holds (Data, Device_Identifier, Device_Required);
      if Valid then
         Result :=
           (Specification     => Field_16 (Data, Device_Spec_Version),
            Logical_Units     => Field_8 (Data, Device_Number_LU),
            Manufacturer      => Field_16 (Data, Device_Manufacturer),
            Product_Name      => Field_8 (Data, Device_Product_Name),
            Manufacturer_Name => Field_8 (Data, Device_Manufacturer_Name),
            Serial_Number     => Field_8 (Data, Device_Serial_Number));
      end if;
   end Decode_Device;

   procedure Decode_Geometry
     (Data : Bytes; Result : out Geometry_Information; Valid : out Boolean) is
   begin
      Result := (others => <>);
      Valid := Holds (Data, Geometry_Identifier, Geometry_Required);
      if Valid then
         Result :=
           (Raw_Capacity_Sectors => Field_64 (Data, Geometry_Raw_Capacity),
            Maximum_Units        => Field_8 (Data, Geometry_Max_LU));
      end if;
   end Decode_Geometry;

   procedure Decode_Unit
     (Data : Bytes; Index : Unsigned_8; Result : out Unit_Information;
      Valid : out Boolean) is
   begin
      Result := (others => <>);
      Valid := Holds (Data, Unit_Identifier, Unit_Required)
        and then Field_8 (Data, Unit_Index_Offset) = Index
        and then Field_8 (Data, Unit_Block_Size) in Block_Size_Exponent;
      if Valid then
         Result :=
           (Enabled        => Field_8 (Data, Unit_Enable) = Unit_Enabled_Value,
            Write_Protect  => Field_8 (Data, Unit_Write_Protect),
            Block_Exponent => Field_8 (Data, Unit_Block_Size),
            Block_Count    => Field_64 (Data, Unit_Block_Count));
      end if;
   end Decode_Unit;

   procedure Decode_String
     (Data : Bytes; Result : out Text; Valid : out Boolean) is
   begin
      Result := (others => <>);
      Valid := Holds (Data, String_Identifier, Header_Bytes);
      if not Valid then
         return;
      end if;
      declare
         Units : constant Natural :=
           (Natural (Data (Data'First + Length_Offset)) - Header_Bytes) / 2;
         Count : constant Text_Length := Natural'Min (Units, Maximum_Text);
      begin
         for Index in 1 .. Count loop
            declare
               Unit : constant Unsigned_16 :=
                 Field_16 (Data, Header_Bytes + 2 * (Index - 1));
            begin
               --  Devices pad the name with NUL code units: the text ends there.
               exit when Unit = 0;
               Result.Value (Index) :=
                 (if Unit in Printable_First .. Printable_Last
                  then Character'Val (Unit) else '?');
               Result.Length := Index;
            end;
         end loop;
      end;
   end Decode_String;
end UFS_Descriptors;
