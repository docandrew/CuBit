------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Disk_Layout with SPARK_Mode is

   Byte_Bits : constant := 8;
   Byte_Mask : constant := 16#FF#;

   procedure Put_8 (Data : in out Bytes; Offset : Natural; Value : Unsigned_8) is
   begin
      Data (Offset + 1) := Value;
   end Put_8;

   procedure Put_16 (Data : in out Bytes; Offset : Natural; Value : Unsigned_16) is
   begin
      Data (Offset + 1) := Unsigned_8 (Value and Byte_Mask);
      Data (Offset + 2) := Unsigned_8 (Shift_Right (Value, Byte_Bits));
   end Put_16;

   procedure Put_32 (Data : in out Bytes; Offset : Natural; Value : Unsigned_32) is
   begin
      for I in 0 .. 3 loop
         Data (Offset + 1 + I) :=
           Unsigned_8 (Shift_Right (Value, Byte_Bits * I) and Byte_Mask);
      end loop;
   end Put_32;

   procedure Put_64 (Data : in out Bytes; Offset : Natural; Value : Unsigned_64) is
   begin
      for I in 0 .. 7 loop
         Data (Offset + 1 + I) :=
           Unsigned_8 (Shift_Right (Value, Byte_Bits * I) and Byte_Mask);
      end loop;
   end Put_64;

   procedure Put_Text (Data : in out Bytes; Offset : Natural; Text : String) is
   begin
      for I in 0 .. Text'Length - 1 loop
         Data (Offset + 1 + I) := Character'Pos (Text (Text'First + I));
      end loop;
   end Put_Text;

   procedure Put_GUID (Data : in out Bytes; Offset : Natural; Value : GUID) is
   begin
      for I in Value'Range loop
         Data (Offset + I) := Value (I);
      end loop;
   end Put_GUID;

end Disk_Layout;
