------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Numbers as text for the driver's bring-up log lines.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package UFS_Text with SPARK_Mode, Pure is
   Decimal_Digits : constant := 20;   --  Unsigned_64'Last has 20
   Hex_Digits     : constant := 16;
   subtype Hex_Width is Positive range 1 .. Hex_Digits;

   function Decimal (Value : Unsigned_64) return String
     with Post => Decimal'Result'First = 1
                  and then Decimal'Result'Length in 1 .. Decimal_Digits;

   --  A byte count in one compact unit: "512K", "260M", "118G", "2T"
   --  (rounded down; a unit is used once the count reaches ten of it).
   function Size (Bytes : Unsigned_64) return String;

   --  Width hex digits, zero padded, no prefix.
   function Hex (Value : Unsigned_64; Width : Hex_Width) return String
     with Post => Hex'Result'Length = Width;
end UFS_Text;
