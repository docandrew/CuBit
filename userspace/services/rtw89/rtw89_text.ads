------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Number formatting for rtw89.drv's stage lines (no runtime image units).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package RTW89_Text with Pure is

   function Hex (Value : Unsigned_64; Digits_Count : Positive) return String;
   function Hex2 (Value : Unsigned_8) return String is (Hex (Unsigned_64 (Value), 2));
   function Dec (Value : Unsigned_64) return String;

end RTW89_Text;
