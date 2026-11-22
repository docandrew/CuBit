with Interfaces; use Interfaces;
package body Boot_Font with SPARK_Mode is
   function Pixel (C : Character; X : Column; Y : Row) return Boolean is
   begin
      if C not in ' ' .. '~' then return False; end if;
      return (fontMap (Character'Pos (C) - 31) (Height - Y) and
              Shift_Left (Unsigned_8 (1), Width - 1 - X)) /= 0;
   end Pixel;
end Boot_Font;
