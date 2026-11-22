------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Desktop_Load_Bars with SPARK_Mode is

   function Used_Percent (Total, Free : Unsigned_64) return Percent is
      Used : Unsigned_64;
   begin
      if Total = 0 or else Free > Total then
         return 0;
      end if;
      Used := Total - Free;
      --  Used * 100 / Total without overflow: scale the divisor instead
      --  when Used is large.
      if Total <= Unsigned_64'Last / 100 then
         return Percent (Unsigned_64'Min (100, Used * 100 / Total));
      else
         --  Total is at least 100 here: whole hundredths of it.
         return Percent (Unsigned_64'Min (100, Used / (Total / 100)));
      end if;
   end Used_Percent;

   function Filled (Height : Natural; Used : Percent) return Natural is
      F : constant Natural := Height * Used / 100;
   begin
      if Used > 0 and then Height > 0 and then F = 0 then
         return 1;
      end if;
      return F;
   end Filled;
end Desktop_Load_Bars;
