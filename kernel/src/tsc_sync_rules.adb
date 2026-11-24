-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- The TSC agreement rules (tsc_sync_rules.ads).
-------------------------------------------------------------------------------
pragma Ada_2022;

package body TSC_Sync_Rules with SPARK_Mode is

   function To_Adjust (Raw : Unsigned_64) return Adjust_Value is
   begin
      if Raw < Sign_Bit then
         return Adjust_Value (Raw);
      end if;
      declare
         --  Bits below the sign bit: the value above the type's first.
         Low : constant Unsigned_64 := Raw - Sign_Bit;
         Result : constant Adjust_Value := Adjust_Value (Low) + Adjust_Value'First;
      begin
         pragma Assert (Low < Sign_Bit and then Low + Sign_Bit = Raw);
         pragma Assert (Result < 0);
         pragma Assert (Result - Adjust_Value'First = Adjust_Value (Low));
         pragma Assert (Unsigned_64 (Result - Adjust_Value'First) = Low);
         return Result;
      end;
   end To_Adjust;

end TSC_Sync_Rules;
