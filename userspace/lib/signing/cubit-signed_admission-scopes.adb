-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------
package body CuBit.Signed_Admission.Scopes with SPARK_Mode => On is

   function Covers (Prefix, Identity : Identity_Text) return Boolean is
   begin
      if Prefix.Length = 1 and then Prefix.Data (1) = Every_Identity then
         return True;
      elsif Prefix.Length = 0 or else Identity.Length < Prefix.Length then
         return False;
      end if;
      for I in 1 .. Prefix.Length loop
         pragma Loop_Invariant
           (Identity.Data (1 .. I - 1) = Prefix.Data (1 .. I - 1));
         if Identity.Data (I) /= Prefix.Data (I) then
            return False;
         end if;
      end loop;
      return Identity.Length = Prefix.Length
        or else Identity.Data (Prefix.Length + 1) = Label_Separator;
   end Covers;

   function In_Scope
     (Prefixes : Prefix_Array; First : Prefix_Index; Last : Natural; Identity : Identity_Text)
      return Boolean
   is
   begin
      for I in Prefixes'Range loop
         pragma Loop_Invariant
           (for all J in Prefixes'First .. I - 1 =>
              not (J >= First and then J <= Last and then Covers (Prefixes (J), Identity)));
         if I >= First and then I <= Last and then Covers (Prefixes (I), Identity) then
            return True;
         end if;
      end loop;
      return False;
   end In_Scope;

end CuBit.Signed_Admission.Scopes;
