-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------
package body CuBit.Signed_Admission.Revocations with SPARK_Mode => On is

   function Less (A, B : Digest) return Boolean is
   begin
      for I in Digest'Range loop
         pragma Loop_Invariant (for all J in Digest'First .. I - 1 => A (J) = B (J));
         if A (I) /= B (I) then
            return A (I) < B (I);
         end if;
      end loop;
      return False;
   end Less;

   --  A < B < C means A /= C (the order is a strict order).
   procedure Lemma_Ordered_Distinct (A, B, C : Digest)
   with Ghost, Global => null,
        Pre  => Less_Spec (A, B) and then Less_Spec (B, C),
        Post => A /= C
   is
   begin
      null;
   end Lemma_Ordered_Distinct;

   function Contains (List : Digest_Array; Item : Digest) return Boolean is
   begin
      if List'Length = 0 then
         return False;
      end if;
      declare
         Low  : Positive := List'First;
         High : Natural := List'Last;
         Mid  : Positive;
      begin
      while Low <= High loop
         pragma Loop_Invariant (Low >= List'First and then High <= List'Last);
         pragma Loop_Invariant (for all K in List'First .. Low - 1 => List (K) /= Item);
         pragma Loop_Invariant (for all K in High + 1 .. List'Last => List (K) /= Item);
         pragma Loop_Variant (Decreases => High - Low);
         Mid := Low + (High - Low) / 2;
         if List (Mid) = Item then
            return True;
         elsif Less (List (Mid), Item) then
            for K in List'First .. Mid - 1 loop
               Lemma_Ordered_Distinct (List (K), List (Mid), Item);
               pragma Loop_Invariant (for all J in List'First .. K => List (J) /= Item);
            end loop;
            Low := Mid + 1;
         else
            pragma Assert (Less_Spec (Item, List (Mid)));
            for K in Mid + 1 .. List'Last loop
               Lemma_Ordered_Distinct (Item, List (Mid), List (K));
               pragma Loop_Invariant (for all J in Mid + 1 .. K => List (J) /= Item);
            end loop;
            High := Mid - 1;
         end if;
      end loop;
      end;
      return False;
   end Contains;

end CuBit.Signed_Admission.Revocations;
