------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Seqlock_Model with SPARK_Mode is

   procedure Lemma_Counter_Monotonic (H : History; First, Last : Step) is
   begin
      for S in First .. Last loop
         pragma Loop_Invariant
           (for all T in First .. S => H (First).Sequence <= H (T).Sequence);
         pragma Loop_Invariant
           (for all T in First .. S => H (T).Sequence <= H (S).Sequence);
         if S < Last then
            pragma Assert (Writer_Step (H (S), H (S + 1)));
            pragma Assert (H (S).Sequence <= H (S + 1).Sequence);
         end if;
      end loop;
   end Lemma_Counter_Monotonic;

   procedure Theorem_Stable_Read (H : History; First, Last : Step) is
   begin
      Lemma_Counter_Monotonic (H, First, Last);
      for S in First .. Last loop
         pragma Loop_Invariant
           (for all T in First .. S =>
              H (T).Values = H (First).Values
              and then H (T).Sequence = H (First).Sequence);
         if S < Last then
            pragma Assert (H (S + 1).Sequence = H (First).Sequence);
            pragma Assert (not Writing (H (S).Sequence));
            pragma Assert (Writer_Step (H (S), H (S + 1)));
            pragma Assert (H (S + 1).Values = H (S).Values);
         end if;
      end loop;
   end Theorem_Stable_Read;

   procedure Theorem_Counters_Grow (H : History; First, Last : Step) is
   begin
      for S in First .. Last loop
         pragma Loop_Invariant (Not_Behind (H (First).Values, H (S).Values));
         if S < Last then
            pragma Assert (Writer_Step (H (S), H (S + 1)));
            pragma Assert (Not_Behind (H (S).Values, H (S + 1).Values));
         end if;
      end loop;
   end Theorem_Counters_Grow;

   procedure Lemma_Accounted_Admitted
     (F : Fields; Ran : Charged_State; Ticks : Unsigned_64;
      Next : Charged_State; Now : Unsigned_64; Started : Boolean)
   is null;

end Seqlock_Model;
