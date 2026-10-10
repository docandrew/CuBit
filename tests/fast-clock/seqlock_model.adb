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
              H (T).Fields = H (First).Fields
              and then H (T).Sequence = H (First).Sequence);
         if S < Last then
            --  The counter cannot leave its even value and come back.
            pragma Assert (H (S + 1).Sequence = H (First).Sequence);
            pragma Assert (not Writing (H (S).Sequence));
            pragma Assert (Writer_Step (H (S), H (S + 1)));
            pragma Assert (H (S + 1).Fields = H (S).Fields);
         end if;
      end loop;
   end Theorem_Stable_Read;

   procedure Theorem_Rebase_Monotonic
     (Old       : Parameters;
      At_Ticks  : Unsigned_64;
      Frequency : Counter_Frequency;
      Earlier   : Unsigned_64;
      Later     : Unsigned_64)
   is
      New_Clock : constant Parameters := Rebase (Old, At_Ticks, Frequency);
   begin
      Lemma_Time_Monotonic (Old, Earlier, At_Ticks);
      pragma Assert (New_Clock.Base_Time = Time_At (Old, At_Ticks));
      pragma Assert (Time_At (New_Clock, Later) >= New_Clock.Base_Time);
   end Theorem_Rebase_Monotonic;

end Seqlock_Model;
