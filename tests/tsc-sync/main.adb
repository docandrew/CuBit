--  Hosted test of TSC_Sync_Rules: the TSC_ADJUST plans, the register's
--  signed reading, the warp samples and verdict, compensation, the step
--  after each check and the shared decision, against plain references; then
--  the whole check simulated on two counters a fixed offset apart, taking
--  turns: a skew is seen from the right side, compensated once, and the
--  second check agrees; a skew both ways is never compensated.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with TSC_Sync_Rules; use TSC_Sync_Rules;

procedure Main is
   Checks : Natural := 0;

   type U64_List is array (Positive range <>) of Unsigned_64;
   Raws : constant U64_List :=
     [0, 1, 2, 16#3B#, 2 ** 31, 2 ** 62, 2 ** 63 - 1, 2 ** 63, 2 ** 63 + 1,
      Unsigned_64'Last - 1_400_000_000, Unsigned_64'Last - 1, Unsigned_64'Last];

   type Adjust_List is array (Positive range <>) of Adjust_Value;
   Adjusts : constant Adjust_List :=
     [Adjust_Value'First, Adjust_Value'First + 1, -1_400_000_000, -1, 0, 1,
      1_400_000_000, 2 ** 48, Adjust_Value'Last - 1, Adjust_Value'Last];

   --  Two's complement reference for the register's bits.
   function Reference_Adjust (Raw : Unsigned_64) return Adjust_Value is
     (if Raw < 2 ** 63 then Adjust_Value (Raw)
      else Adjust_Value (Long_Long_Integer (Raw - 2 ** 63) - 2 ** 62 - 2 ** 62));

   --  One check simulated: counter of the boot CPU = real time, the
   --  secondary CPU's = real time + Offset; they take turns Step apart under
   --  one spin word, each comparing with the other's last published value.
   procedure Simulate (Offset : Long_Long_Integer; Step : Unsigned_64; Turns : Positive;
                       Boot, Secondary : out Observation)
   is
      Base : constant Long_Long_Integer := 10_000_000_000;
      Last : Unsigned_64 := 0;
      Now : Unsigned_64;
   begin
      Boot := No_Observation;
      Secondary := No_Observation;
      for K in 0 .. Turns - 1 loop
         if K mod 2 = 0 then
            Now := Unsigned_64 (Base + Long_Long_Integer (K) * Long_Long_Integer (Step));
            Boot := Observed (Boot, Last, Now);
         else
            Now := Unsigned_64 (Base + Long_Long_Integer (K) * Long_Long_Integer (Step) + Offset);
            Secondary := Observed (Secondary, Last, Now);
         end if;
         Last := Now;
      end loop;
   end Simulate;

begin
   --  The register's bits, both ways.
   for Raw of Raws loop
      pragma Assert (To_Adjust (Raw) = Reference_Adjust (Raw));
      pragma Assert (To_Raw (To_Adjust (Raw)) = Raw);
      Checks := Checks + 2;
   end loop;
   for A of Adjusts loop
      pragma Assert (To_Adjust (To_Raw (A)) = A);
      Checks := Checks + 1;
   end loop;
   pragma Assert (To_Adjust (Unsigned_64'Last) = -1);
   pragma Assert (To_Raw (-1_400_000_000) = Unsigned_64'Last - 1_400_000_000 + 1);
   Checks := Checks + 2;

   --  Boot CPU: forced to 0 when nonzero; a secondary CPU: the boot CPU's.
   for A of Adjusts loop
      pragma Assert (Boot_Plan (A) = (Write => A /= 0, Value => 0));
      for Ref of Adjusts loop
         pragma Assert (Secondary_Plan (A, Ref) = (Write => A /= Ref, Value => Ref));
         Checks := Checks + 1;
      end loop;
      Checks := Checks + 1;
   end loop;

   --  One sample: a warp only when the other's value is above this one.
   for P of Raws loop
      for N of Raws loop
         declare
            O : constant Observation := Observed (No_Observation, P, N);
         begin
            pragma Assert (Warp (P, N) = (if P > N then P - N else 0));
            pragma Assert (O.Rounds = 1 and then O.Warps = (if P > N then 1 else 0));
            pragma Assert (O.Max_Warp = Warp (P, N));
            Checks := Checks + 3;
         end;
      end loop;
   end loop;

   --  Bounded waits.
   pragma Assert (Limit_Ticks (806_400_000, 2) = 1_612_800);
   pragma Assert (Limit_Ticks (0, 100) = 100);
   pragma Assert (Limit_Ticks (999, 7) = 7);
   pragma Assert (Limit_Ticks (Unsigned_64'Last, 1_000) = Unsigned_64'Last / 1_000 * 1_000);
   pragma Assert (Limit_Ticks (Unsigned_64'Last, 1) = Unsigned_64'Last / 1_000);
   pragma Assert (not Expired (100, 199, 100) and then Expired (100, 200, 100));
   pragma Assert (Expired (Unsigned_64'Last - 10, 90, 100));      --  across a wrap
   pragma Assert (not Expired (Unsigned_64'Last - 10, 88, 100));
   Checks := Checks + 8;

   --  Verdicts.
   declare
      Clean : constant Observation := (Rounds => Minimum_Rounds, Warps => 0, Max_Warp => 0);
      Few : constant Observation := (Rounds => Minimum_Rounds - 1, Warps => 0, Max_Warp => 0);
      Warped : constant Observation := (Rounds => Minimum_Rounds, Warps => 3, Max_Warp => 500);
      Huge : constant Observation :=
        (Rounds => Minimum_Rounds, Warps => 1, Max_Warp => Maximum_Compensation + 1);
   begin
      pragma Assert (Verdict_Of (True, Clean, Clean) = Agree);
      pragma Assert (Verdict_Of (False, Clean, Clean) = Incomplete);
      pragma Assert (Verdict_Of (True, Few, Clean) = Incomplete);
      pragma Assert (Verdict_Of (True, Clean, Few) = Incomplete);
      pragma Assert (Verdict_Of (True, Warped, Clean) = Disagree);
      pragma Assert (Verdict_Of (True, Clean, Warped) = Disagree);
      pragma Assert (Verdict_Of (False, Clean, Warped) = Incomplete);
      pragma Assert (Largest_Warp (Warped, Huge) = Maximum_Compensation + 1);
      --  One side only, and not too far: compensable.
      pragma Assert (Compensable (Warped, Clean) and then Compensable (Clean, Warped));
      pragma Assert (not Compensable (Warped, Warped));
      pragma Assert (not Compensable (Clean, Clean));
      pragma Assert (not Compensable (Clean, Huge));
      --  The secondary CPU behind moves forward; ahead moves back.
      pragma Assert (Compensated (100, Clean, Warped) = 600);
      pragma Assert (Compensated (100, Warped, Clean) = -400);
      pragma Assert (Compensated (Adjust_Value'Last - 1, Clean, Warped) = Adjust_Value'Last);
      pragma Assert (Compensated (Adjust_Value'First + 1, Warped, Clean) = Adjust_Value'First);
      Checks := Checks + 16;

      --  The step after each check.
      for Number in Check_Number loop
         for Result in Verdict loop
            for Can in Boolean loop
               for Pair in 1 .. 3 loop
                  declare
                     B : constant Observation := (case Pair is when 1 => Warped, when 2 => Clean, when others => Warped);
                     S : constant Observation := (case Pair is when 1 => Clean, when 2 => Warped, when others => Warped);
                     Expected : constant Step :=
                       (if Result = Agree then Done_Agree
                        elsif Number = Maximum_Checks then Done_Disagree
                        elsif Result = Incomplete then Check_Again
                        elsif Can and then Pair /= 3 then Compensate
                        else Done_Disagree);
                  begin
                     pragma Assert (Next_Step (Number, Result, Can, B, S) = Expected);
                     Checks := Checks + 1;
                  end;
               end loop;
            end loop;
         end loop;
      end loop;
   end;

   --  Shared: every CPU so far, and this one; never again once lost.
   pragma Assert (Still_Shared (True, Done_Agree));
   pragma Assert (not Still_Shared (True, Done_Disagree));
   pragma Assert (not Still_Shared (False, Done_Agree));
   pragma Assert (not Still_Shared (False, Done_Disagree));
   Checks := Checks + 4;

   --  The whole check on two counters Offset apart, turns Step apart.
   declare
      Step_Ticks : constant Unsigned_64 := 200;
      type Offset_List is array (Positive range <>) of Long_Long_Integer;
      Offsets : constant Offset_List :=
        [0, 1, 199, 200, 201, 5_000, 1_400_000_000, -1, -199, -200, -201, -5_000, -1_400_000_000];
      Boot, Secondary : Observation;
      Adjust : Adjust_Value;
      Next : Step;
   begin
      for Offset of Offsets loop
         Simulate (Offset, Step_Ticks, 1_000, Boot, Secondary);
         --  Seen from the side behind, and only when the skew outruns a turn.
         pragma Assert ((Boot.Warps > 0) = (Offset > Long_Long_Integer (Step_Ticks)));
         pragma Assert ((Secondary.Warps > 0) = (Offset < -Long_Long_Integer (Step_Ticks)));
         Next := Next_Step (1, Verdict_Of (True, Boot, Secondary), True, Boot, Secondary);
         if abs Offset <= Long_Long_Integer (Step_Ticks) then
            pragma Assert (Next = Done_Agree);
         else
            pragma Assert (Next = Compensate);
            --  TSC_ADJUST moves the secondary counter; the second check sees
            --  a residue within one turn, so it agrees.
            Adjust := Compensated (0, Boot, Secondary);
            pragma Assert (abs (Offset + Long_Long_Integer (Adjust)) <= Long_Long_Integer (Step_Ticks));
            Simulate (Offset + Long_Long_Integer (Adjust), Step_Ticks, 1_000, Boot, Secondary);
            Next := Next_Step (2, Verdict_Of (True, Boot, Secondary), True, Boot, Secondary);
            pragma Assert (Next = Done_Agree);
         end if;
         Checks := Checks + 3;
      end loop;

      --  Without TSC_ADJUST a skew stays: not one clock.
      Simulate (1_400_000_000, Step_Ticks, 1_000, Boot, Secondary);
      pragma Assert (Next_Step (1, Verdict_Of (True, Boot, Secondary), False, Boot, Secondary)
                     = Done_Disagree);
      --  A counter that warps both ways (no one offset) is never compensated.
      Boot := Observed (Observed (No_Observation, 0, 10), 30, 20);
      Secondary := Observed (Observed (No_Observation, 0, 10), 50, 20);
      for I in 1 .. Minimum_Rounds loop
         Boot := Observed (Boot, 0, 1);
         Secondary := Observed (Secondary, 0, 1);
      end loop;
      pragma Assert (Next_Step (1, Verdict_Of (True, Boot, Secondary), True, Boot, Secondary)
                     = Done_Disagree);
      Checks := Checks + 2;
   end;

   Put_Line ("tsc-sync:" & Natural'Image (Checks) & " checks PASS");
end Main;
