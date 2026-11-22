--  Hosted checks for Work_Stealing (docs/threads.md). Assertions enabled.
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Work_Stealing; use Work_Stealing;

procedure Main is
   Checked : Natural := 0;

   --  Reference rule, written independently of the package.
   function Reference (C : Candidate; Now, Rate, Age : Unsigned_64) return Boolean is
      Min : Unsigned_64;
   begin
      if C.Priority < 0 or else C.Pinned or else C.Closing or else C.Executing
        or else Rate = 0
      then
         return False;
      end if;
      if Age = 0 then
         Min := 0;
      elsif Age > Unsigned_64'Last / Rate then
         Min := Unsigned_64'Last;
      else
         Min := Rate * Age;
      end if;
      return Now >= C.Queued_At and then Now - C.Queued_At >= Min;
   end Reference;

   Times : constant array (1 .. 7) of Unsigned_64 :=
     [0, 1, 999, 1_000, 1_001, Unsigned_64'Last - 1, Unsigned_64'Last];
   Rates : constant array (1 .. 4) of Unsigned_64 := [0, 1, 2, Unsigned_64'Last];
   Ages  : constant array (1 .. 4) of Unsigned_64 := [0, 1, 500, Unsigned_64'Last];
begin
   --  Exhaustive over flags, priorities around the idle boundary, and time,
   --  rate and age boundaries (including clock reversal and saturation).
   for Priority in -2 .. 2 loop
      for Flags in 0 .. 7 loop
         for Queued of Times loop
            for Now of Times loop
               for Rate of Rates loop
                  for Age of Ages loop
                     declare
                        C : constant Candidate :=
                          (Priority  => Priority,
                           Pinned    => Flags mod 2 = 1,
                           Closing   => (Flags / 2) mod 2 = 1,
                           Executing => (Flags / 4) mod 2 = 1,
                           Queued_At => Queued);
                     begin
                        pragma Assert (Eligible (C, Now, Rate, Age) =
                                       Reference (C, Now, Rate, Age));
                        Checked := Checked + 1;
                     end;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;

   --  Clock reversal: a stamp from a CPU whose TSC reads ahead is not aged.
   pragma Assert (not Aged (Queued_At => 1_000, Now => 999, Min => 0));
   --  A freshly woken entry (age below the threshold) stays put.
   pragma Assert (not Eligible ((5, False, False, False, 1_000), 1_499, 1, 500));
   pragma Assert (Eligible ((5, False, False, False, 1_000), 1_500, 1, 500));
   --  Idle threads (priority -1) and pinned work never move.
   pragma Assert (not Eligible ((-1, False, False, False, 0), 10_000, 1, 500));
   pragma Assert (not Eligible ((5, True, False, False, 0), 10_000, 1, 500));

   --  Selection model: first eligible in a sorted list is the best eligible.
   declare
      P : constant Priorities (1 .. 6) := [9, 7, 7, 5, 3, -1];
      E : constant Eligibility (1 .. 6) := [False, False, True, True, False, False];
   begin
      pragma Assert (First_Eligible (E) = 3);
      for I in E'Range loop
         if E (I) then
            pragma Assert (P (First_Eligible (E)) >= P (I));
         end if;
      end loop;
      pragma Assert (First_Eligible (Eligibility'(1 .. 3 => False)) = 0);
   end;

   Ada.Text_IO.Put_Line
     ("PASS: work stealing eligibility," & Natural'Image (Checked) &
      " boundary cases, clock reversal, saturation, selection model");
end Main;
