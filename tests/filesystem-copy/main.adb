--  Hosted tests for Copy_Slices (docs/filesystem-protocol-v2.md step 5): a
--  model of copies against a source that grows and shrinks while they run.
--  Each copy ends exactly once; the bytes copied are a prefix of the range,
--  never past the wanted length or the source's end when each slice was cut.
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Interfaces; use Interfaces;
with Copy_Slices; use Copy_Slices;

procedure Main is
   Failures, Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         if Failures <= 20 then
            Ada.Text_IO.Put_Line ("FAIL: " & What);
         end if;
      end if;
   end Check;

   subtype Small is Natural range 0 .. 1_000_000;
   package Random is new Ada.Numerics.Discrete_Random (Small);
   G : Random.Generator;
   function R (N : Positive) return Natural is (Random.Random (G) mod N);

   C : Copy;
   Admitted : Boolean;
   Endings : array (Ending) of Natural := [others => 0];
begin
   Random.Reset (G, 2026_1009);
   --  Admission.
   Admit (0, 0, 0, C, Admitted);
   Check (not Admitted, "zero length refused");
   Admit (Maximum_Offset + 1, 0, 1, C, Admitted);
   Check (not Admitted, "source offset refused");
   Admit (0, Maximum_Offset + 1, 1, C, Admitted);
   Check (not Admitted, "target offset refused");
   Admit (0, 0, Maximum_Offset + 1, C, Admitted);
   Check (not Admitted, "length refused");
   Admit (Maximum_Offset, Maximum_Offset, Copy_To_End, C, Admitted);
   Check (Admitted and then C.Wanted = Maximum_Offset, "to end admitted");
   Check (Next (C, Unsigned_64'Last, Positive'Last) = Unsigned_64 (Positive'Last), "huge source");
   Check (Next (C, Maximum_Offset, Positive'Last) = 0, "source end");

   for Round in 1 .. 20_000 loop
      declare
         Source_Size : Unsigned_64 := Unsigned_64 (R (1_000_000));
         Length : constant Unsigned_64 :=
           (if R (3) = 0 then Copy_To_End else Unsigned_64 (1 + R (1_200_000)));
         Source_At : constant Unsigned_64 := Unsigned_64 (R (1_100_000));
         Limit : constant Positive := 1 + R (70_000);
         Cancel_At : constant Natural := R (200);
         Deadline_At : constant Natural := R (300);
         Fail_At : constant Natural := R (400);
         Ended : Ending := Going;
         Pass : Natural := 0;
         Last_Done : Unsigned_64 := 0;
      begin
         Admit (Source_At, Unsigned_64 (R (1000)), Length, C, Admitted);
         Check (Admitted, "admitted");
         while Ended = Going loop
            Pass := Pass + 1;
            if R (10) = 0 then   --  the source changes under the copy
               Source_Size := Unsigned_64 (R (1_000_000));
            end if;
            declare
               Slice : constant Unsigned_64 := Next (C, Source_Size, Limit);
               Failed : constant Boolean := Pass = Fail_At;
               Copied : constant Unsigned_64 := (if Failed then Slice / 2 else Slice);
            begin
               Check (Slice <= Unsigned_64 (Limit) and then C.Done + Slice <= C.Wanted
                      and then (Slice = 0 or else Source_Position (C) + Slice <= Source_Size),
                      "slice bounds");
               Advance (C, Copied);
               Check (C.Done >= Last_Done and then C.Done <= C.Wanted, "monotonic prefix");
               Last_Done := C.Done;
               Ended := Decide (Failed, Pass = Cancel_At, Slice = 0, Pass = Deadline_At);
            end;
            Check (Pass < 100_000, "terminates");
            exit when Pass >= 100_000;
         end loop;
         Endings (Ended) := Endings (Ended) + 1;
         if Ended = Complete and then Length /= Copy_To_End then
            Check (C.Done = Length or else Source_Position (C) >= Source_Size, "complete is full or EOF");
         end if;
      end;
   end loop;
   Check (Decide (True, True, True, True) = Failed and then Decide (False, True, True, True) = Cancelled
          and then Decide (False, False, True, True) = Complete
          and then Decide (False, False, False, True) = Deadline_Reached
          and then Decide (False, False, False, False) = Going, "decision order");
   Check (Endings (Complete) > 0 and then Endings (Cancelled) > 0 and then Endings (Failed) > 0
          and then Endings (Deadline_Reached) > 0, "all endings exercised");
   Ada.Text_IO.Put_Line ("copies: complete" & Endings (Complete)'Image & ", cancelled" &
     Endings (Cancelled)'Image & ", deadline" & Endings (Deadline_Reached)'Image & ", failed" &
     Endings (Failed)'Image);
   Ada.Text_IO.Put_Line ("FILESYSTEM-COPY:" & (if Failures = 0 then Checks'Image & " checks PASS"
                         else Failures'Image & " of" & Checks'Image & " checks FAIL"));
end Main;
