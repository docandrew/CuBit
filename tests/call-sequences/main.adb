--  Hosted tests for Call_Sequences: a caller times out and calls again; the
--  late answer to the first call is refused, the second call's own answer
--  is accepted, and a thread at the last sequence makes no more calls.
pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Call_Sequences; use Call_Sequences;

procedure Main is
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   Current : Sequence := 0;
   First, Second : Sequence;
begin
   Current := Next (Current);
   First := Current;
   Check (Accepts (True, Current, First), "an answer to the waited call");
   Check (not Accepts (False, Current, First), "nothing once the caller stops waiting");

   --  The first call timed out; the caller begins another.
   Current := Next (Current);
   Second := Current;
   Check (not Accepts (True, Current, First), "the late answer is refused");
   Check (Accepts (True, Current, Second), "the new call's own answer");

   --  Rollover: retired at the maximum, never wrapped.
   Check (Can_Begin (Sequence'Last - 1), "below the last, a call begins");
   Check (Next (Sequence'Last - 1) = Sequence'Last, "the last sequence");
   Check (not Can_Begin (Sequence'Last), "at the last, no more calls");

   if Failures = 0 then
      Ada.Text_IO.Put_Line ("call-sequences: PASS");
   else
      Ada.Text_IO.Put_Line ("call-sequences: FAIL" & Failures'Image);
   end if;
end Main;
