------------------------------------------------------------------------------
--  Hosted check of the kernel trace object's request decision
--  (Trace_ABI.Decide): every label, length, right and kernel build.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Trace_ABI; use Trace_ABI;

procedure Main is
   Checks, Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & What);
      end if;
   end Check;
   function Refused (D : Decision; Why : Outcome) return Boolean is
     (not D.Valid and then D.Why = Why);
begin
   for Writable in Boolean loop
      for Timeline in Boolean loop
         for Label in Unsigned_64 range 0 .. 8 loop
            for Length in Unsigned_64 range 0 .. 4 loop
               declare
                  D : constant Decision := Decide (Writable, Label, Length, Timeline);
                  Is_Op : constant Boolean := Label in 1 .. 5;
                  Local : constant Boolean := Label in 3 .. 5;
               begin
                  if not Writable then
                     Check (Refused (D, Not_Permitted), "no Write refused first");
                  elsif not Is_Op then
                     Check (Refused (D, Unknown_Operation), "unknown label" & Label'Image);
                  elsif Length /= 0 then
                     Check (Refused (D, Malformed), "words refused");
                  elsif Local and then not Timeline then
                     Check (Refused (D, Not_In_This_Kernel), "timeline without LATENCY_TRACE");
                  else
                     Check (D.Valid and then Operation'Enum_Rep (D.Op) = Label, "accepted" & Label'Image);
                  end if;
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   --  The wire codes are fixed (the C header and the runtime use them).
   Check (Operation'Enum_Rep (Reset) = 1 and then Operation'Enum_Rep (Summary) = 2
          and then Operation'Enum_Rep (Dump_Local) = 5, "operation codes");
   Check (Outcome_Word_Of (Done) = 1 and then Outcome_Word_Of (Not_In_This_Kernel) = 5
          and then not Is_Outcome_Word (0) and then not Is_Outcome_Word (6), "outcome codes");
   Ada.Text_IO.Put_Line ("trace-object: " & (if Failures = 0 then "PASS" else "FAIL") &
                         Checks'Image & " checks");
end Main;
