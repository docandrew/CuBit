------------------------------------------------------------------------------
--  Hosted check of the process-list object's request decision
--  (Process_List_ABI.Refusal, Entries_In): every right, label and length.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Process_List_ABI; use Process_List_ABI;

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
begin
   for Readable in Boolean loop
      for Label in Unsigned_64 range 0 .. 4 loop
         for Length in Unsigned_64 range 0 .. 4 loop
            declare
               R : constant Outcome := Refusal (Readable, Label, Length);
            begin
               if not Readable then
                  Check (R = Not_Permitted, "no Read refused first");
               elsif Label /= 1 then
                  Check (R = Unknown_Operation, "unknown label" & Label'Image);
               elsif Length /= 2 then
                  Check (R = Malformed, "length" & Length'Image);
               else
                  Check (R = Listed, "a list request");
               end if;
            end;
         end loop;
      end loop;
   end loop;
   --  Whole entries only: a buffer smaller than one entry gets none.
   Check (Entries_In (0) = 0 and then Entries_In (Entry_Bytes - 1) = 0
          and then Entries_In (Entry_Bytes) = 1 and then Entries_In (8_192) = 256, "entries in a buffer");
   Check (Outcome_Word_Of (Listed) = 1 and then Outcome_Word_Of (Destination_Unwritable) = 5
          and then not Is_Outcome_Word (0) and then not Is_Outcome_Word (6), "outcome codes");
   Ada.Text_IO.Put_Line ("process-list-object: " & (if Failures = 0 then "PASS" else "FAIL") &
                         Checks'Image & " checks");
end Main;
