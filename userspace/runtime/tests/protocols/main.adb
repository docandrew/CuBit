with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with CuBit.Protocols; use CuBit.Protocols;

procedure Main is
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         Put_Line ("PASS " & Name);
      else
         Put_Line ("FAIL " & Name);
         Failures := Failures + 1;
      end if;
   end Check;

   Expected : constant Schema_Contract := TEXT_LINE_CONTRACT;
   Candidate : Schema_Contract;
begin
   Check (Valid (Expected), "accept bounded text schema");
   Check (Compatible (Expected, Expected), "accept identical stream schema");

   Candidate := Expected;
   Candidate.Identity := Candidate.Identity + 1;
   Check
     (not Compatible (Expected, Candidate), "reject stream schema identity");

   Candidate := Expected;
   Candidate.Version := Candidate.Version + 1;
   Check
     (not Compatible (Expected, Candidate), "reject stream schema version");

   Candidate := Expected;
   Candidate.Sizing := Fixed_Size;
   Check
     (not Compatible (Expected, Candidate), "reject stream sizing mode");

   Candidate := Expected;
   Candidate.Wire_Size := 42;
   Check
     (not Compatible (Expected, Candidate), "reject stream size bound");

   Check
     (Valid (CCL_TEST_INCREMENT), "accept shared CCL IPC contract");

   if Failures /= 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
