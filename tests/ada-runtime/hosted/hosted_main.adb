--  ada-runtime-check's Main on Linux (the native one prints through
--  CuBit.Messages).
with Ada.Command_Line;
with Ada.Text_IO;
with Check_Results; use Check_Results;
with Exception_Checks;
with Finalization_Checks;
with Pool_Checks;
with Tagged_Checks;

procedure Hosted_Main is
begin
   Tagged_Checks.Run;
   Finalization_Checks.Run;
   Pool_Checks.Run;
   Exception_Checks.Run;
   if Failures = 0 then
      Ada.Text_IO.Put_Line ("TEST: PASS ada-runtime (hosted)");
   else
      Ada.Text_IO.Put_Line ("TEST: FAIL ada-runtime (hosted)");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Hosted_Main;
