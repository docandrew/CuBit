--  Ada.Command_Line on CuBit (docs/process-arguments.md): started by
--  spawn-check with arguments; reports on the kernel console and exits
--  with 43 when every argument arrived.
with Ada.Command_Line; use Ada.Command_Line;
with CuBit.Messages; use CuBit.Messages;

procedure Main is
   Expected_Exit : constant := 43;
   OK : constant Boolean :=
     Argument_Count = 3
     and then Command_Name = "ada-args-check.app"
     and then Argument (1) = "alpha"
     and then Argument (2) = "two words"
     and then Argument (3) = "";
begin
   if OK then
      debugPrint ("ada-args-check: Ada.Command_Line PASS" & ASCII.LF);
      Set_Exit_Status (Expected_Exit);
   else
      debugPrint ("ada-args-check: Ada.Command_Line FAIL" & ASCII.LF);
      Set_Exit_Status (Failure);
   end if;
end Main;
