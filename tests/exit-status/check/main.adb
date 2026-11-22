------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  exit-status-check (tests/exit-status, headless test exit-status;
--  LIVE-002): the regression for a returning main's exit status. It prints
--  its PID and how it will end, then:
--    Returns       returns from Main without setting a status: exit code 0
--                  (Ada.Command_Line's Success). Before the fix the binder's
--                  main was a procedure and the code was whatever RAX held,
--                  so this path leaves a nonzero value in RAX on purpose;
--    Sets_Failure  sets Ada.Command_Line.Failure, then returns: exit code 1;
--    Raises        raises an exception nothing handles: the last-chance
--                  handler's CuBit.Program_Endings.Unhandled_Exception_Status
--                  (134), unchanged by the fix.
--  The headless test pairs each PID with procmgr's "exited code N" line.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Console_Route;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Process_IDs.Text;
with Parameters;
with Startup;
pragma Unreferenced (Startup);

procedure Main is
   use Parameters;
   P : constant Exit_Status_Parameters_Value := Value;

   --  A value the last call before the return leaves in RAX.
   Leftover_Value : constant Unsigned_64 := 16#5A#;
   function Leftover return Unsigned_64 with No_Inline;
   function Leftover return Unsigned_64 is (Leftover_Value);
   Sink : Unsigned_64 with Volatile;

   --  As procmgr prints it ("pid N").
   Pid : constant String :=
     CuBit.Process_IDs.Text.Image (CuBit.Process_IDs.From_Word (Own_Process_Word));
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   debugPrint ("exit-status-check: pid " & Pid & " ends " &
               (case P.ending is
                   when Returns => "Returns",
                   when Sets_Failure => "Sets_Failure",
                   when Raises => "Raises") & ASCII.LF);
   case P.ending is
      when Returns =>
         Sink := Leftover;
      when Sets_Failure =>
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      when Raises =>
         raise Program_Error with "exit-status-check: Raises";
   end case;
end Main;
