------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Ada.Command_Line for CuBit (RM A.15): the arguments come from the launch
--  block the kernel maps read-only at CuBit.Launch_Arguments.Block_Address,
--  validated with the proved CuBit.Launch_Arguments.Validate before first
--  use (docs/process-arguments.md). A process started without a block (or
--  with an invalid one) has no arguments and an empty Command_Name.
--  Set_Exit_Status sets the code the process exits with when its main
--  subprogram returns (userspace/c/crt0.S). The environment is not exposed
--  here.
------------------------------------------------------------------------------
package Ada.Command_Line is
   pragma Preelaborate;

   function Argument_Count return Natural;

   --  Constraint_Error if Number > Argument_Count.
   function Argument (Number : Positive) return String;

   function Command_Name return String;

   type Exit_Status is new Integer;

   Success : constant Exit_Status;
   Failure : constant Exit_Status;

   procedure Set_Exit_Status (Code : Exit_Status);

private
   Success : constant Exit_Status := 0;
   Failure : constant Exit_Status := 1;
end Ada.Command_Line;
