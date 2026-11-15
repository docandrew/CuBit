with ACPI_One_Service;
with Ada.Command_Line;
with ACPI_Native_Instance;
-- The exit code is explicit: the binder's main is a procedure, so a plain
-- return would exit with whatever RAX held. 0 only after a Stop was served
-- and the root retired QUIESCENT (procmgr's graceful replacement reads it
-- as "quiesced, resources released"); otherwise failing. An authenticated
-- retained-region Stop has a separate typed exit disposition: hardware is
-- confirmed quiet, but REG0 was not run and the process namespace is discarded.
procedure Main is
begin
   ACPI_Native_Instance.Start;
   Ada.Command_Line.Set_Exit_Status
     (if ACPI_Native_Instance.Stopped_And_Retired then Ada.Command_Line.Success
      else Ada.Command_Line.Failure);
exception
   when ACPI_One_Service.Region_State_Released_On_Stop =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
end Main;
