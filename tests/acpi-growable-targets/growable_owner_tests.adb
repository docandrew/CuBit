pragma Ada_2022;
with AML_Objects;
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Firmware_Tables;
with Locked_Test_Core;
with Locked_Test_Owner;
with Locked_Owner_Model;
with ACPI_Target_Heap_Test_Control;
with ACPI_Capacity_Diagnostics;
with ACPI_Startup_Diagnostics;
with ACPI_Bootstrap;
procedure Growable_Owner_Tests is
   package C renames Locked_Test_Core;
   package Owner renames Locked_Test_Owner;
   package Model renames Locked_Owner_Model;
   package D renames ACPI_Capacity_Diagnostics;
   Service : C.State (1, 4_096, 4_096, 1, Value_Object_Capacity => AML_Objects.Max_Objects, Value_Byte_Capacity => AML_Objects.Max_Bytes, Value_Element_Capacity => AML_Objects.Max_Elements);
   Stage : C.Stage_Status;
   Freeze : C.Freeze_Status;
   Status : C.Values.Access_Status;
   Report : C.Execution_Report;
   Good : Boolean;
   File : Ada.Streams.Stream_IO.File_Type;
   Checks : Natural := 0;
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status, C.Values.Module_Outcome;
   use type Ada.Streams.Stream_Element_Offset, D.Entry_Count;
   procedure Check (Value : Boolean) is
   begin Checks := Checks + 1; if not Value then raise Program_Error with Checks'Image; end if; end Check;
begin
   if Model.Mode = 14 then ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := 2; end if;
   Model.Configure;
   Owner.Begin_Startup (Model.Destination, 1_000, Good); Check (Good);
   Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (2));
   declare
      Size : constant Positive := Positive (Ada.Streams.Stream_IO.Size (File));
      Bytes : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Size));
      Last : Ada.Streams.Stream_Element_Offset;
      Data : Firmware_Tables.Bytes (1 .. Size);
   begin
      Ada.Streams.Stream_IO.Read (File, Bytes, Last); Check (Last = Bytes'Last);
      Ada.Streams.Stream_IO.Close (File);
      for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Bytes (Ada.Streams.Stream_Element_Offset (I))); end loop;
      C.Stage_Table (Service, 1, C.DSDT, Data, Stage); Check (Stage = C.Staged);
   end;
   C.Freeze_Catalog (Service, Freeze); Check (Freeze = C.Catalog_Frozen_OK);
   Owner.Execute_Module_Startup (Service, 10_000, Report, Status); Check (Status = C.Values.Available);
   if Model.Mode = 13 then
      Check (Report.Modules.Outcome = C.Values.Modules_Completed);
      Check (not D.Present (Owner.Observe.Capacity));
      Check (Model.Loads = 33 and then Model.Acquires = 33 and then Model.Releases = 33);
   else
      Check (Model.Mode = 14 and then Report.Modules.Outcome = C.Values.Module_Rejected);
      Check (Report.Modules.Field_Failure.Present and then Report.Modules.Field_Failure.Name = "F032");
      Check (D.Present (Owner.Observe.Capacity) and then Owner.Observe.Capacity.Demand = 33
        and then Owner.Observe.Capacity.Capacity = 32);
      Check (Model.Loads = 32 and then Model.Acquires = 32 and then Model.Releases = 32);
      declare
         Finished : constant ACPI_Bootstrap.Finish_Report :=
           (Stage => ACPI_Bootstrap.Execution_Failed, Execution => Report, others => <>);
      begin Ada.Text_IO.Put_Line (ACPI_Startup_Diagnostics.Format_Field_Failure (Finished, Owner.Observe.Capacity)); end;
   end if;
   Owner.Close_Startup;
   Check (Model.Acquires = Model.Releases);
   Ada.Text_IO.Put_Line ("PASS growable owner checks=" & Checks'Image);
end Growable_Owner_Tests;
