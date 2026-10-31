pragma Ada_2022;
with Interfaces;
with CuBit.Messages;
with CuBit.ACPI_Windows;
with ACPI_Backend_FADT_Native;
with ACPI_Backend_Binding_Types;
with ACPI_Backend_Kernel_Handles;
with ACPI_Backend_Native_Catalog;
with ACPI_Backend_Startup_Native;
with ACPI_Backend_Server;
with ACPI_Backend_Loop_Native;
with ACPI_Native_Log;
with ACPI_Native_Events;
with ACPI_FADT;
with Startup;
procedure ACPI_Backend_Main is
   use Interfaces;
   package B renames ACPI_Backend_Binding_Types;
   package Tables renames ACPI_Backend_FADT_Native;
   -- Slice1 has no native mappings. Zero VA bounds deliberately disable the
   -- memory binding arena; any future memory slice needs explicit reservation.
   No_Memory_Arena : constant Unsigned_64 := 0;
   Unused_Mapping_Page_Limit : constant Positive := 1;
   Named_Register_Limit : constant Positive := 1;
   Scope_Limit : constant Positive := 1;
   package Native is new ACPI_Backend_Native_Catalog
     (CuBit.ACPI_Windows.Maximum_IO_Windows, Named_Register_Limit, Scope_Limit,
      No_Memory_Arena, No_Memory_Arena, Unused_Mapping_Page_Limit);
   package Catalog renames Native.Owner;
   package Bindings is new ACPI_Backend_Startup_Native (Catalog);
   Hardware : Catalog.Context;
   Bound_Run : Bindings.Owner.Context;
   procedure Event
     (Publisher : CuBit.Messages.Process_ID; Item : CuBit.Messages.Message; Stop : out Boolean) is
      Handled : Boolean;
   begin ACPI_Native_Events.Handle (Publisher, Item, Stop, Handled); end Event;
   procedure Quarantine (Item : Catalog.Statistics) is
   begin
      ACPI_Native_Log.Warning ("acpi-backend: stage hardware fail QUARANTINED pending=" & Natural'Image (Item.Pending));
   end Quarantine;
   package Server is new ACPI_Backend_Server
     (Catalog, ACPI_Backend_Loop_Native.Poll_Completion, CuBit.Messages.Poll_Event,
      CuBit.Messages.Poll_Service_Request, CuBit.Messages.reply,
      CuBit.Messages.Wait_For_Activity_Until, Event, Quarantine);
   Startup_Timeout_Microseconds : constant B.Tick := 10_000_000;
   Inventory_Description_Budget : constant Positive := 4096;
   Register_Service_Role : constant Unsigned_64 := 35; -- agreed acpi-registers catalog role
   use type B.Tick, Tables.Source.Outcome, Bindings.Owner.Outcome, Server.Reason;
   Now, Deadline : B.Tick;
   Available : Boolean;
   Facts : ACPI_FADT.Description;
   Read_Result : Tables.Report;
   Bound : Bindings.Owner.Report;
   Loop_Result : Server.Report;
   Registration_Result : Unsigned_64;
begin
   ACPI_Native_Log.Info ("acpi-backend: stage service-start ok io-only");
   ACPI_Backend_Kernel_Handles.Read_Clock (Now, Available);
   if not Available or else Now >= B.No_Finite_Deadline - Startup_Timeout_Microseconds then
      ACPI_Native_Log.Warning ("acpi-backend: stage startup fail CLOCK_UNAVAILABLE");
      ACPI_Native_Log.Warning ("acpi-backend: bring-up reached service-start (1/5)"); return;
   end if;
   Deadline := Now + Startup_Timeout_Microseconds;
   declare
      Grant : aliased constant CuBit.ACPI_Windows.Grant := Startup.Hardware;
   begin
      if Grant.Memory_Used /= 0 then
         ACPI_Native_Log.Warning ("acpi-backend: stage hardware fail MEMORY_UNSUPPORTED");
         ACPI_Native_Log.Warning ("acpi-backend: bring-up reached service-start (1/5)"); return;
      end if;
      Tables.Read (Startup.Tables, Tables.Source.R.Tick (Deadline), Inventory_Description_Budget, Facts, Read_Result);
      if Read_Result.Source_Result.Status /= Tables.Source.FADT_Ready then
         ACPI_Native_Log.Warning ("acpi-backend: stage fadt-source fail " & Read_Result.Source_Result.Status'Image &
           " copy=" & Read_Result.Source_Result.Copy.Status'Image & " kernel=" & Read_Result.Last_Kernel_Status'Image);
         ACPI_Native_Log.Warning ("acpi-backend: bring-up reached service-start (1/5)"); return;
      end if;
      ACPI_Native_Log.Info ("acpi-backend: stage fadt-source ok rev=" & Unsigned_8'Image (Facts.Revision) &
        " bytes=" & Unsigned_64'Image (Read_Result.Source_Result.Required_Bytes));
      Bindings.Initialize (Bound_Run, Hardware, Grant, Facts, Deadline, Bound);
      if Bound.Status /= Bindings.Owner.Ready then
         ACPI_Native_Log.Warning ("acpi-backend: stage hardware fail " & Bound.Status'Image &
           " binding=" & Bound.Binding.Status'Image & " windows=" & Natural'Image (Bound.Bound_Windows));
         ACPI_Native_Log.Warning ("acpi-backend: bring-up reached fadt-source (2/5)"); return;
      end if;
      ACPI_Native_Log.Info ("acpi-backend: stage hardware ok windows=" & Natural'Image (Bound.Bound_Windows));
      ACPI_Native_Log.Info ("acpi-backend: stage catalog ok named=" & Natural'Image (Catalog.Observe (Hardware).Registers) &
        " timer=" & Bound.Timer_Status'Image & " block=" & Bound.Timer_Block'Image);
      Registration_Result := CuBit.Messages.registerDriver (Register_Service_Role, Startup.Registration);
      if Registration_Result = Unsigned_64'Last then
         ACPI_Native_Log.Warning ("acpi-backend: stage registration fail REFUSED");
         ACPI_Native_Log.Warning ("acpi-backend: bring-up reached catalog (4/5)"); return;
      end if;
      ACPI_Native_Log.Info ("acpi-backend: stage registration ok timer-only");
      ACPI_Native_Log.Info ("acpi-backend: bring-up complete io-only");
      Server.Serve (Hardware, Loop_Result);
      if Loop_Result.Status = Server.Stop_Requested then
         ACPI_Native_Log.Info ("acpi-backend: stage serve ok STOP_REQUESTED");
      else ACPI_Native_Log.Warning ("acpi-backend: stage serve fail " & Loop_Result.Status'Image); end if;
   end;
end ACPI_Backend_Main;
