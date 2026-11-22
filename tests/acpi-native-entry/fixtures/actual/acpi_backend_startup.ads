pragma Ada_2022;
with Interfaces;
with ACPI_Backend_Catalog;
with ACPI_Backend_Binding_Types;
with ACPI_Backend_PM_Timer;
with ACPI_FADT;
generic
   with package Catalog is new ACPI_Backend_Catalog (<>);
   with procedure Read_Clock
     (Now : out ACPI_Backend_Binding_Types.Tick; Available : out Boolean);
   Maximum_IO_Windows : Positive := 16;
   Maximum_Memory_Windows : Positive := 16;
package ACPI_Backend_Startup is
   use Interfaces;
   package B renames ACPI_Backend_Binding_Types;
   package W renames Catalog.W;
   package Timer renames ACPI_Backend_PM_Timer;
   subtype IO_Count is Natural range 0 .. Maximum_IO_Windows;
   subtype Memory_Count is Natural range 0 .. Maximum_Memory_Windows;
   subtype Port_Count is Unsigned_32 range 1 .. 65_536;
   type IO_Window is record
      Ports : Catalog.Port_Handle := Catalog.Null_Ports;
      First : Unsigned_16 := 0;
      Count : Port_Count := 1;
   end record;
   type Memory_Window is record
      Memory : Catalog.Memory_Handle := Catalog.Null_Memory;
      First_Page : Unsigned_64 := 0;
      Pages : Unsigned_64 := 1;
   end record;
   type IO_Windows is array (1 .. Maximum_IO_Windows) of IO_Window;
   type Memory_Windows is array (1 .. Maximum_Memory_Windows) of Memory_Window;
   type Grant is record
      IO_Used : IO_Count := 0;
      IO : IO_Windows;
      Memory_Used : Memory_Count := 0;
      Memory : Memory_Windows;
   end record;
   type Outcome is (Not_Attempted, Ready, Already_Consumed, Catalog_Not_Empty,
      Invalid_Grant, Clock_Failed, Binding_Failed, Scope_Failed, Record_Failed, Seal_Failed);
   type Report is record
      Status : Outcome := Not_Attempted;
      Bound_Windows : Natural := 0;
      Failed_Window : W.Window_ID := W.No_Window;
      Binding : B.Report;
      Timer_Status : Timer.Status := Timer.Timer_Unavailable;
      Timer_Block : Timer.R.Status := Timer.R.Absent;
      Timer_Admission : W.Outcome := W.Invalid_Record;
   end record;
   type Context is limited private;
   function Observe (Run : Context) return Report;
   -- Trusted launch only; no wire path invokes this. Copy only used slots
   -- before any clock/handle/map callback. Aliased input avoids an implicit
   -- whole-grant copy of unused runtime fields. Validate the entire sorted,
   -- nonoverlapping batch before binding anything; kernel handle facts must
   -- exactly match descriptions, and Bind rechecks them. No outward rounding.
   -- Caller owns the empty Catalog and a reserved VA arena for its mappings;
   -- retain handles, catalog and reservations after every failure/uncertainty.
   -- Only the read-only FADT PM timer is published in this first native slice.
   -- Ready with no timer is an explicit empty inventory, not device absence.
   procedure Initialize
     (Run : in out Context; Hardware : in out Catalog.Context;
      Input : aliased in Grant; FADT : ACPI_FADT.Description;
      Deadline : B.Tick; Result : out Report);
private
   type Phase is (Fresh, Building, Finished);
   type Context is limited record
      Stage : Phase := Fresh;
      Info : Report;
   end record;
end ACPI_Backend_Startup;
