pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Local_Module_Startup;
with CuBit.ACPI_Windows;
with AML_Field_Access;
with ACPI_FADT;
with ACPI_Local_Hardware.Pooled_Fields;
with AML_Execute;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Fields is new Hardware.Pooled_Fields
     (Core => Core,Maps => Regions.Cache,Memory => Memory,others => <>);
package ACPI_Local_Memory.Modules is
   procedure Configure (Input : CuBit.ACPI_Windows.Grant;
      Hardware_Deadline_Microseconds : Hardware.B.Tick;
      Pool : Pool_Configuration; PCI : PCI_Configuration;
      Symbol : AML_Field_Access.Endpoint_ID; Accepted : out Boolean);
   -- Captures typed startup inputs once; no hardware/IPC/table reads yet.
   -- Root calls this before the loader; the loader callback below then
   -- initializes Hardware -> Memory -> Fields from the SAME copied FADT.
   procedure Initialize_Local (Facts : ACPI_FADT.Description; Accepted : out Boolean);
   procedure Close_Local;
   -- Trusted implementation callbacks for the instance, never endpoints.
   package Startup is new ACPI_Local_Module_Startup
     (Core,Initialize_Local,Fields.Execute_Module_Startup,Close_Local);
   function Observe return Startup.Report renames Startup.Observe;
   procedure Execute_Module_Startup (Service : in out Core.State; Budget : Natural;
      Result : out Core.Execution_Report; Status : out Core.Values.Access_Status)
      renames Startup.Execute_Module_Startup;
   procedure Prepare_Fields (Service : Core.State; Accepted : out Boolean)
      renames Fields.Prepare_Fields;
   procedure Evaluate_Named (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result; Status : out Core.Values.Access_Status)
      renames Fields.Evaluate_Named;
   procedure Invoke_Retained (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result; Status : out Core.Values.Access_Status)
      renames Fields.Invoke_Retained;
   procedure Invoke_With_Arguments (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result; Status : out Core.Values.Access_Status)
      renames Fields.Invoke_With_Arguments;
   procedure Finish_Startup (Accepted : out Boolean) renames Fields.Finish_Startup;
   procedure Cleanup (Completed : out Boolean) renames Fields.Cleanup;
   -- These are the SAME retained provider operations, without another cache,
   -- execution owner or selection/fallback state. Finish startup only after
   -- device initialization; runtime queries continue afterward. Root closes
   -- this package via Close_Local and drives hardware maintenance separately.
private
   Configured, Consumed : Boolean := False;
   Saved_Grant : aliased CuBit.ACPI_Windows.Grant;
   Saved_Deadline : Hardware.B.Tick := 0;
   Saved_Pool : Pool_Configuration;
   Saved_PCI : PCI_Configuration;
   Saved_Symbol : AML_Field_Access.Endpoint_ID := AML_Field_Access.No_Endpoint;
end ACPI_Local_Memory.Modules;
