pragma Ada_2022;
with AML_Field_Access;
with AML_Field_Layout;
with ACPI_Service_Core;
with ACPI_Field_Provider_View;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Provider is new ACPI_Field_Provider_View (<>);
   Maximum_Bindings : Positive := AML_Field_Access.Default_Maximum_Bindings;
   -- Explicit startup-only forwarding; default remains disabled. The Core
   -- validates the live original definition before invoking this callback.
   Enable_Startup_Firmware_Resolution : Boolean := False;
   with procedure Resolve_Startup_Firmware_Target
     (IO : in out Provider.Context; Declaration : Core.Namespace.Operation_Region_Attributes;
      Geometry : AML_Field_Layout.Register_Geometry;
      Target : in out AML_Field_Access.Target; Accepted : in out Boolean) is null;
package ACPI_Field_Module_Startup is
   package Fields is new Core.Field_IO
     (Provider.Context, Provider.Pending, Provider.Describe, Provider.Dispatch,
      Provider.Reconciled, Maximum_Bindings, Provider.Authorize,
      Resolve_Module_Target => Provider.Resolve_Module_Target,
      Begin_Field => Provider.Begin_Field, End_Field => Provider.End_Field,
      Allow_Locked_NVS_Reads => Provider.Supports_Locked_NVS_Reads,
      Enable_Startup_Firmware_Resolution => Enable_Startup_Firmware_Resolution,
      Resolve_Startup_Firmware_Target => Resolve_Startup_Firmware_Target);
   -- Trusted startup callback bridge. Provision Provider before this call;
   -- the outer snapshot/controller owns the one-attempt startup latch.
   -- Never serve or retry after module effects/failure/uncertainty. Preserve
   -- Service, provider and every backend reservation independently.
   procedure Execute_Frozen
     (Service : in out Core.State; IO : in out Provider.Context;
      Budget : Natural; Report : out Core.Execution_Report;
      Status : out Core.Values.Access_Status);
end ACPI_Field_Module_Startup;
