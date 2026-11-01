pragma Ada_2022;
with AML_Field_Access;
with ACPI_Service_Core;
with ACPI_Field_Provider_View;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Provider is new ACPI_Field_Provider_View (<>);
   Maximum_Bindings : Positive := AML_Field_Access.Default_Maximum_Bindings;
package ACPI_Field_Module_Startup is
   package Fields is new Core.Field_IO
     (Provider.Context, Provider.Pending, Provider.Describe, Provider.Dispatch,
      Provider.Reconciled, Maximum_Bindings, Provider.Authorize,
      Resolve_Module_Target => Provider.Resolve_Module_Target,
      Begin_Field => Provider.Begin_Field, End_Field => Provider.End_Field,
      Allow_Locked_NVS_Reads => Provider.Supports_Locked_NVS_Reads);
   -- Trusted startup callback bridge. Provision Provider before this call;
   -- the outer snapshot/controller owns the one-attempt startup latch.
   -- Never serve or retry after module effects/failure/uncertainty. Preserve
   -- Service, provider and every backend reservation independently.
   procedure Execute_Frozen
     (Service : in out Core.State; IO : in out Provider.Context;
      Budget : Natural; Report : out Core.Execution_Report;
      Status : out Core.Values.Access_Status);
end ACPI_Field_Module_Startup;
