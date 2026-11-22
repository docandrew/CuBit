pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Field_Provider_View;
with ACPI_Field_Module_Startup;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
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
package ACPI_Field_Bindings is
   package Startup is new ACPI_Field_Module_Startup (Core, Provider, Maximum_Bindings, Enable_Startup_Firmware_Resolution,
      Resolve_Startup_Firmware_Target);
   package Fields renames Startup.Fields;
   type Outcome is (Not_Attempted, Built, Built_With_Gaps, Already_Consumed,
                    Provider_Unavailable, Pending_IO, Namespace_Unavailable, Binding_Refused);
   type Report is record
      Status : Outcome := Not_Attempted;
      Region_Entries, Direct_Fields, Read_Fields, Write_Fields : Natural := 0;
      Missing_Fields, Unsupported_Fields, Unkeyed_Fields, Direction_Conflicts : Natural := 0;
      Failed_Node : Core.Namespace.Node_ID := Core.Namespace.Root;
      Failure : Core.Values.Access_Status := Core.Values.Available;
   end record;
   type Context is limited private;
   function Observe (Run : Context) return Report;
   -- One attempt after module execution and Initialize_Members, before root/
   -- _SB._INI. No AML or IPC is executed here. Targets come exclusively from
   -- the independently provisioned provider; metadata cannot create authority.
   -- Publish only the completed set. Expected missing/unsupported fields yield
   -- Built_With_Gaps; structural/quota failures expose no partial set.
   -- This does not install controllers, connect regions, run _REG or establish
   -- complete region/field-graph coverage. The region barrier remains separate.
   procedure Build
     (Run : in out Context; Service : Core.State; IO : in out Provider.Context; Result : out Report);
   -- Copied bindings are metadata. Check the original service/provider context
   -- before exporting; all normal Field_IO liveness/rights/uncertainty checks
   -- remain mandatory on every use. No refresh/reset/rearm exists.
   procedure Copy_Bindings
     (Run : Context; Service : Core.State; IO : Provider.Context;
      Items : out Fields.Binding_Set; Accepted : out Boolean);
private
   type Phase is (Fresh, Building, Published, Failed);
   type Context is limited record
      Stage : Phase := Fresh;
      Info : Report;
      Root : Core.Node_Descriptor := Core.Values.No_Node;
      Provider_ID : AML_Identity.Identity := AML_Identity.No_Identity;
      Items : Fields.Binding_Set;
   end record;
end ACPI_Field_Bindings;
