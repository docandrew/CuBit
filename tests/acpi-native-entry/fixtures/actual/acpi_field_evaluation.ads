pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Field_Inventory_Owner;
with ACPI_Field_Bindings;
with AML_Field_Access;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Provider is new ACPI_Field_Inventory_Owner (<>);
   Maximum_Bindings : Positive := AML_Field_Access.Default_Maximum_Bindings;
package ACPI_Field_Evaluation is
   package Bindings is new ACPI_Field_Bindings (Core, Provider, Maximum_Bindings);
   type Context is limited private;
   function Observe (Run : Context) return Bindings.Report;
   -- Adapter for the trusted Device_Initialization evaluator callback. Build
   -- the metadata cache once, after Initialize_Members and before the first
   -- root/_SB._INI. Callers with no such method may Prepare at their region
   -- barrier. This never establishes region readiness or runs _REG.
   -- IO and Service remain caller-owned; no alias or buffer is retained.
   procedure Prepare
     (Run : in out Context; Service : Core.State; IO : Provider.Context;
      Accepted : out Boolean);
   procedure Evaluate_Named
     (Run : in out Context; Service : aliased in out Core.State;
      IO : in out Provider.Context; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status)
     with Pre => Core.Valid (Service) and then not Result'Constrained;
   -- Arguments borrow this exact arena's retained values. The underlying
   -- evaluator owns result retention and cleanup; this adapter copies none.
   procedure Invoke_With_Arguments
     (Run : in out Context; Service : aliased in out Core.State;
      IO : in out Provider.Context; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status)
     with Pre => Core.Valid (Service) and then not Result'Constrained;
private
   type Context is limited record
      Cache : Bindings.Context;
   end record;
end ACPI_Field_Evaluation;
