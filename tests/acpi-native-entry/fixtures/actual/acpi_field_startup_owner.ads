pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Inventory_Protocol;
with ACPI_Field_Inventory_Owner;
with ACPI_Field_Evaluation;
with AML_Field_Access;
generic
   with package Core is new ACPI_Service_Core (<>);
   type Transport_State is limited private;
   type Endpoint is private;
   No_Endpoint : Endpoint;
   with procedure Read_Clock
     (Now : out ACPI_Inventory_Protocol.Wire.Time_Instant; Available : out Boolean);
   with procedure Exchange
     (Transport : in out Transport_State; Destination : Endpoint;
      Deadline : ACPI_Inventory_Protocol.Wire.Time_Instant;
      Item : ACPI_Inventory_Protocol.Wire.Packet;
      Reply : out ACPI_Inventory_Protocol.Wire.Packet;
      Delivered : out ACPI_Inventory_Protocol.Wire.Delivery);
   Maximum_Registers : Positive := AML_Field_Access.Default_Maximum_Registers;
   Maximum_Bindings : Positive := AML_Field_Access.Default_Maximum_Bindings;
   Maximum_Confirmations : Positive := AML_Field_Access.Default_Maximum_Confirmations;
package ACPI_Field_Startup_Owner is
   package Wire renames ACPI_Inventory_Protocol.Wire;
   type Phase is (Fresh, Provisioning, Ready, Failed, Closed);
   type Report is record
      Stage : Phase := Fresh;
      Module_Attempted, Module_Completed, IO_Pending : Boolean := False;
      Registers : Natural := 0;
   end record;
   function Observe return Report;
   -- One package instance is one serialized process lifetime. Destination
   -- comes from the trusted typed startup binding, never a client request.
   -- Deadline is absolute MILLISECONDS in the IPC clock epoch; never cast a
   -- snapshot/microsecond deadline. No refresh, retarget, reset or rearm.
   procedure Begin_Startup
     (Destination : Endpoint; Deadline : Wire.Time_Instant; Accepted : out Boolean);
   -- Closing denies further calls but retains provider/uncertainty state.
   procedure Close_Startup;
   procedure Execute_Module_Startup
     (Service : in out Core.State; Budget : Natural;
      Result : out Core.Execution_Report; Status : out Core.Values.Access_Status);
   -- Callback-compatible trusted entry points. No arena/buffer alias is saved.
   -- Module success precedes these; Initialize_Members precedes first Prepare
   -- or evaluation. The cache never installs a handler or establishes _REG.
   procedure Prepare_Fields (Service : Core.State; Accepted : out Boolean);
   procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status)
     with Pre => Core.Valid (Service) and then not Result'Constrained;
   procedure Invoke_With_Arguments
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status)
     with Pre => Core.Valid (Service) and then not Result'Constrained;
   function Uncertain return AML_Field_Access.Observation;
private
   procedure Read_Window (Now, Deadline : out Wire.Time_Instant);
   package Provider is new ACPI_Field_Inventory_Owner
     (Transport_State, Endpoint, No_Endpoint, Read_Clock, Exchange, Read_Window,
      Maximum_Registers, Maximum_Confirmations);
   package Evaluation is new ACPI_Field_Evaluation (Core, Provider, Maximum_Bindings);
   Stage : Phase := Fresh;
   Until_Time, Last_Sample : Wire.Time_Instant := 0;
   Module_Attempted, Module_Completed : Boolean := False;
   IO : Provider.Context;
   Cache : Evaluation.Context;
end ACPI_Field_Startup_Owner;
