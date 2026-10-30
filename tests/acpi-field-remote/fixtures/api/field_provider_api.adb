pragma Ada_2022;
with ACPI_Field_Native_Provider;
with ACPI_Region_Protocol;
with ACPI_Service_Core;
with AML_Delays;
with AML_Execute;
with ACPI_Region_Initialization;
procedure Field_Provider_API is
   procedure Window (Now, Deadline : out ACPI_Region_Protocol.Time_Instant) is
   begin Now := 10; Deadline := 99; end Window;
   package Native is new ACPI_Field_Native_Provider (Window);
   package P renames Native.Provider;
   package Service is new ACPI_Service_Core (AML_Delays.Unavailable_Provider);
   package Fields is new Service.Field_IO
     (P.Context, P.Pending, P.Describe, P.Dispatch, P.Reconciled,
      Authorize_Target => P.Authorize);
   type Controller is limited record
      IO : P.Context;
      Bindings : Fields.Binding_Set;
   end record;
   function Ready (Context : Controller; Region : Service.Node_Descriptor;
      Data : Service.Namespace.Operation_Region_Attributes) return Boolean is
      pragma Unreferenced (Context, Region, Data);
   begin return False; end Ready;
   function PCI_Root (Context : Controller; Region : Service.Node_Descriptor) return Boolean is
      pragma Unreferenced (Context, Region);
   begin return False; end PCI_Root;
   procedure Invoke
     (State : aliased in out Service.State; Context : in out Controller;
      Node : Service.Namespace.Node_ID; Args : AML_Execute.Arguments;
      Argument_Count : Natural; Budget : Natural; Result : out Service.Values.Result;
      Status : out Service.Values.Access_Status) is
   begin
      Fields.Invoke_Retained (State,Context.IO,Context.Bindings,Node,Args,
         Argument_Count,Budget,Result,Status);
   end Invoke;
   package Connections is new ACPI_Region_Initialization (Service,Controller,Ready,PCI_Root,Invoke);
   pragma Unreferenced (Connections);
begin
   null;
end Field_Provider_API;
