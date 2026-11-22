with ACPI_Service;
with AML_Execute;
with AML_Decode;
with AML_Names;
package ACPI_Test_Results is
   type Holder is limited private;
   procedure Drop (Service : in out ACPI_Service.State; Held : in out Holder);
   procedure Invoke
     (Service : aliased in out ACPI_Service.State; Held : in out Holder;
      Node : ACPI_Service.Namespace.Node_ID; Args : AML_Execute.Arguments;
      Count, Budget : Natural; Result : out ACPI_Service.Values.Result);
   procedure Seal (Service : in out ACPI_Service.State);
   function Child (Service : ACPI_Service.State; Parent : ACPI_Service.Namespace.Node_ID;
      Part : AML_Names.Segment) return ACPI_Service.Namespace.Node_ID;
   function Integer_Data (Service : in out ACPI_Service.State;
      Node : ACPI_Service.Namespace.Node_ID) return AML_Decode.Integer_Value;
   function Describe (Service : ACPI_Service.State; Handle : ACPI_Service.Values.Value_Handle)
      return ACPI_Service.Values.Value_Description;
   function Bytes (Service : ACPI_Service.State; Handle : ACPI_Service.Values.Value_Handle)
      return AML_Decode.Bytes;
   procedure Emit (Service : in out ACPI_Service.State;
      Handle : ACPI_Service.Values.Value_Handle; Depth : Natural := 0);
   procedure Emit_Named (Service : in out ACPI_Service.State; Node : ACPI_Service.Namespace.Node_ID);
private
   type Holder is limited record
      Root : ACPI_Service.Values.Value_Handle := ACPI_Service.Values.No_Value;
   end record;
end ACPI_Test_Results;
