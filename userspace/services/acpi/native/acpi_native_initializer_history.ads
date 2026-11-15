pragma Ada_2022;
with ACPI_Service;
with ACPI_Initializer_History;
with AML_Execute;
with Interfaces;
-- Process-lived copied initializer provenance. Never captures Core/handles,
-- evaluates AML or performs IO. No reset or startup retry operation exists.
package ACPI_Native_Initializer_History is
   package Encoding is new ACPI_Initializer_History
     (Maximum_Invocations => ACPI_Service.Namespace.Node_ID'Last);
   Path_Segments : constant Positive := 32;
   Segment_Characters : constant Positive := 4;
   Path_Characters : constant Positive := Path_Segments * (Segment_Characters + 1);
   subtype Path_Text is String (1 .. Path_Characters);
   type Report is record
      Invocations : Interfaces.Unsigned_32 := 0;
      Ordered_Digest : Interfaces.Unsigned_64 := Encoding.Empty_Digest;
      Complete : Boolean := True;
      Seen : Boolean := False;
      Last_Scope, Last_Method : ACPI_Service.Namespace.Node_ID := ACPI_Service.Namespace.Root;
      Last_Path : Path_Text := [others => ' '];
      Path_Complete : Boolean := False;
      Access_Result : ACPI_Service.Values.Access_Status := ACPI_Service.Values.Wrong_Phase;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
      Reported_Charge : Natural := 0;
   end record;
   -- Copies metadata only, with no AML, allocation, IO or retained handles.
   procedure Copy_Path (Service : ACPI_Service.State; Node : ACPI_Service.Namespace.Node_ID;
      Text : out Path_Text; Complete : out Boolean);
   procedure Note
     (Service : ACPI_Service.State; Scope : ACPI_Service.Namespace.Node_ID; Method : ACPI_Service.Node_Descriptor;
      Access_Result : ACPI_Service.Values.Access_Status; Execution : AML_Execute.Execution_Status;
      Reported_Charge : Natural);
   function Observe return Report;
end ACPI_Native_Initializer_History;
