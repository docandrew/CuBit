pragma Ada_2022;
with Ada.Exceptions;
with Ada.Text_IO;
with ACPI_Service_Core;
with AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Names;
with Metadata_Return_Fixture;
procedure Metadata_Return_Tests is
   procedure Reject_Delay (Item : AML_Delays.Request; Result : out AML_Delays.Outcome) is
      pragma Unreferenced (Item);
   begin
      Result := AML_Delays.Failed;
   end Reject_Delay;
   package C is new ACPI_Service_Core
     (Perform_Delay => Reject_Delay,
      Namespace_Node_Capacity => 64, Aggregate_Method_Capacity => 2048,
      Retained_Result_Capacity => 4);
   package N renames C.Namespace;
   package V renames C.Values;
   use type C.Install_Status, V.Access_Status, V.Value_Handle, V.Value_Kind;
   use type AML_Execute.Execution_Status, AML_Execute.Named_Metadata_Type;
   use type AML_Decode.Integer_Value;
   Service : aliased C.State
     (1, Metadata_Return_Fixture.Data'Length, Metadata_Return_Fixture.Data'Length,
      1, 256, 4096, 128);
   Installed : C.Install_Status;
   Initialized : N.Initialization_Report;
   Access_Result : V.Access_Status;
   Checks : Natural := 0;
   Current_Case : AML_Names.Segment := "LOAD";
   procedure Check (Condition : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Message; end if;
   end Check;
   function Find (Part : AML_Names.Segment) return N.Node_ID is
      Node : N.Node_ID;
      Status : V.Access_Status;
   begin
      C.Find_Child (Service, N.Root, Part, Node, Status);
      Check (Status = V.Available and then Node /= N.Root, "method lookup " & Part);
      return Node;
   end Find;
   procedure Scalar (Part : AML_Names.Segment; Expected : AML_Decode.Integer_Value) is
      Result : AML_Execute.Execution_Result;
      Node : N.Node_ID;
   begin
      Current_Case := Part;
      Node := Find (Part);
      Ada.Text_IO.Put_Line ("BEGIN " & Part);
      C.Invoke_Scalar (Service, Node, [others => 0], 0, 50_000, Result);
      Ada.Text_IO.Put_Line ("CALL " & Part & " status=" & Result.Status'Image);
      Check (Result.Status = AML_Execute.Returned, "nested call status " & Part);
      Check (Result.Value = Expected, "nested call value " & Part);
      Check (Natural (C.Retained_Results (Service)) = 0, "retained scalar result " & Part);
   end Scalar;
   procedure Metadata (Part : AML_Names.Segment; Kind : AML_Execute.Named_Metadata_Type) is
      Result : V.Result;
      Description : V.Value_Description;
      Status : V.Access_Status;
      Handle : V.Value_Handle;
   begin
      Current_Case := Part;
      C.Invoke_Retained (Service, Find (Part), [others => 0], 0, 50_000, Result, Status);
      Check (Status = V.Available and then Result.Status = AML_Execute.Metadata_Returned,
             "root metadata result " & Part);
      Handle := Result.Handle;
      Check (Handle /= V.No_Value and then Natural (C.Retained_Results (Service)) = 1,
             "root metadata retention " & Part);
      C.Describe_Result (Service, Handle, Description, Status);
      Check (Status = V.Available and then Description.Kind = V.Metadata_Description
        and then Description.Object_Type = Kind, "root metadata description " & Part);
      C.Release_Result (Service, Handle, Status);
      Check (Status = V.Available and then Handle = V.No_Value
        and then Natural (C.Retained_Results (Service)) = 0, "root metadata release " & Part);
   end Metadata;
begin
   C.Install (Service, 1, C.DSDT, Metadata_Return_Fixture.Data, Installed);
   Check (Installed = C.Installed, "install immutable fixture");
   C.Initialize_Members (Service, Initialized, Access_Result);
   Check (Access_Result = V.Available and then Initialized.Missing = 0
     and then Initialized.Unsupported = 0, "initialize immutable fixture");
   Scalar ("CINT", 16#61#);
   Scalar ("CBUF", 2);
   Metadata ("DRET", AML_Execute.Device_Metadata);
   Metadata ("TRET", AML_Execute.Thermal_Metadata);
   Scalar ("TD00", 16#51#);
   Scalar ("TT00", 16#52#);
   Scalar ("TD01", 16#53#);
   Scalar ("TT01", 16#54#);
   Scalar ("TD02", 6);
   Scalar ("TT02", 13);
   Scalar ("TD03", 6);
   Scalar ("TT03", 13);
   declare Result : AML_Execute.Execution_Result; begin
      Current_Case := "ARIT";
      C.Invoke_Scalar (Service, Find ("TD00"), [others => 0], 1, 50_000, Result);
      Check (Result.Status = AML_Execute.Argument_Mismatch, "ordinary method arity preserved");
      Check (Natural (C.Retained_Results (Service)) = 0, "arity refusal retained result");
   end;
   Ada.Text_IO.Put_Line ("METADATA RETURN: PASS" & Checks'Image);
exception
   when Error : others =>
      Ada.Text_IO.Put_Line ("METADATA_RETURN_FAILURE case=" & Current_Case);
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
      raise;
end Metadata_Return_Tests;
