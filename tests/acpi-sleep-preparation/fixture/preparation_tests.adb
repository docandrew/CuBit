pragma Ada_2022;
with ACPI_Service; with ACPI_Hosted_Heap; with ACPI_Sleep_Preparation;
with AML_Decode; with AML_Objects; with AML_Execute; with Firmware_Tables;
with Ada.Command_Line; with Ada.Streams; with Ada.Streams.Stream_IO; with Ada.Text_IO;
pragma Elaborate_All (ACPI_Hosted_Heap);
procedure Preparation_Tests is
   package C renames ACPI_Service;
   package P is new ACPI_Sleep_Preparation (C);
   package IO renames Ada.Streams.Stream_IO;
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status, C.Namespace.Lookup_Status;
   use type AML_Execute.Execution_Status, AML_Decode.Integer_Value;
   use type P.Outcome, P.Sleep_State;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   function Read_Table return Firmware_Tables.Bytes is
      File : IO.File_Type;
   begin
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
      declare
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : Firmware_Tables.Bytes (1 .. Raw'Length);
      begin
         IO.Read (File, Raw, Last); IO.Close (File);
         Check (Natural (Last) = Raw'Length, "complete table read");
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read_Table;
   Table : constant Firmware_Tables.Bytes := Read_Table;
   Mode : constant String := Ada.Command_Line.Argument (2);
   Service : aliased C.State (1, Table'Length, Table'Length, 1,
      Value_Object_Capacity => AML_Objects.Max_Objects, Value_Byte_Capacity => AML_Objects.Max_Bytes,
      Value_Element_Capacity => AML_Objects.Max_Elements);
   Staged : C.Stage_Status; Frozen : C.Freeze_Status;
   Execution : C.Execution_Report; Members : C.Namespace.Initialization_Report;
   Status : C.Values.Access_Status; Report : P.Preparation_Report;
   function Number (Name : String) return AML_Decode.Integer_Value is
      Found : C.Namespace.Lookup_Result; Item : C.Node_Descriptor; Value : C.Values.Result;
   begin
      C.Find_Object (Service, C.Namespace.Root, Name, Found, Status);
      Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found, "copied counter lookup");
      C.Describe_Node (Service, Found.Node, Item, Status);
      Check (Status = C.Values.Available, "counter metadata");
      C.Evaluate_Named (Service, Item, 1000, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned, "counter is integer");
      Check (C.Retained_Results (Service) = 0, "no retained counter result");
      return Value.Number;
   end Number;
   procedure Prepare (Target : P.Sleep_State; Budget : Natural) is
   begin
      P.Prepare (Service, Target, Budget, Report);
      Check (C.Valid (Service) and then Report.Target = Target, "same valid service and target");
      Check (Report.Charged <= Budget, "one caller budget");
      Check (C.Retained_Results (Service) = 0 and then Report.Cleanup_Result = C.Values.Available,
        "all ignored retained results released");
   end Prepare;
begin
   C.Stage_Table (Service, 1, C.DSDT, Table, Staged);
   Check (Staged = C.Staged, "stage fixture");
   C.Freeze_Catalog (Service, Frozen);
   Check (Frozen = C.Catalog_Frozen_OK, "freeze fixture");
   C.Execute_Frozen (Service, 10000, Execution, Status);
   Check (Status = C.Values.Available, "module execution");
   C.Initialize_Members (Service, Members, Status);
   Check (Status = C.Values.Available, "initialize members");
   Prepare (P.S5, 0);
   Check (Report.Status = P.Budget_Exhausted and then not Report.Present and then not Report.Invoked,
     "zero budget never invokes method");
   Check (Number ("CALL") = 0, "preparation is not an autonomous startup action");
   if Mode = "normal" then
      for Target in P.Sleep_State loop
         Prepare (Target, 1000);
         Check (Report.Status = P.Method_Completed and then Report.Invoked and then Report.Present,
           "normal root method completed");
         Check (Report.Execution = AML_Execute.No_Return, "no return is normal for PTS");
         Check (Number ("SEEN") = AML_Decode.Integer_Value (P.Sleep_State'Enum_Rep (Target)), "exact integer state argument");
         Check (Number ("CALL") = AML_Decode.Integer_Value (P.Sleep_State'Enum_Rep (Target)), "one invocation per request");
      end loop;
   elsif Mode in "absent" | "wrong-kind" then
      Prepare (P.S5, 1000);
      Check (not Report.Invoked and then Report.Charged = 0, "absent or wrong object never invoked");
      Check (Report.Status = (if Mode = "absent" then P.Not_Declared else P.Wrong_Object_Type), "typed missing or invalid root method");
      Check (Number ("CALL") = 0, "never invoke same-named nested method");
   else
      Prepare (P.S5, (if Mode = "budget" then 32 else 1000));
      Check (Report.Invoked and then Report.Present, "single bounded invocation attempted");
      if Mode = "failure" then
         Check (Report.Status = P.Invocation_Failed and then Report.Execution = AML_Execute.Division_By_Zero,
           "method failure is explicit");
      elsif Mode = "budget" then
         Check (Report.Status = P.Budget_Exhausted and then Report.Execution = AML_Execute.Budget_Exceeded,
           "infinite AML yields bounded budget result");
      else
         Check (Report.Status = P.Method_Completed, "normal ignored result is cleaned");
         if Mode = "package" then Check (Report.Execution = AML_Execute.Object_Returned, "retained package witness"); end if;
      end if;
      Check (Number ("CALL") = 1, "no internal retry or rollback of prior effects");
      Check (Number ("SEEN") = 5, "S5 argument reaches actual AML");
   end if;
   Ada.Text_IO.Put_Line ("Preparation " & Mode & " checks" & Checks'Image);
end Preparation_Tests;
