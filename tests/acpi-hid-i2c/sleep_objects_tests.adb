pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service_Core;
with ACPI_Sleep_Objects;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
procedure Sleep_Objects_Tests is
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 128,
      Aggregate_Method_Capacity => 4096, Retained_Result_Capacity => 8);
   package Q is new ACPI_Sleep_Objects (C);
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
   use type Q.Query_Outcome;
   use type Q.Support_Indication;
   use type Q.Register_Model;
   use type Q.Sleep_Type;
   use type Q.Sleep_Field;
   Service : aliased C.State (1, 4096, 4096, 1);
   Checks : Natural := 0;
   Report : Q.Query_Report;
   procedure Check (OK : Boolean; Label_Text : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Label_Text; end if;
   end Check;
   function Find (Name : String) return C.Node_Descriptor is
      Node : C.Namespace.Node_ID;
      Item : C.Node_Descriptor;
      Status : C.Values.Access_Status;
   begin
      C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
      Check (Status = C.Values.Available, "find " & Name);
      C.Describe_Node (Service, Node, Item, Status);
      Check (Status = C.Values.Available, "describe " & Name);
      return Item;
   end Find;
   Setter : C.Node_Descriptor;
   procedure Set_Mode (Mode : Natural) is
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
   begin
      C.Invoke_Retained (Service, C.Values.Position (Setter), [0 => AML_Decode.Integer_Value (Mode), others => 0],
        1, 1000, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.No_Return, "set mode");
   end Set_Mode;
   function Number (Name : String) return Natural is
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
   begin
      C.Evaluate_Named (Service, Find (Name), 1000, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned, "counter");
      return Natural (Value.Number);
   end Number;
begin
   Check (Ada.Command_Line.Argument_Count = 1, "fixture path");
   declare
      Input : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Open (Input, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));
      Ada.Streams.Stream_IO.Read (Input, Stream, Last); Ada.Streams.Stream_IO.Close (Input);
      declare
         Data : Firmware_Tables.Bytes (1 .. Natural (Last));
         Stage : C.Stage_Status;
         Freeze : C.Freeze_Status;
         Execution : C.Execution_Report;
         Members : C.Namespace.Initialization_Report;
         Status : C.Values.Access_Status;
      begin
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Stream (Ada.Streams.Stream_Element_Offset (I))); end loop;
         C.Stage_Table (Service, 1, C.DSDT, Data, Stage); Check (Stage = C.Staged, "stage");
         C.Freeze_Catalog (Service, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "freeze");
         C.Execute_Frozen (Service, 1000, Execution, Status);
         Check (Status = C.Values.Available and then Execution.Modules.Outcome = C.Values.Modules_Completed, "modules");
         C.Initialize_Members (Service, Members, Status); Check (Status = C.Values.Available, "members");
      end;
   end;
   Setter := Find ("SETM");
   for Model in Q.Register_Model loop
      for Mode in 0 .. 10 loop
         Set_Mode (Mode);
         Q.Query (Service, Q.S3, Model, (if Mode = 10 then 100 else 1000), Report);
         Check (Report.Present and then Report.Support = Q.Declared_State, "root declaration observed");
         if Mode = 10 then Check (Report.Outcome = Q.Budget_Exhausted, "bounded method even when return unused");
         elsif Model = Q.No_Sleep_Registers then
            Check (Report.Outcome = Q.Return_Ignored and then not Report.Package_Inspected
              and then Report.Charged = Report.AML_Charged, "evaluate then ignore unused return");
         elsif Mode in 8 | 9 then Check (Report.Outcome = Q.Wrong_Result_Type, "package required for used fields");
         elsif Mode = 3 or else (Mode = 2 and then Model = Q.PM1A_And_B) then
            Check (Report.Outcome = Q.Missing_Elements, "used fields must exist");
         elsif Mode = 4 or else (Mode = 5 and then Model = Q.PM1A_And_B) then
            Check (Report.Outcome = Q.Invalid_Type_Value, "no sleep-type truncation");
            if Mode = 5 then Check (Report.Raw_B = 16#100000001#, "full invalid B diagnostics"); end if;
         elsif Mode = 6 or else (Mode = 7 and then Model = Q.PM1A_And_B) then
            Check (Report.Outcome = Q.Wrong_Element_Type, "typed used fields");
         else
            Check (Report.Outcome = Q.Types_Available and then Report.A_Available
              and then Report.B_Available = (Model = Q.PM1A_And_B), "used types available");
            Check (Report.Type_A = (if Mode = 1 then 7 else 1), "type A");
            if Model = Q.PM1A_And_B then Check (Report.Type_B = (if Mode = 1 then 0 else 2), "type B");
            else Check (Report.Type_B = 0, "unused B stays absent"); end if;
         end if;
         if Report.Outcome /= Q.Types_Available then
            Check (not Report.A_Available and then not Report.B_Available
              and then Report.Type_A = 0 and then Report.Type_B = 0, "failure never exposes usable types");
         end if;
         Check (Report.Charged <= 1000 and then C.Retained_Results (Service) = 0, "budget and cleanup");
         Check (Number ("CALL") = 1, "exactly one evaluation even for ignored/invalid returns");
      end loop;
   end loop;
   Q.Query (Service, Q.S5, Q.PM1A_And_B, 1000, Report);
   Check (Report.Outcome = Q.Not_Declared and then Report.Support = Q.Required_State
     and then not Report.Present, "missing S5 is still required");
   Q.Query (Service, Q.S1, Q.PM1A_And_B, 1000, Report);
   Check (Report.Outcome = Q.Not_Declared and then Report.Support = Q.No_Declaration, "missing optional state");
   Q.Query (Service, Q.S0, Q.PM1A_And_B, 1000, Report);
   Check (Report.Outcome = Q.Types_Available and then Report.Type_A = 0 and then Report.Type_B = 0, "zero types valid");
   Q.Query (Service, Q.S2, Q.PM1A_And_B, 1000, Report);
   Check (Report.Outcome = Q.Types_Available and then Report.Type_A = 3 and then Report.Type_B = 4, "aliased package");
   Q.Query (Service, Q.S2, Q.PM1A_And_B, 0, Report);
   Check (Report.Outcome = Q.Budget_Exhausted and then Report.Support = Q.Unknown, "no unsupported inference without observation");
   Set_Mode (0);
   Q.Query (Service, Q.S3, Q.PM1A_And_B, 1000, Report);
   declare AML_Cost : constant Natural := Report.AML_Charged; begin
      Set_Mode (0);
      Q.Query (Service, Q.S3, Q.PM1A_And_B, AML_Cost + 1, Report);
      Check (Report.Outcome = Q.Budget_Exhausted and then Report.Charged = AML_Cost + 1
        and then Report.Failure_Field = Q.Field_B and then C.Retained_Results (Service) = 0,
        "field inspection shares AML budget and cleans partial walk");
   end;
   Ada.Text_IO.Put_Line ("ACPI sleep object checks" & Checks'Image);
end Sleep_Objects_Tests;
