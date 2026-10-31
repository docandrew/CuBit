pragma Ada_2022;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with ACPI_Service;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
procedure Retained_Table_Load_Tests is
   package C renames ACPI_Service;
   package V renames C.Values;
   package N renames C.Namespace;
   use type Ada.Streams.Stream_Element_Offset;
   use type Firmware_Tables.Byte;
   use type AML_Decode.Integer_Value, AML_Decode.Bytes;
   use type AML_Execute.Execution_Status;
   use type V.Access_Status, V.Module_Outcome, V.Append_Outcome;
   use type N.Owned.Module_Error_Count;
   use type C.Stage_Status, C.Freeze_Status, C.Execution_Phase_Result, C.Import_Phase;
   use type C.Retained_Table_Phase, C.Retained_Load_Outcome, C.State_Model;
   use type N.Definition_Identity;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   function Read_Table (Argument : Positive) return Firmware_Tables.Bytes is
      package IO renames Ada.Streams.Stream_IO;
      F : IO.File_Type;
   begin
      IO.Open (F, IO.In_File, Ada.Command_Line.Argument (Argument));
      declare
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (F)));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : Firmware_Tables.Bytes (1 .. Raw'Length);
      begin
         IO.Read (F, Raw, Last); IO.Close (F); Check (Last = Raw'Last, "whole copied fixture");
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read_Table;
   Boot : constant Firmware_Tables.Bytes := Read_Table (1);
   Added : constant Firmware_Tables.Bytes := Read_Table (2);
   Mode : constant String := Ada.Command_Line.Argument (3);
   Other_Data : constant Firmware_Tables.Bytes := (if Mode = "distinct" then Read_Table (4) else Added);
   Service, Other : aliased C.State (3, Boot'Length + 2 * Added'Length, Positive'Max (Boot'Length, Added'Length), 3);
   Status : V.Access_Status;
   Report : C.Retained_Load_Report;
   Root, Foreign, Device, Destination, Getter, Method_At : C.Node_Descriptor;
   Counter, Node : N.Node_ID;
   Result : V.Result;
   Held : V.Value_Handle;
   First_Definition, Boot_Definition, Observed_Definition : N.Definition_Identity;
   procedure Counter_Is (Expected : AML_Decode.Integer_Value) is
   begin
      C.Observe_Named_Value (Service, Counter, Result, Status);
      Check (Status = V.Available and then Result.Status = AML_Execute.Returned
        and then Result.Number = Expected, "counter effects never replay");
   end Counter_Is;
   procedure Stage (S : in out C.State) is
      Outcome : C.Stage_Status;
      Frozen : C.Freeze_Status;
   begin
      C.Stage_Table (S, 11, C.DSDT, Boot, Outcome); Check (Outcome = C.Staged, "stage boot");
      C.Stage_Table (S, 22, C.Description, Added, Outcome); Check (Outcome = C.Staged, "stage retained block");
      C.Stage_Table (S, 33, C.Description, Other_Data, Outcome); Check (Outcome = C.Staged, "stage second catalog ID");
      C.Freeze_Catalog (S, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze immutable backing");
   end Stage;
   procedure Execute (S : in out C.State) is
      Execution : C.Execution_Report;
      Initial : N.Initialization_Report;
   begin
      C.Execute_Frozen (S, 10000, Execution, Status);
      Check (Status = V.Available and then Execution.Phase = C.Module_Execution_Reported
        and then Execution.Modules.Outcome = V.Modules_Completed, "boot execution");
      C.Initialize_Members (S, Initial, Status); Check (Status = V.Available, "ready owner");
   end Execute;
   procedure Select_Child (Parent : N.Node_ID; Name : String; Item : out C.Node_Descriptor) is
   begin
      C.Find_Child (Service, Parent, Name, Node, Status);
      Check (Status = V.Available and then Node /= N.Root, "find " & Name);
      C.Describe_Node (Service, Node, Item, Status); Check (Status = V.Available, "describe " & Name);
   end Select_Child;
begin
   Check (C.Retained_Definition (Service, 1) = N.No_Definition and then C.Retained_Definition (Service, 4) = N.No_Definition,
     "no definition for unadmitted/invalid source");
   Check (C.Retained_Load_State (Service, 1) = C.Not_Retained, "no table before admission");
   C.Append_Retained_Module (Service, 2, 10000, Report, Status);
   Check (Status = V.Wrong_Phase and then Report.Outcome = C.Retained_Load_Not_Started, "no append before freeze/execution");
   if Mode = "distinct" then
      Check (Other_Data'Length = Added'Length and then Other_Data /= Added,
        "separately compiled distinct module bytes");
      Check (Other_Data (1 .. Firmware_Tables.Table_Header_Size) = Added (1 .. Firmware_Tables.Table_Header_Size),
        "distinct modules have exact matching headers including checksum");
   end if;
   Stage (Service);
   Check (C.Retained_Load_State (Service, 1) = C.Boot_Managed
     and then C.Retained_Load_State (Service, 2) = C.Retained
     and then C.Retained_Load_State (Service, 4) = C.Not_Retained, "classified source slots");
   C.Append_Retained_Module (Service, 2, 10000, Report, Status);
   Check (Status = V.Wrong_Phase, "frozen is not executed");
   declare Execution : C.Execution_Report; Initial : N.Initialization_Report; begin
      C.Execute_Frozen (Service, 10000, Execution, Status);
      Check (Status = V.Available and then Execution.Modules.Outcome = V.Modules_Completed, "boot only");
      C.Append_Retained_Module (Service, 2, 10000, Report, Status);
      Check (Status = V.Wrong_Phase and then C.Retained_Load_State (Service, 2) = C.Retained,
        "pre-ready refusal does not consume retained block");
      C.Initialize_Members (Service, Initial, Status); Check (Status = V.Available, "seal namespace");
   end;
   C.Describe_Node (Service, N.Root, Root, Status); Check (Status = V.Available, "root identity");
   Select_Child (N.Root, "DEV0", Device); Select_Child (N.Root, "DST0", Destination);
   C.Find_Child (Service, N.Root, "CNT0", Counter, Status); Check (Status = V.Available, "counter"); Counter_Is (0);
   Select_Child (N.Root, "GET0", Getter);
   C.Evaluate_Named (Service, Getter, 10000, Result, Status);
   Check (Status = V.Available and then Result.Status = AML_Execute.Object_Returned, "held buffer"); Held := Result.Handle;
   C.Append_Retained_Module (Service, 2, 0, Report, Status, (V.Selected_Root, Root));
   Check (Status = V.Available and then Report.Append.Outcome = V.Append_Budget_Refused
     and then C.Retained_Load_State (Service, 2) = C.Retained, "zero budget is retryable before effects");
   Check (Report.Definition = N.No_Definition and then C.Retained_Definition (Service, 2) = N.No_Definition,
     "pre-effect refusal leaves no registry identity");
   C.Append_Retained_Module (Service, 1, 10000, Report, Status); Check (Status = V.Wrong_Kind, "boot definition cannot append");
   C.Append_Retained_Module (Service, 4, 10000, Report, Status); Check (Status = V.Invalid_Value, "invalid slot refused");
   C.Append_Retained_Module (Service, 2, 10000, Report, Status, (V.Selected_Root, V.No_Node));
   Check (Status = V.Invalid_Value and then C.Retained_Load_State (Service, 2) = C.Retained, "missing scope not consumed");
   C.Configure_Notifications (Service, True, Status); Check (Status = V.Available, "enable interest");
   C.Append_Retained_Module (Service, 2, 10000, Report, Status);
   Check (Status = V.Invalid_Value and then C.Retained_Load_State (Service, 2) = C.Retained, "notification gate preserves attempt");
   C.Configure_Notifications (Service, False, Status); Check (Status = V.Available, "disable interest");
   Stage (Other); Execute (Other);
   C.Describe_Node (Other, N.Root, Foreign, Status); Check (Status = V.Available, "foreign root");
   C.Append_Retained_Module (Service, 2, 10000, Report, Status, (V.Selected_Root, Foreign));
   Check (Status = V.Invalid_Value and then C.Retained_Load_State (Service, 2) = C.Retained, "foreign scope not consumed");
   C.Append_Retained_Module (Service, 2, 10000, Report, Status, (V.Selected_Root, Device));
   Check (Status = V.Available and then Report.Append.Charged <= 10000 and then C.Retained_Results (Service) = 1,
     "owned append preserves caller pin and budget");
   First_Definition := Report.Definition;
   Check (N.Has_Definition (First_Definition) and then First_Definition = Report.Append.Definition
     and then First_Definition = C.Retained_Definition (Service, 2), "consumed attempt/report/registry share identity");
   declare Before : constant C.State_Model := C.Model (Service) with Ghost; begin
      Observed_Definition := C.Retained_Definition (Service, 2);
      Check (Observed_Definition = First_Definition, "registry read returns persistent identity");
      pragma Assert (C.Model (Service) = Before);
   end;
   Counter_Is (1);
   if Mode in "fail" | "fatal" then
      Check (Report.Outcome = C.Retained_Load_Failed and then C.Retained_Load_State (Service, 2) = C.Failed_Definition
        and then C.Current_Import_Phase (Service) = C.Catalog_Failed, "consumed failure is terminal");
      Check (Report.Append.Failure = (if Mode = "fatal" then AML_Execute.Fatal_Requested else AML_Execute.Division_By_Zero),
        "typed failure retained");
      C.Append_Retained_Module (Service, 2, 10000, Report, Status); Check (Status = V.Wrong_Phase, "same slot cannot replay failure");
      C.Append_Retained_Module (Service, 3, 10000, Report, Status); Check (Status = V.Wrong_Phase, "another ID cannot bypass terminal failure");
      Check (C.Retained_Definition (Service, 2) = First_Definition and then C.Retained_Definition (Service, 3) = N.No_Definition,
        "terminal failure retains diagnostic identity without consuming alias");
      Counter_Is (1);
   elsif Mode = "recover" then
      Check (Report.Outcome = C.Retained_Load_Completed and then C.Retained_Load_State (Service, 2) = C.Loaded_Definition,
        "original Divide fixture now recovers in registered append");
      Check (C.Module_Errors (Service).Count = 1 and then C.Module_Errors (Service).Definition = First_Definition,
        "recovered original fixture retains typed owner diagnostic");
      C.Append_Retained_Module (Service, 2, 10000, Report, Status);
      Check (Status = V.Available and then Report.Outcome = C.Retained_Load_Already_Present and then Report.Existing_Index = 2,
        "recovered source cannot replay");
      C.Append_Retained_Module (Service, 3, 10000, Report, Status, (V.Selected_Root, Destination));
      Check (Status = V.Available and then Report.Outcome = C.Retained_Load_Already_Present and then Report.Existing_Index = 2
        and then C.Retained_Load_State (Service, 3) = C.Retained, "recovered content alias remains unconsumed");
      Counter_Is (1);
      declare Data : AML_Decode.Bytes (1 .. 3); Copied : Natural; begin
         C.Read_Result_Bytes (Service, Held, 0, Data, Copied, Status);
         Check (Status = V.Available and then Copied = 3 and then Data = [1,2,3], "retained buffer survives recovered append");
      end;
      C.Release_Result (Service, Held, Status); Check (Status = V.Available, "release recovered caller pin");
   else
      Check (Report.Outcome = C.Retained_Load_Completed and then C.Retained_Load_State (Service, 2) = C.Loaded_Definition,
        "loaded state retained by service");
      C.Describe_Definition (Service, Device, Boot_Definition, Status);
      Check (Status = V.Available and then N.Has_Definition (Boot_Definition) and then Boot_Definition /= First_Definition
        and then C.Retained_Definition (Service, 1) = Boot_Definition, "boot provenance shares authoritative registry");
      Select_Child (V.Position (Device), "NVAL", Method_At);
      C.Describe_Definition (Service, Method_At, Observed_Definition, Status);
      Check (Status = V.Available and then Observed_Definition = First_Definition, "registry identity matches actual namespace member");
      C.Evaluate_Named (Service, Method_At, 10000, Result, Status);
      Check (Status = V.Available and then Result.Status = AML_Execute.Returned and then Result.Number = 42, "first method placement");
      C.Append_Retained_Module (Service, 2, 10000, Report, Status);
      Check (Status = V.Available and then Report.Outcome = C.Retained_Load_Already_Present
        and then Report.Existing_Index = 2 and then Report.Append.Charged = 0, "same retained table never replays");
      Check (Report.Definition = First_Definition and then Report.Append.Definition = N.No_Definition,
        "same-slot duplicate reports existing owner without another append");
      C.Append_Retained_Module (Service, 3, 10000, Report, Status, (V.Selected_Root, Destination));
      if Mode = "distinct" then
         Check (Status = V.Available and then Report.Outcome = C.Retained_Load_Completed
           and then C.Retained_Load_State (Service, 3) = C.Loaded_Definition, "identical header is not identical definition bytes");
         Check (N.Has_Definition (Report.Definition) and then Report.Definition /= First_Definition
           and then C.Retained_Definition (Service, 3) = Report.Definition, "distinct content gets a separate identity");
         Select_Child (V.Position (Destination), "NVAL", Method_At);
         C.Describe_Definition (Service, Method_At, Observed_Definition, Status);
         Check (Status = V.Available and then Observed_Definition = C.Retained_Definition (Service, 3), "second identity matches namespace member");
         C.Evaluate_Named (Service, Method_At, 10000, Result, Status);
         Check (Status = V.Available and then Result.Status = AML_Execute.Returned and then Result.Number = 43, "distinct module method value");
         Counter_Is (2);
      else
         Check (Status = V.Available and then Report.Outcome = C.Retained_Load_Already_Present
           and then Report.Existing_Index = 2 and then C.Retained_Load_State (Service, 3) = C.Retained,
           "byte-identical block with another ID is detected without consuming it");
         Check (Report.Definition = First_Definition and then C.Retained_Definition (Service, 3) = N.No_Definition,
           "content alias returns canonical identity without consuming its slot");
         Counter_Is (1);
      end if;
      declare Data : AML_Decode.Bytes (1 .. 3); Copied : Natural; begin
         C.Read_Result_Bytes (Service, Held, 0, Data, Copied, Status);
         Check (Status = V.Available and then Copied = 3 and then Data = [1,2,3], "retained buffer survives owner appends");
      end;
      C.Release_Result (Service, Held, Status); Check (Status = V.Available, "release caller pin");
   end if;
   C.Append_Retained_Module (Other, 2, 10000, Report, Status);
   Check (Status = V.Available and then Report.Outcome =
     (if Mode in "fail" | "fatal" then C.Retained_Load_Failed else C.Retained_Load_Completed),
     "separate service owns an independent attempt");
   Check (N.Has_Definition (Report.Definition) and then Report.Definition /= First_Definition
     and then C.Retained_Definition (Other, 2) = Report.Definition, "independent service has independent definition record");
   for I in Added'Range loop
      Check (C.Table_Byte (Service, 2, I - 1) = Added (I), "immutable retained bytes");
   end loop;
   Check (C.Table_Count (Service) = 3 and then C.Valid (Service), "catalog and owner remain valid");
   Ada.Text_IO.Put_Line ("Retained table load checks" & Checks'Image);
end Retained_Table_Load_Tests;
