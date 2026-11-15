with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces;
with AML_Objects;
with ACPI_Service_Core;
with ACPI_Battery_Information;
with ACPI_Battery_Information_Reader;
with ACPI_Battery_Legacy_Information;
with ACPI_Battery_Legacy_Information_Reader;
with ACPI_Battery_Information_Selection;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
with ACPI_Battery;
with ACPI_Battery_Observation;
with ACPI_Battery_Cache;
with ACPI_Battery_Cache.Observers;
with ACPI_Power_Query_Protocol;
procedure Collector_Pages_Tests is
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 128,
      Aggregate_Method_Capacity => 8192, Retained_Result_Capacity => 8);
   package F is new ACPI_Battery_Information (8);
   package G is new ACPI_Battery_Legacy_Information (16);
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status;
   use type C.Values.Module_Outcome, AML_Execute.Execution_Status;
   Service, Other : aliased C.State (1, 4096, 4096, 1,
     Value_Object_Capacity => AML_Objects.Max_Objects,
     Value_Byte_Capacity => AML_Objects.Max_Bytes,
     Value_Element_Capacity => AML_Objects.Max_Elements);
   Checks, Extended_Calls, Legacy_Calls : Natural := 0;
   type Fault is (Normal, Overcharge, Unknown_Field);
   Extended_Fault, Legacy_Fault : Fault := Normal;
   procedure Check (OK : Boolean; Label_Text : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Label_Text; end if; end Check;
   procedure Evaluate (State : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Value : out C.Values.Result; Status : out C.Values.Access_Status;
      Mode : Fault) is
   begin
      if Mode = Unknown_Field then
         Value := (Status => AML_Execute.Field_Indeterminate, Charged => 1);
         Status := C.Values.Available; return;
      end if;
      C.Evaluate_Named (State, Item, Budget, Value, Status);
      if Mode = Overcharge then Value.Charged := Budget + 1; end if;
   end Evaluate;
   procedure Extended_Evaluate (State : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Value : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin Extended_Calls := Extended_Calls + 1; Evaluate (State, Item, Budget, Value, Status, Extended_Fault); end Extended_Evaluate;
   procedure Legacy_Evaluate (State : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Value : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin Legacy_Calls := Legacy_Calls + 1; Evaluate (State, Item, Budget, Value, Status, Legacy_Fault); end Legacy_Evaluate;
   package E is new ACPI_Battery_Information_Reader (C, F, Extended_Evaluate);
   package L is new ACPI_Battery_Legacy_Information_Reader (C, G, Legacy_Evaluate);
   package S is new ACPI_Battery_Information_Selection (C, F, G, E, L);
   use type S.Source_Kind;

   Extended, Legacy, Missing, Foreign, Setter : C.Node_Descriptor;
   function Find (State : C.State; Name : String) return C.Node_Descriptor is
      Node : C.Namespace.Node_ID; Item : C.Node_Descriptor; Status : C.Values.Access_Status;
   begin
      C.Find_Child (State, C.Namespace.Root, Name, Node, Status);
      Check (Status = C.Values.Available, "find " & Name);
      C.Describe_Node (State, Node, Item, Status);
      Check (Status = C.Values.Available, "describe " & Name); return Item;
   end Find;
   procedure Prepare (State : aliased in out C.State; Data : Firmware_Tables.Bytes) is
      Stage : C.Stage_Status; Freeze : C.Freeze_Status; Execution : C.Execution_Report;
      Members : C.Namespace.Initialization_Report; Status : C.Values.Access_Status;
   begin
      C.Stage_Table (State, 1, C.DSDT, Data, Stage); Check (Stage = C.Staged, "stage");
      C.Freeze_Catalog (State, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (State, 1000, Execution, Status);
      Check (Status = C.Values.Available and then Execution.Modules.Outcome = C.Values.Modules_Completed, "modules");
      C.Initialize_Members (State, Members, Status); Check (Status = C.Values.Available, "members");
   end Prepare;
   procedure Mode (Number : Natural) is
      Value : C.Values.Result; Status : C.Values.Access_Status;
   begin
      C.Invoke_Retained (Service, C.Values.Position (Setter),
        [0 => AML_Decode.Integer_Value (Number), others => 0], 1, 1000, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.No_Return, "mode setter");
   end Mode;
   Dynamic_Calls : Natural := 0;
   procedure Dynamic_Evaluate (State : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Value : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin
      Dynamic_Calls := Dynamic_Calls + 1;
      C.Evaluate_Named (State, Item, Budget, Value, Status);
   end Dynamic_Evaluate;
   package D is new ACPI_Battery (C, Dynamic_Evaluate);
   package Collector is new ACPI_Battery_Observation (C, F, D, S);
   package Store is new ACPI_Battery_Cache (C, F, D, S, Collector);
   package Pages is new Store.Observers;
   package Protocol renames ACPI_Power_Query_Protocol;
   use type Collector.Outcome, Collector.Phase, Collector.Observation;
   use type Protocol.Information_Source;
   use type Interfaces.Unsigned_64;
   Report : Collector.Query_Report;
   View : Store.Samples.View;
   procedure Query (Item : C.Node_Descriptor; Budget : Natural := 1000) is
   begin
      Extended_Calls := 0; Legacy_Calls := 0; Dynamic_Calls := 0;
      Collector.Query (Service, Item, Budget, Report);
      Check (Report.Charged <= Budget and then C.Retained_Results (Service) = 0, "collector roots/budget");
      Check (Report.Status = Collector.Sampled or else Report.Value = Collector.Empty_Observation,
        "collector no partial bundle");
   end Query;
   procedure Observe (Expected : Protocol.Information_Source) is
      Source : Pages.Words;
   begin
      View := (others => <>); View.Status := Store.Samples.Available;
      View.Revision := 1; View.Value := Report.Value;
      Source := Pages.Header_View (View);
      Check (Source (2) = 2 and then Source (3) = 21, "new raw schema only");
      Source := Pages.Read_View (View, 1, 21, 0);
      Check (Source (1) = 0 and then Source (2) = Protocol.Information_Source'Enum_Rep (Expected)
        and then Source (3) = 0, "raw information source");
      for Page in Pages.Page loop
         if Page in Pages.BIX_Version | Pages.Cycles | Pages.Sampling_Max | Pages.Sampling_Min |
           Pages.Accuracy | Pages.Averaging | Pages.Swap
         then
            Source := Pages.Read_View (View, 1, Pages.Page'Enum_Rep (Page), 0);
            Check ((if Expected = Protocol.BIF then Source (1) = 14 and then Source (2) = 0 and then
              Source (3) = 0 else Source (1) = 0), "extended page applicability");
         end if;
      end loop;
   end Observe;
   Baseline : Natural;
begin
   declare
      Input : Ada.Streams.Stream_IO.File_Type; Bytes : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Open (Input, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));
      Ada.Streams.Stream_IO.Read (Input, Bytes, Last); Ada.Streams.Stream_IO.Close (Input);
      declare Buffer_Data : Firmware_Tables.Bytes (1 .. Natural (Last)); begin
         for I in Buffer_Data'Range loop Buffer_Data (I) := Firmware_Tables.Byte (Bytes (Ada.Streams.Stream_Element_Offset (I))); end loop;
         Prepare (Service, Buffer_Data); Prepare (Other, Buffer_Data);
      end;
   end;
   Extended := Find (Service, "EXT0"); Legacy := Find (Service, "BAT0");
   Missing := Find (Service, "NON0"); Setter := Find (Service, "SELM"); Foreign := Find (Other, "BAT0");
   Query (Extended);
   Check (Report.Status = Collector.Sampled and then Extended_Calls = 1 and then Legacy_Calls = 0
     and then Dynamic_Calls = 4 and then Report.Value.Information.Source = S.Extended_Reported,
     "BIX pipeline order"); Observe (Protocol.BIX);
   Query (Legacy);
   Check (Report.Status = Collector.Sampled and then Extended_Calls = 0 and then Legacy_Calls = 1
     and then Dynamic_Calls = 4 and then Report.Value.Information.Source = S.Legacy_Reported,
     "BIF pipeline order"); Observe (Protocol.BIF); Baseline := Report.Charged;
   Query (Missing); Check (Report.Status /= Collector.Sampled, "no inherited information");
   Query (Foreign); Check (Report.Status = Collector.Identification_Failed and then
     Dynamic_Calls = 0 and then Extended_Calls = 0 and then Legacy_Calls = 0, "foreign collector context");
   Extended_Fault := Unknown_Field; Query (Extended);
   Check (Report.Status = Collector.Provider_Contract_Error and then Dynamic_Calls = 2
     and then Legacy_Calls = 0, "BIX uncertainty stops without legacy fallback");
   Extended_Fault := Normal;
   Mode (1); Query (Extended);
   Check (Report.Status = Collector.Information_Failed and then Legacy_Calls = 0 and then
     Dynamic_Calls = 2, "no fallback or BST after malformed BIX");
   Mode (8); Query (Legacy);
   Check (Report.Status = Collector.Presence_Changed and then Report.Reached = Collector.Confirm_Presence,
     "hot-swap invalidates selected information");
   Mode (0);
   for Budget in 0 .. Baseline loop
      Query (Legacy, Budget);
      Check (Report.Status = (if Budget = Baseline then Collector.Sampled else Collector.Budget_Exhausted),
        "whole pipeline single budget");
   end loop;
   Legacy_Fault := Overcharge; Query (Legacy);
   Check (Report.Status = Collector.Provider_Contract_Error and then Dynamic_Calls = 2,
     "legacy overcharge stops before BST");
   Legacy_Fault := Unknown_Field; Query (Legacy);
   Check (Report.Status = Collector.Provider_Contract_Error and then Dynamic_Calls = 2,
     "uncertainty stops before BST even with zero roots");
   Put_Line ("Battery collector/page checks" & Checks'Image);
end Collector_Pages_Tests;
