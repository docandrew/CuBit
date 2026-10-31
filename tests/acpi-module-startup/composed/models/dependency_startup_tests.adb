pragma Ada_2022;
with Ada.Command_Line; with Ada.Streams; with Ada.Streams.Stream_IO; with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service_Core; with ACPI_FADT.Transactions;
with AML_Delays; with AML_Decode; with AML_Execute; with AML_Field_Access;
with AML_Field_Layout; with AML_Identity; with AML_Identity.Issuer;
procedure Dependency_Startup_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Retained_Result_Capacity => 4);
   package A renames AML_Field_Access; package E renames AML_Execute;
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status, C.Values.Module_Outcome;
   use type E.Execution_Status, AML_Identity.Identity, A.Register_ID, A.Endpoint_ID, A.Observation;
   type Dependency_Kind is (Wide_Selector, Wide_Data);
   Kind : constant Dependency_Kind := Dependency_Kind'Value (Ada.Command_Line.Argument (2));
   type Scenario is (Ordinary, Source_Read_Denied, Source_Write_Denied, Missing_Last,
      Ambiguous_Last, Mixed_Providers, Requests_Ten, Before_Effect, After_Effect, Read_Unsupported);
   Selected : constant Scenario := Scenario'Value (Ada.Command_Line.Argument (3));
   Revision : constant Positive := Positive'Value (Ada.Command_Line.Argument (4));
   Fault_Call : constant Natural := (if Ada.Command_Line.Argument_Count > 4
      then Natural'Value (Ada.Command_Line.Argument (5)) else 0);
   subtype Offset is Natural range 0 .. 10;
   type Cells is array (Offset) of Unsigned_64;
   First_ID : constant A.Register_ID := 16#E000_0000_0000_0001#;
   function ID (N : Offset) return A.Register_ID is (First_ID + A.Register_ID (N));
   Last_Source : constant Offset := (if Kind = Wide_Selector then 9 else 10);
   Expected : constant Cells :=
      (if Kind = Wide_Data then [2, 16#C3#, 0, 0, 0, 0, 0, 0, 0, 0, 0]
       elsif Revision = 1 then [16#10#,16#32#,16#54#,16#76#,0,0,0,0,0,0,16#A6#]
       else [16#10#,16#32#,16#54#,16#76#,16#98#,16#BA#,16#DC#,16#FE#,0,0,16#A6#]);
   Write_Episode : constant Positive := (if Kind = Wide_Selector then 22 else 11);
   type Planned_Request is record
      Cell : Offset;
      Writing : Boolean;
      Value : Unsigned_64;
   end record;
   function Expected_Request (At_Call : Positive) return Planned_Request is
   begin
      if Kind = Wide_Data then
         return (Offset (At_Call - 1), True, Expected (Offset (At_Call - 1)));
      elsif At_Call in 1 .. 10 then
         return (Offset (At_Call - 1), True, Expected (Offset (At_Call - 1)));
      elsif At_Call = 11 or else At_Call = 33 then return (10, False, 0);
      elsif At_Call in 12 .. 21 then return (Offset (At_Call - 12), True, Expected (Offset (At_Call - 12)));
      elsif At_Call = 22 then return (10, True, 16#A6#);
      else return (Offset (At_Call - 23), True, Expected (Offset (At_Call - 23))); end if;
   end Expected_Request;
   type Context is limited record
      Owner, Other : AML_Identity.Identity := AML_Identity.No_Identity;
      Data : Cells := [others => 16#A5#];
      Calls : Natural := 0;
      Receipt, Confirmed : A.Observation;
   end record;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Kind'Image & " " & Selected'Image & ": " & Why; end if; end Check;
   function Pending (IO : Context) return Boolean is (IO.Receipt.Present);
   function Source_Cell (N : Offset) return Boolean is
     (if Kind = Wide_Selector then N < 10 else N > 0);
   procedure Describe (IO : Context; Target : A.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
   begin
      Accepted := not Pending (IO) and then Target.Provider in IO.Owner | IO.Other
        and then Target.Endpoint = 7 and then Target.Epoch = 1 and then Target.Register in First_ID .. ID (10);
      Geometry := (Region => (Space => 0, Base => 16#1000#, Length => 11),
         Byte_Offset => (if Accepted then Unsigned_64 (Target.Register - First_ID) else 0),
         Width => ACPI_FADT.Transactions.Byte_Access);
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target;
      For_Write : Boolean; Accepted : out Boolean) is
      Geometry : AML_Field_Layout.Register_Geometry;
   begin
      Describe (IO, Target, Geometry, Accepted);
      Accepted := Accepted and then not
        ((For_Write and then Selected = Source_Write_Denied and then Target.Register = ID (Last_Source))
         or else (not For_Write and then Selected = Source_Read_Denied
           and then Source_Cell (Offset (Geometry.Byte_Offset))));
   end Authorize;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      N : constant Offset := Offset (Item.Destination.Register - First_ID);
      Plan : Planned_Request;
      Allowed : Boolean;
      procedure Unknown is
      begin
         IO.Receipt := (True, IO.Owner, Item, 16#FFFF_0000_0000_0001#);
         Reply := (A.Indeterminate, IO.Owner, 0, IO.Receipt.Operation);
      end Unknown;
   begin
      Authorize (IO, Item.Destination, Item.For_Write, Allowed); Check (Allowed, "current named-cell rights");
      IO.Calls := IO.Calls + 1; Plan := Expected_Request (IO.Calls);
      Check (N = Plan.Cell and then Item.For_Write = Plan.Writing
         and then (not Item.For_Write or else Item.Value = Plan.Value), "exact wide dependency request order/value");
      if Selected = Before_Effect and then IO.Calls = Fault_Call then Unknown; return; end if;
      if Item.For_Write then IO.Data (N) := Item.Value; end if;
      if Selected = After_Effect and then IO.Calls = Fault_Call then Unknown; return; end if;
      Reply := (A.Completed, IO.Owner, (if Item.For_Write then 0 else IO.Data (N)), A.No_Operation);
   end Dispatch;
   procedure Confirm (IO : Context; Want : A.Observation; Accepted : out Boolean) is
   begin Accepted := Want.Present and then not Pending (IO) and then IO.Confirmed = Want; end Confirm;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirm,
      Authorize_Target => Authorize, Maximum_Bindings => 12);
   package Budgeted is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirm,
      Authorize_Target => Authorize, Maximum_Bindings => 12, Maximum_Requests => 10);
   Service : aliased C.State (1, 4096, 4096, Load_Capacity => 1); IO : Context;
   Targets : F.Module_Target_Set := [others => A.No_Target]; Count : F.Module_Target_Count := 11;
   Report : C.Execution_Report; Status : C.Values.Access_Status; Good : Boolean;
begin
   declare
      package SIO renames Ada.Streams.Stream_IO;
      File : SIO.File_Type; Data : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset; Stage : C.Stage_Status; Freeze : C.Freeze_Status;
   begin
      SIO.Open (File, SIO.In_File, Ada.Command_Line.Argument (1)); SIO.Read (File, Data, Last); SIO.Close (File);
      declare Image : AML_Decode.Bytes (1 .. Natural (Last)); begin
         for I in Image'Range loop Image (I) := Unsigned_8 (Data (Ada.Streams.Stream_Element_Offset (I))); end loop;
         C.Stage_Table (Service, 41, C.DSDT, Image, Stage); Check (Stage = C.Staged, "stage");
      end;
      C.Freeze_Catalog (Service, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "freeze");
   end;
   AML_Identity.Issuer.Issue (IO.Owner, Good); Check (Good, "primary provider");
   AML_Identity.Issuer.Issue (IO.Other, Good); Check (Good, "secondary provider");
   for N in Offset loop Targets (N + 1) := (IO.Owner, 7, 1, ID (N)); end loop;
   case Selected is
      when Missing_Last =>
         for P in Last_Source + 1 .. 10 loop Targets (P) := Targets (P + 1); end loop;
         Targets (11) := A.No_Target; Count := 10;
      when Ambiguous_Last => Targets (12) := Targets (Last_Source + 1); Count := 12;
      when Mixed_Providers => Targets (Last_Source + 1).Provider := IO.Other;
      when others => null;
   end case;
   if Selected = Requests_Ten then
      declare T : Budgeted.Module_Target_Set := [others => A.No_Target]; begin
         for P in 1 .. 11 loop T (P) := Targets (P); end loop;
         Budgeted.Execute_Frozen (Service, IO, T, 11, 10000, Report, Status);
      end;
   else F.Execute_Frozen (Service, IO, Targets, Count, 10000, Report, Status); end if;
   Check (Status = C.Values.Available and then Report.Modules.Charged <= 10000, "bounded module report");
   if Selected in Ordinary | Source_Read_Denied then
      Check (Report.Modules.Outcome = C.Values.Modules_Completed
         and then IO.Calls = (if Kind = Wide_Selector then 33 else 11) and then IO.Data = Expected,
         "full zero-extended wide dependency without source reads");
      if Kind = Wide_Selector then
         declare
            Init : C.Namespace.Initialization_Report; Node : C.Namespace.Node_ID;
            Item : C.Node_Descriptor; Value : C.Values.Result;
         begin
            C.Initialize_Members (Service, Init, Status); Check (Status = C.Values.Available, "seal namespace");
            C.Find_Child (Service, C.Namespace.Root, "SEEN", Node, Status); Check (Status = C.Values.Available, "observed value");
            C.Describe_Node (Service, Node, Item, Status); Check (Status = C.Values.Available, "value identity");
            C.Evaluate_Named (Service, Item, 1000, Value, Status);
            Check (Status = C.Values.Available and then Value.Status = E.Returned and then Value.Number = 16#A6#, "parent data readback");
         end;
      end if;
   elsif Selected in Before_Effect | After_Effect | Requests_Ten then
      declare
         Stop_At : constant Natural := (if Selected = Requests_Ten then 10 else Fault_Call);
         Observed : Cells := [others => 16#A5#];
      begin
         for Step in 1 .. Stop_At loop
            declare Want : constant Planned_Request := Expected_Request (Step); begin
               if Want.Writing and then (Selected = Requests_Ten or else Step < Stop_At or else Selected = After_Effect)
               then Observed (Want.Cell) := Want.Value; end if;
            end;
         end loop;
         Check (Stop_At in 1 .. Write_Episode and then Report.Modules.Outcome = C.Values.Module_Rejected
            and then IO.Calls = Stop_At and then IO.Data = Observed, "only attributable prefix effects retained");
         if Selected = Requests_Ten then
            Check (Report.Modules.Failure = E.Value_Limit and then not Pending (IO), "one request counter across parent and wide child");
         else
            declare Want : constant Planned_Request := Expected_Request (Fault_Call); begin
               Check (Report.Modules.Failure = E.Field_Indeterminate and then Pending (IO)
                  and then F.Uncertain (Service) = IO.Receipt
                  and then IO.Receipt.Item.Destination.Register = ID (Want.Cell)
                  and then IO.Receipt.Item.For_Write = Want.Writing, "actual wide leaf uncertainty retained");
            end;
         end if;
      end;
   else
      Check (Report.Modules.Outcome = C.Values.Module_Rejected and then IO.Calls = 0
         and then IO.Data = Cells'[others => 16#A5#], "unsupported or incomplete dependency refuses before effects");
   end if;
   declare Before : constant Natural := IO.Calls; begin
      F.Execute_Frozen (Service, IO, Targets, Count, 10000, Report, Status);
      Check (Status /= C.Values.Available and then IO.Calls = Before, "no startup replay");
   end;
   Check (C.Valid (Service) and then C.Retained_Results (Service) = 0, "valid owner with no lost roots");
   Ada.Text_IO.Put_Line ("Dependency startup checks" & Checks'Image);
end Dependency_Startup_Tests;
