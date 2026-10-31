pragma Ada_2022;
with Ada.Command_Line; with Ada.Streams; with Ada.Streams.Stream_IO; with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service_Core; with ACPI_FADT.Transactions;
with AML_Delays; with AML_Decode; with AML_Execute;
with AML_Field_Access; with AML_Field_Layout; with AML_Identity; with AML_Identity.Issuer;
procedure Reciprocal_Startup_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Retained_Result_Capacity => 4);
   package A renames AML_Field_Access; package E renames AML_Execute;
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status, C.Values.Module_Outcome;
   use type E.Execution_Status, AML_Identity.Identity, A.Register_ID, A.Endpoint_ID, A.Observation;
   type Scenario is (Ordinary, Missing_Data, Missing_Index, Ambiguous, Mixed_Providers,
      Unused_Provider, Stale, Read_Denied, Write_Denied, Selector_Denied,
      Index_Denied, Geometry_Drift, Requests_Five, Before_Effect, After_Effect);
   Selected : constant Scenario := Scenario'Value (Ada.Command_Line.Argument (2));
   Fault_Call : constant Natural := (if Ada.Command_Line.Argument_Count > 2
      then Natural'Value (Ada.Command_Line.Argument (3)) else 0);
   type Slot is (Index_Port, Bank_Port, Data_Port, Unused_Port);
   type IDs is array (Slot) of A.Register_ID;
   Registers : constant IDs := [16#8000_0000_0000_0011#, 16#9000_0000_0000_0022#,
      16#FFFF_FFFF_FFFF_FF97#, 16#B000_0000_0000_0044#];
   type Cells is array (Slot) of Unsigned_64;
   type Context is limited record
      Owner, Other : AML_Identity.Identity := AML_Identity.No_Identity;
      Data : Cells := [Data_Port => 16#A5#, others => 0];
      Calls : Natural := 0;
      Trace : String (1 .. 16) := [others => ' '];
      Receipt, Confirmed : A.Observation;
   end record;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Selected'Image & ": " & Why; end if; end Check;
   function Pending (IO : Context) return Boolean is (IO.Receipt.Present);
   function Locate (ID : A.Register_ID) return Slot is
   begin
      for S in Slot loop if Registers (S) = ID then return S; end if; end loop;
      raise Program_Error with "unexpected named target";
   end Locate;
   procedure Describe (IO : Context; Target : A.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
      Found : Boolean := False;
      S : Slot := Index_Port;
   begin
      for Candidate in Slot loop
         if Registers (Candidate) = Target.Register then S := Candidate; Found := True; end if;
      end loop;
      Geometry := (Region => (Space => 0, Base => 16#1000#, Length => 4),
         Byte_Offset => (if Selected = Geometry_Drift and then S = Data_Port and then IO.Calls > 0
            then 3 else Unsigned_64 (Slot'Pos (S))),
         Width => ACPI_FADT.Transactions.Byte_Access);
      Accepted := Found and then not Pending (IO)
        and then Target.Provider in IO.Owner | IO.Other
        and then Target.Endpoint = 7 and then Target.Epoch = 1;
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target;
      For_Write : Boolean; Accepted : out Boolean) is
      Geometry : AML_Field_Layout.Register_Geometry;
   begin
      Describe (IO, Target, Geometry, Accepted);
      Accepted := Accepted and then not
        ((Target.Register = Registers (Data_Port) and then
          (if For_Write then Selected = Write_Denied else Selected = Read_Denied))
         or else (For_Write and then Selected = Selector_Denied and then Target.Register = Registers (Bank_Port))
         or else (For_Write and then Selected = Index_Denied and then Target.Register = Registers (Index_Port)));
   end Authorize;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      S : constant Slot := Locate (Item.Destination.Register);
      Allowed : Boolean;
      procedure Unknown is
      begin
         IO.Receipt := (True, IO.Owner, Item, 16#FFFF_0000_0000_0001#);
         Reply := (A.Indeterminate, IO.Owner, 0, IO.Receipt.Operation);
      end Unknown;
   begin
      Authorize (IO, Item.Destination, Item.For_Write, Allowed);
      Check (Allowed, "dispatch has current rights");
      IO.Calls := IO.Calls + 1;
      IO.Trace (IO.Calls) := (case S is when Index_Port => 'I', when Bank_Port => 'B',
         when Data_Port => (if Item.For_Write then 'W' else 'r'), when Unused_Port => 'X');
      if Selected = Before_Effect and then IO.Calls = Fault_Call then Unknown; return; end if;
      if Item.For_Write then IO.Data (S) := Item.Value; end if;
      if Selected = After_Effect and then IO.Calls = Fault_Call then Unknown; return; end if;
      Reply := (A.Completed, IO.Owner, (if Item.For_Write then 0 else IO.Data (S)), A.No_Operation);
   end Dispatch;
   procedure Confirm (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
   begin Accepted := Expected.Present and then not Pending (IO) and then IO.Confirmed = Expected; end Confirm;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirm,
      Authorize_Target => Authorize, Maximum_Bindings => 4);
   package Budgeted is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirm,
      Authorize_Target => Authorize, Maximum_Bindings => 4, Maximum_Requests => 5);
   Service : aliased C.State (1, 4096, 4096, Load_Capacity => 1);
   IO : Context;
   Targets : F.Module_Target_Set := [others => A.No_Target];
   Count : F.Module_Target_Count := 3;
   Report : C.Execution_Report;
   Status : C.Values.Access_Status;
   Good : Boolean;
   Expected_Trace : constant String := "IBrIBWIBr";
begin
   declare
      package SIO renames Ada.Streams.Stream_IO;
      File : SIO.File_Type;
      Data : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
      Stage : C.Stage_Status; Freeze : C.Freeze_Status;
   begin
      SIO.Open (File, SIO.In_File, Ada.Command_Line.Argument (1));
      SIO.Read (File, Data, Last); SIO.Close (File);
      declare Image : AML_Decode.Bytes (1 .. Natural (Last)); begin
         for I in Image'Range loop Image (I) := Unsigned_8 (Data (Ada.Streams.Stream_Element_Offset (I))); end loop;
         C.Stage_Table (Service, 41, C.DSDT, Image, Stage); Check (Stage = C.Staged, "stage");
      end;
      C.Freeze_Catalog (Service, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "freeze");
   end;
   AML_Identity.Issuer.Issue (IO.Owner, Good); Check (Good, "primary provider");
   AML_Identity.Issuer.Issue (IO.Other, Good); Check (Good, "secondary provider");
   for S in Slot range Index_Port .. Data_Port loop
      Targets (Slot'Pos (S) + 1) := (IO.Owner, 7, 1, Registers (S));
   end loop;
   case Selected is
      when Missing_Data => Count := 2; Targets (3) := A.No_Target;
      when Missing_Index => Count := 2; Targets (1) := Targets (2); Targets (2) := Targets (3); Targets (3) := A.No_Target;
      when Ambiguous => Count := 4; Targets (4) := Targets (3);
      when Mixed_Providers => Targets (3).Provider := IO.Other;
      when Unused_Provider => Count := 4; Targets (4) := (IO.Other, 7, 1, Registers (Unused_Port));
      when Stale => Targets (2).Epoch := 2;
      when others => null;
   end case;
   if Selected = Requests_Five then
      declare Limited_Targets : Budgeted.Module_Target_Set := [others => A.No_Target]; begin
         for P in 1 .. 3 loop Limited_Targets (P) := Targets (P); end loop;
         Budgeted.Execute_Frozen (Service, IO, Limited_Targets, 3, 10000, Report, Status);
      end;
   else F.Execute_Frozen (Service, IO, Targets, Count, 10000, Report, Status); end if;
   Check (Status = C.Values.Available and then Report.Modules.Charged <= 10000, "bounded module result");
   if Selected in Ordinary | Unused_Provider then
      Check (Report.Modules.Outcome = C.Values.Modules_Completed and then IO.Calls = 9
         and then IO.Trace (1 .. IO.Calls) = Expected_Trace, "exact bank/index/read/bank/index/write/read order");
      Check (IO.Data = Cells'[3, 4, 16#A6#, 0], "partial value and neighbors");
      declare
         Init : C.Namespace.Initialization_Report; Node : C.Namespace.Node_ID;
         Item : C.Node_Descriptor; Value : C.Values.Result;
      begin
         C.Initialize_Members (Service, Init, Status); Check (Status = C.Values.Available, "seal namespace");
         C.Find_Child (Service, C.Namespace.Root, "OUT0", Node, Status); Check (Status = C.Values.Available, "output node");
         C.Describe_Node (Service, Node, Item, Status); Check (Status = C.Values.Available, "output identity");
         C.Evaluate_Named (Service, Item, 1000, Value, Status);
         Check (Status = C.Values.Available and then Value.Status = E.Returned and then Value.Number = 16#A6#, "module readback");
      end;
   elsif Selected in Before_Effect | After_Effect then
      Check (Fault_Call in 1 .. 6 and then Report.Modules.Outcome = C.Values.Module_Rejected
         and then Report.Modules.Failure = E.Field_Indeterminate, "uncertain nested request is terminal");
      Check (IO.Calls = Fault_Call and then IO.Trace (1 .. IO.Calls) = Expected_Trace (1 .. IO.Calls), "uncertainty preserves exact prefix");
      Check (Pending (IO) and then F.Uncertain (Service) = IO.Receipt, "actual leaf receipt retained");
      Check (IO.Receipt.Item.Destination.Register = Registers
         ((case Fault_Call is when 1 | 4 => Index_Port, when 2 | 5 => Bank_Port, when others => Data_Port))
         and then IO.Receipt.Item.For_Write = (Fault_Call /= 3), "receipt identifies exact selector/data operation");
      Check (IO.Data (Index_Port) = (if Fault_Call > 1 or else Selected = After_Effect then 3 else 0)
         and then IO.Data (Bank_Port) = (if Fault_Call > 2 or else (Fault_Call = 2 and then Selected = After_Effect) then 4 else 0)
         and then IO.Data (Data_Port) = (if Fault_Call = 6 and then Selected = After_Effect then 16#A6# else 16#A5#),
         "only possible prefix effects retained");
   elsif Selected = Requests_Five then
      Check (Report.Modules.Outcome = C.Values.Module_Rejected and then Report.Modules.Failure = E.Value_Limit
         and then IO.Calls = 5 and then IO.Trace (1 .. 5) = Expected_Trace (1 .. 5), "one request allowance across all selectors and data");
      Check (IO.Data = Cells'[3, 4, 16#A5#, 0] and then not Pending (IO), "known prefix without invented uncertainty");
   elsif Selected = Geometry_Drift then
      -- The inner banked selector completes its still-admitted DAT0 write;
      -- returning to the outer indexed field rechecks MDAT before accessing it.
      Check (Report.Modules.Outcome = C.Values.Module_Rejected and then IO.Calls = 2
         and then IO.Trace (1 .. 2) = "IB" and then IO.Data = Cells'[3, 4, 16#A5#, 0],
         "outer geometry loss preserves completed inner selection without data access");
   else
      Check (Report.Modules.Outcome = C.Values.Module_Rejected and then IO.Calls = 0
         and then IO.Data = Cells'[0, 0, 16#A5#, 0], "whole dependency graph refuses before effects");
   end if;
   declare Before : constant Natural := IO.Calls; begin
      F.Execute_Frozen (Service, IO, Targets, Count, 10000, Report, Status);
      Check (Status /= C.Values.Available and then IO.Calls = Before, "no startup replay");
   end;
   Check (C.Valid (Service) and then C.Retained_Results (Service) = 0, "valid namespace with no lost retained roots");
   Ada.Text_IO.Put_Line ("Reciprocal startup checks" & Checks'Image);
end Reciprocal_Startup_Tests;
