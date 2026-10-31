pragma Ada_2022;
with Ada.Command_Line; with Ada.Streams; with Ada.Streams.Stream_IO; with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service_Core; with ACPI_FADT.Transactions;
with AML_Delays; with AML_Decode; with AML_Execute;
with AML_Field_Access; with AML_Field_Layout; with AML_Identity; with AML_Identity.Issuer;
procedure Wide_Startup_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Retained_Result_Capacity => 4);
   package A renames AML_Field_Access; package E renames AML_Execute;
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status, C.Values.Module_Outcome, C.Values.Value_Handle;
   use type E.Execution_Status, AML_Identity.Identity, A.Register_ID, A.Endpoint_ID, A.Observation;
   use type AML_Decode.Bytes;
   type Field_Kind is (Direct_Field, Bank_Field, Index_Field);
   Kind : constant Field_Kind := Field_Kind'Value (Ada.Command_Line.Argument (2));
   type Scenario is (Ordinary, Scalar, Missing_Last, Ambiguous_Last, Denied_Write,
      Denied_Read, Mixed_Providers, Unknown_Last);
   Selected : constant Scenario := Scenario'Value (Ada.Command_Line.Argument (3));
   Revision : constant Positive := Positive'Value (Ada.Command_Line.Argument (4));
   First_ID : constant A.Register_ID := 16#E000_0000_0000_0001#;
   Data_Count : constant Positive := 10;
   subtype Data_Index is Natural range 0 .. Data_Count - 1;
   subtype Physical_Offset is Natural range 0 .. Data_Count;
   type Cells is array (Data_Index) of Unsigned_64;
   Buffer_Bytes : constant AML_Decode.Bytes := [16#88#,16#77#,16#66#,16#55#,16#44#,16#33#,16#22#,16#11#,16#FE#];
   Expected_Bytes : constant AML_Decode.Bytes :=
      (if Selected /= Scalar then Buffer_Bytes
       elsif Revision = 1 then [16#10#,16#32#,16#54#,16#76#,0,0,0,0,0]
       else [16#10#,16#32#,16#54#,16#76#,16#98#,16#BA#,16#DC#,16#FE#,0]);
   Expected : constant Cells :=
      (if Selected /= Scalar then [16#85#,16#78#,16#67#,16#56#,16#45#,16#34#,16#23#,16#12#,16#E1#,16#AF#]
       elsif Revision = 1 then [16#05#,16#21#,16#43#,16#65#,16#07#,0,0,0,0,16#A0#]
       else [16#05#,16#21#,16#43#,16#65#,16#87#,16#A9#,16#CB#,16#ED#,16#0F#,16#A0#]);
   function ID (Offset : Physical_Offset) return A.Register_ID is (First_ID + A.Register_ID (Offset));
   Last_Offset : constant Physical_Offset := (if Kind = Index_Field then 1 else Data_Count);
   type Context is limited record
      Owner, Other : AML_Identity.Identity := AML_Identity.No_Identity;
      Data : Cells := [others => 16#A5#];
      Selector : Unsigned_64 := 0;
      Calls, Selections : Natural := 0;
      Receipt, Confirmed : A.Observation;
   end record;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Kind'Image & " " & Selected'Image & ": " & Why; end if; end Check;
   function Pending (IO : Context) return Boolean is (IO.Receipt.Present);
   procedure Describe (IO : Context; Target : A.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
   begin
      Accepted := not Pending (IO) and then Target.Provider in IO.Owner | IO.Other
        and then Target.Endpoint = 7 and then Target.Epoch = 1
        and then Target.Register in First_ID .. ID (Last_Offset);
      Geometry := (Region => (Space => 0, Base => 16#1000#, Length => 11),
         Byte_Offset => (if Accepted then Unsigned_64 (Target.Register - First_ID) else 0),
         Width => ACPI_FADT.Transactions.Byte_Access);
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target;
      For_Write : Boolean; Accepted : out Boolean) is
      Geometry : AML_Field_Layout.Register_Geometry;
   begin
      Describe (IO, Target, Geometry, Accepted);
      Accepted := Accepted and then not (Target.Register = ID (Last_Offset) and then
        (if For_Write then Selected = Denied_Write else Selected = Denied_Read));
   end Authorize;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Offset : constant Physical_Offset := Physical_Offset (Item.Destination.Register - First_ID);
      Allowed : Boolean;
      Index : Data_Index := Data_Index'First;
   begin
      Authorize (IO, Item.Destination, Item.For_Write, Allowed);
      Check (Allowed, "every physical request has current named rights");
      IO.Calls := IO.Calls + 1;
      if Offset = 0 then
         Check (Item.For_Write, "selectors are not read for whole-byte stores");
         Check (Item.Value = 3 or else (Kind = Index_Field and then Item.Value <= Unsigned_64 (Data_Index'Last)), "selector range");
         IO.Selector := Item.Value; IO.Selections := IO.Selections + 1;
      else
         if Kind = Index_Field then Index := Data_Index (IO.Selector);
         else Index := Data_Index (Offset - 1); end if;
         if Kind = Bank_Field then Check (IO.Selector = 3, "bank selected before data"); end if;
         if Item.For_Write then IO.Data (Index) := Item.Value; end if;
      end if;
      if Selected = Unknown_Last and then Offset /= 0 and then Index = Data_Index'Last and then Item.For_Write then
         IO.Receipt := (True, IO.Owner, Item, 16#FFFF_0000_0000_0001#);
         Reply := (A.Indeterminate, IO.Owner, 0, IO.Receipt.Operation);
      else Reply := (A.Completed, IO.Owner, (if Item.For_Write then 0 elsif Offset = 0 then IO.Selector else IO.Data (Index)), A.No_Operation); end if;
   end Dispatch;
   procedure Confirm (IO : Context; Expected_Receipt : A.Observation; Accepted : out Boolean) is
   begin Accepted := Expected_Receipt.Present and then not Pending (IO) and then IO.Confirmed = Expected_Receipt; end Confirm;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirm,
      Authorize_Target => Authorize, Maximum_Bindings => 12);
   use type F.Module_Target_Count;
   Service : aliased C.State (1, 4096, 4096, Load_Capacity => 1); IO : Context;
   Targets : F.Module_Target_Set := [others => A.No_Target];
   Count : F.Module_Target_Count := F.Module_Target_Count (Last_Offset + 1);
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
   for P in Physical_Offset range 0 .. Last_Offset loop Targets (P + 1) := (IO.Owner, 7, 1, ID (P)); end loop;
   if Selected = Missing_Last then Targets (Natural (Count)) := A.No_Target; Count := Count - 1;
   elsif Selected = Ambiguous_Last then Targets (Natural (Count) + 1) := Targets (Natural (Count)); Count := Count + 1;
   elsif Selected = Mixed_Providers then Targets (Natural (Count)).Provider := IO.Other; end if;
   F.Execute_Frozen (Service, IO, Targets, Count, 10000, Report, Status);
   Check (Status = C.Values.Available and then Report.Modules.Charged <= 10000, "bounded module report");
   if Selected in Ordinary | Scalar then
      Check (Report.Modules.Outcome = C.Values.Modules_Completed, "wide module completed");
      Check (IO.Data = Expected and then IO.Calls = (if Kind = Direct_Field then 22 else 44), "all72bits and preserved edge nibbles");
      Check (IO.Selections = (if Kind = Direct_Field then 0 else 22), "selector before each data operation");
      declare
         Init : C.Namespace.Initialization_Report; Node : C.Namespace.Node_ID;
         Item : C.Node_Descriptor; Value : C.Values.Result;
         Handle : C.Values.Value_Handle; Copied : AML_Decode.Bytes (1 .. 9); Number : Natural;
      begin
         C.Initialize_Members (Service, Init, Status); Check (Status = C.Values.Available, "seal namespace");
         C.Find_Child (Service, C.Namespace.Root, "OUT0", Node, Status); Check (Status = C.Values.Available, "output node");
         C.Describe_Node (Service, Node, Item, Status); Check (Status = C.Values.Available, "output identity");
         C.Evaluate_Named (Service, Item, 1000, Value, Status);
         Check (Status = C.Values.Available and then Value.Status = E.Object_Returned, "wide result is a buffer at either AML width");
         Handle := Value.Handle; C.Read_Result_Bytes (Service, Handle, 0, Copied, Number, Status);
         Check (Status = C.Values.Available and then Number = Expected_Bytes'Length and then Copied = Expected_Bytes, "nine-byte result preserved");
         C.Release_Result (Service, Handle, Status); Check (Status = C.Values.Available and then Handle = C.Values.No_Value, "readback released");
      end;
   elsif Selected = Unknown_Last then
      Check (Report.Modules.Outcome = C.Values.Module_Rejected and then Report.Modules.Failure = E.Field_Indeterminate
         and then IO.Calls = (if Kind = Direct_Field then 12 else 24), "last write uncertainty stops before readback");
      Check (IO.Data = Expected and then Pending (IO) and then F.Uncertain (Service) = IO.Receipt
         and then IO.Receipt.Item.Destination.Register = ID (Last_Offset)
         and then IO.Receipt.Item.Value = Expected (Data_Index'Last), "full possible effects and exact last datum receipt retained");
   else
      Check (Report.Modules.Outcome = C.Values.Module_Rejected and then IO.Calls = 0
         and then IO.Data = Cells'[others => 16#A5#], "complete wide association and rights before first effect");
   end if;
   declare Before : constant Natural := IO.Calls; begin
      F.Execute_Frozen (Service, IO, Targets, Count, 10000, Report, Status);
      Check (Status /= C.Values.Available and then IO.Calls = Before, "no startup replay");
   end;
   Check (C.Valid (Service) and then C.Retained_Results (Service) = 0, "valid owner and no lost roots");
   Ada.Text_IO.Put_Line ("Wide startup checks" & Checks'Image);
end Wide_Startup_Tests;
