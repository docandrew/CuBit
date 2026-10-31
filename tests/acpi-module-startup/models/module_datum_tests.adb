pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with ACPI_Service_Core;
with ACPI_FADT.Transactions;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with AML_Identity.Issuer;
with Root_Test_Support; use Root_Test_Support;
procedure Module_Datum_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Retained_Result_Capacity => 32);
   package E renames AML_Execute;
   package A renames AML_Field_Access;
   subtype Bytes is AML_Decode.Bytes;
   use type Bytes;
   use type E.Execution_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type AML_Identity.Identity;
   use type A.Register_ID;
   use type A.Endpoint_ID;
   use type A.Observation;
   First_ID : constant A.Register_ID := 16#8000_0000_0000_0041#;
   Second_ID : constant A.Register_ID := 16#FFFF_FFFF_FFFF_FF97#;
   -- Explicit synthetic provider, not the real backend catalog fixture.
   type Context is limited record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      First, Second : Unsigned_64 := 0;
      Deny_Writes, Deny_Reads, Deny_Second_Write : Boolean := False;
      Drift_After_First_Write, Drifted : Boolean := False;
      Calls : Natural := 0;
      Arm_Unknown_Second_Write : Boolean := False;
      Receipt, Confirmed : A.Observation;
   end record;
   function Pending (IO : Context) return Boolean is (IO.Receipt.Present);
   procedure Describe (IO : Context; Target : A.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (Region => (Space => 3, Base => 0, Length => 2),
         Byte_Offset => (if Target.Register = First_ID or else IO.Drifted then 0 else 1),
         Width => ACPI_FADT.Transactions.Byte_Access);
      Accepted := not Pending (IO) and then Target.Provider = IO.Owner
        and then Target.Endpoint = 7 and then Target.Epoch = 1
        and then Target.Register in First_ID | Second_ID;
   end Describe;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Geometry : AML_Field_Layout.Register_Geometry;
      OK : Boolean;
   begin
      Reply := (others => <>);
      Describe (IO, Item.Destination, Geometry, OK);
      if not OK then return; end if;
      IO.Calls := IO.Calls + 1;
      if Item.For_Write then
         if Item.Destination.Register = First_ID then IO.First := Item.Value;
            IO.Drifted := IO.Drift_After_First_Write;
         else IO.Second := Item.Value; end if;
      end if;
      if Item.For_Write and then IO.Arm_Unknown_Second_Write
        and then Item.Destination.Register = Second_ID then
         IO.Arm_Unknown_Second_Write := False;
         IO.Receipt := (True, IO.Owner, Item, 1);
         Reply := (A.Indeterminate, IO.Owner, 0, 1);
      else
         Reply := (A.Completed, IO.Owner,
           (if Item.For_Write then 0 elsif Item.Destination.Register = First_ID
            then IO.First else IO.Second), A.No_Operation);
      end if;
   end Dispatch;
   procedure Confirmed (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
   begin Accepted := Expected.Present and then not Pending (IO) and then IO.Confirmed = Expected; end Confirmed;
   procedure Authorize (IO : Context; Target : A.Target;
      For_Write : Boolean; Accepted : out Boolean) is
      Geometry : AML_Field_Layout.Register_Geometry;
   begin
      Describe (IO, Target, Geometry, Accepted);
      Accepted := Accepted and then not
        (if For_Write then IO.Deny_Writes or else
           (IO.Deny_Second_Write and then Target.Register = Second_ID)
         else IO.Deny_Reads);
   end Authorize;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
      Authorize_Target => Authorize, Maximum_Bindings => 3);
   use type C.Stage_Status, C.Freeze_Status;
   Checks : Natural := 0;
   Mode : constant String := Ada.Command_Line.Argument (1);
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   Partial : constant Boolean := Mode in "partial" | "partial-write-only" | "partial-unknown";
   function Table (Revision : Unsigned_8) return Bytes is
      Base : constant Bytes := (if Mode = "forged-base" then [16#0A#,16#10#] else [0]);
      Bits : constant Bytes := (if Partial then [0,4] & Enc ("PAIR") & [8] else Enc ("PAIR") & [16]);
      Input : constant Bytes := (if Partial then [16#0A#,16#AB#] else [16#0B#,16#CD#,16#AB#]);
      Suffix : constant Bytes := (if Mode = "write-only" then [] else [16#70#] & Enc ("PAIR") & Enc ("OUT0"));
      Payload : constant Bytes := [16#08#] & Enc ("OUT0") & [0]
        & [16#5B#,16#80#] & Enc ("REG0") & [3] & Base & [16#0A#,2]
        & Block ([16#5B#,16#81#], Enc ("REG0") & [1] & Bits)
        & [16#70#] & Input & Enc ("PAIR") & Suffix;
      Header_Bytes : constant Positive := 36;
      Data : Bytes (1 .. Header_Bytes + Payload'Length) := [others => 0];
      Sum : Unsigned_8 := 0;
   begin
      Data (1 .. 4) := Enc ("DSDT"); Data (9) := Revision;
      for I in 0 .. 3 loop Data (5 + I) := Unsigned_8 (Shift_Right (Unsigned_32 (Data'Length), I * 8) and 255); end loop;
      Data (Header_Bytes + 1 .. Data'Last) := Payload;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum; return Data;
   end Table;
begin
   for Revision in Unsigned_8 range 1 .. 2 loop
      declare
         Service : aliased C.State (1, 4096, 4096, Load_Capacity => 1);
         Data : constant Bytes := Table (Revision);
         Staged : C.Stage_Status; Frozen : C.Freeze_Status;
         IO : Context; Targets : F.Module_Target_Set := [others => A.No_Target];
         Count : F.Module_Target_Count := 2;
         Report : C.Execution_Report; Status : C.Values.Access_Status;
         Good : Boolean; Before : Natural;
         Initialized : C.Namespace.Initialization_Report;
         Node : C.Namespace.Node_ID; Item : C.Node_Descriptor; Sample : C.Values.Result;
      begin
         C.Stage_Table (Service, 41, C.DSDT, Data, Staged); Check (Staged = C.Staged, "stage");
         C.Freeze_Catalog (Service, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
         AML_Identity.Issuer.Issue (IO.Owner, Good); Check (Good, "provider identity");
         IO.First := 5; IO.Second := 16#E0#;
         Targets (1) := (Provider => IO.Owner, Endpoint => 7, Register => First_ID, Epoch => 1);
         Targets (2) := (Provider => IO.Owner, Endpoint => 7, Register => Second_ID, Epoch => 1);
         if Mode = "missing" then Targets (2) := A.No_Target; Count := 1;
         elsif Mode = "ambiguous" then Targets (3) := Targets (2); Count := 3;
         elsif Mode = "denied-second" then IO.Deny_Second_Write := True;
         elsif Mode in "write-only" | "partial-write-only" then IO.Deny_Reads := True;
         elsif Mode = "stale" then Targets (1).Epoch := 2; Targets (2).Epoch := 2;
         elsif Mode in "unknown" | "partial-unknown" then IO.Arm_Unknown_Second_Write := True;
         elsif Mode = "drift" then IO.Drift_After_First_Write := True;
         end if;
         if Mode = "default" then C.Execute_Frozen (Service, 10000, Report, Status);
         else F.Execute_Frozen (Service, IO, Targets, Count, 10000, Report, Status); end if;
         Check (Status = C.Values.Available and then Report.Modules.Charged <= 10000, "bounded report");
         if Mode in "wide" | "partial" | "write-only" then
            Check (Report.Modules.Outcome = C.Values.Modules_Completed, "multi-cell module completed");
            Check (IO.First = (if Partial then 16#B5# else 16#CD#)
              and then IO.Second = (if Partial then 16#EA# else 16#AB#), "exact bytes and preserved neighbors");
            Check (IO.Calls = (if Partial then 6 elsif Mode = "write-only" then 2 else 4), "exact datum accesses");
            C.Initialize_Members (Service, Initialized, Status);
            Check (Status = C.Values.Available, "namespace sealed before metadata readback");
            C.Find_Child (Service, C.Namespace.Root, "OUT0", Node, Status);
            Check (Status = C.Values.Available, "ordinary result node");
            C.Describe_Node (Service, Node, Item, Status); Check (Status = C.Values.Available, "result identity");
            C.Evaluate_Named (Service, Item, 1000, Sample, Status);
            Check (Status = C.Values.Available and then Sample.Status = E.Returned
              and then Sample.Number = (if Mode = "write-only" then 0 elsif Partial then 16#AB# else 16#ABCD#), "scalar readback");
         elsif Mode in "unknown" | "partial-unknown" then
            Check (Report.Modules.Outcome = C.Values.Module_Rejected and then Report.Modules.Failure = E.Field_Indeterminate, "unknown datum completion stops module");
            Check (Pending (IO) and then F.Uncertain (Service) = IO.Receipt
              and then IO.Calls = (if Partial then 4 else 2), "matching unknown receipt retained");
            Check (IO.First = (if Partial then 16#B5# else 16#CD#)
              and then IO.Second = (if Partial then 16#EA# else 16#AB#), "possible effects retained");
         elsif Mode = "drift" then
            Check (Report.Modules.Outcome = C.Values.Module_Rejected and then IO.Calls = 1,
              "second datum geometry rechecked after first effect");
            Check (IO.First = 16#CD# and then IO.Second = 16#E0#, "known partial effect not rolled back");
         else
            Check (Report.Modules.Outcome = C.Values.Module_Rejected and then IO.Calls = 0,
              "missing ambiguous denied stale forged default association refuses before IO");
            Check (IO.First = 5 and then IO.Second = 16#E0#, "refusal preserves backing");
         end if;
         Before := IO.Calls;
         F.Execute_Frozen (Service, IO, Targets, Count, 10000, Report, Status);
         Check (Status /= C.Values.Available and then IO.Calls = Before, "cannot replay completed or failed module");
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Module datum checks" & Checks'Image);
end Module_Datum_Tests;
