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
procedure Module_Field_Tests is
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
      Deny_Writes : Boolean := False;
      Calls : Natural := 0;
      Arm_Unknown_Second_Write : Boolean := False;
      Receipt, Confirmed : A.Observation;
   end record;
   function Pending (IO : Context) return Boolean is (IO.Receipt.Present);
   procedure Describe (IO : Context; Target : A.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (Region => (Space => 3, Base => 0, Length => 2),
         Byte_Offset => (if Target.Register = First_ID then 0 else 1),
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
      Accepted := Accepted and then not (For_Write and then IO.Deny_Writes);
   end Authorize;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
      Authorize_Target => Authorize, Maximum_Bindings => 2);
   use type C.Stage_Status, C.Freeze_Status;
   Checks : Natural := 0;
   Mode : constant String := Ada.Command_Line.Argument (1);
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   function Table (Revision : Unsigned_8) return Bytes is
      Region_Space : constant Unsigned_8 := (if Mode = "forged-space" then 0 else 3);
      Region_Base : constant Bytes := (if Mode = "forged-base" then [16#0A#,16#10#] else [0]);
      Prefix : constant Bytes := [16#5B#,16#80#] & Enc ("REG0") & [Region_Space] & Region_Base & [16#0A#,2]
        & Block ([16#5B#,16#81#], Enc ("REG0") & [1] & Enc ("FLD1") & [8] & Enc ("FLD2") & [8])
        & [16#70#,16#0A#,16#33#] & Enc ("FLD1");
      Suffix : constant Bytes := (if Mode = "fatal" then [16#5B#,16#32#,1,0,0,0,0,16#0A#,7]
        elsif Mode = "read" then [16#70#] & Enc ("FLD1") & Enc ("FLD2")
          & [16#70#,16#0A#,16#44#] & Enc ("FLD1")
        else [16#70#,16#0A#,16#E1#] & Enc ("FLD2") & [16#70#,16#0A#,16#44#] & Enc ("FLD1"));
      Payload : constant Bytes := Prefix & Suffix;
      Data : Bytes (1 .. 36 + Payload'Length) := [others => 0];
      Sum : Unsigned_8 := 0;
   begin
      Data (1 .. 4) := Enc ("DSDT"); Data (9) := Revision;
      for I in 0 .. 3 loop Data (5 + I) := Unsigned_8 (Shift_Right (Unsigned_32 (Data'Length), I * 8) and 255); end loop;
      Data (37 .. Data'Last) := Payload;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum; return Data;
   end Table;
begin
   for Revision in Unsigned_8 range 1 .. 2 loop
      declare
         Service : C.State (1, 4096, 4096, Load_Capacity => 1);
         Data : constant Bytes := Table (Revision);
         Staged : C.Stage_Status;
         Frozen : C.Freeze_Status;
         IO : Context;
         Targets : F.Module_Target_Set := [others => A.No_Target];
         Count : F.Module_Target_Count := 2;
         Report : C.Execution_Report;
         Status : C.Values.Access_Status;
         Good : Boolean;
         Before : Natural;
      begin
         C.Stage_Table (Service, 41, C.DSDT, Data, Staged);
         Check (Staged = C.Staged, "stage");
         C.Freeze_Catalog (Service, Frozen);
         Check (Frozen = C.Catalog_Frozen_OK, "freeze");
         AML_Identity.Issuer.Issue (IO.Owner, Good); Check (Good, "provider identity");
         Targets (1) := (Provider => IO.Owner, Endpoint => 7, Register => First_ID, Epoch => 1);
         Targets (2) := (Provider => IO.Owner, Endpoint => 7, Register => Second_ID, Epoch => 1);
         if Mode = "absent" then Targets := [others => A.No_Target]; Count := 0;
         elsif Mode = "ambiguous" then Targets (2) := Targets (1);
         elsif Mode = "unknown" then IO.Arm_Unknown_Second_Write := True;
         elsif Mode = "denied" then IO.Deny_Writes := True;
         elsif Mode = "stale" then Targets (1).Epoch := 2; Targets (2).Epoch := 2;
         end if;
         if Mode = "default" then
            C.Execute_Frozen (Service, 10000, Report, Status);
         else
            F.Execute_Frozen (Service, IO, Targets, Count, 10000, Report, Status);
         end if;
         Check (Status = C.Values.Available and then Report.Modules.Charged <= 10000, "bounded module execution report");
         if Mode = "normal" then
            Check (Report.Modules.Outcome = C.Values.Modules_Completed, "module completed");
            Check (IO.First = 16#44# and then IO.Second = 16#E1# and then IO.Calls = 3, "exact ordered writes");
         elsif Mode = "read" then
            Check (Report.Modules.Outcome = C.Values.Modules_Completed, "read module completed");
            Check (IO.First = 16#44# and then IO.Second = 16#33# and then IO.Calls = 4, "module read feeds named register write");
         elsif Mode = "unknown" then
            Check (Report.Modules.Outcome = C.Values.Module_Rejected and then Report.Modules.Failure = E.Field_Indeterminate, "uncertain module stopped");
            Check (IO.First = 16#33# and then IO.Second = 16#E1# and then IO.Calls = 2 and then Pending (IO)
              and then F.Uncertain (Service) = IO.Receipt, "effects and matching receipt retained");
         elsif Mode = "fatal" then
            Check (Report.Modules.Outcome = C.Values.Module_Rejected and then Report.Modules.Fatal.Present, "Fatal captured");
            Check (IO.First = 16#33# and then IO.Calls = 1, "Fatal cannot undo hardware write");
         else
            Check (Report.Modules.Outcome = C.Values.Module_Rejected and then IO.Calls = 0, "unprovisioned, forged, ambiguous, denied, stale or default cannot dispatch");
         end if;
         Before := IO.Calls;
         F.Execute_Frozen (Service, IO, Targets, Count, 10000, Report, Status);
         Check (Status /= C.Values.Available and then IO.Calls = Before, "same owner cannot replay a completed or failed module");
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Module field checks" & Checks'Image);
end Module_Field_Tests;
