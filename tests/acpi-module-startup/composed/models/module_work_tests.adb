pragma Ada_2022;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
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
procedure Module_Work_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Retained_Result_Capacity => 4);
   package A renames AML_Field_Access;
   package E renames AML_Execute;
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status, C.Values.Module_Outcome;
   use type E.Execution_Status, AML_Identity.Identity, A.Register_ID, A.Endpoint_ID, A.Observation;
   type Scenario is (Ordinary, Missing, Ambiguous, Stale, Selector_Denied,
      Data_Read_Denied, Data_Write_Denied, Geometry_Drift, Unknown_Write, Default_Path);
   Selected : constant Scenario := Scenario'Value (Ada.Command_Line.Argument (3));
   Expected_Selector : constant Unsigned_64 := Unsigned_64'Value (Ada.Command_Line.Argument (2));
   Selector_ID : constant A.Register_ID := 16#8000_0000_0000_0041#;
   Data_ID : constant A.Register_ID := 16#FFFF_FFFF_FFFF_FF97#;
   type Cell_Values is array (Natural range 0 .. 255) of Unsigned_64;
   type Context is limited record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Selector : Unsigned_64 := 0;
      Data : Cell_Values := [others => 0];
      Calls, Selector_Writes : Natural := 0;
      Receipt, Confirmed : A.Observation;
   end record;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Selected'Image & ": " & Why; end if; end Check;
   function Pending (IO : Context) return Boolean is (IO.Receipt.Present);
   procedure Describe (IO : Context; Target : A.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (Region => (Space => 3, Base => 0, Length => 2),
         Byte_Offset => (if Target.Register = Selector_ID or else
           (Selected = Geometry_Drift and then IO.Selector_Writes > 0) then 0 else 1),
         Width => ACPI_FADT.Transactions.Byte_Access);
      Accepted := not Pending (IO) and then Target.Provider = IO.Owner
        and then Target.Endpoint = 7 and then Target.Epoch = 1
        and then Target.Register in Selector_ID | Data_ID;
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target;
      For_Write : Boolean; Accepted : out Boolean) is
      Geometry : AML_Field_Layout.Register_Geometry;
   begin
      Describe (IO, Target, Geometry, Accepted);
      Accepted := Accepted and then not
        ((Selected = Selector_Denied and then For_Write and then Target.Register = Selector_ID)
         or else (Selected = Data_Read_Denied and then not For_Write and then Target.Register = Data_ID)
         or else (Selected = Data_Write_Denied and then For_Write and then Target.Register = Data_ID));
   end Authorize;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Allowed : Boolean;
   begin
      Authorize (IO, Item.Destination, Item.For_Write, Allowed);
      Check (Allowed, "no dispatch of an unauthorized target");
      IO.Calls := IO.Calls + 1;
      if Item.For_Write then
         if Item.Destination.Register = Selector_ID then
            Check (Item.Value <= 255, "selector fits the named cell");
            IO.Selector := Item.Value; IO.Selector_Writes := IO.Selector_Writes + 1;
         else IO.Data (Natural (IO.Selector)) := Item.Value; end if;
      end if;
      if Selected = Unknown_Write and then Item.For_Write and then Item.Destination.Register = Data_ID then
         IO.Receipt := (True, IO.Owner, Item, 1);
         Reply := (A.Indeterminate, IO.Owner, 0, 1);
      else
         Reply := (A.Completed, IO.Owner,
           (if Item.For_Write then 0 elsif Item.Destination.Register = Selector_ID
            then IO.Selector else IO.Data (Natural (IO.Selector))), A.No_Operation);
      end if;
   end Dispatch;
   procedure Confirm (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
   begin Accepted := Expected.Present and then not Pending (IO) and then IO.Confirmed = Expected; end Confirm;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirm,
      Authorize_Target => Authorize, Maximum_Bindings => 3, Maximum_Work => 1);
   Service : aliased C.State (1, 4096, 4096, Load_Capacity => 1);
   IO : Context;
   Targets : F.Module_Target_Set := [others => A.No_Target];
   Count : F.Module_Target_Count := 2;
   Report : C.Execution_Report;
   Status : C.Values.Access_Status;
   Good : Boolean;
begin
   declare
      package SIO renames Ada.Streams.Stream_IO;
      File : SIO.File_Type;
      Data : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
   begin
      SIO.Open (File, SIO.In_File, Ada.Command_Line.Argument (1));
      SIO.Read (File, Data, Last); SIO.Close (File);
      declare Image : AML_Decode.Bytes (1 .. Natural (Last)); begin
         for I in Image'Range loop Image (I) := Unsigned_8 (Data (Ada.Streams.Stream_Element_Offset (I))); end loop;
         C.Stage_Table (Service, 41, C.DSDT, Image, Staged); Check (Staged = C.Staged, "stage");
      end;
      C.Freeze_Catalog (Service, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
   end;
   AML_Identity.Issuer.Issue (IO.Owner, Good); Check (Good, "provider identity");
   IO.Data (Natural (Expected_Selector)) := 16#A5#;
   Targets (1) := (IO.Owner, 7, 1, Selector_ID); Targets (2) := (IO.Owner, 7, 1, Data_ID);
   if Selected = Missing then Count := 1; Targets (2) := A.No_Target;
   elsif Selected = Ambiguous then Targets (3) := Targets (2); Count := 3;
   elsif Selected = Stale then Targets (1).Epoch := 2; Targets (2).Epoch := 2; end if;
   if Selected = Default_Path then C.Execute_Frozen (Service, 10000, Report, Status);
   else F.Execute_Frozen (Service, IO, Targets, Count, 10000, Report, Status); end if;
   Check (Status = C.Values.Available and then Report.Modules.Charged <= 10000, "bounded module report");
   Check (Report.Modules.Outcome = C.Values.Module_Rejected
      and then Report.Modules.Failure = E.Value_Limit and then IO.Calls = 0
      and then IO.Selector = 0 and then IO.Data (Natural (Expected_Selector)) = 16#A5#,
      "planning consumes the field work allowance before effects");
   declare Before : constant Natural := IO.Calls; begin
      F.Execute_Frozen (Service, IO, Targets, Count, 10000, Report, Status);
      Check (Status /= C.Values.Available and then IO.Calls = Before, "cannot replay startup");
   end;
   Check (C.Valid (Service), "valid owner after startup");
   Ada.Text_IO.Put_Line ("Module work checks" & Checks'Image);
end Module_Work_Tests;
