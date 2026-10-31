pragma Ada_2022;
with Ada.Streams.Stream_IO;
with AML_Copied_Tables;
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
procedure Module_Copied_Field_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Retained_Result_Capacity => 32,
      Copied_Table_Capacity => 1, Copied_Byte_Capacity => 4096);
   package E renames AML_Execute;
   package A renames AML_Field_Access;
   subtype Bytes is AML_Decode.Bytes;
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
   use type AML_Copied_Tables.Outcome;
   Checks : Natural := 0;
   Mode : constant String := Ada.Command_Line.Argument (3);
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   function Read_File (Name : String) return Bytes is
      package SIO renames Ada.Streams.Stream_IO;
      File : SIO.File_Type;
      use type SIO.Count;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      SIO.Open (File, SIO.In_File, Name);
      if SIO.Size (File) > 4096 then raise Program_Error with "fixture too large"; end if;
      declare
         Data : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (SIO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset;
         Result : Bytes (1 .. Data'Length);
      begin
         SIO.Read (File, Data, Last); SIO.Close (File);
         if Last /= Data'Last then raise Program_Error with "short fixture"; end if;
         for I in Result'Range loop Result (I) := Unsigned_8 (Data (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Result;
      end;
   end Read_File;
begin
   for Revision in Unsigned_8 range 1 .. 2 loop
      declare
         Service : aliased C.State (1, 4096, 4096, Load_Capacity => 2);
         Data : constant Bytes := Read_File (Ada.Command_Line.Argument (Natural (Revision)));
         Copied : Bytes := Read_File (Ada.Command_Line.Argument (4));
         Copy_Report : AML_Copied_Tables.Report;
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
         C.Stage_Copied_Source (Service, Copied, 10000, Copy_Report, Status);
         Check (Status = C.Values.Available and then Copy_Report.Status = AML_Copied_Tables.Added, "copy source retained");
         Copied := [others => 0];
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
            Check (IO.First = 16#44# and then IO.Second = 16#E1# and then IO.Calls = 3, "exact ordered writes across copied LoadTable");
            declare
               Initial : C.Namespace.Initialization_Report;
               procedure Value_Is (Name : String; Expected : Unsigned_64) is
                  Node : C.Namespace.Node_ID;
                  Item : C.Node_Descriptor;
                  Value : C.Values.Result;
               begin
                  C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
                  Check (Status = C.Values.Available, "find " & Name);
                  C.Describe_Node (Service, Node, Item, Status);
                  Check (Status = C.Values.Available, "describe " & Name);
                  C.Evaluate_Named (Service, Item, 10000, Value, Status);
                  Check (Status = C.Values.Available and then Value.Status = E.Returned
                    and then Value.Number = Expected, "value " & Name);
               end Value_Is;
            begin
               C.Initialize_Members (Service, Initial, Status);
               Check (Status = C.Values.Available, "ready after copied Field module");
               Value_Is ("LRES", (if Revision = 1 then 16#FFFF_FFFF# else Unsigned_64'Last));
               declare
                  Node : C.Namespace.Node_ID;
                  Item : C.Node_Descriptor;
                  Value : C.Values.Result;
               begin
                  C.Find_Child (Service, C.Namespace.Root, "DUPL", Node, Status);
                  Check (Status = C.Values.Available, "find duplicate method");
                  C.Describe_Node (Service, Node, Item, Status);
                  Check (Status = C.Values.Available, "describe duplicate method");
                  C.Evaluate_Named (Service, Item, 10000, Value, Status);
                  Check (Status = C.Values.Available and then Value.Status = E.Duplicate_Name,
                    "duplicate copied load refused without replay");
               end;
               Check (IO.Calls = 3 and then C.Retained_Results (Service) = 0,
                 "duplicate load has no extra register effects or result roots");
            end;
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
   Ada.Text_IO.Put_Line ("Module copied field checks" & Checks'Image);
end Module_Copied_Field_Tests;
