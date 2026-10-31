pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service_Core;
with ACPI_Field_Hosted_Provider;
with ACPI_Region_Policy;
with ACPI_FADT.Transactions;
with AML_Delays;
with AML_Execute;
with AML_Field_Access;
with AML_References;
with Multiple_Oracle_Data;
procedure Multiple_Core_Replay_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Retained_Result_Capacity => 32);
   package T renames ACPI_FADT.Transactions;
   package E renames AML_Execute;
   package A renames AML_Field_Access;
   package R renames AML_References;
   package D renames Multiple_Oracle_Data;
   use type E.Execution_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Namespace.Lookup_Status;
   use type A.Observation;
   use type A.Target;
   use type R.Node_Position;
   type Register_Index is range 1 .. 3;
   type Register_Values is array (Register_Index) of Unsigned_64;
   type Register_Counts is array (Register_Index) of Natural;
   type Event is record
      Slot : Register_Index := 1;
      Write : Boolean := False;
      Value : Unsigned_64 := 0;
   end record;
   subtype Trace_Count is Natural range 0 .. 64;
   type Events is array (Positive range 1 .. Trace_Count'Last) of Event;
   type Hardware is record
      Values : Register_Values := [others => 0];
      Reads, Writes : Register_Counts := [others => 0];
      Count : Trace_Count := 0;
      Trace : Events;
   end record;
   type Controller_Mode is (Ordinary, Unknown_Second_Write);
   Mode : Controller_Mode := Ordinary;
   procedure Transact (H : in out Hardware;
      Space : ACPI_Region_Policy.Address_Space; Address : Unsigned_64;
      Width : T.Access_Width; For_Write : Boolean; Input : Unsigned_64;
      Output : out Unsigned_64; Completed : out Boolean) is
      use type ACPI_Region_Policy.Address_Space;
      Slot : Register_Index;
      Size : constant Unsigned_64 := Unsigned_64 (T.Octets (Width));
   begin
      -- Trusted hosted bus geometry, independent of opaque register IDs.
      if Space /= ACPI_Region_Policy.Memory_Space
        or else T.Octets (Width) not in 1 | 2 | 4 then
         raise Program_Error with "unexpected bus space or width";
      end if;
      if Address = 16#4000# then Slot := 1;
      elsif Address = 16#4000# + Size then Slot := 2;
      elsif Address = 16#4000# + 2 * Size then Slot := 3;
      else raise Program_Error with "unexpected bus address";
      end if;
      Output := 0;
      if For_Write then
         H.Writes (Slot) := H.Writes (Slot) + 1;
         H.Values (Slot) := Input;
      else
         H.Reads (Slot) := H.Reads (Slot) + 1;
         Output := H.Values (Slot);
      end if;
      H.Count := H.Count + 1;
      H.Trace (H.Count) := (Slot, For_Write, (if For_Write then Input else Output));
      Completed := True;
      if Mode = Unknown_Second_Write and then For_Write and then Slot = 2 then
         Mode := Ordinary;
         Completed := False;
      end if;
   end Transact;
   package P is new ACPI_Field_Hosted_Provider (Hardware, Transact,
      Maximum_Registers => 3, Maximum_Confirmations => 2);
   package F is new C.Field_IO
     (P.Context, P.Pending, P.Describe, P.Dispatch, P.Reconciled, Authorize_Target => P.Authorize,
      Maximum_Bindings => 3);
   IDs : constant array (Register_Index) of A.Register_ID :=
     [16#8000_0000_0000_0041#, A.Register_ID'Last, 16#C000_0000_0000_0097#];
   Names : constant array (Register_Index) of D.Method_Name := ["F001", "F002", "F003"];
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Why & Checks'Image; end if;
   end Check;
begin
   for Case_ID in D.Case_Number loop
      declare
         Width : constant T.Access_Width := D.Width (Case_ID);
         -- Values come from this table's recorded post-TEST/FAIL probes.
         First_Written : constant Unsigned_64 := D.Expected (Case_ID, 3).Value;
         Second_Written : constant Unsigned_64 := D.Expected (Case_ID, 4).Value;
         Third_Written : constant Unsigned_64 := D.Expected (Case_ID, 5).Value;
         Failed_First_Write : constant Unsigned_64 := D.Expected (Case_ID, 8).Value;
         Completed_Source_Marker : constant Unsigned_64 := D.Expected (Case_ID, 2).Value;
         S : aliased C.State (1, 4096, 4096, 1);
         IO, Other : P.Context;
         Items : F.Binding_Set;
         Targets : array (Register_Index) of A.Target;
         Nodes : array (Register_Index) of C.Namespace.Node_ID;
         Region_Node : C.Namespace.Node_ID;
         Key : C.Values.Field_Key;
         Description : C.Values.Field_Description;
         Status : C.Values.Access_Status;
         Staged : C.Stage_Status;
         Frozen : C.Freeze_Status;
         Report : C.Execution_Report;
         Members : C.Namespace.Initialization_Report;
         Found : C.Namespace.Lookup_Result;
         Result : E.Execution_Result;
         OK : Boolean;
         Before, After : Hardware;
         Saved : A.Observation;
         procedure Find (Name : String) is
         begin
            C.Find_Object (S, C.Namespace.Root,
              [Name (Name'First), Name (Name'First + 1),
               Name (Name'First + 2), Name (Name'First + 3)], Found, Status);
            Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found, "find " & Name);
         end Find;
         procedure Run (Name : String; Expected : E.Execution_Status) is
         begin
            Find (Name);
            F.Invoke_Scalar (S, IO, Items, Found.Node, [others => 0], 0, 10000, Result);
            Check (Result.Status = Expected, Name & " actual " & Result.Status'Image);
            Check (Result.Charged <= 10000, "fuel bound");
            Check (C.Retained_Results (S) = 0, "retained cleanup");
         end Run;
      begin
         C.Stage_Table (S, 41, C.DSDT, D.Table_Bytes (Case_ID), Staged);
         Check (Staged = C.Staged, "stage cached table");
         C.Freeze_Catalog (S, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
         C.Execute_Frozen (S, 10000, Report, Status);
         Check (Status = C.Values.Available and then Report.Modules.Outcome = C.Values.Modules_Completed, "module");
         C.Initialize_Members (S, Members, Status);
         Check (Status = C.Values.Available, "initialize");
         Find ("RGN0"); Region_Node := Found.Node;
         for I in Register_Index loop
            declare
               Configuration : ACPI_Region_Policy.Configuration :=
                 (Tag => 17, Base => 16#4000#, Length => 16,
                  Space => ACPI_Region_Policy.Memory_Space, Readable => True, Writable => True,
                  Widths => [others => False],
                  Register_Authority => (Resource_ID => Unsigned_64 (IDs (I)),
                    Readable => True, Writable => True, Delegable => False,
                    Write_Mask => 2 ** (8 * T.Octets (Width)) - 1),
                  Register_Offset => Unsigned_64 (I - Register_Index'First) * Unsigned_64 (T.Octets (Width)),
                  Register_Width => Width);
            begin
               Find (Names (I)); Nodes (I) := Found.Node;
               C.Read_Field_Descriptor (S, Found.Node, Description, Key, Status);
               Check (Status = C.Values.Available, "copied field descriptor");
               Check (Description.Region.Address = 16#4000# and then
                      Description.Region.Length = 16, "one shared region geometry");
               Configuration.Widths (Width) := True;
               P.Install (IO, 7, Configuration, True, OK); Check (OK, "install real policy");
               Targets (I) := (P.Identity (IO), 7, 1, IDs (I));
               F.Add_Binding (S, IO, Key, Targets (I), Items, Status);
               Check (Status = C.Values.Available, "add real binding");
               P.Install (Other, 7, Configuration, True, OK); Check (OK, "independent context");
            end;
         end loop;
         for Step in D.Step_Number loop
            declare
               Expected : constant D.Expectation := D.Expected (Case_ID, Step);
            begin
               Before := P.Observe_Hardware (IO);
               Run (Expected.Name, Expected.Status);
               if Expected.Status = E.Returned then
                  Check (Result.Value = Expected.Value, "cached ACPICA scalar");
               end if;
               After := P.Observe_Hardware (IO);
               -- Counts/trace below are hosted assertions, not ACPICA bus observations.
               if Expected.Name = "TEST" then
                  Check (After.Count = Before.Count + 6 and then
                    After.Trace (Before.Count + 1) = (1, True, First_Written) and then
                    After.Trace (Before.Count + 2) = (1, False, First_Written) and then
                    After.Trace (Before.Count + 3) = (2, True, Second_Written) and then
                    After.Trace (Before.Count + 4) = (3, True, Third_Written) and then
                    After.Trace (Before.Count + 5) = (1, False, First_Written) and then
                    After.Trace (Before.Count + 6) = (2, False, Second_Written), "TEST ordered three writes and reads");
               elsif Expected.Name = "FAIL" then
                  Check (After.Count = Before.Count + 1 and then
                    After.Trace (After.Count) = (1, True, Failed_First_Write), "FAIL first write only");
               elsif Expected.Name = "OBS0" then
                  Check (After = Before, "marker observation has no bus access");
               else
                  Check (After.Count = Before.Count + 1 and then
                    not After.Trace (After.Count).Write, "probe single read");
               end if;
               Check (not P.Pending (IO) and then not F.Uncertain (S).Present, "ordinary completion");
            end;
         end loop;
         Before := P.Observe_Hardware (IO);
         Mode := Unknown_Second_Write;
         Run ("TEST", E.Field_Indeterminate);
         After := P.Observe_Hardware (IO);
         Check (Mode = Ordinary, "explicit unknown mode consumed");
         Check (After.Count = Before.Count + 3 and then
           After.Trace (Before.Count + 1) = (1, True, First_Written) and then
           After.Trace (Before.Count + 2) = (1, False, First_Written) and then
           After.Trace (Before.Count + 3) = (2, True, Second_Written), "unknown second write ordered trace");
         Check (After.Values (1) = First_Written and then After.Values (2) = Second_Written
           and then After.Values (3) = Before.Values (3)
           and then After.Writes (3) = Before.Writes (3), "effects retained; no third write");
         Saved := F.Uncertain (S);
         Check (Saved.Present and then Saved = P.Uncertain (IO) and then
           Saved.Item.Destination = Targets (2) and then Saved.Item.For_Write
           and then Saved.Item.Value = Second_Written and then P.Pending (IO), "full selected target receipt");
         Check (R.Kind (F.Original_Field (S)) in R.Named_Cell | R.Name_Member and then
           R.Named_Node (F.Original_Field (S)) = R.Node_Position (Nodes (2)) and then
           R.Kind (F.Original_Region (S)) in R.Named_Cell | R.Name_Member and then
           R.Named_Node (F.Original_Region (S)) = R.Node_Position (Region_Node), "selected field and region attribution");
         Run ("TEST", E.Field_Indeterminate);
         C.Invoke_Scalar (S, Found.Node, [others => 0], 0, 10000, Result);
         Check (Result.Status = E.Field_Indeterminate, "default blocked");
         F.Invoke_Scalar (S, Other, Items, Found.Node, [others => 0], 0, 10000, Result);
         Check (Result.Status = E.Field_Indeterminate and then
           P.Observe_Hardware (Other).Count = 0, "context substitution blocked");
         Check (P.Observe_Hardware (IO) = After, "blocked calls never replay");
         P.Confirm_Completion (IO, Saved, True, OK); Check (OK, "exact trusted confirmation");
         Check (not P.Pending (IO) and then F.Uncertain (S) = Saved, "provider and arena facts separate");
         Run ("OBS0", E.Field_Indeterminate);
         F.Reconcile (S, IO, Saved, Status); Check (Status = C.Values.Available, "own arena reconcile");
         Check (not F.Uncertain (S).Present, "arena uncertainty cleared");
         P.Reconciled (IO, Saved, OK); Check (OK, "history before trusted retirement");
         P.Retire_Confirmation (IO, Saved, True, OK); Check (OK, "trusted history retirement");
         P.Reconciled (IO, Saved, OK); Check (not OK, "retired exact receipt absent");
         Run ("OBS0", E.Returned); Check (Result.Value = Completed_Source_Marker, "source effects before unknown retained");
         Check (P.Observe_Hardware (IO) = After, "confirmation reconciliation retirement never replay");
         Run ("PR02", E.Unsupported_Value);
         Check (P.Observe_Hardware (IO) = After, "selected policy remains revoked");
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Multiple Core replay checks" & Checks'Image);
end Multiple_Core_Replay_Tests;
