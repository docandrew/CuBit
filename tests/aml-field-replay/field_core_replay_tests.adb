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
with Field_Oracle_Data;
procedure Field_Core_Replay_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Retained_Result_Capacity => 32);
   package T renames ACPI_FADT.Transactions;
   package E renames AML_Execute;
   package D renames Field_Oracle_Data;
   use type E.Execution_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Namespace.Lookup_Status;
   type Hardware is record
      Value : Unsigned_64 := 0;
      Reads, Writes : Natural := 0;
   end record;
   procedure Transact (H : in out Hardware;
      Space : ACPI_Region_Policy.Address_Space; Address : Unsigned_64;
      Width : T.Access_Width; For_Write : Boolean; Input : Unsigned_64;
      Output : out Unsigned_64; Completed : out Boolean) is
      use type ACPI_Region_Policy.Address_Space;
   begin
      -- Host-only adapter assertion, not an ACPICA transaction-count oracle.
      if Space /= ACPI_Region_Policy.Memory_Space or else Address /= 16#4000#
        or else T.Octets (Width) not in 1 | 2 | 4 then
         raise Program_Error with "unexpected admitted bus geometry";
      end if;
      Completed := True; Output := 0;
      if For_Write then H.Writes := H.Writes + 1; H.Value := Input;
      else H.Reads := H.Reads + 1; Output := H.Value; end if;
   end Transact;
   package P is new ACPI_Field_Hosted_Provider (Hardware, Transact);
   package F is new C.Field_IO
     (P.Context, P.Pending, P.Describe, P.Dispatch, P.Reconciled);
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
         S : aliased C.State (1, 4096, 4096);
         IO : P.Context;
         Item : F.Binding;
         Configuration : ACPI_Region_Policy.Configuration :=
           (Tag => 17, Base => 16#4000#, Length => Unsigned_64 (T.Octets (Width)),
            Space => ACPI_Region_Policy.Memory_Space, Readable => True, Writable => True,
            Widths => [others => False],
            Register_Authority => (Resource_ID => 9, Readable => True,
               Writable => True, Delegable => False,
               Write_Mask => 2 ** (8 * T.Octets (Width)) - 1),
            Register_Offset => 0, Register_Width => Width);
         Target : AML_Field_Access.Target := (Endpoint => 7, Epoch => 1, Register => 9, others => <>);
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
         Reads, Writes : Natural := 0;
      begin
         C.Stage_Table (S, 41, C.DSDT, D.Table_Bytes (Case_ID), Staged);
         Check (Staged = C.Staged, "stage");
         C.Freeze_Catalog (S, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
         C.Execute_Frozen (S, 10000, Report, Status);
         Check (Status = C.Values.Available and then Report.Modules.Outcome = C.Values.Modules_Completed, "module");
         C.Initialize_Members (S, Members, Status);
         Check (Status = C.Values.Available, "member initialization");
         C.Find_Object (S, C.Namespace.Root, "FLD0", Found, Status);
         Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found, "field");
         C.Read_Field_Descriptor (S, Found.Node, Description, Key, Status);
         Check (Status = C.Values.Available, "descriptor");
         Check (Description.Region.Address = 16#4000# and then
                Description.Region.Length = Configuration.Length, "copied region geometry");
         Configuration.Widths (Width) := True;
         P.Install (IO, 7, Configuration, True, OK); Check (OK, "trusted install");
         Target.Provider := P.Identity (IO);
         F.Bind (S, IO, Key, Target, Item, Status); Check (Status = C.Values.Available, "bind");
         for Step in D.Step_Number loop
            declare
               Expected : constant D.Expectation := D.Expected (Case_ID, Step);
            begin
               C.Find_Object (S, C.Namespace.Root,
                  [Expected.Name (1), Expected.Name (2), Expected.Name (3), Expected.Name (4)], Found, Status);
               Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found, "method");
               F.Invoke_Scalar (S, IO, Item, Found.Node, [others => 0], 0, 10000, Result);
               Check (Result.Status = Expected.Status,
                  "case" & Case_ID'Image & " step" & Step'Image & " actual " & Result.Status'Image);
               Check (Result.Charged <= 10000, "budget");
               Check (C.Retained_Results (S) = 0, "retained cleanup");
               if Expected.Status = E.Returned then
                  Check (Result.Status = E.Returned and then Result.Value = Expected.Value, "reference scalar");
               end if;
               Check (P.Observe_Hardware (IO).Value = D.Stored_Value (Case_ID), "retained register including failed Store");
               if Expected.Name = "TEST" then Writes := Writes + 1; Reads := Reads + 1;
               elsif Expected.Name = "PROB" then Reads := Reads + 1; end if;
               Check (P.Observe_Hardware (IO).Reads = Reads and then
                      P.Observe_Hardware (IO).Writes = Writes, "host-only dispatch count");
               Check (not P.Pending (IO) and then not F.Uncertain (S).Present, "no unknown completion");
            end;
         end loop;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Field Core replay checks" & Checks'Image);
end Field_Core_Replay_Tests;
