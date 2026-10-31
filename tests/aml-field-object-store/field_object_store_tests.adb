pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with AML_Delays;
with AML_Execute;
with AML_Field_Access;
with ACPI_Service_Core;
with ACPI_Field_Hosted_Provider;
with ACPI_Region_Policy;
with ACPI_FADT.Transactions;
with Field_Object_Store_Data;
procedure Field_Object_Store_Tests is
   package D renames Field_Object_Store_Data;
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Retained_Result_Capacity => 1);
   package E renames AML_Execute;
   package A renames AML_Field_Access;
   package T renames ACPI_FADT.Transactions;
   use type E.Execution_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   type Register_Index is range 1 .. 3;
   type Register_Values is array (Register_Index) of Unsigned_64;
   type Register_Counts is array (Register_Index) of Natural;
   type Hardware is record
      Values : Register_Values := [15, 0, 0];
      Reads, Writes : Register_Counts := [others => 0];
   end record;
   procedure Transact (H : in out Hardware;
      Space : ACPI_Region_Policy.Address_Space; Address : Unsigned_64;
      Width : T.Access_Width; For_Write : Boolean; Input : Unsigned_64;
      Output : out Unsigned_64; Completed : out Boolean) is
      use type ACPI_Region_Policy.Address_Space;
      Slot : Register_Index;
   begin
      if Space /= ACPI_Region_Policy.Memory_Space or else Width not in T.Byte_Access | T.Word_Access | T.Dword_Access then
         raise Program_Error with "fixture bus geometry";
      end if;
      if Address = 16#4000# then Slot := 1;
      elsif Address = 16#4000# + Unsigned_64 (T.Octets (Width)) then Slot := 2;
      elsif Address = 16#4000# + 2 * Unsigned_64 (T.Octets (Width)) then Slot := 3;
      else raise Program_Error with "fixture bus address"; end if;
      Output := 0;
      if For_Write then
         H.Values (Slot) := Input; H.Writes (Slot) := H.Writes (Slot) + 1;
      else
         Output := H.Values (Slot); H.Reads (Slot) := H.Reads (Slot) + 1;
      end if;
      Completed := True;
   end Transact;
   package P is new ACPI_Field_Hosted_Provider (Hardware, Transact,
      Maximum_Registers => 3, Maximum_Confirmations => 2);
   package F is new C.Field_IO
     (P.Context, P.Pending, P.Describe, P.Dispatch, P.Reconciled, Authorize_Target => P.Authorize,
      Maximum_Bindings => 3);
   IDs : constant array (Register_Index) of A.Register_ID :=
     [16#8000_0000_0000_0041#, A.Register_ID'Last, 16#C000_0000_0000_0097#];
   type Field_Names is array (Register_Index) of String (1 .. 4);
   Names : constant Field_Names := ["F001", "F002", "F003"];
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Why & Checks'Image; end if;
   end Check;
begin
   for Case_ID in D.Case_Number loop
      declare
         S : aliased C.State (1, 4096, 4096, 1);
         IO : P.Context;
         Items : F.Binding_Set;
         Targets : array (Register_Index) of A.Target;
         Status : C.Values.Access_Status;
         Staged : C.Stage_Status;
         Frozen : C.Freeze_Status;
         Execution : C.Execution_Report;
         Members : C.Namespace.Initialization_Report;
         Node : C.Namespace.Node_ID;
         Descriptor : C.Node_Descriptor;
         Description : C.Values.Field_Description;
         Key : C.Values.Field_Key;
         Value : C.Values.Result;
         OK : Boolean;
         procedure Find (Parent : C.Namespace.Node_ID; Name : String;
           Result : out C.Namespace.Node_ID) is
         begin
            C.Find_Child (S, Parent,
              [Name (Name'First), Name (Name'First + 1), Name (Name'First + 2), Name (Name'First + 3)],
              Result, Status);
            Check (Status = C.Values.Available, "find " & Name);
         end Find;
      begin
         C.Stage_Table (S, 41, C.DSDT, D.Table_Bytes (Case_ID), Staged);
         Check (Staged = C.Staged, "stage exact cached table");
         C.Freeze_Catalog (S, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
         C.Execute_Frozen (S, 10000, Execution, Status);
         Check (Status = C.Values.Available and then Execution.Modules.Outcome = C.Values.Modules_Completed, "modules");
         C.Initialize_Members (S, Members, Status); Check (Status = C.Values.Available, "members");
         for I in Register_Index loop
            declare
               Configuration : ACPI_Region_Policy.Configuration :=
                 (Tag => 17, Base => 16#4000#, Length => 16,
                  Space => ACPI_Region_Policy.Memory_Space, Readable => True, Writable => True,
                  Widths => [others => False],
                  Register_Authority => (Resource_ID => Unsigned_64 (IDs (I)),
                    Readable => True, Writable => True, Delegable => False, Write_Mask => 2 ** (8 * T.Octets (D.Access_Width_Of (Case_ID))) - 1),
                  Register_Offset => Unsigned_64 (I - Register_Index'First) * Unsigned_64 (T.Octets (D.Access_Width_Of (Case_ID))),
                  Register_Width => D.Access_Width_Of (Case_ID));
            begin
               Configuration.Widths (D.Access_Width_Of (Case_ID)) := True;
               P.Install (IO, 7, Configuration, True, OK); Check (OK, "real catalog provision");
               Targets (I) := (P.Identity (IO), 7, 1, IDs (I));
               Find (C.Namespace.Root, Names (I), Node);
               C.Read_Field_Descriptor (S, Node, Description, Key, Status);
               Check (Status = C.Values.Available, "root field descriptor");
               F.Add_Binding (S, IO, Key, Targets (I), Items, Status);
               Check (Status = C.Values.Available, "root field binding");
            end;
         end loop;
         for Step in D.Step_Number loop
            declare
               Expected : constant D.Expectation := D.Expected (Case_ID, Step);
            begin
               Find (C.Namespace.Root, Expected.Name, Node);
               C.Describe_Node (S, Node, Descriptor, Status);
               Check (Status = C.Values.Available, "named descriptor");
               F.Evaluate_Named (S, IO, Items, Descriptor, 10000, Value, Status);
               Check (Status = C.Values.Available and then Value.Status = Expected.Status,
                 Expected.Name & " typed result " & Value.Status'Image);
               Check (Value.Charged <= 10000, "charged bound");
               Check (Value.Number = Expected.Value, "actual reference scalar");
               Check (C.Retained_Results (S) = 0, "no retained result leak");
            end;
         end loop;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Field object store checks" & Checks'Image);
end Field_Object_Store_Tests;
