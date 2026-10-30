pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Field_Access;
with ACPI_Service_Core;
with ACPI_Field_Hosted_Provider;
with ACPI_Region_Policy;
with ACPI_FADT.Transactions;
with Initializer_Oracle_Data;
procedure Initializer_Cached_Replay_Tests is
   package D renames Initializer_Oracle_Data;
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Retained_Result_Capacity => 1);
   package E renames AML_Execute;
   package A renames AML_Field_Access;
   package T renames ACPI_FADT.Transactions;
   subtype Bytes is AML_Decode.Bytes;
   use type AML_Decode.Bytes;
   use type E.Execution_Status;
   use type C.Values.Access_Status;
   use type C.Values.Value_Kind;
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
      if Space /= ACPI_Region_Policy.Memory_Space or else T.Octets (Width) /= 1 then
         raise Program_Error with "fixture bus geometry";
      end if;
      case Address is
         when 16#4000# => Slot := 1;
         when 16#4001# => Slot := 2;
         when 16#4002# => Slot := 3;
         when others => raise Program_Error with "fixture bus address";
      end case;
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
      Maximum_Bindings => 4);
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
         S : aliased C.State (1, 4096, 4096);
         IO : P.Context;
         Items : F.Binding_Set;
         Targets : array (Register_Index) of A.Target;
         Status : C.Values.Access_Status;
         Staged : C.Stage_Status;
         Frozen : C.Freeze_Status;
         Execution : C.Execution_Report;
         Members : C.Namespace.Initialization_Report;
         Node, System_Bus, Device : C.Namespace.Node_ID;
         Descriptor : C.Node_Descriptor;
         Description : C.Values.Field_Description;
         Key : C.Values.Field_Key;
         Value : C.Values.Result;
         Result_Description : C.Values.Value_Description;
         Buffer_Value : Bytes (1 .. 2);
         Copied : Natural;
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
         Find (C.Namespace.Root, "_SB_", System_Bus); Find (System_Bus, "DVA0", Device);
         for I in Register_Index loop
            declare
               Configuration : ACPI_Region_Policy.Configuration :=
                 (Tag => 17, Base => 16#4000#, Length => 16,
                  Space => ACPI_Region_Policy.Memory_Space, Readable => True, Writable => True,
                  Widths => [others => False],
                  Register_Authority => (Resource_ID => Unsigned_64 (IDs (I)),
                    Readable => True, Writable => True, Delegable => False, Write_Mask => 255),
                  Register_Offset => Unsigned_64 (I - Register_Index'First),
                  Register_Width => T.Byte_Access);
            begin
               Configuration.Widths (T.Byte_Access) := True;
               P.Install (IO, 7, Configuration, True, OK); Check (OK, "real catalog provision");
               Targets (I) := (P.Identity (IO), 7, 1, IDs (I));
               Find (C.Namespace.Root, Names (I), Node);
               C.Read_Field_Descriptor (S, Node, Description, Key, Status);
               Check (Status = C.Values.Available, "root field descriptor");
               F.Add_Binding (S, IO, Key, Targets (I), Items, Status);
               Check (Status = C.Values.Available, "root field binding");
            end;
         end loop;
         if D.Direct_Status (Case_ID) then
            Find (Device, "_STA", Node);
            C.Read_Field_Descriptor (S, Node, Description, Key, Status);
            Check (Status = C.Values.Available, "direct status descriptor");
            F.Add_Binding (S, IO, Key, Targets (1), Items, Status);
            Check (Status = C.Values.Available, "fourth binding on first register");
         end if;
         for Step in D.Step_Number loop
            declare
               Expected : constant D.Expectation := D.Expected (Case_ID, Step);
            begin
               Find ((if Expected.In_Device then Device else C.Namespace.Root), Expected.Name, Node);
               C.Describe_Node (S, Node, Descriptor, Status);
               Check (Status = C.Values.Available, "named descriptor");
               F.Evaluate_Named (S, IO, Items, Descriptor, 10000, Value, Status);
               Check (Status = C.Values.Available and then Value.Status = Expected.Status,
                 Expected.Name & " typed result " & Value.Status'Image);
               Check (Value.Charged <= 10000, "charged bound");
               if Expected.Status = E.Returned then
                  Check (Value.Number = Expected.Value, "actual reference scalar");
               elsif Expected.Status = E.Object_Returned then
                  C.Describe_Result (S, Value.Handle, Result_Description, Status);
                  Check (Status = C.Values.Available and then
                    Result_Description.Kind = C.Values.Buffer_Description and then
                    Result_Description.Length = 2, "actual reference buffer type and length");
                  C.Read_Result_Bytes (S, Value.Handle, 0, Buffer_Value, Copied, Status);
                  Check (Status = C.Values.Available and then Copied = 2 and then
                    Buffer_Value = Expected.Buffer_Value, "actual reference buffer bytes");
                  C.Release_Result (S, Value.Handle, Status);
                  Check (Status = C.Values.Available, "compound result released");
               end if;
               Check (C.Retained_Results (S) = 0, "no retained result leak");
            end;
         end loop;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Initializer cached replay checks" & Checks'Image);
end Initializer_Cached_Replay_Tests;
