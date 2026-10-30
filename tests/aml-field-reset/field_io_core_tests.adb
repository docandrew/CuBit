pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with AML_Namespace;
with ACPI_Field_Hosted_Provider;
with ACPI_Region_Policy;
with ACPI_FADT.Transactions;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Field_Access;
with AML_References;
with AML_Table_Backing;
with Root_Test_Support; use Root_Test_Support;
procedure Field_IO_Core_Tests is
   package N is new AML_Namespace (Perform_Delay => AML_Delays.Unavailable_Provider,
      Capacity => 64, Max_Retained_Roots => 32);
   package V is new N.Owned.Collecting;
   package T renames ACPI_FADT.Transactions;
   package E renames AML_Execute;
   subtype Bytes is AML_Decode.Bytes;
   use type Bytes;
   use type E.Execution_Status;
   use type V.Access_Status;
   use type V.Module_Outcome;
   use type AML_Field_Access.Observation;
   use type AML_References.Reference;
   type Hardware is record
      Value : Unsigned_64 := 16#5A#;
      Reads, Writes : Natural := 0;
   end record;
   procedure Transact (H : in out Hardware;
      Space : ACPI_Region_Policy.Address_Space; Address : Unsigned_64;
      Width : T.Access_Width; For_Write : Boolean; Input : Unsigned_64;
      Output : out Unsigned_64; Completed : out Boolean) is
      pragma Unreferenced (Space, Address);
   begin
      Completed := True; Output := 0;
      if For_Write then
         H.Writes := H.Writes + 1; H.Value := Input;
         -- E1 models a real submitted write without terminal evidence.
         if Input = 16#E1# then Completed := False; end if;
      else
         H.Reads := H.Reads + 1;
         if H.Value = 16#E2# then
            Output := 2 ** (8 * T.Octets (Width));
         else Output := H.Value; end if;
      end if;
   end Transact;
   package P is new ACPI_Field_Hosted_Provider (Hardware, Transact);
   package VF is new V.Field_IO
     (P.Context, P.Pending, P.Describe, P.Dispatch, P.Reconciled, Authorize_Target => P.Authorize);
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Why & Checks'Image; end if;
   end Check;
   function Table (Revision : Unsigned_8; Payload : Bytes) return Bytes is
      Data : Bytes (1 .. 36 + Payload'Length) := [others => 0];
      Sum : Unsigned_8 := 0;
   begin
      Data (1 .. 4) := Enc ("DSDT"); Data (9) := Revision;
      for I in 0 .. 3 loop
         Data (5 + I) := Unsigned_8 (Shift_Right (Unsigned_32 (Data'Length), I * 8) and 255);
      end loop;
      Data (37 .. Data'Last) := Payload;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum; return Data;
   end Table;
   function Payload (Width : T.Access_Width) return Bytes is
      Flag : constant Unsigned_8 := Unsigned_8 (T.Access_Width'Pos (Width) + 1);
   begin
      return [16#5B#,16#80#] & Enc ("REG0") & [0,16#0C#,0,16#10#,0,0,16#0A#,4]
        & Block ([16#5B#,16#81#], Enc ("REG0") & [Flag] & Enc ("FLD0")
                 & [Unsigned_8 (8 * T.Octets (Width))])
        & Method ("READ", [16#A4#] & Enc ("FLD0"))
        & Method ("NEST", [16#A4#] & Enc ("READ"))
        & Method ("DREF", [16#A4#,16#83#,16#71#] & Enc ("FLD0"))
        & Block ([16#14#], Enc ("WRIT") & [1,16#70#,16#68#] & Enc ("FLD0") & [16#A4#,0])
        & Block ([16#14#], Enc ("REFW") & [1,16#70#,16#71#] & Enc ("FLD0")
                 & [16#60#,16#70#,16#68#,16#83#,16#60#,16#A4#,0])
        & Method ("BADV", [16#70#] & Block ([16#11#], [0]) & Enc ("FLD0") & [16#A4#,0])
        & Method ("ERRW", [16#70#,16#0A#,16#33#] & Enc ("FLD0") & [16#A4#,16#78#,1,0,0,0])
        & Method ("UNKN", [16#70#,16#0A#,16#E1#] & Enc ("FLD0")
                 & [16#70#,1] & Enc ("FLD0") & [16#A4#,0]);
   end Payload;
   procedure Reset_Case (Revision : Unsigned_8) is
      Data : constant Bytes := Table (Revision, Payload (T.Byte_Access));
      Input : aliased AML_Table_Backing.State (1, Data'Length);
      A : V.Arena;
      IO : P.Context;
      Configuration : constant ACPI_Region_Policy.Configuration :=
        (Tag => 17, Base => 16#1000#, Length => 4,
         Space => ACPI_Region_Policy.Memory_Space, Readable => True, Writable => True,
         Widths => [T.Byte_Access => True, others => False],
         Register_Authority => (Resource_ID => 9, Readable => True, Writable => True,
                               Delegable => False, Write_Mask => 255),
         Register_Offset => 0, Register_Width => T.Byte_Access);
      Target : AML_Field_Access.Target := (Endpoint => 7, Epoch => 1, Register => 9, others => <>);
      Binding : VF.Binding;
      Key : V.Field_Key;
      Description : V.Field_Description;
      Status : V.Access_Status;
      Report : V.Module_Report;
      Init : N.Initialization_Report;
      Node, Field : N.Node_ID;
      Result : V.Result;
      Saved, Wrong : AML_Field_Access.Observation;
      Field_Ref, Region_Ref : AML_References.Reference;
      OK : Boolean;
   begin
      Input.Count := 1; Input.Tables (1) := (0, Data'Length); Input.Data.Bytes := Data;
      V.Initialize (A, Status); Check (Status = V.Available, "reset case initialize");
      V.Execute_Modules (A, Input, 10000, Report, Status);
      Check (Status = V.Available and then Report.Outcome = V.Modules_Completed, "reset case module");
      V.Seal (A, Init, Status); Check (Status = V.Available, "reset case seal");
      V.Find_Child (A, N.Root, "FLD0", Field, Status);
      Check (Status = V.Available, "reset field");
      V.Read_Field_Descriptor (A, Field, Description, Key, Status);
      Check (Status = V.Available, "reset descriptor");
      P.Install (IO, 7, Configuration, True, OK); Check (OK, "reset provider install");
      Target.Provider := P.Identity (IO);
      VF.Bind (A, IO, Key, Target, Binding, Status); Check (Status = V.Available, "reset binding");
      V.Find_Child (A, N.Root, "UNKN", Node, Status);
      Check (Status = V.Available, "reset method");
      VF.Invoke (A, IO, Binding, Input, Node, [others => (V.Immediate_Argument, 0)],
                 0, 1000, Result, Status);
      Check (Status = V.Available and then Result.Status = E.Field_Indeterminate, "reset unknown");
      Saved := VF.Uncertain (A); Field_Ref := VF.Original_Field (A); Region_Ref := VF.Original_Region (A);
      V.Reset (A, Status); Check (Status = V.Available, "unrelated metadata reset");
      Check (VF.Uncertain (A) = Saved and then VF.Original_Field (A) = Field_Ref
             and then VF.Original_Region (A) = Region_Ref, "reset preserves attribution");
      Check (not V.Field_Key_Matches (A, Key), "old field key stale");
      Wrong := Saved; Wrong.Operation := AML_Field_Access.No_Operation;
      P.Confirm_Completion (IO, Wrong, True, OK); Check (not OK and then P.Pending (IO), "wrong receipt");
      P.Confirm_Completion (IO, Saved, True, OK); Check (OK and then not P.Pending (IO), "post reset terminal");
      VF.Reconcile (A, IO, Saved, Status); Check (Status = V.Available, "historical reconcile");
      Check (not VF.Uncertain (A).Present and then not V.Field_Key_Matches (A, Key), "reconcile no resurrection");
      VF.Reconcile (A, IO, Saved, Status); Check (Status /= V.Available, "post reset replay");
      V.Execute_Modules (A, Input, 10000, Report, Status);
      Check (Status = V.Available and then Report.Outcome = V.Modules_Completed, "fresh module after reconciliation");
      V.Seal (A, Init, Status); Check (Status = V.Available, "fresh seal");
      P.Install (IO, 7, Configuration, True, OK); Check (OK, "fresh policy epoch");
      Target.Epoch := 2;
      VF.Bind (A, IO, Key, Target, Binding, Status); Check (Status /= V.Available, "old key still denied");
   end Reset_Case;
begin
   for Revision in Unsigned_8 range 1 .. 2 loop Reset_Case (Revision); end loop;
   Ada.Text_IO.Put_Line ("Field IO Core checks" & Checks'Image);
end Field_IO_Core_Tests;
