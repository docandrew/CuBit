pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service_Core;
with ACPI_Field_Hosted_Provider;
with ACPI_Region_Policy;
with ACPI_FADT.Transactions;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Field_Access;
with AML_References;
with Root_Test_Support; use Root_Test_Support;
procedure Field_Deref_Unknown_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Retained_Result_Capacity => 32);
   package T renames ACPI_FADT.Transactions;
   package E renames AML_Execute;
   subtype Bytes is AML_Decode.Bytes;
   use type Bytes;
   use type E.Execution_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Namespace.Lookup_Status;
   use type AML_Field_Access.Observation;
   use type AML_References.Reference;
   type Hardware is record
      Value : Unsigned_64 := 16#5A#;
      Reads, Writes : Natural := 0;
   end record;
   Arm_Read_Unknown : Boolean := False;
   procedure Transact (H : in out Hardware;
      Space : ACPI_Region_Policy.Address_Space; Address : Unsigned_64;
      Width : T.Access_Width; For_Write : Boolean; Input : Unsigned_64;
      Output : out Unsigned_64; Completed : out Boolean) is
      pragma Unreferenced (Space, Address, Width);
   begin
      Completed := True; Output := 0;
      if For_Write then
         H.Writes := H.Writes + 1; H.Value := Input;
      else
         H.Reads := H.Reads + 1; Output := H.Value;
         if Arm_Read_Unknown then
            Arm_Read_Unknown := False; Completed := False;
         end if;
      end if;
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
        & Method ("DREF", [16#70#,16#83#,16#71#] & Enc ("FLD0")
                 & [16#60#,16#70#,16#0A#,16#44#] & Enc ("FLD0") & [16#A4#,16#60#])
        & Block ([16#14#], Enc ("WRIT") & [1,16#70#,16#68#] & Enc ("FLD0") & [16#A4#,0])
        & Block ([16#14#], Enc ("REFW") & [1,16#70#,16#71#] & Enc ("FLD0")
                 & [16#60#,16#70#,16#68#,16#83#,16#60#,16#A4#,0])
        & Method ("BADV", [16#70#] & Block ([16#11#], [0]) & Enc ("FLD0") & [16#A4#,0])
        & Method ("ERRW", [16#70#,16#0A#,16#33#] & Enc ("FLD0") & [16#A4#,16#78#,1,0,0,0])
        & Method ("UNKN", [16#70#,16#0A#,16#E1#] & Enc ("FLD0")
                 & [16#70#,1] & Enc ("FLD0") & [16#A4#,0]);
   end Payload;
begin
   for Revision in Unsigned_8 range 1 .. 2 loop
      for Width in T.Access_Width range T.Byte_Access .. T.Dword_Access loop
         declare
            S : aliased C.State (1, 4096, 4096);
            IO, Other : P.Context;
            Item : F.Binding;
            Configuration : ACPI_Region_Policy.Configuration :=
              (Tag => 17, Base => 16#1000#, Length => 4,
               Space => ACPI_Region_Policy.Memory_Space,
               Readable => True, Writable => True,
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
            Saved : AML_Field_Access.Observation;
            Before : Hardware;
            procedure Run (Name : String; Expected : E.Execution_Status;
                           Value : Unsigned_64 := 0; Budget : Natural := 1000) is
            begin
               C.Find_Object (S, C.Namespace.Root, [Name (Name'First), Name (Name'First + 1), Name (Name'First + 2), Name (Name'First + 3)], Found, Status);
               Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found, "method");
               F.Invoke_Scalar (S, IO, Item, Found.Node, [0 => Value, others => 0],
                                (if Name in "WRIT" | "REFW" then 1 else 0), Budget, Result);
               Check (Result.Status = Expected, Name & " status " & Result.Status'Image);
               Check (C.Retained_Results (S) = 0, "retained cleanup");
            end Run;
         begin
            C.Stage_Table (S, 1, C.DSDT, Table (Revision, Payload (Width)), Staged);
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
            Configuration.Widths (Width) := True;
            P.Install (IO, 7, Configuration, True, OK); Check (OK, "trusted install");
            Target.Provider := P.Identity (IO);
            F.Bind (S, IO, Key, Target, Item, Status); Check (Status = C.Values.Available, "bind");
            P.Install (Other, 7, Configuration, True, OK); Check (OK, "independent context");
            Run ("DREF", E.Returned); Check (Result.Value = 16#5A#, "ordinary dereference");
            Before := P.Observe_Hardware (IO);
            Arm_Read_Unknown := True;
            Run ("DREF", E.Field_Indeterminate);
            Check (not Arm_Read_Unknown, "submitted read consumed explicit fault mode");
            Check (P.Observe_Hardware (IO).Reads = Before.Reads + 1 and then
                   P.Observe_Hardware (IO).Writes = Before.Writes, "one unknown read no write");
            Saved := F.Uncertain (S);
            Check (Saved.Present and then P.Pending (IO), "read pending observation");
            Check (F.Original_Field (S) /= AML_References.No_Reference and then
                   F.Original_Region (S) /= AML_References.No_Reference, "historical identities");
            Run ("WRIT", E.Field_Indeterminate, 16#44#);
            Run ("DREF", E.Field_Indeterminate);
            Check (P.Observe_Hardware (IO).Reads = Before.Reads + 1 and then
                   P.Observe_Hardware (IO).Writes = Before.Writes, "no replay or later write");
            C.Find_Object (S, C.Namespace.Root, "DREF", Found, Status);
            C.Invoke_Scalar (S, Found.Node, [others => 0], 0, 1000, Result);
            Check (Result.Status = E.Field_Indeterminate, "default invoke preserves unknown");
            F.Invoke_Scalar (S, Other, Item, Found.Node, [others => 0], 0, 1000, Result);
            Check (Result.Status = E.Field_Indeterminate and then
                   P.Observe_Hardware (Other).Reads = 0 and then
                   P.Observe_Hardware (Other).Writes = 0, "context substitution cannot replay");
            Check (F.Uncertain (S) = Saved and then P.Pending (IO), "receipt remains latched");
            Check (C.Retained_Results (S) = 0, "failure cleanup");
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Field Deref unknown checks" & Checks'Image);
end Field_Deref_Unknown_Tests;
