pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service; use ACPI_Service;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
procedure Field_Declaration_Tests is
   use type Namespace.Bind_Status;
   use type AML_Execute.Execution_Status;
   use type Firmware_Tables.Bytes;
   subtype Bytes is Firmware_Tables.Bytes;
   Checks : Natural := 0;
   procedure Check (B : Boolean) is
   begin
      Checks := Checks + 1;
      if not B then raise Program_Error with Checks'Image; end if;
   end Check;
   function Enc (S : String) return Bytes is
      B : Bytes (1 .. S'Length);
   begin
      for I in B'Range loop B (I) := Character'Pos (S (S'First + I - 1)); end loop;
      return B;
   end Enc;
   function Method (Name : String; Code : Bytes) return Bytes is
     ([16#14#, Unsigned_8 (6 + Code'Length)] & Enc (Name) & [0] & Code);
   function Table (Name : String; Revision : Unsigned_8; Data : Bytes) return Bytes is
      B : Bytes (1 .. 36 + Data'Length) := [others => 0];
      Sum : Unsigned_8 := 0;
   begin
      for I in 1 .. 4 loop
         B (I) := Character'Pos (Name (I));
         B (4 + I) := Unsigned_8 (Shift_Right (Unsigned_32 (B'Length), 8 * (I - 1)) and 255);
      end loop;
      B (9) := Revision;
      B (37 .. B'Last) := Data;
      for V of B loop Sum := Sum + V; end loop;
      B (10) := 0 - Sum;
      return B;
   end Table;
   function Field (Entries : Bytes; Flags : Unsigned_8 := 1) return Bytes is
     ([16#5B#, 16#81#, Unsigned_8 (6 + Entries'Length)] & Enc ("RGN0") & [Flags] & Entries);
   Skip_Header : constant Bytes := [0, 16#40#, 16#12#];
   Named : constant Bytes := Enc ("FLD0") & [8];
   Code : constant Bytes :=
     Method ("READ", Field (Skip_Header & Named) & [16#A4#] & Enc ("FLD0")) &
     Method ("TYPE", Field (Skip_Header & Named) & [16#A4#,16#8E#] & Enc ("FLD0")) &
     Method ("DUPE", Field (Skip_Header & Named & Named) & [16#A4#,0]) &
     Method ("BADF", Field (Skip_Header & Named, 16#80#) & [16#A4#,0]) &
     Method ("TRUN", Field (Skip_Header & Enc ("FLD0")) & [16#A4#,0]) &
     Method ("OVER", Field ([0,16#40#,16#20#] & Named) & [16#A4#,0]) &
     Method ("ACCS", Field (Skip_Header & [1,1,0] & Named) & [16#A4#] & Enc ("FLD0"));
   Service : aliased State := Fresh;
   Installed : Install_Status;
   Region : Namespace.Node_ID;
   Bound : Namespace.Bind_Status;
   R : AML_Execute.Execution_Result;
   Base_Count : Namespace.Node_ID;
   procedure Run (Name : String; Expected : AML_Execute.Execution_Status;
                  Value : AML_Decode.Integer_Value := 0; Budget : Natural := 1000) is
      Tree : constant Namespace.State := Snapshot (Service);
      Node : constant Namespace.Node_ID := Namespace.Child (Tree, Namespace.Root, Name);
   begin
      Check (Node /= Namespace.Root);
      Invoke (Service, Node, [others => 0], 0, Budget, R);
      Check (R.Status = Expected);
      Check (R.Charged <= Budget);
      if Expected = AML_Execute.Returned then Check (R.Value = Value); end if;
      Check (Namespace.Count (Snapshot (Service)) = Base_Count);
   end Run;
begin
   Install (Service, 1, DSDT, Table ("DSDT",2,Code), Installed);
   Check (Installed = ACPI_Service.Installed);
   Install (Service, 2, Description, Table ("TEST",2,[16#AA#,16#BB#]), Installed);
   Check (Installed = ACPI_Service.Installed);
   Declare_Table_Region (Service, Namespace.Root, "RGN0", (Name => "TEST", others => <>), Region, Bound);
   Check (Bound = Namespace.Bound);
   Base_Count := Namespace.Count (Snapshot (Service));
   for I in 1 .. 100 loop
      Run ("READ", AML_Execute.Returned, 16#AA#);
      Run ("TYPE", AML_Execute.Returned, 5);
      Run ("ACCS", AML_Execute.Returned, 16#AA#);
   end loop;
   Run ("DUPE", AML_Execute.Duplicate_Name);
   Run ("BADF", AML_Execute.Unsupported);
   Run ("TRUN", AML_Execute.Bad_Package);
   Run ("OVER", AML_Execute.Bad_Package);
   Run ("READ", AML_Execute.Budget_Exceeded, Budget => 1);
   Ada.Text_IO.Put_Line ("AML field declaration checks:" & Checks'Image);
end Field_Declaration_Tests;
