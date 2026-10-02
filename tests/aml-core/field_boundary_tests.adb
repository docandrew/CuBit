pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service; use ACPI_Service;
with AML_Execute;
with Firmware_Tables;
procedure Field_Boundary_Tests is
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
   Full_Code : constant Bytes :=
     [16#5B#, 16#81#, 14] & Enc ("RGN0") & [1,0,16#40#,16#12#] &
     Enc ("FLD0") & [8,16#A4#] & Enc ("FLD0");
   procedure Exercise (Code : Bytes; Budget : Natural; Complete : Boolean) is
      Service : aliased State := Fresh;
      Status : Install_Status;
      Bound : Namespace.Bind_Status;
      Region : Namespace.Node_ID;
      R : AML_Execute.Execution_Result;
      Before : Namespace.Node_ID;
   begin
      Install (Service, 1, DSDT, Table ("DSDT",2,Method ("READ",Code)),Status);
      Check (Status = Installed);
      Install (Service, 2, Description, Table ("TEST",2,[16#AA#,16#BB#]),Status);
      Check (Status = Installed);
      Declare_Table_Region (Service, Namespace.Root, "RGN0", (Name => "TEST",others => <>), Region, Bound);
      Check (Bound = Namespace.Bound);
      Before := Namespace.Count (Snapshot (Service));
      Invoke (Service, Namespace.Child (Snapshot (Service),Namespace.Root,"READ"),
              [others => 0],0,Budget,R);
      Check (R.Charged <= Budget);
      Check (Namespace.Count (Snapshot (Service)) = Before);
      if not Complete then
         Check (R.Status not in AML_Execute.Returned | AML_Execute.Object_Returned);
      elsif R.Status = AML_Execute.Returned then
         Check (R.Value = 16#AA#);
      else
         Check (R.Status = AML_Execute.Budget_Exceeded);
      end if;
      if Budget = 0 then Check (R.Status = AML_Execute.Budget_Exceeded); end if;
      if Complete and Budget = 100 then Check (R.Status = AML_Execute.Returned); end if;
      -- Repeat on the same namespace: failed/truncated execution must not leave
      -- a dynamic field that causes Duplicate_Name on the next invocation.
      Invoke (Service, Namespace.Child (Snapshot (Service),Namespace.Root,"READ"),
              [others => 0],0,Budget,R);
      Check (R.Status /= AML_Execute.Duplicate_Name);
      Check (Namespace.Count (Snapshot (Service)) = Before);
   end Exercise;
begin
   for Prefix in 0 .. Full_Code'Length - 1 loop
      Exercise (Full_Code (1 .. Prefix),100,False);
   end loop;
   for Budget in 0 .. 100 loop Exercise (Full_Code,Budget,True); end loop;
   Ada.Text_IO.Put_Line ("AML field boundary checks:" & Checks'Image);
end Field_Boundary_Tests;
