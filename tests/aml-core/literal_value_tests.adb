pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service; use ACPI_Service;
with AML_Execute;
with AML_Objects;
with Firmware_Tables;
procedure Literal_Value_Tests is
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
   Text : constant Bytes := [16#0D#] & Enc ("DSDT") & [0];
   Buffer_Code : constant Bytes := [16#11#,7,16#0A#,4,1,2,3,4];
   Code : constant Bytes :=
     Method ("TEXT", [16#A4#] & Text) &
     Method ("EMPT", [16#A4#,16#0D#,0]) &
     Method ("BUFF", [16#A4#] & Buffer_Code) &
     Method ("ZERO", [16#A4#,16#11#,3,16#0A#,0]) &
     ([16#14#,8] & Enc ("ECHO") & [1,16#A4#,16#68#]) &
     Method ("NEST", [16#A4#] & Enc ("ECHO") & Text) &
     Method ("TYPE", [16#70#] & Text & [16#60#,16#A4#,16#8E#,16#60#]) &
     Method ("SIZE", [16#70#] & Buffer_Code & [16#60#,16#A4#,16#87#,16#60#]) &
     Method ("BIGN", [16#A4#,16#11#,5,16#0B#,0,4,0]);
   Service : aliased State := Fresh;
   Status : Install_Status;
   R : AML_Execute.Execution_Result;
   procedure Run (Name : String; Budget : Natural := 100) is
   begin
      Invoke (Service, Namespace.Child (Snapshot (Service),Namespace.Root,Name),
              [others => 0],0,Budget,R);
      Check (R.Charged <= Budget);
   end Run;
   procedure Expect (Name : String; Kind : Natural; Data : Bytes) is
   begin
      Run (Name);
      Check (R.Status = AML_Execute.Object_Returned);
      Check (R.Object.Type_Code = Kind);
      Check (R.Object.Size = Data'Length);
      Check (Bytes (AML_Objects.Byte_Data (Namespace.Value_Store (Snapshot (Service)),R.Object.ID)) = Data);
   end Expect;
begin
   Install (Service,1,DSDT,Table ("DSDT",2,Code),Status);
   Check (Status = Installed);
   Expect ("TEXT",2,Enc ("DSDT"));
   Expect ("EMPT",2,[]);
   Expect ("BUFF",3,[1,2,3,4]);
   Expect ("ZERO",3,[]);
   Expect ("NEST",2,Enc ("DSDT"));
   Run ("TYPE"); Check (R.Status = AML_Execute.Returned and then R.Value = 2);
   Run ("SIZE"); Check (R.Status = AML_Execute.Returned and then R.Value = 4);
   Run ("TEXT",0); Check (R.Status = AML_Execute.Budget_Exceeded);
   for I in 1 .. 100 loop
      Run ("BIGN");
      exit when R.Status = AML_Execute.Value_Limit;
      Check (R.Status = AML_Execute.Object_Returned and then R.Object.Size = 1024);
   end loop;
   Check (R.Status = AML_Execute.Value_Limit);
   Ada.Text_IO.Put_Line ("AML literal value checks:" & Checks'Image);
end Literal_Value_Tests;
