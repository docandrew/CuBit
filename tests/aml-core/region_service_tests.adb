pragma Ada_2022;
with ACPI_Test_Results;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service; use ACPI_Service;
with AML_Execute;

with Firmware_Tables;
procedure Region_Service_Tests is
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
   function Text (S : String) return Bytes is ([16#0D#] & Enc (S) & [0]);
   function Region (Sig : Bytes; OEM : Bytes := Text (""); ID : Bytes := Text ("")) return Bytes is
     ([16#5B#,16#88#] & Enc ("REG0") & Sig & OEM & ID);
   Good : constant Bytes := Region (Text ("DSDT"));
   Field_Code : constant Bytes := [16#5B#,16#81#,11] & Enc ("REG0") & [0] & Enc ("FLD0") & [8];
   Code : constant Bytes :=
     Method ("EMPT", Region (Text ("")) & [16#A4#,1]) &
     Method ("SHRT", Region (Text ("DSD")) & [16#A4#,1]) &
     Method ("BOTH", Region (Text ("bad!"),Text ("1234567")) & [16#A4#,1]) &
     Method ("LONG", Region (Text ("DSDTextra")) & [16#A4#,16#8E#] & Enc ("REG0")) &
     Method ("ZSIG", Region ([0]) & [16#A4#,1]) &
     Method ("ALRT", Region (Text ("ASF!")) & [16#A4#,16#8E#] & Enc ("REG0")) &
     Method ("DIGT", Region (Text ("1ABC")) & [16#A4#,16#8E#] & Enc ("REG0")) &
     Method ("LOWR", Region (Text ("asf!")) & [16#A4#,1]) &
     Method ("BANG", Region (Text ("AS!F")) & [16#A4#,1]) &
     Method ("GOOD", Good & [16#A4#,16#8E#] & Enc ("REG0")) &
     Method ("READ", Good & Field_Code & [16#A4#] & Enc ("FLD0")) &
     Method ("MISS", Region (Text ("NONE")) & [16#A4#,1]) &
     Method ("SELF", Region ([16#8E#] & Enc ("REG0")) & [16#A4#,1]) &
     Method ("DUPL", Good & Good & [16#A4#,1]) &
     Method ("MSEL", [16#A4#] & Text ("DSDT")) &
     Method ("DYNM", Region (Enc ("MSEL")) & [16#A4#,16#8E#] & Enc ("REG0")) &
     Method ("BUFF", Region (Text ("DSDT"), [16#11#,3,16#0A#,0]) & [16#A4#,1]) &
     Method ("INTG", Region (Text ("DSDT"), [0]) & [16#A4#,1]);
   Service : aliased State (Max_Tables, Max_Total_Bytes, Max_Table_Bytes);
   Status : Install_Status;
   R : AML_Execute.Execution_Result;
   Initial_Count : Namespace.Node_ID;
   procedure Run (Name : String; Budget : Natural := 100) is
   begin
      Invoke_Scalar (Service, ACPI_Test_Results.Child (Service,Namespace.Root,Name),
              [others => 0],0,Budget,R);
      Check (R.Charged <= Budget);
      Check (Observe (Service).Objects = Initial_Count);
   end Run;
begin
   Install (Service,1,DSDT,Table ("DSDT",2,Code),Status);
   Check (Status = Installed);
   Install (Service,2,Description,Table ("ASF!",1,[]),Status);
   Check (Status = Installed);
   Install (Service,3,Description,Table ("1ABC",1,[]),Status);
   Check (Status = Installed);
   ACPI_Test_Results.Seal (Service);
   Initial_Count := Observe (Service).Objects;
   for I in 1 .. 20 loop
      Run ("EMPT"); Check (R.Status = AML_Execute.Bad_Name);
      Run ("SHRT"); Check (R.Status = AML_Execute.Bad_Name);
      Run ("BOTH"); Check (R.Status = AML_Execute.Bad_Name);
      Run ("LONG"); Check (R.Status = AML_Execute.Returned and then R.Value = 10);
      Run ("ZSIG"); Check (R.Status = AML_Execute.Unknown_Name);
      Run ("ALRT"); Check (R.Status = AML_Execute.Returned and then R.Value = 10);
      Run ("DIGT"); Check (R.Status = AML_Execute.Returned and then R.Value = 10);
      Run ("LOWR"); Check (R.Status = AML_Execute.Bad_Name);
      Run ("BANG"); Check (R.Status = AML_Execute.Bad_Name);
      Run ("GOOD"); Check (R.Status = AML_Execute.Returned and then R.Value = 10);
      Run ("READ"); Check (R.Status = AML_Execute.Returned and then R.Value = Character'Pos ('D'));
      Run ("MISS"); Check (R.Status = AML_Execute.Unknown_Name);
      Run ("SELF"); Check (R.Status = AML_Execute.Uninitialized);
      Run ("DUPL"); Check (R.Status = AML_Execute.Duplicate_Name);
      Run ("DYNM"); Check (R.Status = AML_Execute.Returned and then R.Value = 10);
      Run ("BUFF"); Check (R.Status = AML_Execute.Returned and then R.Value = 1);
      Run ("INTG"); Check (R.Status = AML_Execute.Unsupported_Value);
   end loop;
   for Budget in 0 .. 50 loop
      Run ("READ", Budget);
      Check (R.Status in AML_Execute.Returned | AML_Execute.Budget_Exceeded);
   end loop;
   Ada.Text_IO.Put_Line ("DataTableRegion service checks:" & Checks'Image);
end Region_Service_Tests;
