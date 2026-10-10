pragma Ada_2022;
with ACPI_Test_Results;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service; use ACPI_Service;
with AML_Execute;
with AML_Objects;
with Firmware_Tables;
procedure Literal_Value_Tests is
   use type AML_Execute.Execution_Status;
   use type Values.Access_Status;
   use type Values.Value_Handle;
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
   Big_Buffer_Length : constant Positive := 1_024;
   Live_Buffer_Capacity : constant Positive := AML_Objects.Max_Bytes / Big_Buffer_Length;
   Released_Iterations : constant Positive := 100;
   pragma Compile_Time_Error
     (AML_Objects.Max_Bytes mod Big_Buffer_Length /= 0 or else
      Live_Buffer_Capacity >= Max_Namespace_Nodes or else
      Live_Buffer_Capacity >= AML_Objects.Max_Objects,
      "literal quota fixture must exhaust bytes before pins or object slots");
   -- The AML WordConst below is the same length used by the quota assertions.
   Code : constant Bytes :=
     Method ("TEXT", [16#A4#] & Text) &
     Method ("EMPT", [16#A4#,16#0D#,0]) &
     Method ("BUFF", [16#A4#] & Buffer_Code) &
     Method ("ZERO", [16#A4#,16#11#,3,16#0A#,0]) &
     ([16#14#,8] & Enc ("ECHO") & [1,16#A4#,16#68#]) &
     Method ("NEST", [16#A4#] & Enc ("ECHO") & Text) &
     Method ("TYPE", [16#70#] & Text & [16#60#,16#A4#,16#8E#,16#60#]) &
     Method ("SIZE", [16#70#] & Buffer_Code & [16#60#,16#A4#,16#87#,16#60#]) &
     Method ("BIGN", [16#A4#,16#11#,5,16#0B#,
       Unsigned_8 (Big_Buffer_Length mod 256), Unsigned_8 (Big_Buffer_Length / 256), 0]);
   Service : aliased State (Max_Tables, Max_Total_Bytes, Max_Table_Bytes);
   Held : ACPI_Test_Results.Holder;
   Status : Install_Status;
   R : Values.Result;
   Pins : array (Positive range 1 .. Live_Buffer_Capacity) of Values.Value_Handle :=
     [others => Values.No_Value];
   Retention_Status : Values.Access_Status;
   procedure Drop_Pins (Verify : Boolean := True) is
   begin
      for Pin of Pins loop
         if Pin /= Values.No_Value then
            Release_Result (Service, Pin, Retention_Status);
            if Verify then Check (Retention_Status = Values.Available); end if;
         end if;
      end loop;
   end Drop_Pins;
   procedure Run (Name : String; Budget : Natural := 100) is
   begin
      ACPI_Test_Results.Invoke (Service, Held, ACPI_Test_Results.Child (Service,Namespace.Root,Name),
              [others => 0],0,Budget,R);
      Check (R.Charged <= Budget);
   end Run;
   procedure Expect (Name : String; Kind : Natural; Data : Bytes) is
   begin
      Run (Name);
      Check (R.Status = AML_Execute.Object_Returned);
      Check (Values.Value_Kind'Pos (ACPI_Test_Results.Describe (Service, R.Handle).Kind) + 1 = Kind);
      Check (ACPI_Test_Results.Describe (Service, R.Handle).Length = Data'Length);
      Check (Bytes (ACPI_Test_Results.Bytes (Service, R.Handle)) = Data);
   end Expect;
begin
   Install (Service,1,DSDT,Table ("DSDT",2,Code),Status);
   Check (Status = Installed);
   ACPI_Test_Results.Seal (Service);
   Expect ("TEXT",2,Enc ("DSDT"));
   Expect ("EMPT",2,[]);
   Expect ("BUFF",3,[1,2,3,4]);
   Expect ("ZERO",3,[]);
   Expect ("NEST",2,Enc ("DSDT"));
   Run ("TYPE"); Check (R.Status = AML_Execute.Returned and then R.Number = 2);
   Run ("SIZE"); Check (R.Status = AML_Execute.Returned and then R.Number = 4);
   Run ("TEXT",0); Check (R.Status = AML_Execute.Budget_Exceeded);
   -- Holder drops the previous result before each invocation. These results
   -- are dead, so repeated calls must reclaim them rather than exhaust storage.
   for I in 1 .. Released_Iterations loop
      Run ("BIGN");
      Check (R.Status = AML_Execute.Object_Returned and then
        ACPI_Test_Results.Describe (Service, R.Handle).Length = Big_Buffer_Length);
      Check (Observe (Service).Value_Objects <= 1 and then
        Observe (Service).Value_Bytes <= Big_Buffer_Length and then
        Observe (Service).Package_Elements = 0);
   end loop;
   ACPI_Test_Results.Drop (Service, Held);
   -- Prove this is byte exhaustion rather than pin or object exhaustion.
   for I in Pins'Range loop
      Invoke_Retained (Service, ACPI_Test_Results.Child (Service, Namespace.Root, "BIGN"),
        [others => 0], 0, 100, R, Retention_Status);
      Check (Retention_Status = Values.Available and then R.Status = AML_Execute.Object_Returned);
      Pins (I) := R.Handle;
      Check (Retained_Results (Service) = I and then
        Observe (Service).Value_Bytes = I * Big_Buffer_Length);
   end loop;
   Invoke_Retained (Service, ACPI_Test_Results.Child (Service, Namespace.Root, "BIGN"),
     [others => 0], 0, 100, R, Retention_Status);
   Check (Retention_Status = Values.Available and then R.Status = AML_Execute.Value_Limit
     and then R.Charged > 0 and then Retained_Results (Service) = Live_Buffer_Capacity
     and then Observe (Service).Value_Bytes = AML_Objects.Max_Bytes);
   for Pin of Pins loop
      Check (ACPI_Test_Results.Describe (Service, Pin).Length = Big_Buffer_Length);
      Check (Bytes (ACPI_Test_Results.Bytes (Service, Pin)) =
        Bytes'(1 .. Big_Buffer_Length => 0));
   end loop;
   Drop_Pins;
   Check (Retained_Results (Service) = 0);
   Run ("BIGN");
   Check (R.Status = AML_Execute.Object_Returned and then
     ACPI_Test_Results.Describe (Service, R.Handle).Length = Big_Buffer_Length);
   Check (Observe (Service).Value_Objects <= 1 and then
     Observe (Service).Value_Bytes <= Big_Buffer_Length);
   Ada.Text_IO.Put_Line ("AML literal value checks:" & Checks'Image);
   ACPI_Test_Results.Drop (Service, Held);
exception
   when others =>
      Drop_Pins (Verify => False);
      ACPI_Test_Results.Drop (Service, Held);
      raise;
end Literal_Value_Tests;
