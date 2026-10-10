pragma Ada_2022;
with ACPI_Test_Results;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service; use ACPI_Service;
with AML_Decode;
with AML_Table_Backing;
with AML_Field_Data;
with AML_Execute;
with AML_Objects;
with Firmware_Tables;
procedure Service_Field_Execution_Tests is
   use type Namespace.Bind_Status;
   use type AML_Execute.Execution_Status;
   use type Values.Value_Kind;
   use type Values.Access_Status;
   use type Values.Value_Handle;
   use type AML_Decode.Bytes;
   use type AML_Decode.Status;
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
   Code : constant Bytes :=
     Method ("READ", [16#A4#] & Enc ("FLD0")) &
     Method ("TYPE", [16#A4#, 16#8E#] & Enc ("FLD0")) &
     Method ("NEST", [16#A4#] & Enc ("READ")) &
     Method ("LOCL", [16#70#] & Enc ("FLD0") & [16#60#, 16#A4#, 16#8E#, 16#60#]) &
     Method ("WRIT", [16#70#, 1] & Enc ("FLD0") & [16#A4#, 0]);
   Octet_Bits : constant Positive := 8;
   Large_Field_Bits : constant Positive := 8_192;
   Large_Field_Bytes : constant Positive := Large_Field_Bits / Octet_Bits;
   Held_Capacity : constant Positive := AML_Objects.Max_Bytes / Large_Field_Bytes;
   Released_Reads : constant Positive := 64;
   pragma Compile_Time_Error
     (Large_Field_Bits mod Octet_Bits /= 0 or else
      AML_Objects.Max_Bytes mod Large_Field_Bytes /= 0 or else
      Held_Capacity >= Max_Namespace_Nodes or else
      Held_Capacity >= AML_Objects.Max_Objects,
      "field fixture must exhaust backing bytes before pins or object slots");
   Counts : constant array (Positive range <>) of Natural :=
     [0, 1, 7, 8, 31, 32, 33, 63, 64, 65, 127, Large_Field_Bits];
   Payload : Bytes (1 .. 1100);
begin
   declare
      Input : aliased AML_Table_Backing.State (1, 64);
      Result : AML_Field_Data.Read_Result;
      procedure Reject (Table, Extent : Positive; Offset, Bits : Natural) is
      begin
         Result := AML_Table_Backing.Read_Field (Input, Table, Extent, Offset, Bits);
         Check (Result.Status /= AML_Decode.Accepted);
      end Reject;
   begin
      Reject (1, 64, 0, 8);
      Input.Count := 1;
      Input.Tables (1) := (Offset => 0, Extent => 64);
      Input.Data := [others => 16#80#];
      Result := AML_Table_Backing.Read_Field (Input, 1, 64, 0, 8);
      Check (Result.Status = AML_Decode.Accepted and then Result.Content (1) = 16#80#);
      Reject (2, 64, 0, 8);
      Reject (1, 63, 0, 8);
      Reject (1, 64, 511, 2);
      Input.Count := 2; Reject (1, 64, 0, 8);
      Input.Count := 1;
      Input.Tables (1).Offset := Natural'Last; Reject (1, 64, 0, 8);
      Input.Tables (1) := (Offset => 1, Extent => 64); Reject (1, 64, 0, 8);
      Input.Tables (1) := (Offset => 0, Extent => 0); Reject (1, 1, 0, 8);
   end;
   for I in Payload'Range loop Payload (I) := Unsigned_8 ((I * 37 + 17) mod 256); end loop;
   for Revision in Unsigned_8 range 1 .. 2 loop
      for Bits of Counts loop
         declare
            Service : aliased State (Max_Tables, Max_Total_Bytes, Max_Table_Bytes);
   Held : ACPI_Test_Results.Holder;
            Installed_Result : Install_Status;
            Bound : Namespace.Bind_Status;
            Region, Field : Namespace.Node_ID;
            Raw : Bytes := Table ("TEST", 2, Payload);
            Expected : AML_Decode.Bytes (1 .. Bits / 8 + (if Bits mod 8 = 0 then 0 else 1)) := [others => 0];
            Expected_Integer : Unsigned_64 := 0;
            Integer_Field : constant Boolean := Bits <= (if Revision = 1 then 32 else 64);
            Result : Values.Result;
            Pins : array (Positive range 1 .. Held_Capacity) of Values.Value_Handle :=
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

               Node : constant Namespace.Node_ID := ACPI_Test_Results.Child (Service, Namespace.Root, Name);
            begin
               ACPI_Test_Results.Invoke (Service, Held, Node, [others => 0], 0, Budget, Result);
            end Run;
            procedure Check_Value is

            begin
               if Integer_Field then
                  Check (Result.Status = AML_Execute.Returned and then Result.Number = Expected_Integer);
               else
                  Check (Result.Status = AML_Execute.Object_Returned);
                  Check (ACPI_Test_Results.Describe (Service, Result.Handle).Kind = Values.Buffer_Description);
                  Check (ACPI_Test_Results.Bytes (Service, Result.Handle) = Expected);
               end if;
            end Check_Value;
         begin
            for B in 0 .. Bits - 1 loop
               if (Natural (Payload (1 + (B + 3) / 8)) / 2 ** ((B + 3) mod 8)) mod 2 /= 0 then
                  Expected (1 + B / 8) := Expected (1 + B / 8) + Unsigned_8 (2 ** (B mod 8));
                  if Integer_Field then Expected_Integer := Expected_Integer or Shift_Left (Unsigned_64'(1), B); end if;
               end if;
            end loop;
            Install (Service, 1, DSDT, Table ("DSDT", Revision, Code), Installed_Result);
            Check (Installed_Result = Installed);
            Install (Service, 2, Description, Raw, Installed_Result);
            Check (Installed_Result = Installed);
            Raw := [others => 0];
            Check (Raw (37) = 0);
            Declare_Table_Region (Service, Namespace.Root, "REG0", (Name => "TEST", others => <>), Region, Bound);
            Check (Bound = Namespace.Bound);
            Declare_Table_Field (Service, Namespace.Root, "FLD0", Region, 36 * 8 + 3, Bits, Field, Bound);
            Check (Bound = Namespace.Bound);
            Check (Field > Namespace.Root);
            ACPI_Test_Results.Seal (Service);
            for I in 1 .. 3 loop
               Run ("TYPE");
               Check (Result.Status = AML_Execute.Returned and then Result.Number = 5);
               Check (Observe (Service).Value_Objects = 0);
            end loop;
            Run ("READ"); Check_Value;
            Run ("NEST"); Check_Value;
            Run ("LOCL");
            Check (Result.Status = AML_Execute.Returned and then Result.Number = (if Integer_Field then 1 else 3));
            Run ("WRIT"); Check (Result.Status = AML_Execute.Unsupported);
            Run ("READ", 0); Check (Result.Status = AML_Execute.Budget_Exceeded);
            Check (Table_Byte (Service, 2, 36) = Payload (1));
            if Bits = Large_Field_Bits then
               -- Holder releases each preceding value; only the latest field
               -- Buffer should remain live after the next materializing read.
               for I in 1 .. Released_Reads loop
                  Run ("READ");
                  Check_Value;
                  Check (Observe (Service).Value_Objects <= 1 and then
                    Observe (Service).Value_Bytes <= Large_Field_Bytes and then
                    Observe (Service).Package_Elements = 0);
               end loop;
               ACPI_Test_Results.Drop (Service, Held);
               for I in Pins'Range loop
                  Invoke_Retained (Service,
                    ACPI_Test_Results.Child (Service, Namespace.Root, "READ"),
                    [others => 0], 0, 100, Result, Retention_Status);
                  Check (Retention_Status = Values.Available and then
                    Result.Status = AML_Execute.Object_Returned);
                  Pins (I) := Result.Handle;
                  Check (Retained_Results (Service) = I and then
                    Observe (Service).Value_Bytes = I * Large_Field_Bytes);
               end loop;
               Invoke_Retained (Service,
                 ACPI_Test_Results.Child (Service, Namespace.Root, "READ"),
                 [others => 0], 0, 100, Result, Retention_Status);
               Check (Retention_Status = Values.Available and then
                 Result.Status = AML_Execute.Value_Limit and then Result.Charged > 0
                 and then Retained_Results (Service) = Held_Capacity
                 and then Observe (Service).Value_Bytes = AML_Objects.Max_Bytes);
               for Pin of Pins loop
                  Check (ACPI_Test_Results.Describe (Service, Pin).Kind = Values.Buffer_Description);
                  Check (ACPI_Test_Results.Bytes (Service, Pin) = Expected);
               end loop;
               -- Type inspection allocates no AML value even at full backing.
               Run ("TYPE"); Check (Result.Status = AML_Execute.Returned and then Result.Number = 5);
               Check (Observe (Service).Value_Bytes = AML_Objects.Max_Bytes);
               Drop_Pins;
               Check (Retained_Results (Service) = 0);
               Run ("READ"); Check_Value;
               Check (Observe (Service).Value_Objects <= 1 and then
                 Observe (Service).Value_Bytes <= Large_Field_Bytes);
            end if;
            ACPI_Test_Results.Drop (Service, Held);
         exception
            when others =>
               Drop_Pins (Verify => False);
               ACPI_Test_Results.Drop (Service, Held);
               raise;
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("ACPI field execution checks:" & Checks'Image);
end Service_Field_Execution_Tests;
