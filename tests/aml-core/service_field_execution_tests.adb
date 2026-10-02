pragma Ada_2022;
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
   use type AML_Objects.Object_Kind;
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
   Counts : constant array (Positive range <>) of Natural :=
     [0, 1, 7, 8, 31, 32, 33, 63, 64, 65, 127, 8192];
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
            Service : aliased State := Fresh;
            Installed_Result : Install_Status;
            Bound : Namespace.Bind_Status;
            Region, Field : Namespace.Node_ID;
            Raw : Bytes := Table ("TEST", 2, Payload);
            Expected : AML_Decode.Bytes (1 .. Bits / 8 + (if Bits mod 8 = 0 then 0 else 1)) := [others => 0];
            Expected_Integer : Unsigned_64 := 0;
            Integer_Field : constant Boolean := Bits <= (if Revision = 1 then 32 else 64);
            Result : AML_Execute.Execution_Result;
            procedure Run (Name : String; Budget : Natural := 100) is
               Tree : constant Namespace.State := Snapshot (Service);
               Node : constant Namespace.Node_ID := Namespace.Child (Tree, Namespace.Root, Name);
            begin
               Invoke (Service, Node, [others => 0], 0, Budget, Result);
            end Run;
            procedure Check_Value is
               Tree : constant Namespace.State := Snapshot (Service);
               Store : constant AML_Objects.State := Namespace.Value_Store (Tree);
            begin
               if Integer_Field then
                  Check (Result.Status = AML_Execute.Returned and then Result.Value = Expected_Integer);
               else
                  Check (Result.Status = AML_Execute.Object_Returned);
                  Check (AML_Objects.Kind (Store, Result.Object.ID) = AML_Objects.Buffer_Object);
                  Check (AML_Objects.Byte_Data (Store, Result.Object.ID) = Expected);
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
            for I in 1 .. 3 loop
               Run ("TYPE");
               Check (Result.Status = AML_Execute.Returned and then Result.Value = 5);
               Check (Observe (Service).Value_Objects = 0);
            end loop;
            Run ("READ"); Check_Value;
            Run ("NEST"); Check_Value;
            Run ("LOCL");
            Check (Result.Status = AML_Execute.Returned and then Result.Value = (if Integer_Field then 1 else 3));
            Run ("WRIT"); Check (Result.Status = AML_Execute.Unsupported);
            Run ("READ", 0); Check (Result.Status = AML_Execute.Budget_Exceeded);
            Check (Table_Byte (Service, 2, 36) = Payload (1));
            if Bits = 8192 then
               for I in 1 .. 64 loop
                  Run ("READ");
                  exit when Result.Status = AML_Execute.Value_Limit;
                  Check_Value;
               end loop;
               Check (Result.Status = AML_Execute.Value_Limit);
               Run ("TYPE"); Check (Result.Status = AML_Execute.Returned and then Result.Value = 5);
            end if;
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("ACPI field execution checks:" & Checks'Image);
end Service_Field_Execution_Tests;
