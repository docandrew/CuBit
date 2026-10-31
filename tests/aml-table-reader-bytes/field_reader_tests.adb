pragma Ada_2022;
with Ada.Text_IO;
with AML_Table_Backing;
with AML_Table_Readers;
with AML_Table_Sources;
with AML_Copied_Tables;
with Firmware_Tables.Identifiers;
procedure Field_Reader_Tests is
   package F renames Firmware_Tables;
   package B renames AML_Table_Backing;
   package R renames AML_Table_Readers;
   package S renames AML_Table_Sources;
   package C renames AML_Copied_Tables;
   use type F.Byte;
   use type F.Bytes;
   use type B.Spans;
   use type R.Read_Status;
   use type C.Outcome;
   use type C.Table_ID;
   use type C.Store_Model;
   Octet_Bits : constant Positive := 8;
   Payload_Bytes : constant Positive := 16;
   Extent : constant Positive := F.Table_Header_Size + Payload_Bytes;
   Gap : constant Positive := 9;
   Prefix : constant Positive := 3;
   Pool_Capacity : constant Positive := 2;
   subtype Table_Bytes is F.Bytes (1 .. Extent);
   type Table_Set is array (Positive range <>) of Table_Bytes;
   function Make_Table (Seed : Positive) return Table_Bytes is
      Data : Table_Bytes := [others => 0];
      Name : constant F.Signature := "TEST";
      OEM : constant F.Identifiers.OEM_Name := "CUBIT0";
      Model : constant F.Identifiers.OEM_Table_Name := "BITVIEWS";
      Sum : F.Byte := 0;
   begin
      for I in Name'Range loop Data (I) := Character'Pos (Name (I)); end loop;
      Data (5) := F.Byte (Extent); Data (9) := 2;
      for I in OEM'Range loop Data (10 + I) := Character'Pos (OEM (I)); end loop;
      for I in Model'Range loop Data (16 + I) := Character'Pos (Model (I)); end loop;
      for I in F.Table_Header_Size + 1 .. Extent loop Data (I) := F.Byte ((I * Seed + 129) mod 256); end loop;
      for Byte of Data loop Sum := Sum + Byte; end loop;
      Data (10) := 0 - Sum;
      return Data;
   end Make_Table;
   Boot_Data : constant Table_Set := [Make_Table (1), Make_Table (3)];
   Copy_Data : constant Table_Set := [Make_Table (5), Make_Table (7)];
   Boot : aliased B.State (Pool_Capacity, Prefix + Pool_Capacity * (Extent + Gap));
   Copies : C.State (Pool_Capacity, Pool_Capacity * Extent);
   Checks, Views, Logical_Bytes : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin
      if not OK then raise Program_Error with Why; end if;
      Checks := Checks + 1;
   end Check;
   function Oracle (Data : Table_Bytes; Offset, Bits, Index : Natural) return F.Byte is
      Result : Natural := 0;
      Position : Natural;
   begin
      -- Deliberately bit-by-bit, rather than shifted-byte assembly.
      for Bit in 0 .. Octet_Bits - 1 loop
         if Index * Octet_Bits + Bit < Bits then
            Position := Offset + Index * Octet_Bits + Bit;
            if (Natural (Data (Position / Octet_Bits + 1)) /
              (2 ** (Position mod Octet_Bits))) mod 2 = 1
            then Result := Result + 2 ** Bit; end if;
         end if;
      end loop;
      return F.Byte (Result);
   end Oracle;
   procedure Refused (View : R.Field_View; Index : Natural;
      Expected : R.Read_Status) is
      Value : F.Byte := F.Byte'Last;
      Status : R.Read_Status;
   begin
      R.Read_Field_Byte (Boot, Copies, View, Index, Value, Status);
      Check (Status = Expected and then Value = 0, "zeroed typed refusal");
   end Refused;
   procedure Matrix (Source : S.Source; Data : Table_Bytes) is
      View : R.Field_View := (Source, Extent, 0, 0);
      Value : F.Byte;
      Status : R.Read_Status;
      Max_Test_Bits : constant Positive := 25;
   begin
      for Offset in 0 .. Extent * Octet_Bits loop
         for Bits in 0 .. Natural'Min (Max_Test_Bits, Extent * Octet_Bits - Offset) loop
            View.Offset := Offset; View.Bits := Bits;
            Check (R.Valid_Field (Boot, Copies, View), "valid view");
            Check (R.Field_Length (View) = Bits / Octet_Bits +
              (if Bits mod Octet_Bits = 0 then 0 else 1), "logical length");
            Views := Views + 1;
            for Index in 1 .. R.Field_Length (View) loop
               R.Read_Field_Byte (Boot, Copies, View, Index - 1, Value, Status);
               Check (Status = R.Readable and then Value = Oracle (Data, Offset, Bits, Index - 1),
                 "logical bit model");
               Logical_Bytes := Logical_Bytes + 1;
            end loop;
            Refused (View, R.Field_Length (View), R.Invalid_Window);
            Refused (View, Natural'Last, R.Invalid_Window);
         end loop;
      end loop;
      View := (Source, Extent + 1, 0, 1);
      Check (not R.Valid_Field (Boot, Copies, View), "stale extent rejected");
      Refused (View, 0, R.Invalid_Window);
      View := (Source, Extent, Natural'Last, 1);
      Check (not R.Valid_Field (Boot, Copies, View), "offset overflow rejected");
      Refused (View, 0, R.Invalid_Window);
      View := (Source, Extent, 1, Natural'Last);
      Check (not R.Valid_Field (Boot, Copies, View), "bit extent overflow rejected");
      Refused (View, 0, R.Invalid_Window);
      View := (Source, Extent, Extent * Octet_Bits, 1);
      Refused (View, 0, R.Invalid_Window);
   end Matrix;
   Report : C.Report;
begin
   Boot.Count := Pool_Capacity;
   Boot.Data.Bytes := [others => 16#A5#];
   for I in 1 .. Pool_Capacity loop
      Boot.Tables (I) := (Prefix + (I - 1) * (Extent + Gap), Extent);
      Boot.Data.Bytes (Boot.Tables (I).Offset + 1 .. Boot.Tables (I).Offset + Extent) := Boot_Data (I);
      C.Retain (Copies, Copy_Data (I), 10000, Report);
      Check (Report.Status = C.Added and then Report.ID = C.Table_ID (I), "owned copy admission");
   end loop;
   declare
      Before_Tables : constant B.Spans := Boot.Tables;
      Before_Bytes : constant F.Bytes := Boot.Data.Bytes;
      Before_Copies : constant C.Store_Model := C.Model (Copies) with Ghost;
   begin
      for I in 1 .. Pool_Capacity loop
         Matrix ((S.Boot_Source, S.Boot_Table_ID (I)), Boot_Data (I));
         Matrix ((S.Copied_Source, S.Copied_Table_ID (I)), Copy_Data (I));
      end loop;
      Refused ((S.Absent_Source, Extent, 0, 1), 0, R.Source_Missing);
      Refused (((S.Boot_Source, S.Boot_Table_ID (Pool_Capacity + 1)), Extent, 0, 1), 0, R.Source_Missing);
      Refused (((S.Copied_Source, S.Copied_Table_ID (Pool_Capacity + 1)), Extent, 0, 1), 0, R.Source_Missing);
      Check (Boot.Tables = Before_Tables and then Boot.Data.Bytes = Before_Bytes
        and then Boot.Count = Pool_Capacity, "boot owner unchanged");
      pragma Assert (C.Model (Copies) = Before_Copies);
   end;
   Boot.Tables (2) := (Boot.Byte_Capacity, Extent);
   Refused (((S.Boot_Source, 2), Extent, 0, 1), 0, R.Invalid_Inventory);
   Boot.Count := Pool_Capacity + 1;
   Refused (((S.Copied_Source, 1), Extent, 0, 1), 0, R.Invalid_Inventory);
   Ada.Text_IO.Put_Line ("Field reader checks" & Checks'Image &
     " views" & Views'Image & " bytes" & Logical_Bytes'Image);
end Field_Reader_Tests;
