pragma Ada_2022;
with ACPI_Test_Results;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service; use ACPI_Service;
with AML_Execute;
with Firmware_Tables;
procedure Service_Field_Runner is
   use type Firmware_Tables.Bytes;
   use type Ada.Streams.Stream_Element_Offset;
   use type Namespace.Bind_Status;
   use type AML_Execute.Execution_Status;
   subtype Bytes is Firmware_Tables.Bytes;
   package IO renames Ada.Streams.Stream_IO;
   File : IO.File_Type;
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
   Service : aliased State (Max_Tables, Max_Total_Bytes, Max_Table_Bytes);
   Held : ACPI_Test_Results.Holder;
   Status : Install_Status;
   Bound : Namespace.Bind_Status;
   Region : Namespace.Node_ID;
   Revision : constant Unsigned_8 := Unsigned_8'Value (Ada.Command_Line.Argument (2));
   Offset : constant Natural := Natural'Value (Ada.Command_Line.Argument (3));
   Bits : constant Natural := Natural'Value (Ada.Command_Line.Argument (4));
   function Length_Code (N : Natural) return Bytes is
   begin
      if N < 64 then return [Unsigned_8 (N)];
      elsif N < 4096 then return [16#40# or Unsigned_8 (N mod 16), Unsigned_8 (N / 16)];
      elsif N < 1_048_576 then
         return [16#80# or Unsigned_8 (N mod 16), Unsigned_8 ((N / 16) mod 256), Unsigned_8 (N / 4096)];
      else raise Program_Error with "fixture length"; end if;
   end Length_Code;
   Entries : constant Bytes := [0] & Length_Code (Offset) & Enc ("FLD0") & Length_Code (Bits);
   Declaration : constant Bytes := [16#5B#,16#81#, Unsigned_8 (6 + Entries'Length)] & Enc ("REG0") & [1] & Entries;
   Result : Values.Result;
begin
   Install (Service, 1, DSDT, Table ("DSDT", Revision,
     Method ("READ", Declaration & [16#A4#] & Enc ("FLD0"))), Status);
   if Status /= Installed then raise Program_Error with "method install"; end if;
   IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
   declare
      Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
      Last : Ada.Streams.Stream_Element_Offset;
      Data : Bytes (1 .. Raw'Length);
      Sum : Unsigned_8 := 0;
   begin
      IO.Read (File, Raw, Last); IO.Close (File);
      if Last /= Raw'Last then raise Program_Error with "short read"; end if;
      for I in Data'Range loop Data (I) := Unsigned_8 (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
      -- Retain the reference's payload verbatim as a description table. Only
      -- signature/checksum change; comparisons deliberately start after byte36.
      if Offset < 36 * 8 then raise Program_Error with "header comparison forbidden"; end if;
      Data (1 .. 4) := Enc ("TEST"); Data (10) := 0;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum;
      Install (Service, 2, Description, Data, Status);
      if Status /= Installed then raise Program_Error with "data install"; end if;
   end;
   Declare_Table_Region (Service, Namespace.Root, "REG0", (Name => "TEST", others => <>), Region, Bound);
   if Bound /= Namespace.Bound then raise Program_Error with "region binding"; end if;
   ACPI_Test_Results.Seal (Service);
   ACPI_Test_Results.Invoke (Service, Held, ACPI_Test_Results.Child (Service, Namespace.Root, "READ"),
           [others => 0], 0, 100, Result);
   if Result.Status = AML_Execute.Returned then
      Ada.Text_IO.Put_Line ("INTEGER" & Result.Number'Image);
   elsif Result.Status = AML_Execute.Object_Returned then
      begin
         Ada.Text_IO.Put ("BUFFER");
         for B of ACPI_Test_Results.Bytes (Service, Result.Handle) loop Ada.Text_IO.Put (B'Image); end loop;
         Ada.Text_IO.New_Line;
      end;
   else
      raise Program_Error with Result.Status'Image;
   end if;
   ACPI_Test_Results.Drop (Service, Held);
exception
   when others =>
      ACPI_Test_Results.Drop (Service, Held);
      raise;
end Service_Field_Runner;
