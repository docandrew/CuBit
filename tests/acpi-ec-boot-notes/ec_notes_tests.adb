pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Interfaces; use Interfaces;
with Firmware_Tables;
with ACPI_Service;
with ACPI_Requests;
with ACPI_Requests.EC_Boot_Notes;
with ACPI_EC_Boot_Observations;
with ACPI_EC_Boot_Table;
with ACPI_EC_Boot_Diagnostics;
with ACPI_Startup_Diagnostics;
with ACPI_Native_EC_Boot_Notes;
with ACPI_Native_Log;
procedure EC_Notes_Tests is
   package Q renames ACPI_Requests;
   package C renames ACPI_Service;
   package F renames Firmware_Tables;
   package O renames ACPI_EC_Boot_Observations;
   package E renames ACPI_EC_Boot_Table;
   package D renames ACPI_EC_Boot_Diagnostics;
   package IO renames Ada.Streams.Stream_IO;
   use type Q.Outcome, Q.Phase, C.Metrics;
   use type O.Outcome, O.Match_Count, O.Catalog_Position, O.Byte_Extent;
   use type E.Decode_Status, E.Path_Status, E.Register_Status;
   Mode : constant String := Ada.Command_Line.Argument (2);
   Header_Length_Offset : constant := 4;
   Revision_Offset : constant := 8;
   Checksum_Offset : constant := 9;
   Control_Offset : constant := 36;
   Data_Offset : constant := 48;
   UID_Offset : constant := 60;
   GPE_Offset : constant := 64;
   Address_Offset : constant := 4;
   Width_Offset : constant := 1;
   Access_Offset : constant := 3;
   Bits_Per_Byte : constant := 8;
   Word_Bytes : constant := 4;
   Address_Bytes : constant := 8;
   type Register_Offsets is array (Positive range <>) of Natural;
   Register_Positions : constant Register_Offsets := [Control_Offset, Data_Offset];
   Byte_Mask : constant Unsigned_64 := 255;
   Table_Capacity : constant := 3;
   Byte_Capacity : constant := 15_000;
   Table_Limit : constant := 5_000;
   Server : Q.State (Table_Capacity, Byte_Capacity, Table_Limit, Load_Capacity => Table_Capacity, Initial_Revision => 0);
   Result : O.Report;
   Reply : Q.Response;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Why; end if;
   end Check;
   function Read_Table (Name : String) return F.Bytes is
      File : IO.File_Type;
      Data : Ada.Streams.Stream_Element_Array (1 .. 4_096);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      IO.Open (File, IO.In_File, Name);
      IO.Read (File, Data, Last); IO.Close (File);
      return Bytes : F.Bytes (1 .. Natural (Last)) do
         for I in Bytes'Range loop Bytes (I) := F.Byte (Data (Ada.Streams.Stream_Element_Offset (I))); end loop;
      end return;
   end Read_Table;
   function EC_Table return F.Bytes is
      Name : constant String := (if Mode = "invalid-path" then "\BAD?" else "\_SB.EC0");
      Extent : constant Positive :=
        (if Mode = "malformed" then E.Fixed_Size
         elsif Mode = "oversize" then Positive (O.Default_Inspection_Bytes) + 1
         else E.Fixed_Size + Name'Length + 1);
      Bytes : F.Bytes (1 .. Extent) := [others => 0];
      Sum : F.Byte := 0;
      procedure Put (Offset : Natural; Value : Unsigned_64; Width : Positive) is
      begin
         for I in 0 .. Width - 1 loop Bytes (Offset + I + 1) :=
           F.Byte (Shift_Right (Value, I * Bits_Per_Byte) and Byte_Mask); end loop;
      end Put;
   begin
      Bytes (1 .. 4) := [Character'Pos ('E'), Character'Pos ('C'), Character'Pos ('D'), Character'Pos ('T')];
      Put (Header_Length_Offset, Unsigned_64 (Bytes'Length), Word_Bytes);
      Put (Revision_Offset, 1, 1);
      for Offset of Register_Positions loop
         Put (Offset, 1, 1); Put (Offset + Width_Offset, Bits_Per_Byte, 1);
         Put (Offset + Access_Offset, 1, 1);
      end loop;
      Put (Control_Offset + Address_Offset, (if Mode = "unsupported" then 65_536 else 16#66#), Address_Bytes);
      Put (Data_Offset + Address_Offset, (if Mode = "alias" then 16#66# else 16#62#), Address_Bytes);
      Put (UID_Offset, (if Mode = "wide-uid" then Unsigned_64 (Unsigned_32'Last) else 7), Word_Bytes);
      Put (GPE_Offset, 9, 1);
      if Mode /= "malformed" then
         for I in Name'Range loop Bytes (E.Fixed_Size + I) := Character'Pos (Name (I)); end loop;
      end if;
      for B of Bytes loop Sum := Sum + B; end loop;
      Bytes (Checksum_Offset + 1) := -Sum;
      return Bytes;
   end EC_Table;
   procedure Send (Label : Unsigned_32; Count : Unsigned_64 := 0) is
   begin
      Q.Handle (Server, Q.Snapshot_Provider,
        (Label => Label, Data => [Q.Revision (Server), Count, 0, 0], others => <>), Reply);
      Check (Reply.Status = Q.OK, "snapshot request");
   end Send;
begin
   Q.EC_Boot_Notes.Observe (Server, Result);
   Check (Result.Status = O.Catalog_Not_Ready, "idle catalog hidden");
   Send (Q.Start_Snapshot, (if Mode = "none" then 1 elsif Mode = "duplicate" then 3 else 2));
   declare
      AML : constant F.Bytes := Read_Table (Ada.Command_Line.Argument (1));
   begin
      Q.Import_Block (Server, Q.Snapshot_Provider, Q.Revision (Server), 1, C.DSDT, AML, Reply);
      Check (Reply.Status = Q.OK, "DSDT admitted");
   end;
   if Mode /= "none" then
      declare
         EC : constant F.Bytes := (if Mode = "reference" then Read_Table (Ada.Command_Line.Argument (3)) else EC_Table);
      begin
         Q.Import_Block (Server, Q.Snapshot_Provider, Q.Revision (Server), 2, C.Description, EC, Reply);
         Check (Reply.Status = Q.OK, "ECDT SDT admitted");
         if Mode = "duplicate" then
            Q.Import_Block (Server, Q.Snapshot_Provider, Q.Revision (Server), 3, C.Description, EC, Reply);
            Check (Reply.Status = Q.OK, "second ECDT SDT admitted");
         end if;
      end;
   end if;
   Q.EC_Boot_Notes.Observe (Server, Result);
   Check (Result.Status = O.Catalog_Not_Ready and then Result.Matches = 0, "receiving prefix hidden");
   Send (Q.Finish_Snapshot);
   Check (Q.Current (Server) = Q.Complete, "snapshot complete");
   declare
      Revision : constant Q.Revision_Number := Q.Revision (Server);
      Metrics : constant C.Metrics := Q.Observe (Server);
   begin
      Q.EC_Boot_Notes.Observe (Server, Result,
        (if Mode = "small-limit" then 1 else O.Default_Inspection_Bytes));
      Check (Q.Revision (Server) = Revision and then Q.Observe (Server) = Metrics,
        "inspection preserves revision and metrics");
   end;
   if Mode = "none" then
      Check (Result.Status = O.Missing_ECDT and then Result.Table = 0, "missing");
   elsif Mode = "duplicate" then
      Check (Result.Status = O.Multiple_ECDT and then Result.Matches = 2 and then Result.Table = 0, "duplicate refuses selection");
   elsif Mode = "small-limit" or else Mode = "oversize" then
      Check (Result.Status = O.Inspection_Limit and then Result.Required_Bytes > 0,
        "inspection budget is qualified refusal");
      Check (Result.Description.Status = E.Invalid_Table, "no partial description");
   else
      Check (Result.Status = O.Inspected and then Result.Table = 2 and then Result.Matches = 1, "one exact table");
      if Mode = "malformed" then
         Check (Result.Description.Status = E.Truncated_Fixed_Fields, "malformed body classified");
      else
         Check (Result.Description.Status = E.Decoded and then Result.Description.Control.Address =
           (if Mode = "unsupported" then 65_536 else 16#66#), "raw control preserved");
         Check (Result.Description.UID = (if Mode = "wide-uid" then Unsigned_32'Last elsif Mode = "reference" then 0 else 7), "full UID preserved");
         Check (Result.Description.GPE_Bit = 9, "GPE is bit not IRQ");
         Check (Result.Path.Status = (if Mode = "invalid-path" then E.Invalid_Path else E.Parsed), "rooted path classification");
         Check (Result.Registers = (if Mode = "unsupported" then E.IO_Address_Too_Wide
           elsif Mode = "alias" then E.Aliased_Registers else E.Byte_Registers_Described), "register compatibility");
      end if;
   end if;
   if Mode = "hardware" then
      Check (not ACPI_Startup_Diagnostics.Device_Initialization_Completed (Q.Device_Completion (Server)),
        "hardware devices incomplete independently of catalog");
      Check (D.Described (Result), "ECDT still observable beside unavailable provider");
   end if;
   declare
      Line : constant String := D.Format (Result);
   begin
      Check (Line'Length <= D.Maximum_Text_Length, "bounded diagnostic");
      if D.Described (Result) then
         Check (Ada.Strings.Fixed.Index (Line, "gpe-bit=0x09") > 0 and then
           Ada.Strings.Fixed.Index (Line, "authority=NOT_ESTABLISHED") > 0, "diagnostic scope explicit");
      end if;
      Ada.Text_IO.Put_Line (Line);
   end;
   ACPI_Native_EC_Boot_Notes.Observe (Server,
     (if Mode = "small-limit" then 1 else O.Default_Inspection_Bytes));
   Check (ACPI_Native_Log.Records = 1, "native hook emits exactly one record");
   Check (ACPI_Native_Log.Warnings =
     (if Mode = "none" or else Mode = "normal" or else Mode = "hardware"
        or else Mode = "wide-uid" or else Mode = "reference" then 0 else 1),
     "native hook severity distinguishes observations and refusals");
   Ada.Text_IO.Put_Line ("EC boot observation checks " & Checks'Image);
end EC_Notes_Tests;
