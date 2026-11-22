------------------------------------------------------------------------------
--  ccl-device-index: an image's driver index (HW-002, docs/device-matching.md)
--
--  ccl-device-index --output devices.index NAME=PATH ...
--
--  Reads each payload's .cubit.devices section, skips payloads without one,
--  refuses an image in which two drivers have equally specific entries
--  that can match the same device, and writes CCL.Device_Index's format.
--  Linux-hosted build step; the bytes it reads are untrusted and decoded
--  with the proved CCL.Device_Sections.Decode.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;
with CCL.Device_Sections;
with CCL.Device_Index;

procedure Device_Index is
   package DS renames CCL.Device_Sections;
   package DI renames CCL.Device_Index;
   use type DS.Decode_Status;

   --  ELF64 (System V gABI): the header and section header fields read here.
   ELF_HEADER_BYTES     : constant := 64;
   HEADER_SECTION_TABLE : constant := 40;
   HEADER_ENTRY_SIZE    : constant := 58;
   HEADER_SECTION_COUNT : constant := 60;
   SECTION_HEADER_BYTES : constant := 64;
   SECTION_TYPE         : constant := 4;
   SECTION_OFFSET       : constant := 24;
   SECTION_SIZE         : constant := 32;
   SHT_PROGBITS         : constant := 1;
   ELF_MAGIC            : constant := 16#464C_457F#;
   MAGIC_BYTES          : constant := 4;

   type Bytes_Access is access Stream_Element_Array;
   procedure Free is new Ada.Unchecked_Deallocation (Stream_Element_Array, Bytes_Access);

   Failed : exception;
   Index : DI.Index;

   function Field (Data : Stream_Element_Array; Offset, Bytes : Unsigned_64) return Unsigned_64 is
      Value : Unsigned_64 := 0;
   begin
      if Offset + Bytes > Unsigned_64 (Data'Length) then
         raise Failed with "truncated ELF";
      end if;
      for I in reverse 0 .. Bytes - 1 loop
         Value := Shift_Left (Value, 8) or
           Unsigned_64 (Data (Data'First + Stream_Element_Offset (Offset + I)));
      end loop;
      return Value;
   end Field;

   function Read_File (Path : String) return Bytes_Access is
      use Ada.Streams.Stream_IO;
      F : Ada.Streams.Stream_IO.File_Type;
      Size : constant Stream_Element_Offset :=
        Stream_Element_Offset (Ada.Directories.Size (Path));
      Result : constant Bytes_Access := new Stream_Element_Array (1 .. Size);
      Last : Stream_Element_Offset;
   begin
      Open (F, Ada.Streams.Stream_IO.In_File, Path);
      Read (F, Result.all, Last);
      Close (F);
      if Last /= Size then
         raise Failed with Path & ": short read";
      end if;
      return Result;
   end Read_File;

   --  The .cubit.devices section's bytes, or an empty array when the file
   --  is not an ELF image or has none.
   function Devices_Section (Data : Stream_Element_Array) return DS.Byte_Array is
      Length : constant Unsigned_64 := Unsigned_64 (Data'Length);
      Table, Count, Header, At_Offset, Bytes : Unsigned_64;
   begin
      if Length < ELF_HEADER_BYTES or else Field (Data, 0, MAGIC_BYTES) /= ELF_MAGIC then
         return [];
      end if;
      Table := Field (Data, HEADER_SECTION_TABLE, 8);
      Count := Field (Data, HEADER_SECTION_COUNT, 2);
      if Field (Data, HEADER_ENTRY_SIZE, 2) /= SECTION_HEADER_BYTES
        or else Table > Length
        or else Count > (Length - Table) / SECTION_HEADER_BYTES
      then
         return [];
      end if;
      for N in 1 .. Count loop
         Header := Table + (N - 1) * SECTION_HEADER_BYTES;
         At_Offset := Field (Data, Header + SECTION_OFFSET, 8);
         Bytes := Field (Data, Header + SECTION_SIZE, 8);
         if Field (Data, Header + SECTION_TYPE, 4) = SHT_PROGBITS
           and then Bytes in MAGIC_BYTES .. DS.MAX_SECTION_BYTES
           and then At_Offset <= Length and then Bytes <= Length - At_Offset
           and then Field (Data, At_Offset, MAGIC_BYTES) = DS.MAGIC
         then
            declare
               Result : DS.Byte_Array (1 .. Natural (Bytes));
            begin
               for I in Result'Range loop
                  Result (I) := Unsigned_8
                    (Data (Data'First + Stream_Element_Offset (At_Offset) + Stream_Element_Offset (I - 1)));
               end loop;
               return Result;
            end;
         end if;
      end loop;
      return [];
   end Devices_Section;

   function Hex (Value : Unsigned_64; Digits_Count : Positive) return String is
      Characters : constant String := "0123456789ABCDEF";
      Result : String (1 .. Digits_Count);
      Remaining : Unsigned_64 := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Characters (Natural (Remaining and 15) + 1);
         Remaining := Shift_Right (Remaining, 4);
      end loop;
      return Result;
   end Hex;

   --  An entry in the notation of the manifest.
   function Image (M : DS.Device_Match) return String is
     (case M.Kind is
         when DS.PCI_IDs =>
            "PCI_IDs " & Hex (Unsigned_64 (M.Vendor), 4) & ":" & Hex (Unsigned_64 (M.First), 4)
            & ".." & Hex (Unsigned_64 (M.Last), 4),
         when DS.PCI_Class =>
            "PCI_Class " & Hex (Unsigned_64 (M.Class), 2) & "." & Hex (Unsigned_64 (M.Subclass), 2)
            & "." & Hex (Unsigned_64 (M.Program), 2),
         when DS.USB_Class =>
            "USB_Class " & Hex (Unsigned_64 (M.Class), 2) & "." & Hex (Unsigned_64 (M.Subclass), 2)
            & "." & Hex (Unsigned_64 (M.Program), 2),
         when DS.ACPI_ID => "ACPI_ID " & String (M.ID (1 .. M.ID_Length)),
         when DS.Platform => "Platform " & DS.Platform_Device'Image (M.Device));

   function Name_Of (D : DI.Driver_Number) return String is
     (Index.Drivers (D).Name.Text (1 .. Index.Drivers (D).Name.Length));

   type Text_Access is access String;
   Output_Path : Text_Access;
   Argument_Number : Positive := 1;
begin
   while Argument_Number <= Argument_Count loop
      declare
         Item : constant String := Argument (Argument_Number);
         Equals : Natural := 0;
      begin
         if Item = "--output" and then Argument_Number < Argument_Count then
            Output_Path := new String'(Argument (Argument_Number + 1));
            Argument_Number := Argument_Number + 1;
         else
            for I in Item'Range loop
               if Item (I) = '=' and then Equals = 0 then
                  Equals := I;
               end if;
            end loop;
            if Equals <= Item'First or else Equals = Item'Last then
               raise Failed with "expected NAME=PATH, got " & Item;
            end if;
            declare
               Name : constant String := Item (Item'First .. Equals - 1);
               Data : Bytes_Access := Read_File (Item (Equals + 1 .. Item'Last));
               Section : constant DS.Byte_Array := Devices_Section (Data.all);
               Table : DS.Match_Table;
               Status : DS.Decode_Status;
               Entry_Name : DI.Driver_Name;
            begin
               Free (Data);
               if Section'Length > 0 then
                  DS.Decode (Section, Table, Status);
                  if Status /= DS.Decoded then
                     raise Failed with Name & ": malformed .cubit.devices (" &
                       DS.Decode_Status'Image (Status) & ")";
                  end if;
                  if Name'Length > DI.NAME_BYTES then
                     raise Failed with Name & ": driver file name longer than" &
                       DI.NAME_BYTES'Image & " characters";
                  end if;
                  Entry_Name.Length := Name'Length;
                  Entry_Name.Text (1 .. Name'Length) := Name;
                  if not DI.Valid_Name (Entry_Name) then
                     raise Failed with Name & ": driver file names use a-z, 0-9, '.', '_' and '-'";
                  end if;
                  if DI.Find (Index, Name) /= 0 then
                     raise Failed with Name & ": named twice";
                  end if;
                  if Index.Count = DI.MAX_DRIVERS then
                     raise Failed with "more than" & DI.MAX_DRIVERS'Image & " drivers";
                  end if;
                  Index.Count := Index.Count + 1;
                  Index.Drivers (Index.Count) := (Name => Entry_Name, Matches => Table);
               end if;
            end;
         end if;
      end;
      Argument_Number := Argument_Number + 1;
   end loop;
   if Output_Path = null then
      raise Failed with "usage: ccl-device-index --output FILE NAME=PATH ...";
   end if;

   declare
      Found : Boolean;
      Left, Right : DI.Driver_Count;
      Left_Entry, Right_Entry : DS.Entry_Count;
   begin
      DI.First_Ambiguity (Index, Found, Left, Right, Left_Entry, Right_Entry);
      if Found then
         raise Failed with "ambiguous drivers: " & Name_Of (Left) & " ("
           & Image (Index.Drivers (Left).Matches.Entries (Left_Entry)) & ") and "
           & Name_Of (Right) & " (" & Image (Index.Drivers (Right).Matches.Entries (Right_Entry))
           & ") match a device equally specifically. Fix: narrow one of them, "
           & "or leave one driver out of the image.";
      end if;
   end;

   for D in 1 .. Index.Count loop
      for E in 1 .. Index.Drivers (D).Matches.Count loop
         Put_Line ("ccl-device-index: " & Name_Of (D) & " drives "
                   & Image (Index.Drivers (D).Matches.Entries (E)));
      end loop;
   end loop;

   declare
      use Ada.Streams.Stream_IO;
      Bytes : DI.Index_Bytes;
      Length : DI.Index_Length;
      F : Ada.Streams.Stream_IO.File_Type;
      Out_Bytes : Stream_Element_Array (1 .. Stream_Element_Offset (DI.MAX_INDEX_BYTES));
   begin
      DI.Encode (Index, Bytes, Length);
      for I in 1 .. Length loop
         Out_Bytes (Stream_Element_Offset (I)) := Stream_Element (Bytes (I));
      end loop;
      Create (F, Ada.Streams.Stream_IO.Out_File, Output_Path.all);
      Write (F, Out_Bytes (1 .. Stream_Element_Offset (Length)));
      Close (F);
   end;
exception
   when E : Failed =>
      Put_Line (Standard_Error, "ccl-device-index: " & Ada.Exceptions.Exception_Message (E));
      Set_Exit_Status (Ada.Command_Line.Failure);
end Device_Index;
