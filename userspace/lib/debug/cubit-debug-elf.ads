------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The sections of an ELF64 debug file (or unstripped executable) the
--  engine reads.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Debug.Bytes;

package CuBit.Debug.Elf is
   package B renames CuBit.Debug.Bytes;

   type Section_Name is
     (Debug_Info, Debug_Abbrev, Debug_Line, Debug_Str, Debug_Line_Str,
      Debug_Loclists, Debug_Rnglists, Debug_Addr, Debug_Str_Offsets,
      Debug_Loc, Debug_Ranges,
      Eh_Frame, Text, Build_Id_Note);

   type Section is record
      Present    : Boolean := False;
      In_File    : Boolean := False;   --  False: NOBITS (a stripped debug file's copy)
      Compressed : Boolean := False;   --  SHF_COMPRESSED: not read by the preview
      File_Offset : B.Offset := 0;
      Size       : B.Offset := 0;
      Address    : Unsigned_64 := 0;   --  where it is loaded (allocated sections)
   end record;
   type Section_Table is array (Section_Name) of Section;

   type Image_Result is (Opened, Not_Elf, Truncated);

   procedure Read_Sections
     (Data : B.Byte_Buffer; Table : out Section_Table; Result : out Image_Result);

   --  The GNU build-id bytes from the note section, Length 0 when none.
   procedure Read_Build_Id
     (Data : B.Byte_Buffer; Table : Section_Table;
      Id : out String; Length : out Natural)
   with Pre => Id'Length = 2 * Build_Id_Bytes;
end CuBit.Debug.Elf;
