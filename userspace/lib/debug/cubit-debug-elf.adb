------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Debug.Elf is

   ELF_Magic       : constant := 16#464C_457F#;
   Class_64        : constant := 2;
   Little_Endian   : constant := 1;
   Ident_Class_At  : constant := 4;
   Ident_Data_At   : constant := 5;
   Shoff_At        : constant := 40;
   Shentsize_At    : constant := 58;
   Shnum_At        : constant := 60;
   Shstrndx_At     : constant := 62;
   Section_Header_Bytes : constant := 64;
   SHT_NOBITS      : constant := 8;
   SHF_COMPRESSED  : constant := 16#800#;
   Name_Ceiling    : constant := 64;
   Section_Ceiling : constant := 4096;
   GNU_Build_Id    : constant := 3;

   function Name_Of (S : Section_Name) return String is
     (case S is
         when Debug_Info => ".debug_info",
         when Debug_Abbrev => ".debug_abbrev",
         when Debug_Line => ".debug_line",
         when Debug_Str => ".debug_str",
         when Debug_Line_Str => ".debug_line_str",
         when Debug_Loclists => ".debug_loclists",
         when Debug_Rnglists => ".debug_rnglists",
         when Debug_Addr => ".debug_addr",
         when Debug_Str_Offsets => ".debug_str_offsets",
         when Debug_Loc => ".debug_loc",
         when Debug_Ranges => ".debug_ranges",
         when Eh_Frame => ".eh_frame",
         when Text => ".text",
         when Build_Id_Note => ".note.gnu.build-id");

   procedure Read_Sections
     (Data : B.Byte_Buffer; Table : out Section_Table; Result : out Image_Result)
   is
      C : B.Cursor;
      V, Shoff, Shentsize, Shnum, Shstrndx, Str_Offset : Unsigned_64;
      Limit : constant B.Offset := B.Offset (Data'Length);
   begin
      Table := [others => <>];
      Result := Not_Elf;
      if Data'First /= 0 or else Limit < Section_Header_Bytes then
         return;
      end if;
      C := B.Make (0, Limit);
      B.U32 (Data, C, V);
      if V /= ELF_Magic or else Data (Ident_Class_At) /= Class_64
        or else Data (Ident_Data_At) /= Little_Endian
      then
         return;
      end if;
      Result := Truncated;
      C := B.Make (Shoff_At, Limit);
      B.U64 (Data, C, Shoff);
      C := B.Make (Shentsize_At, Limit);
      B.U16 (Data, C, Shentsize);
      B.U16 (Data, C, Shnum);
      B.U16 (Data, C, Shstrndx);
      if not C.Ok or else Shentsize /= Section_Header_Bytes or else Shnum > Section_Ceiling
        or else Shstrndx >= Shnum or else Shoff > Limit
        or else Shnum * Section_Header_Bytes > Limit - Shoff
      then
         return;
      end if;
      --  The section-name string table's file offset.
      C := B.Make (Shoff + Shstrndx * Section_Header_Bytes + 24, Limit);
      B.U64 (Data, C, Str_Offset);
      if not C.Ok then
         return;
      end if;
      for I in 0 .. Shnum - 1 loop
         declare
            Base : constant B.Offset := Shoff + I * Section_Header_Bytes;
            Name_Index, Kind, Flags, Addr, Off, Size : Unsigned_64;
         begin
            C := B.Make (Base, Limit);
            B.U32 (Data, C, Name_Index);
            B.U32 (Data, C, Kind);
            B.U64 (Data, C, Flags);
            B.U64 (Data, C, Addr);
            B.U64 (Data, C, Off);
            B.U64 (Data, C, Size);
            if C.Ok and then Str_Offset <= Limit and then Name_Index < Limit - Str_Offset then
               declare
                  Name : constant String := B.C_String (Data, Str_Offset + Name_Index, Name_Ceiling);
               begin
                  for S in Section_Name loop
                     if Name = Name_Of (S) and then not Table (S).Present then
                        Table (S) :=
                          (Present => True,
                           In_File => Kind /= SHT_NOBITS and then Off <= Limit
                                      and then Size <= Limit - Off,
                           Compressed => (Flags and SHF_COMPRESSED) /= 0,
                           File_Offset => Off, Size => Size, Address => Addr);
                     end if;
                  end loop;
               end;
            end if;
         end;
      end loop;
      Result := Opened;
   end Read_Sections;

   procedure Read_Build_Id
     (Data : B.Byte_Buffer; Table : Section_Table;
      Id : out String; Length : out Natural)
   is
      Hex : constant String := "0123456789abcdef";
      S : constant Section := Table (Build_Id_Note);
      C : B.Cursor;
      Name_Size, Desc_Size, Kind, Byte : Unsigned_64;
   begin
      Id := [others => '0'];
      Length := 0;
      if not S.In_File then
         return;
      end if;
      C := B.Make (S.File_Offset, S.File_Offset + S.Size);
      B.U32 (Data, C, Name_Size);
      B.U32 (Data, C, Desc_Size);
      B.U32 (Data, C, Kind);
      if not C.Ok or else Kind /= GNU_Build_Id or else Desc_Size /= Build_Id_Bytes then
         return;
      end if;
      B.Skip (C, (Name_Size + 3) / 4 * 4);
      for I in 0 .. Build_Id_Bytes - 1 loop
         B.U8 (Data, C, Byte);
         exit when not C.Ok;
         Id (Id'First + 2 * I) := Hex (Natural (Byte / 16) + 1);
         Id (Id'First + 2 * I + 1) := Hex (Natural (Byte mod 16) + 1);
      end loop;
      if C.Ok then
         Length := 2 * Build_Id_Bytes;
      end if;
   end Read_Build_Id;
end CuBit.Debug.Elf;
