pragma Ada_2022;

package body Manifest_Fingerprints with SPARK_Mode is

   ELF_Header_Bytes   : constant := 64;
   Section_Header_Bytes : constant := 64;
   SHOFF_At     : constant := 40;
   SHENTSIZE_At : constant := 58;
   SHNUM_At     : constant := 60;
   SHSTRNDX_At  : constant := 62;
   SH_Name_At   : constant := 0;
   SH_Type_At   : constant := 4;
   SH_Offset_At : constant := 24;
   SH_Size_At   : constant := 32;
   SHT_PROGBITS : constant := 1;
   Size_Field_Bytes : constant := 8;
   Offset_Field_Bytes : constant := 8;
   Word_Field_Bytes : constant := 4;
   Half_Field_Bytes : constant := 2;
   Byte_Base : constant := 256;

   --  The little-endian Width-byte field at 0-based Offset of Image, when
   --  it is at most Bound (Within); otherwise Within is False. Natural
   --  arithmetic only: a field naming more than the image is refused
   --  before it is used as an index.
   procedure Field
     (Image : Bytes; Offset : Natural; Width : Positive; Bound : Natural;
      Value : out Natural; Within : out Boolean)
   with Pre  => Image'First = 1 and then Width <= 8
                and then Offset <= Image'Length and then Width <= Image'Length - Offset,
        Post => (if Within then Value <= Bound else Value = 0)
   is
      Byte : Natural;
   begin
      Value := 0;
      Within := True;
      for K in reverse 1 .. Width loop
         pragma Loop_Invariant (Within and then Value <= Bound);
         Byte := Natural (Image (Offset + K));
         if Bound < Byte or else Value > (Bound - Byte) / Byte_Base then
            Value := 0;
            Within := False;
            return;
         end if;
         Value := Value * Byte_Base + Byte;
      end loop;
   end Field;

   procedure Fingerprint
     (Image : Bytes; Into : out Bytes; Length : out Fingerprint_Length; Result : out Reading)
   is
      Size : constant Natural := Image'Length;
      Entry_Size, Shoff, Shnum, Shstrndx : Natural;
      Names_Offset, Names_Size : Natural := 0;
      Used : Fingerprint_Length := 0;
      Ok : Boolean;

      --  Whether the name at Name (0-based, in the names section) starts
      --  with Section_Prefix and ends inside it; Name_Length its length.
      procedure Name_Of (Name : Natural; Matches : out Boolean; Name_Length : out Natural)
      with Pre  => Image'First = 1 and then Size = Image'Length
                   and then Names_Offset <= Size and then Names_Size <= Size - Names_Offset,
           Post => (if Matches then Name < Names_Size
                      and then Name_Length <= Names_Size - Name
                      and then Name_Length <= Maximum_Fingerprint_Bytes)
      is
         Position : Natural;
      begin
         Matches := False;
         Name_Length := 0;
         if Name >= Names_Size then
            return;
         end if;
         Position := Name;
         while Position < Names_Size
           and then Image (Names_Offset + Position + 1) /= 0
           and then Name_Length < Maximum_Fingerprint_Bytes
         loop
            pragma Loop_Invariant (Position >= Name and then Position < Names_Size);
            pragma Loop_Invariant (Name_Length = Position - Name);
            pragma Loop_Invariant (Name_Length < Maximum_Fingerprint_Bytes);
            Position := Position + 1;
            Name_Length := Name_Length + 1;
         end loop;
         if Position >= Names_Size or else Name_Length < Section_Prefix'Length then
            Name_Length := 0;
            return;
         end if;
         Matches := True;
         for K in 1 .. Section_Prefix'Length loop
            if Image (Names_Offset + Name + K) /=
              Character'Pos (Section_Prefix (Section_Prefix'First + K - 1))
            then
               Matches := False;
            end if;
         end loop;
      end Name_Of;
   begin
      Into := [others => 0];
      Length := 0;
      if Size < ELF_Header_Bytes then
         Result := Not_ELF64;
         return;
      end if;
      Field (Image, SHENTSIZE_At, Half_Field_Bytes, Size, Entry_Size, Ok);
      if not Ok or else Entry_Size /= Section_Header_Bytes then
         Result := Not_ELF64;
         return;
      end if;
      Field (Image, SHOFF_At, Offset_Field_Bytes, Size, Shoff, Ok);
      if not Ok then
         Result := Headers_Outside;
         return;
      end if;
      Field (Image, SHNUM_At, Half_Field_Bytes, (Size - Shoff) / Section_Header_Bytes, Shnum, Ok);
      if not Ok then
         Result := Headers_Outside;
         return;
      end if;
      Field (Image, SHSTRNDX_At, Half_Field_Bytes, Size, Shstrndx, Ok);
      if not Ok or else Shstrndx >= Shnum then
         Result := Headers_Outside;
         return;
      end if;
      declare
         Names_Header : constant Natural := Shoff + Shstrndx * Section_Header_Bytes;
      begin
         Field (Image, Names_Header + SH_Offset_At, Offset_Field_Bytes, Size, Names_Offset, Ok);
         if Ok then
            Field (Image, Names_Header + SH_Size_At, Size_Field_Bytes, Size - Names_Offset, Names_Size, Ok);
         end if;
         if not Ok then
            Result := Headers_Outside;
            return;
         end if;
      end;
      for I in 0 .. Shnum - 1 loop
         pragma Loop_Invariant (Used <= Maximum_Fingerprint_Bytes);
         pragma Loop_Invariant (Names_Offset <= Size and then Names_Size <= Size - Names_Offset);
         declare
            Header : constant Natural := Shoff + I * Section_Header_Bytes;
            Kind, Name, Offset, Bytes_In : Natural;
            Kind_Ok, Name_Ok, Offset_Ok, Size_Ok : Boolean;
            Matches : Boolean := False;
            Name_Length : Natural := 0;
         begin
            Field (Image, Header + SH_Type_At, Word_Field_Bytes, SHT_PROGBITS, Kind, Kind_Ok);
            Field (Image, Header + SH_Name_At, Word_Field_Bytes, Size, Name, Name_Ok);
            if Kind_Ok and then Kind = SHT_PROGBITS and then Name_Ok then
               Name_Of (Name, Matches, Name_Length);
            end if;
            if Matches then
               Field (Image, Header + SH_Offset_At, Offset_Field_Bytes, Size, Offset, Offset_Ok);
               Size_Ok := False;
               Bytes_In := 0;
               if Offset_Ok then
                  Field (Image, Header + SH_Size_At, Size_Field_Bytes, Size - Offset, Bytes_In, Size_Ok);
               end if;
               if not Size_Ok then
                  Result := Section_Outside;
                  Into := [others => 0];
                  return;
               end if;
               if Bytes_In > Maximum_Fingerprint_Bytes
                 or else Name_Length + 1 + Size_Field_Bytes + Bytes_In > Maximum_Fingerprint_Bytes - Used
               then
                  Result := Too_Large;
                  Into := [others => 0];
                  return;
               end if;
               declare
                  Name_At : constant Natural := Names_Offset + Name;
                  Start : constant Natural := Used;
               begin
                  for K in 1 .. Name_Length loop
                     pragma Loop_Invariant (Used = Start + K - 1);
                     Used := Used + 1;
                     Into (Used) := Image (Name_At + K);
                  end loop;
               end;
               Used := Used + 1;
               Into (Used) := 0;
               declare
                  Start : constant Natural := Used;
                  Remaining : Natural := Bytes_In;
               begin
                  for K in 1 .. Size_Field_Bytes loop
                     pragma Loop_Invariant (Used = Start + K - 1);
                     Used := Used + 1;
                     Into (Used) := Unsigned_8 (Remaining mod Byte_Base);
                     Remaining := Remaining / Byte_Base;
                  end loop;
               end;
               declare
                  Start : constant Natural := Used;
               begin
                  for K in 1 .. Bytes_In loop
                     pragma Loop_Invariant (Used = Start + K - 1);
                     Used := Used + 1;
                     Into (Used) := Image (Offset + K);
                  end loop;
               end;
            end if;
         end;
      end loop;
      Length := Used;
      Result := Read;
   end Fingerprint;
end Manifest_Fingerprints;
