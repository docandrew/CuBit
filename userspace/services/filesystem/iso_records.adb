package body ISO_Records with SPARK_Mode => On is
   subtype Word_Start is Natural range 0 .. Block_Bytes - 8;
   function LE32 (D : Sector; P : Word_Start) return Unsigned_32 is
     (Unsigned_32 (D (P)) or Shift_Left (Unsigned_32 (D (P + 1)), 8) or
      Shift_Left (Unsigned_32 (D (P + 2)), 16) or
      Shift_Left (Unsigned_32 (D (P + 3)), 24));
   function Dual32 (D : Sector; P : Word_Start) return Boolean is
     (LE32 (D, P) =
       (Shift_Left (Unsigned_32 (D (P + 4)), 24) or
        Shift_Left (Unsigned_32 (D (P + 5)), 16) or
        Shift_Left (Unsigned_32 (D (P + 6)), 8) or Unsigned_32 (D (P + 7))));
   function Header_Valid (Data : Sector) return Boolean is
     (Data (1 .. 5) = [67, 68, 48, 48, 49] and then Data (6) = 1);

   procedure Decode_Record
     (Data : Sector; Offset : Natural; Volume_Blocks : Unsigned_64;
      Item : out File_Record; Consumed : out Natural; Valid : out Boolean)
   is
      Length, Names : Natural;
      Blocks : Unsigned_64;
   begin
      Item := (others => <>); Consumed := 0; Valid := False;
      if Offset > Block_Bytes - 34 then return; end if;
      Length := Natural (Data (Offset));
      Names := Natural (Data (Offset + 32));
      if Length < 34 or else Length > Block_Bytes - Offset or else
        Names not in 1 .. 207 or else 33 + Names > Length or else
        Data (Offset + 1) /= 0 or else -- extended attributes unsupported
        not Dual32 (Data, Offset + 2) or else
        not Dual32 (Data, Offset + 10) or else
        (Data (Offset + 25) and not Unsigned_8 (3)) /= 0 or else
        Data (Offset + 26) /= 0 or else Data (Offset + 27) /= 0 or else
        Data (Offset + 28 .. Offset + 31) /= [1, 0, 0, 1]
      then return; end if;
      Blocks := (Unsigned_64 (LE32 (Data, Offset + 10)) + 2047) / 2048;
      if Unsigned_64 (LE32 (Data, Offset + 2)) > Volume_Blocks or else
        Blocks > Volume_Blocks - Unsigned_64 (LE32 (Data, Offset + 2))
      then return; end if;
      Item.Extent := LE32 (Data, Offset + 2);
      Item.Bytes := LE32 (Data, Offset + 10);
      Item.Directory := (Data (Offset + 25) and 2) /= 0;
      Item.Name_Length := Names;
      for I in 1 .. Names loop
         Item.Name (I) := Character'Val (Data (Offset + 32 + I));
      end loop;
      Consumed := Length;
      Valid := True;
   end Decode_Record;

   procedure Decode_Volume
     (Data : Sector; Media_Blocks : Unsigned_64;
      Volume_Blocks : out Unsigned_64; Root : out File_Record;
      Valid : out Boolean)
   is
      Consumed : Natural;
   begin
      Volume_Blocks := 0; Root := (others => <>); Valid := False;
      if not Header_Valid (Data) or else Data (0) /= 1 or else
        not Dual32 (Data, 80) or else
        Data (120 .. 123) /= [1, 0, 0, 1] or else
        Data (124 .. 127) /= [1, 0, 0, 1] or else
        Data (128 .. 131) /= [0, 8, 8, 0] or else Data (881) /= 1 or else
        LE32 (Data, 80) < 17 or else
        Unsigned_64 (LE32 (Data, 80)) > Media_Blocks
      then return; end if;
      Decode_Record (Data, 156, Unsigned_64 (LE32 (Data, 80)), Root,
                     Consumed, Valid);
      Valid := Valid and then Consumed = 34 and then Root.Directory and then
        Root.Bytes > 0 and then Root.Bytes mod 2048 = 0 and then
        Root.Name_Length = 1 and then Root.Name (1) = Character'Val (0);
      if Valid then Volume_Blocks := Unsigned_64 (LE32 (Data, 80));
      else Root := (others => <>); end if;
   end Decode_Volume;

   function Fold (C : Character) return Character is
     (if C in 'a' .. 'z' then Character'Val (Character'Pos (C) - 32) else C);

   function Matches (Item : File_Record; Name : String) return Boolean is
      Length : Natural := Item.Name_Length;
   begin
      if Length >= 2 and then Item.Name (Length - 1 .. Length) = ";1" then
         Length := Length - 2;
      end if;
      --  ISO identifiers for files without an extension can end in a dot.
      if Length > 0 and then Item.Name (Length) = '.' then
         Length := Length - 1;
      end if;
      if Name'Length /= Length or else Length = 0 then return False; end if;
      for I in 1 .. Length loop
         if Fold (Item.Name (I)) /= Fold (Name (Name'First + (I - 1))) then
            return False;
         end if;
      end loop;
      return True;
   end Matches;
end ISO_Records;
