package body Lexicon_Images with SPARK_Mode is

   MAGIC : constant String := "CUBITLEX";
   MAGIC_BYTES : constant := 8;
   VERSION_AT  : constant := 8;
   SECTIONS_AT : constant := 12;
   SECTION_ENTRY_BYTES : constant := 8;

   --  Little-endian fields at P (absolute).
   function U8 (Data : Byte_Array; P : Image_Offset) return Unsigned_8 is (Data (P))
     with Pre => Data'First = 0 and then P < Data'Length;

   function U16 (Data : Byte_Array; P : Image_Offset) return Unsigned_16 is
     (Unsigned_16 (Data (P)) or Shift_Left (Unsigned_16 (Data (P + 1)), 8))
     with Pre => Data'First = 0 and then Data'Length >= 2 and then P <= Data'Length - 2;

   function U32 (Data : Byte_Array; P : Image_Offset) return Unsigned_32 is
     (Unsigned_32 (Data (P)) or Shift_Left (Unsigned_32 (Data (P + 1)), 8)
      or Shift_Left (Unsigned_32 (Data (P + 2)), 16) or Shift_Left (Unsigned_32 (Data (P + 3)), 24))
     with Pre => Data'First = 0 and then Data'Length >= 4 and then P <= Data'Length - 4;

   --  A 32-bit field as an image size; False when it cannot be one.
   procedure To_Size (V : Unsigned_32; Size : out Image_Size; Ok : out Boolean)
     with Post => (if Ok then Size = Image_Size (V))
   is
   begin
      if V <= Unsigned_32 (MAXIMUM_IMAGE_BYTES) then
         Size := Image_Size (V);
         Ok := True;
      else
         Size := 0;
         Ok := False;
      end if;
   end To_Size;

   procedure Open (Data : Byte_Array; Result : out Layout; Status : out Open_Status) is
      Offset, Count : Image_Size;
      Ok_Offset, Ok_Count : Boolean;
      Entry_At : Image_Offset;
   begin
      Result := (Length => 0, Sections => [others => (0, 0)]);
      if Data'Length < HEADER_BYTES then
         Status := Too_Short;
         return;
      end if;
      for I in 0 .. MAGIC_BYTES - 1 loop
         if Data (I) /= Character'Pos (MAGIC (MAGIC'First + I)) then
            Status := Bad_Magic;
            return;
         end if;
      end loop;
      if U32 (Data, VERSION_AT) /= FORMAT_VERSION then
         Status := Wrong_Version;
         return;
      end if;
      Result.Length := Data'Length;
      for S in Section loop
         pragma Loop_Invariant (Result.Length = Data'Length);
         pragma Loop_Invariant
           (for all Earlier in Section =>
              (if Earlier < S then
                 Result.Sections (Earlier).Offset <= Result.Length
                 and then Long_Long_Integer (Result.Sections (Earlier).Count)
                          * Long_Long_Integer (WIDTH (Earlier))
                          <= Long_Long_Integer (Result.Length - Result.Sections (Earlier).Offset)
               else Result.Sections (Earlier) = (0, 0)));
         Entry_At := SECTIONS_AT + Section'Pos (S) * SECTION_ENTRY_BYTES;
         To_Size (U32 (Data, Entry_At), Offset, Ok_Offset);
         To_Size (U32 (Data, Entry_At + 4), Count, Ok_Count);
         if not Ok_Offset or else not Ok_Count or else Offset < HEADER_BYTES
           or else Offset > Data'Length
           or else Long_Long_Integer (Count) * Long_Long_Integer (WIDTH (S))
                   > Long_Long_Integer (Data'Length - Offset)
         then
            Result := (Length => 0, Sections => [others => (0, 0)]);
            Status := Bad_Table;
            return;
         end if;
         Result.Sections (S) := (Offset => Offset, Count => Count);
      end loop;
      Status := Opened;
   end Open;

   procedure Declared_Size (Header : Byte_Array; Size : out Image_Size; Status : out Open_Status) is
      Entry_At : constant Image_Offset := SECTIONS_AT + Section'Pos (Text) * SECTION_ENTRY_BYTES;
      Offset, Count : Unsigned_32;
   begin
      Size := 0;
      for I in 0 .. MAGIC_BYTES - 1 loop
         if Header (I) /= Character'Pos (MAGIC (MAGIC'First + I)) then
            Status := Bad_Magic;
            return;
         end if;
      end loop;
      if U32 (Header, VERSION_AT) /= FORMAT_VERSION then
         Status := Wrong_Version;
         return;
      end if;
      Offset := U32 (Header, Entry_At);
      Count := U32 (Header, Entry_At + 4);
      if Offset > Unsigned_32 (MAXIMUM_IMAGE_BYTES) or else Count > Unsigned_32 (MAXIMUM_IMAGE_BYTES) - Offset then
         Status := Too_Large;
         return;
      end if;
      Size := Image_Size (Offset + Count);
      Status := Opened;
   end Declared_Size;

   --  The first byte of row R of table S.
   function Row_At (L : Layout; S : Section; R : Image_Size) return Image_Offset is
     (L.Sections (S).Offset + R * WIDTH (S))
     with Pre  => Fits (L) and then R < L.Sections (S).Count,
          Post => Row_At'Result <= L.Length - WIDTH (S);

   --  Proof of Row_At's bound, kept out of every caller.
   procedure Row_Bound (L : Layout; S : Section; R : Image_Size)
     with Ghost, Pre => Fits (L) and then R < L.Sections (S).Count,
          Post => Long_Long_Integer (L.Sections (S).Offset) + Long_Long_Integer (R) * Long_Long_Integer (WIDTH (S))
                  + Long_Long_Integer (WIDTH (S)) <= Long_Long_Integer (L.Length)
   is
   begin
      pragma Assert (Long_Long_Integer (R) + 1 <= Long_Long_Integer (L.Sections (S).Count));
      pragma Assert (Long_Long_Integer (R) * Long_Long_Integer (WIDTH (S)) + Long_Long_Integer (WIDTH (S))
                     <= Long_Long_Integer (L.Sections (S).Count) * Long_Long_Integer (WIDTH (S)));
   end Row_Bound;

   --  Fields compared as exact integers: a 32-bit field always fits.
   subtype Wide is Long_Long_Integer;

   --  A slice of the Text table, from a relative offset and a length.
   procedure Text_Of (L : Layout; Relative : Unsigned_32; Length : Unsigned_32;
                      Slice : out Text_Slice; Valid : out Boolean)
     with Pre  => Fits (L),
          Post => (if Valid then Slice.First <= L.Length and then Slice.Length <= L.Length - Slice.First
                   and then Slice.Length = Image_Size (Length))
   is
      Text_Count : constant Image_Size := L.Sections (Text).Count;
      R : constant Wide := Wide (Relative);
      N : constant Wide := Wide (Length);
   begin
      Slice := (0, 0);
      Valid := R <= Wide (Text_Count) and then N <= Wide (Text_Count) - R;
      if Valid then
         pragma Assert (WIDTH (Text) = 1);
         pragma Assert (L.Sections (Text).Offset <= L.Length);
         pragma Assert (Wide (Text_Count) * Wide (WIDTH (Text)) <= Wide (L.Length - L.Sections (Text).Offset));
         Slice := (First => L.Sections (Text).Offset + Image_Size (R), Length => Image_Size (N));
      end if;
   end Text_Of;

   --  A first/count pair against a table of Limit rows.
   procedure Range_Of (First_Field, Count_Field : Unsigned_32; Limit : Image_Size;
                       First, Count : out Image_Size; Valid : out Boolean)
     with Post => (if Valid then First <= Limit and then Count <= Limit - First)
   is
      F : constant Wide := Wide (First_Field);
      N : constant Wide := Wide (Count_Field);
   begin
      First := 0;
      Count := 0;
      Valid := F <= Wide (Limit) and then N <= Wide (Limit) - F;
      if Valid then
         First := Image_Size (F);
         Count := Image_Size (N);
      end if;
   end Range_Of;

   --  A 32-bit row number below Limit.
   procedure Index_Of (V : Unsigned_32; Limit : Image_Size; Index : out Image_Size; Valid : out Boolean)
     with Post => (if Valid then Index < Limit)
   is
      W : constant Wide := Wide (V);
   begin
      Index := 0;
      Valid := W < Wide (Limit);
      if Valid then
         Index := Image_Size (W);
      end if;
   end Index_Of;

   function Read_Word (Data : Byte_Array; L : Layout; W : Word_Id) return Word_Record is
      Result : Word_Record;
      P : Image_Offset;
      First, Count : Image_Size;
      Ok_Key, Ok_Range : Boolean;
   begin
      Row_Bound (L, Words, Image_Size (W));
      P := Row_At (L, Words, Image_Size (W));
      Text_Of (L, U32 (Data, P), Unsigned_32 (U8 (Data, P + 4)), Result.Key, Ok_Key);
      Result.Band := Commonness (U8 (Data, P + 5));
      Range_Of (U32 (Data, P + 8), Unsigned_32 (U16 (Data, P + 6)), L.Sections (Senses).Count,
                First, Count, Ok_Range);
      Result.First_Sense := Sense_Id (First);
      Result.Sense_Count := Count;
      Result.Valid := Ok_Key and then Ok_Range;
      return Result;
   end Read_Word;

   function Read_Sense (Data : Byte_Array; L : Layout; S : Sense_Id) return Sense_Record is
      Result : Sense_Record;
      P : Image_Offset;
      Synset, Word : Image_Size;
      First, Count : Image_Size;
      Ok_Text, Ok_Range, Ok_Synset, Ok_Word : Boolean;
   begin
      Row_Bound (L, Senses, Image_Size (S));
      P := Row_At (L, Senses, Image_Size (S));
      Index_Of (U32 (Data, P), L.Sections (Synsets).Count, Synset, Ok_Synset);
      Index_Of (U32 (Data, P + 4), L.Sections (Words).Count, Word, Ok_Word);
      Text_Of (L, U32 (Data, P + 8), Unsigned_32 (U8 (Data, P + 12)), Result.Display, Ok_Text);
      Range_Of (U32 (Data, P + 14), Unsigned_32 (U8 (Data, P + 13)), L.Sections (Antonyms).Count,
                First, Count, Ok_Range);
      Result.First_Antonym := Row (First);
      Result.Antonym_Count := Count;
      if Ok_Text and then Ok_Range and then Ok_Synset and then Ok_Word then
         Result.Synset := Synset_Id (Synset);
         Result.Word := Word_Id (Word);
         Result.Valid := True;
      end if;
      return Result;
   end Read_Sense;

   function Read_Synset (Data : Byte_Array; L : Layout; S : Synset_Id) return Synset_Record is
      Result : Synset_Record;
      P : Image_Offset;
      Part : Unsigned_8;
      First, Count : Image_Size;
      Ok_Gloss, Ok_Examples, Ok_Members, Ok_Relations : Boolean;
   begin
      Row_Bound (L, Synsets, Image_Size (S));
      P := Row_At (L, Synsets, Image_Size (S));
      --  pos u8 @0, gloss u32 @1 len u16 @5, examples u32 @7 count u8 @11,
      --  members u32 @12 count u8 @16, relations u32 @17 count u16 @21.
      Part := U8 (Data, P);
      Text_Of (L, U32 (Data, P + 1), Unsigned_32 (U16 (Data, P + 5)), Result.Gloss, Ok_Gloss);
      Range_Of (U32 (Data, P + 7), Unsigned_32 (U8 (Data, P + 11)), L.Sections (Examples).Count,
                First, Count, Ok_Examples);
      Result.First_Example := Row (First);
      Result.Example_Count := Count;
      Range_Of (U32 (Data, P + 12), Unsigned_32 (U8 (Data, P + 16)), L.Sections (Members).Count,
                First, Count, Ok_Members);
      Result.First_Member := Row (First);
      Result.Member_Count := Count;
      Range_Of (U32 (Data, P + 17), Unsigned_32 (U16 (Data, P + 21)), L.Sections (Relations).Count,
                First, Count, Ok_Relations);
      Result.First_Relation := Row (First);
      Result.Relation_Count := Count;
      if Part <= Part_Of_Speech'Pos (Part_Of_Speech'Last) then
         Result.Part := Part_Of_Speech'Val (Part);
         Result.Valid := Ok_Gloss and then Ok_Examples and then Ok_Members and then Ok_Relations
           and then Result.Gloss.Length <= MAXIMUM_TEXT_ITEM_BYTES;
      end if;
      return Result;
   end Read_Synset;

   procedure Read_Member (Data : Byte_Array; L : Layout; R : Row; Sense : out Sense_Id; Valid : out Boolean) is
      P : Image_Offset;
      V : Image_Size;
   begin
      Row_Bound (L, Members, Image_Size (R));
      P := Row_At (L, Members, Image_Size (R));
      Index_Of (U32 (Data, P), L.Sections (Senses).Count, V, Valid);
      Sense := Sense_Id (V);
   end Read_Member;

   procedure Read_Antonym (Data : Byte_Array; L : Layout; R : Row; Sense : out Sense_Id; Valid : out Boolean) is
      P : Image_Offset;
      V : Image_Size;
   begin
      Row_Bound (L, Antonyms, Image_Size (R));
      P := Row_At (L, Antonyms, Image_Size (R));
      Index_Of (U32 (Data, P), L.Sections (Senses).Count, V, Valid);
      Sense := Sense_Id (V);
   end Read_Antonym;

   function Read_Relation (Data : Byte_Array; L : Layout; R : Row) return Relation_Record is
      Result : Relation_Record;
      P : Image_Offset;
      Kind : Unsigned_8;
      Target : Image_Size;
      Ok_Target : Boolean;
   begin
      Row_Bound (L, Relations, Image_Size (R));
      P := Row_At (L, Relations, Image_Size (R));
      Kind := U8 (Data, P);
      Index_Of (U32 (Data, P + 1), L.Sections (Synsets).Count, Target, Ok_Target);
      if Kind <= Relation_Kind'Pos (Relation_Kind'Last) and then Ok_Target then
         Result := (Valid => True, Kind => Relation_Kind'Val (Kind), Target => Synset_Id (Target));
      end if;
      return Result;
   end Read_Relation;

   procedure Read_Example (Data : Byte_Array; L : Layout; R : Row; Slice : out Text_Slice; Valid : out Boolean) is
      P : Image_Offset;
   begin
      Row_Bound (L, Examples, Image_Size (R));
      P := Row_At (L, Examples, Image_Size (R));
      Text_Of (L, U32 (Data, P), Unsigned_32 (U16 (Data, P + 4)), Slice, Valid);
      Valid := Valid and then Slice.Length <= MAXIMUM_TEXT_ITEM_BYTES;
   end Read_Example;

   function Read_Exception (Data : Byte_Array; L : Layout; R : Row) return Exception_Record is
      Result : Exception_Record;
      P : Image_Offset;
      Base : Image_Size;
      Ok_Key, Ok_Base : Boolean;
   begin
      Row_Bound (L, Exceptions, Image_Size (R));
      P := Row_At (L, Exceptions, Image_Size (R));
      Text_Of (L, U32 (Data, P), Unsigned_32 (U8 (Data, P + 4)), Result.Key, Ok_Key);
      Index_Of (U32 (Data, P + 5), L.Sections (Words).Count, Base, Ok_Base);
      if Ok_Key and then Ok_Base then
         Result.Base := Word_Id (Base);
         Result.Valid := True;
      end if;
      return Result;
   end Read_Exception;

end Lexicon_Images;
