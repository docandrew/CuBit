package body Bible_Images with SPARK_Mode is

   MAGIC : constant String := "CUBITBIB";
   MAGIC_BYTES : constant := 8;
   VERSION_AT : constant := 8;
   BOOKS_AT : constant := 12;
   PROPERTIES_AT : constant := 16;
   SPANS_AT : constant := 20;
   SECTIONS_AT : constant := 24;
   --  Header property bits (tools/build_bible.py).
   WORDS_OF_CHRIST_PROPERTY : constant Unsigned_32 := 1;
   SECTION_ENTRY_BYTES : constant := 8;
   --  A segment's flags (tools/build_bible.py).
   STANZA_BREAK_FLAG    : constant Unsigned_8 := 1;
   WORDS_OF_CHRIST_FLAG : constant Unsigned_8 := 2;
   JOINED_FLAG          : constant Unsigned_8 := 4;

   --  Fields compared as exact integers: a 32-bit field always fits.
   subtype Wide is Long_Long_Integer;

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

   function Has_Magic (Data : Byte_Array) return Boolean is
     (for all I in 0 .. MAGIC_BYTES - 1 => Data (I) = Character'Pos (MAGIC (MAGIC'First + I)))
   with Pre => Data'First = 0 and then Data'Length >= HEADER_BYTES;

   procedure Open (Data : Byte_Array; Result : out Layout; Status : out Open_Status) is
      Offset, Count : Image_Size;
      Ok_Offset, Ok_Count : Boolean;
      Entry_At : Image_Offset;
   begin
      Result := (Length => 0, Sections => [others => (0, 0)], others => <>);
      if Data'Length < HEADER_BYTES then
         Status := Too_Short;
         return;
      elsif not Has_Magic (Data) then
         Status := Bad_Magic;
         return;
      elsif U32 (Data, VERSION_AT) /= FORMAT_VERSION then
         Status := Wrong_Version;
         return;
      elsif U32 (Data, BOOKS_AT) /= BOOK_COUNT then
         Status := Wrong_Canon;
         return;
      end if;
      --  Whether red letters can be shown: the flag, and the span count it
      --  must agree with.
      declare
         Properties : constant Unsigned_32 := U32 (Data, PROPERTIES_AT);
         Spans : constant Unsigned_32 := U32 (Data, SPANS_AT);
      begin
         if (Properties and not WORDS_OF_CHRIST_PROPERTY) /= 0
           or else Spans > MAXIMUM_SPANS
           or else ((Properties and WORDS_OF_CHRIST_PROPERTY) /= 0) /= (Spans > 0)
         then
            Status := Bad_Properties;
            return;
         end if;
         Result.Words_Of_Christ := Spans > 0;
         Result.Words_Of_Christ_Spans := Span_Count (Spans);
      end;
      Result.Length := Data'Length;
      for S in Section loop
         pragma Loop_Invariant (Result.Length = Data'Length and then Consistent (Result));
         pragma Loop_Invariant
           (for all Earlier in Section =>
              (if Earlier < S then
                 Result.Sections (Earlier).Offset <= Result.Length
                 and then Long_Long_Integer (Result.Sections (Earlier).Count)
                          * Long_Long_Integer (WIDTH (Earlier))
                          <= Long_Long_Integer (Result.Length - Result.Sections (Earlier).Offset)
               else Result.Sections (Earlier) = (0, 0)));
         pragma Loop_Invariant (if S > Books then Result.Sections (Books).Count = BOOK_COUNT);
         Entry_At := SECTIONS_AT + Section'Pos (S) * SECTION_ENTRY_BYTES;
         To_Size (U32 (Data, Entry_At), Offset, Ok_Offset);
         To_Size (U32 (Data, Entry_At + 4), Count, Ok_Count);
         if not Ok_Offset or else not Ok_Count or else Offset < HEADER_BYTES
           or else Offset > Data'Length
           or else Long_Long_Integer (Count) * Long_Long_Integer (WIDTH (S))
                   > Long_Long_Integer (Data'Length - Offset)
           or else (S = Books and then Count /= BOOK_COUNT)
         then
            Result := (Length => 0, Sections => [others => (0, 0)], others => <>);
            Status := Bad_Table;
            return;
         end if;
         Result.Sections (S) := (Offset => Offset, Count => Count);
      end loop;
      Status := Opened;
   end Open;

   procedure Declared_Size (Header : Byte_Array; Size : out Image_Size; Status : out Open_Status) is
      Entry_At : constant Image_Offset := SECTIONS_AT + Section'Pos (About) * SECTION_ENTRY_BYTES;
      Offset, Count : Unsigned_32;
   begin
      Size := 0;
      if not Has_Magic (Header) then
         Status := Bad_Magic;
         return;
      elsif U32 (Header, VERSION_AT) /= FORMAT_VERSION then
         Status := Wrong_Version;
         return;
      end if;
      Offset := U32 (Header, Entry_At);
      Count := U32 (Header, Entry_At + 4);
      if Offset > Unsigned_32 (MAXIMUM_IMAGE_BYTES)
        or else Count > (Unsigned_32 (MAXIMUM_IMAGE_BYTES) - Offset) / Unsigned_32 (WIDTH (About))
      then
         Status := Too_Large;
         return;
      end if;
      Size := Image_Size (Offset + Count * Unsigned_32 (WIDTH (About)));
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

   --  A slice of a byte table (Text or Keys), from a relative offset and a
   --  length.
   procedure Slice_Of (L : Layout; Table : Section; Relative : Unsigned_32; Length : Unsigned_32;
                       Slice : out Text_Slice; Valid : out Boolean)
     with Pre  => Fits (L) and then Table in Keys | Text,
          Post => (if Valid then Slice.First <= L.Length and then Slice.Length <= L.Length - Slice.First
                   and then Slice.Length = Image_Size (Length))
   is
      Table_Count : constant Image_Size := L.Sections (Table).Count;
      R : constant Wide := Wide (Relative);
      N : constant Wide := Wide (Length);
   begin
      Slice := (0, 0);
      Valid := R <= Wide (Table_Count) and then N <= Wide (Table_Count) - R;
      if Valid then
         pragma Assert (WIDTH (Table) = 1);
         pragma Assert (L.Sections (Table).Offset <= L.Length);
         pragma Assert (Wide (Table_Count) * Wide (WIDTH (Table)) <= Wide (L.Length - L.Sections (Table).Offset));
         Slice := (First => L.Sections (Table).Offset + Image_Size (R), Length => Image_Size (N));
      end if;
   end Slice_Of;

   --  A first/count pair against a table of Limit rows.
   procedure Range_Of (First_Field, Count_Field : Unsigned_32; Limit : Image_Size;
                       First, Count : out Image_Size; Valid : out Boolean)
     with Post => (if Valid then First <= Limit and then Count <= Limit - First
                   and then Count = Image_Size (Count_Field))
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

   ---------------------------------------------------------------------------
   --  Books, chapters and verses.
   ---------------------------------------------------------------------------
   procedure Index (Data : Byte_Array; L : Layout; Result : out Book_Index; Status : out Index_Status) is
      Running : Chapter_Total := 0;
      First_Chapter, Chapter_Count, First_Verse, Verse_Count : Image_Size;
      Ok_Range : Boolean;
      P : Image_Offset;
      Verses_Total : constant Image_Size := Count (L, Verses);
   begin
      Result := (others => <>);
      for B in Book loop
         pragma Loop_Invariant (Valid (Result.Table) and then Indexed (L, Result));
         pragma Loop_Invariant
           (for all S in Chapter_Slot => (if S > Running then Result.Table.Verses (S) = 0
                                          and then Result.First_Verse (S) = 0));
         pragma Loop_Invariant
           (for all Later in Book => (if Later >= B then Result.Table.Chapters (Later) = 0
                                      and then Result.Table.Before (Later) = 0));
         Row_Bound (L, Books, Book'Pos (B));
         P := Row_At (L, Books, Book'Pos (B));
         Range_Of (U32 (Data, P), Unsigned_32 (U16 (Data, P + 4)), Count (L, Chapters),
                   First_Chapter, Chapter_Count, Ok_Range);
         if not Ok_Range or else Chapter_Count > MAXIMUM_CHAPTER then
            Result := (others => <>);
            Status := Bad_Book;
            return;
         elsif Chapter_Count > MAXIMUM_CHAPTERS - Running then
            Result := (others => <>);
            Status := Too_Many_Chapters;
            return;
         end if;
         for C in 1 .. Chapter_Count loop
            pragma Loop_Invariant (Valid (Result.Table) and then Indexed (L, Result));
            pragma Loop_Invariant
              (for all S in Chapter_Slot => (if S >= Running + C then Result.Table.Verses (S) = 0
                                             and then Result.First_Verse (S) = 0));
            pragma Loop_Invariant
              (for all Later in Book => (if Later >= B then Result.Table.Chapters (Later) = 0
                                         and then Result.Table.Before (Later) = 0));
            Row_Bound (L, Chapters, First_Chapter + C - 1);
            P := Row_At (L, Chapters, First_Chapter + C - 1);
            Range_Of (U32 (Data, P), Unsigned_32 (U16 (Data, P + 4)), Verses_Total,
                      First_Verse, Verse_Count, Ok_Range);
            if not Ok_Range or else Verse_Count > MAXIMUM_VERSE then
               Result := (others => <>);
               Status := Bad_Chapter;
               return;
            end if;
            Result.Table.Verses (Running + C) := Verse_Count;
            Result.First_Verse (Running + C) := First_Verse;
         end loop;
         Result.Table.Chapters (B) := Chapter_Count;
         Result.Table.Before (B) := Running;
         Running := Running + Chapter_Count;
      end loop;
      Status := Index_Read;
   end Index;

   procedure Locate (L : Layout; I : Book_Index; V : Image_Size; Item : out Reference; Found : out Boolean) is
      S : Chapter_Slot;
   begin
      Item := (others => <>);
      Found := False;
      for B in Book loop
         for C in 1 .. I.Table.Chapters (B) loop
            S := I.Table.Before (B) + C;
            if V >= I.First_Verse (S) and then V - I.First_Verse (S) < I.Table.Verses (S) then
               Item := (Book => B, Chapter => C, Verse => V - I.First_Verse (S) + 1);
               Found := True;
               return;
            end if;
         end loop;
      end loop;
   end Locate;

   ---------------------------------------------------------------------------
   --  Records.
   ---------------------------------------------------------------------------
   function Read_Verse (Data : Byte_Array; L : Layout; V : Image_Size) return Verse_Record is
      Result : Verse_Record;
      P : Image_Offset;
      First, Number : Image_Size;
      Ok_Range : Boolean;
   begin
      Row_Bound (L, Verses, V);
      P := Row_At (L, Verses, V);
      Range_Of (U32 (Data, P), Unsigned_32 (U16 (Data, P + 4)), Count (L, Segments), First, Number, Ok_Range);
      if Ok_Range and then Number <= MAXIMUM_VERSE_SEGMENTS then
         Result := (Valid => True, First_Segment => First, Segments => Number);
      end if;
      return Result;
   end Read_Verse;

   function Read_Segment (Data : Byte_Array; L : Layout; S : Image_Size) return Segment_Record is
      Result : Segment_Record;
      P : Image_Offset;
      Ok_Text : Boolean;
      Kind_Code, Flags : Unsigned_8;
   begin
      Row_Bound (L, Segments, S);
      P := Row_At (L, Segments, S);
      Slice_Of (L, Text, U32 (Data, P), Unsigned_32 (U16 (Data, P + 4)), Result.Content, Ok_Text);
      Kind_Code := U8 (Data, P + 6);
      Flags := U8 (Data, P + 7);
      if Ok_Text and then Result.Content.Length <= MAXIMUM_SEGMENT_BYTES
        and then Kind_Code <= Segment_Kind'Pos (Segment_Kind'Last)
      then
         Result.Kind := Segment_Kind'Val (Kind_Code);
         Result.Stanza_Break := (Flags and STANZA_BREAK_FLAG) /= 0;
         Result.Words_Of_Christ := (Flags and WORDS_OF_CHRIST_FLAG) /= 0;
         Result.Joined := (Flags and JOINED_FLAG) /= 0;
         Result.Valid := True;
      else
         Result.Content := (0, 0);
      end if;
      return Result;
   end Read_Segment;

   function Read_Word (Data : Byte_Array; L : Layout; W : Image_Size) return Word_Record is
      Result : Word_Record;
      P : Image_Offset;
      Ok_Key, Ok_Range : Boolean;
      First, Number : Image_Size;
   begin
      Row_Bound (L, Words, W);
      P := Row_At (L, Words, W);
      Slice_Of (L, Keys, U32 (Data, P), Unsigned_32 (U16 (Data, P + 4)), Result.Key, Ok_Key);
      Range_Of (U32 (Data, P + 8), U32 (Data, P + 12), Count (L, Postings), First, Number, Ok_Range);
      if Ok_Key and then Ok_Range and then Result.Key.Length <= MAXIMUM_KEY_BYTES then
         Result.First_Posting := First;
         Result.Postings := Number;
         Result.Valid := True;
      else
         Result.Key := (0, 0);
      end if;
      return Result;
   end Read_Word;

   procedure Read_Posting (Data : Byte_Array; L : Layout; R : Image_Size; Verse : out Image_Size;
                           Valid : out Boolean)
   is
      P : Image_Offset;
      Field : Unsigned_32;
   begin
      Row_Bound (L, Postings, R);
      P := Row_At (L, Postings, R);
      Field := U32 (Data, P);
      Valid := Wide (Field) < Wide (Count (L, Verses));
      Verse := (if Valid then Image_Size (Field) else 0);
   end Read_Posting;

   procedure Read_About (Data : Byte_Array; L : Layout; Kind : About_Kind; Content : out Text_Slice;
                         Valid : out Boolean)
   is
      P : Image_Offset;
      Ok_Text : Boolean;
      Slice : Text_Slice;
   begin
      Content := (0, 0);
      Valid := False;
      for R in 0 .. Count (L, About) - 1 loop
         Row_Bound (L, About, R);
         P := Row_At (L, About, R);
         if U8 (Data, P + 6) = About_Kind'Pos (Kind) then
            Slice_Of (L, Text, U32 (Data, P), Unsigned_32 (U16 (Data, P + 4)), Slice, Ok_Text);
            if Ok_Text and then Slice.Length <= MAXIMUM_ABOUT_BYTES then
               Content := Slice;
               Valid := True;
            end if;
            return;
         end if;
      end loop;
   end Read_About;

end Bible_Images;
