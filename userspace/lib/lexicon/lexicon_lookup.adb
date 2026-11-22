with Interfaces; use Interfaces;

package body Lexicon_Lookup with SPARK_Mode is

   --  U+2019 RIGHT SINGLE QUOTATION MARK in UTF-8.
   APOSTROPHE_LEAD   : constant Character := Character'Val (16#E2#);
   APOSTROPHE_MIDDLE : constant Character := Character'Val (16#80#);
   APOSTROPHE_LAST   : constant Character := Character'Val (16#99#);
   APOSTROPHE_BYTES  : constant := 3;

   function Is_Upper (C : Character) return Boolean is (C in 'A' .. 'Z');
   function Is_Letter (C : Character) return Boolean is (C in 'a' .. 'z' | 'A' .. 'Z');
   function Is_Digit (C : Character) return Boolean is (C in '0' .. '9');
   function Is_Non_ASCII (C : Character) return Boolean is (Character'Pos (C) >= 16#80#);
   function Is_Gap (C : Character) return Boolean is
     (C = ' ' or else C = '_' or else C = ASCII.HT);

   function Lower (C : Character) return Character is
     (if Is_Upper (C) then Character'Val (Character'Pos (C) + (Character'Pos ('a') - Character'Pos ('A')))
      else C);

   --  A typographic apostrophe starts at I in S.
   function Apostrophe_At (S : String; I : Positive) return Boolean is
     (I >= S'First and then I <= S'Last and then S'Last - I >= APOSTROPHE_BYTES - 1
      and then S (I) = APOSTROPHE_LEAD and then S (I + 1) = APOSTROPHE_MIDDLE
      and then S (I + 2) = APOSTROPHE_LAST)
     with Pre => S'Last < Positive'Last;

   procedure Normalize (Input : String; Result : out Key; Status : out Normal_Status) is
      I : Integer := Input'First;
      Pending_Gap : Boolean := False;
      C : Character;
   begin
      Result := (Length => 0, Text => [others => ' ']);
      Status := Normalized;
      while I <= Input'Last loop
         pragma Loop_Invariant (I >= Input'First);
         pragma Loop_Invariant (Status = Normalized);
         if Is_Gap (Input (I)) then
            Pending_Gap := Result.Length > 0;
            I := I + 1;
         else
            if Apostrophe_At (Input, I) then
               C := ''';
               I := I + APOSTROPHE_BYTES;
            else
               C := Lower (Input (I));
               I := I + 1;
            end if;
            if Pending_Gap then
               if Result.Length = Key_Length'Last then
                  Status := Too_Long;
                  return;
               end if;
               Result.Length := Result.Length + 1;
               Result.Text (Result.Length) := ' ';
               Pending_Gap := False;
            end if;
            if Result.Length = Key_Length'Last then
               Status := Too_Long;
               return;
            end if;
            Result.Length := Result.Length + 1;
            Result.Text (Result.Length) := C;
         end if;
      end loop;
      if Result.Length = 0 then
         Status := Empty;
      end if;
   end Normalize;

   type Ordering is (Less, Equal, Greater);

   --  The image's text in Slice against the first Length bytes of Text.
   function Compare (Data : Byte_Array; Slice : Text_Slice; Text : String; Length : Key_Length)
     return Ordering
     with Pre => Data'First = 0 and then Inside (Data, Slice) and then Text'First = 1
                 and then Length <= Text'Last
   is
      Common : constant Natural := Natural'Min (Slice.Length, Length);
      A, B : Unsigned_8;
   begin
      for I in 0 .. Common - 1 loop
         A := Data (Slice.First + I);
         B := Character'Pos (Text (I + 1));
         if A < B then
            return Less;
         elsif A > B then
            return Greater;
         end if;
      end loop;
      if Slice.Length < Length then
         return Less;
      elsif Slice.Length > Length then
         return Greater;
      end if;
      return Equal;
   end Compare;

   --  The first word whose key is not below Text (1 .. Length): a row in
   --  0 .. Word_Count. Valid False if a record on the way was unreadable.
   procedure Lower_Bound
     (Data : Byte_Array; L : Layout; Text : String; Length : Key_Length; At_Row : out Image_Size;
      Valid : out Boolean)
     with Pre  => Usable (Data, L) and then Text'First = 1 and then Length <= Text'Last,
          Post => At_Row <= Word_Count (L)
   is
      Low : Image_Size := 0;
      High : Image_Size := Word_Count (L);
      Middle : Image_Size;
      Item : Word_Record;
   begin
      Valid := True;
      while Low < High loop
         pragma Loop_Invariant (Low < High and then High <= Word_Count (L));
         pragma Loop_Variant (Decreases => High - Low);
         Middle := Low + (High - Low) / 2;
         Item := Read_Word (Data, L, Word_Id (Middle));
         if not Item.Valid then
            Valid := False;
            At_Row := 0;
            return;
         end if;
         if Compare (Data, Item.Key, Text, Length) = Less then
            Low := Middle + 1;
         else
            High := Middle;
         end if;
      end loop;
      At_Row := Low;
   end Lower_Bound;

   procedure Find (Data : Byte_Array; L : Layout; K : Key; Found : out Boolean; W : out Word_Id) is
      At_Row : Image_Size;
      Valid : Boolean;
      Item : Word_Record;
   begin
      Found := False;
      W := 0;
      Lower_Bound (Data, L, K.Text, K.Length, At_Row, Valid);
      if Valid and then At_Row < Word_Count (L) then
         Item := Read_Word (Data, L, Word_Id (At_Row));
         if Item.Valid and then Compare (Data, Item.Key, K.Text, K.Length) = Equal then
            Found := True;
            W := Word_Id (At_Row);
         end if;
      end if;
   end Find;

   function Sense_Count (Data : Byte_Array; L : Layout; W : Word_Id) return Image_Size is
      Item : constant Word_Record := Read_Word (Data, L, W);
   begin
      return (if Item.Valid then Item.Sense_Count else 0);
   end Sense_Count;

   function Key_Of (Data : Byte_Array; L : Layout; W : Word_Id) return Text_Slice is
      Item : constant Word_Record := Read_Word (Data, L, W);
   begin
      return (if Item.Valid then Item.Key else (0, 0));
   end Key_Of;

   ---------------------------------------------------------------------------
   --  Morphology: WordNet's detachment rules (morphy(7WN)), all parts of
   --  speech together; a candidate counts only if it is a word with senses.
   ---------------------------------------------------------------------------
   MAXIMUM_ENDING : constant := 4;
   subtype Ending_Length is Natural range 0 .. MAXIMUM_ENDING;
   type Rule is record
      Ending : String (1 .. MAXIMUM_ENDING);
      Ending_Bytes : Ending_Length;
      Replacement : String (1 .. MAXIMUM_ENDING);
      Replacement_Bytes : Ending_Length;
   end record
     with Predicate => Rule.Ending_Bytes >= Rule.Replacement_Bytes;

   function R (Ending, Replacement : String) return Rule is
     ((Ending => Ending & [1 .. MAXIMUM_ENDING - Ending'Length => ' '],
       Ending_Bytes => Ending'Length,
       Replacement => Replacement & [1 .. MAXIMUM_ENDING - Replacement'Length => ' '],
       Replacement_Bytes => Replacement'Length))
     with Pre => Ending'First = 1 and then Replacement'First = 1
                 and then Ending'Length <= MAXIMUM_ENDING
                 and then Replacement'Length <= Ending'Length;

   type Rule_Array is array (Positive range <>) of Rule;
   RULES : constant Rule_Array :=
     [R ("s", ""), R ("ses", "s"), R ("xes", "x"), R ("zes", "z"), R ("ches", "ch"),
      R ("shes", "sh"), R ("men", "man"), R ("ies", "y"), R ("es", "e"), R ("es", ""),
      R ("ed", "e"), R ("ed", ""), R ("ing", "e"), R ("ing", ""), R ("er", ""), R ("est", ""),
      R ("er", "e"), R ("est", "e")];

   --  K with Rule applied, if K ends in its ending (and keeps a stem).
   procedure Apply (K : Key; Item : Rule; Result : out Key; Applies : out Boolean)
     with Post => (if Applies then Result.Length > 0)
   is
      Stem : Natural;
   begin
      Result := (Length => 0, Text => [others => ' ']);
      Applies := False;
      if K.Length <= Item.Ending_Bytes then
         return;
      end if;
      Stem := K.Length - Item.Ending_Bytes;
      for I in 1 .. Item.Ending_Bytes loop
         if K.Text (Stem + I) /= Item.Ending (I) then
            return;
         end if;
      end loop;
      Result.Text (1 .. Stem) := K.Text (1 .. Stem);
      for I in 1 .. Item.Replacement_Bytes loop
         pragma Loop_Invariant (Stem + I <= K.Length);
         Result.Text (Stem + I) := Item.Replacement (I);
      end loop;
      Result.Length := Stem + Item.Replacement_Bytes;
      Applies := True;
   end Apply;

   --  Add W to Into (1 .. Count) unless it is there or Into is full.
   procedure Add_Once (Into : in out Word_List; Count : in out Natural; W : Word_Id; L : Layout)
     with Pre  => Into'First = 1 and then Count <= Into'Length
                  and then (for all I in 1 .. Count => Image_Size (Into (I)) < Word_Count (L))
                  and then Image_Size (W) < Word_Count (L),
          Post => Count <= Into'Length
                  and then (for all I in 1 .. Count => Image_Size (Into (I)) < Word_Count (L))
   is
   begin
      for I in 1 .. Count loop
         if Into (I) = W then
            return;
         end if;
      end loop;
      if Count < Into'Length then
         Count := Count + 1;
         Into (Count) := W;
      end if;
   end Add_Once;

   --  The first exception row whose key is not below K's.
   procedure Exception_Bound (Data : Byte_Array; L : Layout; K : Key; At_Row : out Image_Size;
                              Valid : out Boolean)
     with Pre  => Usable (Data, L),
          Post => At_Row <= Count (L, Exceptions)
   is
      Low : Image_Size := 0;
      High : Image_Size := Count (L, Exceptions);
      Middle : Image_Size;
      Item : Exception_Record;
   begin
      Valid := True;
      while Low < High loop
         pragma Loop_Invariant (Low < High and then High <= Count (L, Exceptions));
         pragma Loop_Variant (Decreases => High - Low);
         Middle := Low + (High - Low) / 2;
         Item := Read_Exception (Data, L, Row (Middle));
         if not Item.Valid then
            Valid := False;
            At_Row := 0;
            return;
         end if;
         if Compare (Data, Item.Key, K.Text, K.Length) = Less then
            Low := Middle + 1;
         else
            High := Middle;
         end if;
      end loop;
      At_Row := Low;
   end Exception_Bound;

   procedure Base_Forms (Data : Byte_Array; L : Layout; K : Key; Into : out Word_List; Count : out Natural) is
      Found, Valid, Applies : Boolean;
      W : Word_Id;
      At_Row : Image_Size;
      Item : Exception_Record;
      Stemmed : Key;
   begin
      Into := [others => 0];
      Count := 0;
      Find (Data, L, K, Found, W);
      if Found and then Sense_Count (Data, L, W) > 0 then
         Add_Once (Into, Count, W, L);
      end if;
      Exception_Bound (Data, L, K, At_Row, Valid);
      if Valid then
         while At_Row < Lexicon_Images.Count (L, Exceptions) loop
            pragma Loop_Invariant (Count <= Into'Length);
            pragma Loop_Invariant (for all I in 1 .. Count => Image_Size (Into (I)) < Word_Count (L));
            pragma Loop_Variant (Increases => At_Row);
            Item := Read_Exception (Data, L, Row (At_Row));
            exit when not Item.Valid or else Compare (Data, Item.Key, K.Text, K.Length) /= Equal;
            if Sense_Count (Data, L, Item.Base) > 0 then
               Add_Once (Into, Count, Item.Base, L);
            end if;
            At_Row := At_Row + 1;
         end loop;
      end if;
      for Each of RULES loop
         pragma Loop_Invariant (Count <= Into'Length);
         pragma Loop_Invariant (for all I in 1 .. Count => Image_Size (Into (I)) < Word_Count (L));
         Apply (K, Each, Stemmed, Applies);
         if Applies then
            Find (Data, L, Stemmed, Found, W);
            if Found and then Sense_Count (Data, L, W) > 0 then
               Add_Once (Into, Count, W, L);
            end if;
         end if;
      end loop;
   end Base_Forms;

   --  Listed, an exception, or an inflection of a word with senses.
   function Listed_Or_Inflected (Data : Byte_Array; L : Layout; K : Key) return Boolean
     with Pre => Usable (Data, L)
   is
      Found : Boolean;
      W : Word_Id;
      Bases : Word_List (1 .. 1);
      Count : Natural;
      At_Row : Image_Size;
      Valid : Boolean;
      Item : Exception_Record;
   begin
      Find (Data, L, K, Found, W);
      if Found then
         return True;
      end if;
      Exception_Bound (Data, L, K, At_Row, Valid);
      if Valid and then At_Row < Lexicon_Images.Count (L, Exceptions) then
         Item := Read_Exception (Data, L, Row (At_Row));
         if Item.Valid and then Compare (Data, Item.Key, K.Text, K.Length) = Equal then
            return True;
         end if;
      end if;
      Base_Forms (Data, L, K, Bases, Count);
      return Count > 0;
   end Listed_Or_Inflected;

   function Known (Data : Byte_Array; L : Layout; K : Key) return Boolean is
      Stem : Key;
   begin
      if K.Length = 0 then
         return False;
      end if;
      if Listed_Or_Inflected (Data, L, K) then
         return True;
      end if;
      --  Possessives: dog's, dogs'.
      if K.Length > 2 and then K.Text (K.Length - 1) = ''' and then K.Text (K.Length) = 's' then
         Stem := K;
         Stem.Length := K.Length - 2;
         return Listed_Or_Inflected (Data, L, Stem);
      elsif K.Length > 2 and then K.Text (K.Length - 1) = 's' and then K.Text (K.Length) = ''' then
         Stem := K;
         Stem.Length := K.Length - 1;
         return Listed_Or_Inflected (Data, L, Stem);
      end if;
      return False;
   end Known;

   ---------------------------------------------------------------------------
   --  Near spellings: optimal string alignment distance, cut off past Within.
   ---------------------------------------------------------------------------
   subtype Column is Natural range 0 .. MAXIMUM_WORD_BYTES;
   --  Distances above MAXIMUM_DISTANCE are all "too far".
   TOO_FAR : constant := MAXIMUM_DISTANCE + 1;
   subtype Bounded_Distance is Natural range 0 .. TOO_FAR;
   type Distance_Row is array (Column) of Bounded_Distance;

   function Capped (V : Natural) return Bounded_Distance is (Natural'Min (V, TOO_FAR));

   --  The distance from the image text in Slice to K, or TOO_FAR when it is
   --  more than Within.
   function Distance (Data : Byte_Array; Slice : Text_Slice; K : Key; Within : Edit_Distance)
     return Bounded_Distance
     with Pre => Data'First = 0 and then Inside (Data, Slice) and then Slice.Length <= MAXIMUM_WORD_BYTES
   is
      Two_Back, Previous, Current : Distance_Row := [others => TOO_FAR];
      A_Length : constant Column := Slice.Length;
      B_Length : constant Column := K.Length;
      Cost : Natural;
      Smallest : Bounded_Distance;
      A_I, A_Before : Character;
   begin
      for J in 0 .. B_Length loop
         Previous (J) := Capped (J);
      end loop;
      for I in 1 .. A_Length loop
         A_I := Character'Val (Data (Slice.First + I - 1));
         A_Before := (if I > 1 then Character'Val (Data (Slice.First + I - 2)) else ' ');
         Current (0) := Capped (I);
         Smallest := Current (0);
         for J in 1 .. B_Length loop
            Cost := (if A_I = K.Text (J) then 0 else 1);
            Current (J) := Capped (Natural'Min (Natural'Min (Previous (J) + 1, Current (J - 1) + 1),
                                                Previous (J - 1) + Cost));
            if I > 1 and then J > 1 and then A_I = K.Text (J - 1) and then A_Before = K.Text (J) then
               Current (J) := Natural'Min (Current (J), Capped (Two_Back (J - 2) + 1));
            end if;
            Smallest := Natural'Min (Smallest, Current (J));
         end loop;
         if Smallest > Within then
            return TOO_FAR;
         end if;
         for J in 0 .. B_Length loop
            Two_Back (J) := Previous (J);
            Previous (J) := Current (J);
         end loop;
      end loop;
      return (if Previous (B_Length) <= Within then Previous (B_Length) else TOO_FAR);
   end Distance;

   --  Before: S ranks above T (nearer, a swap, more common, earlier).
   function Before (S, T : Near_Spelling) return Boolean is
     (S.Distance < T.Distance
      or else (S.Distance = T.Distance and then S.Swap and then not T.Swap)
      or else (S.Distance = T.Distance and then S.Swap = T.Swap and then S.Band < T.Band)
      or else (S.Distance = T.Distance and then S.Swap = T.Swap and then S.Band = T.Band
               and then S.Word < T.Word));

   --  The image text in Slice is K with one pair of neighbours swapped.
   function Is_Swap (Data : Byte_Array; Slice : Text_Slice; K : Key) return Boolean
     with Pre => Data'First = 0 and then Inside (Data, Slice)
   is
      First_Difference : Natural := 0;
   begin
      if Slice.Length /= K.Length or else K.Length < 2 then
         return False;
      end if;
      for I in 1 .. K.Length loop
         if Character'Val (Data (Slice.First + I - 1)) /= K.Text (I) then
            First_Difference := I;
            exit;
         end if;
      end loop;
      if First_Difference = 0 or else First_Difference = K.Length then
         return False;
      end if;
      return Character'Val (Data (Slice.First + First_Difference - 1)) = K.Text (First_Difference + 1)
        and then Character'Val (Data (Slice.First + First_Difference)) = K.Text (First_Difference)
        and then (for all I in First_Difference + 2 .. K.Length =>
                    Character'Val (Data (Slice.First + I - 1)) = K.Text (I));
   end Is_Swap;

   procedure Suggest
     (Data : Byte_Array; L : Layout; K : Key; Within : Edit_Distance; Into : out Near_Spelling_List;
      Count : out Natural)
   is
      Prefix : String (1 .. 1) := [1 => ' '];
      First_Row, Last_Row : Image_Size;
      Valid_First, Valid_Last : Boolean;
      Item : Word_Record;
      D : Bounded_Distance;
      Candidate : Near_Spelling;
      Place : Positive;
   begin
      Into := [others => (Word => 0, Distance => 0, Swap => False, Band => WORDNET_ONLY)];
      Count := 0;
      if K.Length = 0 or else Into'Length = 0 then
         return;
      end if;
      Prefix (1) := K.Text (1);
      Lower_Bound (Data, L, Prefix, 1, First_Row, Valid_First);
      if Prefix (1) = Character'Last then
         Last_Row := Word_Count (L);
         Valid_Last := True;
      else
         Prefix (1) := Character'Succ (Prefix (1));
         Lower_Bound (Data, L, Prefix, 1, Last_Row, Valid_Last);
      end if;
      if not Valid_First or else not Valid_Last then
         return;
      end if;
      pragma Assert (Count = 0);
      for Row_At in First_Row .. Last_Row - 1 loop
         pragma Loop_Invariant (Count <= Into'Length);
         pragma Loop_Invariant (for all I in 1 .. Count => Image_Size (Into (I).Word) < Word_Count (L));
         pragma Loop_Invariant (Row_At < Word_Count (L));
         Item := Read_Word (Data, L, Word_Id (Row_At));
         if Item.Valid and then abs (Item.Key.Length - K.Length) <= Within then
            D := Distance (Data, Item.Key, K, Within);
            if D <= Within and then D > 0 then
               Candidate := (Word => Word_Id (Row_At), Distance => D, Band => Item.Band,
                             Swap => D = 1 and then Is_Swap (Data, Item.Key, K));
               --  Insert in rank order; the last one drops off a full list.
               if Count < Into'Length or else Before (Candidate, Into (Count)) then
                  if Count < Into'Length then
                     Count := Count + 1;
                  end if;
                  Place := Count;
                  Into (Place) := Candidate;
                  while Place > 1 and then Before (Candidate, Into (Place - 1)) loop
                     pragma Loop_Invariant (Place in 2 .. Count);
                     pragma Loop_Invariant
                       (for all I in 1 .. Count => Image_Size (Into (I).Word) < Word_Count (L));
                     Into (Place) := Into (Place - 1);
                     Place := Place - 1;
                  end loop;
                  Into (Place) := Candidate;
               end if;
            end if;
         end if;
      end loop;
   end Suggest;

   ---------------------------------------------------------------------------
   --  Checking text.
   ---------------------------------------------------------------------------

   --  The hyphen-separated parts of K are each known.
   function Parts_Known (Data : Byte_Array; L : Layout; K : Key) return Boolean
     with Pre => Usable (Data, L)
   is
      Part : Key;
      Start : Positive := 1;
   begin
      for I in 1 .. K.Length + 1 loop
         if I > K.Length or else K.Text (I) = '-' then
            if I > Start then
               Part := (Length => I - Start, Text => [others => ' ']);
               Part.Text (1 .. I - Start) := K.Text (Start .. I - 1);
               if I - Start > 1 and then not Known (Data, L, Part) then
                  return False;
               end if;
            end if;
            Start := I + 1;
         end if;
         pragma Loop_Invariant (Start <= I + 1);
      end loop;
      return True;
   end Parts_Known;

   procedure Check_Text
     (Data : Byte_Array; L : Layout; Text : String; From : Positive; Into : out Span_List;
      Count : out Natural; Next : out Positive)
   is
      I : Positive := From;
      Start : Positive;
      Has_Digit, Has_Other, Has_Hyphen : Boolean;
      Word : Key;
      Status : Normal_Status;
      Length : Natural;
   begin
      Into := [others => (First => 1, Length => 0)];
      Count := 0;
      while I <= Text'Last loop
         pragma Loop_Invariant (I >= From and then Count <= Into'Length);
         pragma Loop_Invariant
           (for all S in 1 .. Count =>
              Into (S).First >= Text'First and then Into (S).Length > 0
              and then Into (S).First <= Text'Last - Into (S).Length + 1);
         if not Is_Letter (Text (I)) and then not Is_Non_ASCII (Text (I)) then
            I := I + 1;
         else
            Start := I;
            Has_Digit := False;
            Has_Other := False;
            Has_Hyphen := False;
            loop
               pragma Loop_Invariant (I >= Start and then I <= Text'Last and then Start >= From);
               if Apostrophe_At (Text, I) then
                  I := I + APOSTROPHE_BYTES - 1;
               elsif Is_Non_ASCII (Text (I)) then
                  Has_Other := True;
               elsif Is_Digit (Text (I)) then
                  Has_Digit := True;
               elsif Text (I) = '-' then
                  Has_Hyphen := True;
               end if;
               exit when I = Text'Last;
               --  Continue through letters, digits, other bytes, and
               --  apostrophes or hyphens followed by a letter.
               if Is_Letter (Text (I + 1)) or else Is_Digit (Text (I + 1))
                 or else (Is_Non_ASCII (Text (I + 1)) and then not Apostrophe_At (Text, I + 1))
               then
                  I := I + 1;
               elsif (Text (I + 1) = ''' or else Text (I + 1) = '-')
                 and then I + 1 < Text'Last and then Is_Letter (Text (I + 2))
               then
                  I := I + 1;
               elsif Apostrophe_At (Text, I + 1) and then I + APOSTROPHE_BYTES < Text'Last
                 and then Is_Letter (Text (I + APOSTROPHE_BYTES + 1))
               then
                  I := I + 1;
               else
                  exit;
               end if;
            end loop;
            --  The word is Text (Start .. I).
            Length := I - Start + 1;
            if not Has_Digit and then not Has_Other and then Length > 1
              and then Length <= Key_Length'Last
            then
               Normalize (Text (Start .. I), Word, Status);
               if Status = Normalized and then not Known (Data, L, Word)
                 and then not (Has_Hyphen and then Parts_Known (Data, L, Word))
               then
                  if Count = Into'Length then
                     Next := Start;
                     return;
                  end if;
                  Count := Count + 1;
                  Into (Count) := (First => Start, Length => Length);
               end if;
            end if;
            exit when I = Text'Last;
            I := I + 1;
         end if;
      end loop;
      Next := Positive'Max (From, Text'Last + 1);
   end Check_Text;

end Lexicon_Lookup;
