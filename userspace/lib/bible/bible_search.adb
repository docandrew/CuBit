package body Bible_Search with SPARK_Mode is

   ---------------------------------------------------------------------------
   --  Normalize: tools/build_bible.py words_of, on UTF-8 bytes.
   ---------------------------------------------------------------------------
   LEAD_2 : constant Character := Character'Val (16#C3#);   --  U+00C0 .. U+00FF
   LEAD_C4 : constant Character := Character'Val (16#C4#);
   LEAD_C5 : constant Character := Character'Val (16#C5#);
   LEAD_E2 : constant Character := Character'Val (16#E2#);
   PUNCTUATION_2 : constant Character := Character'Val (16#80#);
   RIGHT_QUOTE_3 : constant Character := Character'Val (16#99#);   --  U+2019

   type Fold is record
      Width   : Natural;           --  bytes the sequence takes (0: separator)
      Letters : String (1 .. 2);   --  what it becomes
      Count   : Natural;           --  how many of Letters
      Joiner  : Boolean;           --  an apostrophe: dropped inside a word
   end record;

   SEPARATOR : constant Fold := (Width => 1, Letters => "  ", Count => 0, Joiner => False);

   --  The character at Text (I).
   function Fold_At (Text : String; I : Positive) return Fold
     with Pre  => I in Text'Range,
          Post => Fold_At'Result.Width in 1 .. 3 and then Fold_At'Result.Width - 1 <= Text'Last - I
                  and then Fold_At'Result.Count <= 2
   is
      C : constant Character := Text (I);
      function Next (K : Positive) return Character is (Text (I + K))
        with Pre => K <= 2 and then I in Text'Range and then Text'Last - I >= K;
   begin
      if C in 'a' .. 'z' | '0' .. '9' then
         return (1, C & ' ', 1, False);
      elsif C in 'A' .. 'Z' then
         return (1, Character'Val (Character'Pos (C) + 32) & ' ', 1, False);
      elsif C = ''' then
         return (1, "  ", 0, True);
      elsif I < Text'Last and then C = LEAD_2 then
         case Character'Pos (Next (1)) is
            when 16#A6# | 16#86# => return (2, "ae", 2, False);   --  U+00E6, U+00C6
            when 16#A0# => return (2, "a ", 1, False);            --  U+00E0
            when 16#A9# => return (2, "e ", 1, False);            --  U+00E9
            when others => return (2, "  ", 0, False);
         end case;
      elsif I < Text'Last and then C = LEAD_C4 and then Character'Pos (Next (1)) = 16#93# then
         return (2, "e ", 1, False);                              --  U+0113
      elsif I < Text'Last and then C = LEAD_C5 and then Character'Pos (Next (1)) = 16#8D# then
         return (2, "o ", 1, False);                              --  U+014D
      elsif I < Text'Last - 1 and then C = LEAD_E2 and then Next (1) = PUNCTUATION_2 then
         return (3, "  ", 0, Next (2) = RIGHT_QUOTE_3);
      else
         return SEPARATOR;
      end if;
   end Fold_At;

   procedure Normalize (Text : String; Item : out Query; Status : out Normalize_Status) is
      I : Natural;
      F : Fold;
      Open : Boolean := False;   --  a word is being read
      Too_Long : Boolean := False;
   begin
      Item := (others => <>);
      Status := Normalized;
      if Text'Length > MAXIMUM_QUERY then
         Status := Query_Too_Long;
         return;
      elsif Text'Length = 0 then
         Status := Empty_Query;
         return;
      end if;
      I := Text'First;
      while I <= Text'Last loop
         pragma Loop_Invariant (I >= Text'First);
         pragma Loop_Invariant (for all J in 1 .. Item.Count => Item.Terms (J).Length >= 1);
         pragma Loop_Invariant (if Open then Item.Count >= 1);
         F := Fold_At (Text, I);
         if F.Count > 0 then
            if not Open then
               if Item.Count = MAXIMUM_TERMS then
                  Status := Too_Many_Terms;
                  return;
               end if;
               Item.Count := Item.Count + 1;
               Item.Terms (Item.Count) := (others => <>);
               Open := True;
            end if;
            for K in 1 .. F.Count loop
               pragma Loop_Invariant (Item.Count >= 1 and then Item.Terms (Item.Count).Length <= MAXIMUM_KEY);
               pragma Loop_Invariant (for all J in 1 .. Item.Count - 1 => Item.Terms (J).Length >= 1);
               pragma Loop_Invariant (K = 1 or else Item.Terms (Item.Count).Length >= 1);
               if Item.Terms (Item.Count).Length = MAXIMUM_KEY then
                  Too_Long := True;
               else
                  Item.Terms (Item.Count).Length := Item.Terms (Item.Count).Length + 1;
                  Item.Terms (Item.Count).Text (Item.Terms (Item.Count).Length) := F.Letters (K);
               end if;
            end loop;
            if Too_Long then
               Status := Term_Too_Long;
               return;
            end if;
         elsif not (F.Joiner and then Open) then
            Open := False;
         end if;
         exit when F.Width > Text'Last - I;
         I := I + F.Width;
      end loop;
      if Item.Count = 0 then
         Status := Empty_Query;
      end if;
   end Normalize;

   ---------------------------------------------------------------------------
   --  The index.
   ---------------------------------------------------------------------------
   type Order is (Less, Same, Greater);

   --  The key at Slice against T: by bytes, a shorter prefix first.
   function Compare (Data : Byte_Array; Slice : Text_Slice; T : Term) return Order
     with Pre => Data'First = 0 and then Inside (Data, Slice) and then Slice.Length <= MAXIMUM_KEY
   is
      Shorter : constant Natural := Natural'Min (Slice.Length, T.Length);
      Key_Byte, Term_Byte : Unsigned_8;
   begin
      for J in 1 .. Shorter loop
         Key_Byte := Data (Slice.First + J - 1);
         Term_Byte := Character'Pos (T.Text (J));
         if Key_Byte < Term_Byte then
            return Less;
         elsif Key_Byte > Term_Byte then
            return Greater;
         end if;
      end loop;
      return (if Slice.Length < T.Length then Less elsif Slice.Length = T.Length then Same else Greater);
   end Compare;

   --  The key at Slice starts with T.
   function Starts_With (Data : Byte_Array; Slice : Text_Slice; T : Term) return Boolean is
     (T.Length <= Slice.Length
      and then (for all J in 1 .. T.Length => Data (Slice.First + J - 1) = Character'Pos (T.Text (J))))
   with Pre => Data'First = 0 and then Inside (Data, Slice) and then Slice.Length <= MAXIMUM_KEY;

   --  The first word whose key is not below T (Count (L, Words) when every
   --  key is). Damaged: a word record failed its checks.
   procedure Lower_Bound (Data : Byte_Array; L : Layout; T : Term; First : out Image_Size; Damaged : out Boolean)
     with Pre  => Usable (Data, L),
          Post => First <= Count (L, Words)
   is
      Low : Image_Size := 0;
      High : Image_Size := Count (L, Words);
      Middle : Image_Size;
      W : Word_Record;
   begin
      Damaged := False;
      while Low < High loop
         pragma Loop_Invariant (Low <= High and then High <= Count (L, Words));
         Middle := Low + (High - Low) / 2;
         W := Read_Word (Data, L, Middle);
         if not W.Valid then
            Damaged := True;
            First := Low;
            return;
         end if;
         if Compare (Data, W.Key, T) = Less then
            Low := Middle + 1;
         else
            High := Middle;
         end if;
      end loop;
      First := Low;
   end Lower_Bound;

   procedure Set_Bit (S : in out Verse_Set; V : Image_Size)
     with Pre => V < MAXIMUM_VERSES
   is
   begin
      S (V / 64) := S (V / 64) or Shift_Left (1, V mod 64);
   end Set_Bit;

   --  Mark in Work.Current every verse of the words T names: the one
   --  equal to it, or with Prefix every one it starts.
   procedure Mark (Data : Byte_Array; L : Layout; T : Term; Prefix : Boolean; Words_In_Use : Set_Index;
                   Work : in out Work_Area; Damaged : out Boolean)
     with Pre => Usable (Data, L) and then Count (L, Verses) <= MAXIMUM_VERSES
   is
      W_Index : Image_Size;
      W : Word_Record;
      Verse : Image_Size;
      Ok : Boolean;
   begin
      for K in 0 .. Words_In_Use loop
         Work.Current (K) := 0;
      end loop;
      Lower_Bound (Data, L, T, W_Index, Damaged);
      if Damaged then
         return;
      end if;
      while W_Index < Count (L, Words) loop
         W := Read_Word (Data, L, W_Index);
         if not W.Valid then
            Damaged := True;
            return;
         end if;
         exit when not Starts_With (Data, W.Key, T) or else (not Prefix and then W.Key.Length /= T.Length);
         --  Exact: the one word equal to T, then stop.
         for R in W.First_Posting .. W.First_Posting + W.Postings - 1 loop
            Read_Posting (Data, L, R, Verse, Ok);
            if not Ok then
               Damaged := True;
               return;
            end if;
            Set_Bit (Work.Current, Verse);
         end loop;
         W_Index := (if Prefix then W_Index + 1 else Count (L, Words));
      end loop;
   end Mark;

   procedure Search
     (Data : Byte_Array; L : Layout; Item : Query; Prefix_Last : Boolean; Work : in out Work_Area;
      Hits : out Hit_Array; Found, Total : out Natural; Status : out Search_Status)
   is
      Verse_Rows : constant Image_Size := Count (L, Verses);
      Damaged : Boolean;
      Bits : Unsigned_64;
      V : Image_Size;
   begin
      Hits := [others => 0];
      Found := 0;
      Total := 0;
      if Verse_Rows > MAXIMUM_VERSES then
         Status := Too_Many_Verses;
         return;
      elsif Verse_Rows = 0 then
         Status := Searched;
         return;
      end if;
      declare
         Words_In_Use : constant Set_Index := (Verse_Rows - 1) / 64;
      begin
         for K in 0 .. Words_In_Use loop
            Work.Matched (K) := (if K < Words_In_Use or else Verse_Rows mod 64 = 0 then Unsigned_64'Last
                                 else Shift_Left (1, Verse_Rows mod 64) - 1);
         end loop;
         for I in 1 .. Item.Count loop
            Mark (Data, L, Item.Terms (I), Prefix_Last and then I = Item.Count, Words_In_Use, Work, Damaged);
            if Damaged then
               Status := Damaged_Index;
               return;
            end if;
            for K in 0 .. Words_In_Use loop
               Work.Matched (K) := Work.Matched (K) and Work.Current (K);
            end loop;
         end loop;
         for K in 0 .. Words_In_Use loop
            pragma Loop_Invariant (Found <= Hits'Length and then Found <= Total);
            pragma Loop_Invariant (for all J in 1 .. Found => Hits (J) < Verse_Rows);
            Bits := Work.Matched (K);
            for B in 0 .. 63 loop
               pragma Loop_Invariant (Found <= Hits'Length and then Found <= Total);
               pragma Loop_Invariant (for all J in 1 .. Found => Hits (J) < Verse_Rows);
               if (Bits and Shift_Left (1, B)) /= 0 then
                  V := K * 64 + B;
                  if V < Verse_Rows then
                     if Total < Natural'Last then
                        Total := Total + 1;
                     end if;
                     if Found < Hits'Length then
                        Found := Found + 1;
                        Hits (Found) := V;
                     end if;
                  end if;
               end if;
            end loop;
         end loop;
      end;
      Status := Searched;
   end Search;

end Bible_Search;
