with Lexicon_Lookup; use Lexicon_Lookup;

package body Lexicon_Answers with SPARK_Mode is

   --  Items are written while they fit; the first that does not is replaced
   --  by Continue_At with its number, and writing stops.
   type Writer is record
      Used    : Natural := 0;
      Number  : Item_Number := 0;   --  the next item's number
      From    : Item_Number := 0;   --  items before this are skipped
      Stopped : Boolean := False;
   end record;

   function Room (Into : Answer_Bytes; W : Writer) return Boolean is
     (W.Used <= Into'Length and then (W.Stopped or else Into'Length - W.Used >= CONTINUE_BYTES));

   --  Count one item; True if it is to be written now.
   procedure Admit (Into : in out Answer_Bytes; W : in out Writer; Payload : Payload_Length;
                    Write : out Boolean)
     with Pre  => Into'First = 0 and then Room (Into, W),
          Post => Room (Into, W)
                  and then (if Write then not W.Stopped and then Fits (Into'Length, W.Used, Payload))
   is
   begin
      Write := False;
      if W.Stopped then
         return;
      end if;
      if W.Number >= W.From then
         if Fits (Into'Length, W.Used, Payload) then
            Write := True;
         else
            declare
               Mark : Natural := W.Used;
            begin
               Put_Number (Into, Mark, Continue_At, Unsigned_32 (W.Number));
               W.Used := Mark;
            end;
            W.Stopped := True;
            return;
         end if;
      end if;
      if W.Number = Item_Number'Last then
         W.Stopped := True;   --  unreachable for any real image
         Write := False;
      else
         W.Number := W.Number + 1;
      end if;
   end Admit;

   procedure Emit_Slice
     (Data : Byte_Array; Into : in out Answer_Bytes; W : in out Writer; Kind : Item_Kind; Slice : Text_Slice)
     with Pre  => Data'First = 0 and then Into'First = 0 and then Room (Into, W) and then Inside (Data, Slice)
                  and then Slice.Length <= MAXIMUM_TEXT_ITEM,
          Post => Room (Into, W)
   is
      Write : Boolean;
   begin
      Admit (Into, W, Slice.Length, Write);
      if Write then
         Put_Header (Into, W.Used, Kind, Slice.Length);
         for I in 0 .. Slice.Length - 1 loop
            pragma Loop_Invariant (W.Used = W.Used'Loop_Entry + I);
            Put_Byte (Into, W.Used, Data (Slice.First + I));
         end loop;
      end if;
   end Emit_Slice;

   procedure Emit_Key (Into : in out Answer_Bytes; W : in out Writer; Kind : Item_Kind; K : Key)
     with Pre  => Into'First = 0 and then Room (Into, W),
          Post => Room (Into, W)
   is
      Write : Boolean;
   begin
      Admit (Into, W, K.Length, Write);
      if Write then
         Put_Header (Into, W.Used, Kind, K.Length);
         for I in 1 .. K.Length loop
            pragma Loop_Invariant (W.Used = W.Used'Loop_Entry + I - 1);
            Put_Byte (Into, W.Used, Character'Pos (K.Text (I)));
         end loop;
      end if;
   end Emit_Key;

   procedure Emit_Part (Into : in out Answer_Bytes; W : in out Writer; Part : Part_Of_Speech)
     with Pre  => Into'First = 0 and then Room (Into, W),
          Post => Room (Into, W)
   is
      Write : Boolean;
   begin
      Admit (Into, W, 1, Write);
      if Write then
         Put_Header (Into, W.Used, Sense_Start, 1);
         Put_Byte (Into, W.Used, Part_Of_Speech'Pos (Part));
      end if;
   end Emit_Part;

   procedure Emit_Number (Into : in out Answer_Bytes; W : in out Writer; Kind : Item_Kind; Value : Unsigned_32)
     with Pre  => Into'First = 0 and then Room (Into, W),
          Post => Room (Into, W)
   is
      Write : Boolean;
   begin
      Admit (Into, W, NUMBER_BYTES, Write);
      if Write then
         Put_Number (Into, W.Used, Kind, Value);
      end if;
   end Emit_Number;

   --  The display spelling of sense S, as an item of Kind.
   procedure Emit_Sense_Word
     (Data : Byte_Array; L : Layout; Into : in out Answer_Bytes; W : in out Writer; Kind : Item_Kind;
      S : Sense_Id)
     with Pre  => Usable (Data, L) and then Into'First = 0 and then Room (Into, W)
                  and then Image_Size (S) < Count (L, Senses),
          Post => Room (Into, W)
   is
      Item : constant Sense_Record := Read_Sense (Data, L, S);
   begin
      if Item.Valid then
         Emit_Slice (Data, Into, W, Kind, Item.Display);
      end if;
   end Emit_Sense_Word;

   --  Every member of synset S except sense Except, as items of Kind.
   procedure Emit_Members
     (Data : Byte_Array; L : Layout; Into : in out Answer_Bytes; W : in out Writer; Kind : Item_Kind;
      S : Synset_Id; Except : Sense_Id)
     with Pre  => Usable (Data, L) and then Into'First = 0 and then Room (Into, W)
                  and then Image_Size (S) < Count (L, Synsets),
          Post => Room (Into, W)
   is
      Set : constant Synset_Record := Read_Synset (Data, L, S);
      Member : Sense_Id;
      Valid : Boolean;
   begin
      if not Set.Valid then
         return;
      end if;
      for Offset in 0 .. Set.Member_Count - 1 loop
         pragma Loop_Invariant (Room (Into, W));
         Read_Member (Data, L, Set.First_Member + Row (Offset), Member, Valid);
         if Valid and then Member /= Except then
            Emit_Sense_Word (Data, L, Into, W, Kind, Member);
         end if;
         exit when W.Stopped;
      end loop;
   end Emit_Members;

   procedure Emit_Sense
     (Data : Byte_Array; L : Layout; Into : in out Answer_Bytes; W : in out Writer; Op : Operation;
      S : Sense_Id)
     with Pre  => Usable (Data, L) and then Into'First = 0 and then Room (Into, W)
                  and then Image_Size (S) < Count (L, Senses),
          Post => Room (Into, W)
   is
      Sense : constant Sense_Record := Read_Sense (Data, L, S);
      Set : Synset_Record;
      Example_Text : Text_Slice;
      Target : Sense_Id;
      Valid : Boolean;
      Link : Relation_Record;
   begin
      if not Sense.Valid then
         return;
      end if;
      Set := Read_Synset (Data, L, Sense.Synset);
      if not Set.Valid then
         return;
      end if;
      Emit_Part (Into, W, Set.Part);
      Emit_Slice (Data, Into, W, Definition, Set.Gloss);
      for Offset in 0 .. Set.Example_Count - 1 loop
         pragma Loop_Invariant (Room (Into, W));
         Read_Example (Data, L, Set.First_Example + Row (Offset), Example_Text, Valid);
         if Valid then
            Emit_Slice (Data, Into, W, Example, Example_Text);
         end if;
      end loop;
      Emit_Members (Data, L, Into, W, Synonym, Sense.Synset, S);
      for Offset in 0 .. Sense.Antonym_Count - 1 loop
         pragma Loop_Invariant (Room (Into, W));
         Read_Antonym (Data, L, Sense.First_Antonym + Row (Offset), Target, Valid);
         if Valid then
            Emit_Sense_Word (Data, L, Into, W, Antonym, Target);
         end if;
      end loop;
      if Op = Thesaurus then
         for Offset in 0 .. Set.Relation_Count - 1 loop
            pragma Loop_Invariant (Room (Into, W));
            exit when W.Stopped;
            Link := Read_Relation (Data, L, Set.First_Relation + Row (Offset));
            if Link.Valid then
               Emit_Members
                 (Data, L, Into, W,
                  (case Link.Kind is
                     when Broader => Broader,
                     when Narrower => Narrower,
                     when Similar | See_Also => Similar),
                  Link.Target, S);
            end if;
         end loop;
      end if;
   end Emit_Sense;

   --  Near spellings of K as Suggestion items.
   procedure Emit_Suggestions
     (Data : Byte_Array; L : Layout; Into : in out Answer_Bytes; W : in out Writer; K : Key;
      Suggestions : Suggestion_Count)
     with Pre  => Usable (Data, L) and then Into'First = 0 and then Room (Into, W),
          Post => Room (Into, W)
   is
      Near : Near_Spelling_List (1 .. Suggestions);
      Found : Natural;
   begin
      Lexicon_Lookup.Suggest (Data, L, K, SUGGESTION_DISTANCE, Near, Found);
      for I in 1 .. Found loop
         pragma Loop_Invariant (Room (Into, W));
         Emit_Slice (Data, Into, W, Suggestion, Key_Of (Data, L, Near (I).Word));
      end loop;
   end Emit_Suggestions;

   procedure Answer_Entry
     (Data : Byte_Array; L : Layout; Op : Operation; Text : String; Resume : Unsigned_32;
      Suggestions : Suggestion_Count; Into : in out Answer_Bytes; Used : out Natural)
     with Pre  => Usable (Data, L) and then Into'First = 0 and then Into'Length >= MINIMUM_PAGE_BYTES
                  and then Text'Last < Positive'Last,
          Post => Used <= Into'Length
   is
      W : Writer;
      K : Key;
      Status : Normal_Status;
      Forms : Word_List (1 .. MAXIMUM_FORMS);
      Form_Count : Natural;
      Head : Word_Record;
   begin
      W.From := (if Resume <= Unsigned_32 (Item_Number'Last) then Item_Number (Resume) else Item_Number'Last);
      Normalize (Text, K, Status);
      if Status /= Normalized then
         Used := 0;
         return;
      end if;
      Emit_Key (Into, W, Query, K);
      if Op = Suggest then
         Emit_Suggestions (Data, L, Into, W, K, Suggestions);
      else
         Base_Forms (Data, L, K, Forms, Form_Count);
         if Form_Count = 0 then
            Emit_Suggestions (Data, L, Into, W, K, Suggestions);
         end if;
         for F in 1 .. Form_Count loop
            pragma Loop_Invariant (Room (Into, W));
            Head := Read_Word (Data, L, Forms (F));
            if Head.Valid then
               --  The headword as its first sense spells it ("Paris").
               if Head.Sense_Count > 0 then
                  Emit_Sense_Word (Data, L, Into, W, Headword, Head.First_Sense);
               else
                  Emit_Slice (Data, Into, W, Headword, Head.Key);
               end if;
               Emit_Number (Into, W, Sense_Total, Unsigned_32 (Head.Sense_Count));
               for Offset in 0 .. Head.Sense_Count - 1 loop
                  pragma Loop_Invariant (Room (Into, W));
                  exit when W.Stopped;
                  Emit_Sense (Data, L, Into, W, Op, Head.First_Sense + Sense_Id (Offset));
               end loop;
            end if;
            exit when W.Stopped;
         end loop;
      end if;
      Used := W.Used;
   end Answer_Entry;

   --  Check: misspellings with their suggestions, a whole one at a time.
   SPANS_PER_BATCH : constant := 16;

   procedure Answer_Check
     (Data : Byte_Array; L : Layout; Text : String; Resume : Unsigned_32;
      Suggestions : Suggestion_Count; Into : in out Answer_Bytes; Used : out Natural)
     with Pre  => Usable (Data, L) and then Into'First = 0 and then Into'Length >= MINIMUM_PAGE_BYTES
                  and then Text'Last < Positive'Last,
          Post => Used <= Into'Length
   is
      From : Positive;
      Spans : Span_List (1 .. SPANS_PER_BATCH);
      Found : Natural;
      Next : Positive;
      Near : Near_Spelling_List (1 .. Suggestions);
      Near_Count : Natural;
      Word : Key;
      Status : Normal_Status;
      Before : Natural;
      Slice : Text_Slice;
      Complete : Boolean;
   begin
      Used := 0;
      if Text'Length = 0 or else Long_Long_Integer (Resume) >= Long_Long_Integer (Text'Length) then
         Put_Number (Into, Used, Checked_Upto, Unsigned_32 (Text'Length));
         return;
      end if;
      From := Text'First + Natural (Long_Long_Integer (Resume));
      loop
         pragma Loop_Invariant (Used <= Into'Length - CONTINUE_BYTES);
         pragma Loop_Invariant (From >= Text'First);
         Check_Text (Data, L, Text, From, Spans, Found, Next);
         for S in 1 .. Found loop
            pragma Loop_Invariant (Used <= Into'Length - CONTINUE_BYTES);
            Before := Used;
            if not Fits (Into'Length, Used, SPAN_BYTES) then
               Put_Number (Into, Used, Checked_Upto, Unsigned_32 (Spans (S).First - Text'First));
               return;
            end if;
            Put_Span (Into, Used, Unsigned_32 (Spans (S).First - Text'First),
                      Unsigned_16 (Spans (S).Length));
            Normalize (Text (Spans (S).First .. Spans (S).First + Spans (S).Length - 1), Word, Status);
            Near_Count := 0;
            if Status = Normalized then
               Lexicon_Lookup.Suggest (Data, L, Word, SUGGESTION_DISTANCE, Near, Near_Count);
            end if;
            Complete := True;
            for N in 1 .. Near_Count loop
               pragma Loop_Invariant (Used <= Into'Length - CONTINUE_BYTES);
               Slice := Key_Of (Data, L, Near (N).Word);
               if Slice.Length > MAXIMUM_TEXT_ITEM or else not Fits (Into'Length, Used, Slice.Length) then
                  Complete := False;
                  exit;
               end if;
               Put_Header (Into, Used, Suggestion, Slice.Length);
               for I in 0 .. Slice.Length - 1 loop
                  pragma Loop_Invariant (Used = Used'Loop_Entry + I);
                  Put_Byte (Into, Used, Data (Slice.First + I));
               end loop;
            end loop;
            if not Complete then
               if Before > 0 then
                  --  Send this misspelling whole on the next page.
                  Used := Before;
                  Put_Number (Into, Used, Checked_Upto, Unsigned_32 (Spans (S).First - Text'First));
               else
                  --  Alone on a page and still too long: its first
                  --  suggestions stand, and checking goes on after it.
                  Put_Number (Into, Used, Checked_Upto,
                              Unsigned_32 (Spans (S).First - Text'First + Spans (S).Length));
               end if;
               return;
            end if;
         end loop;
         exit when Next > Text'Last;
         From := Next;
      end loop;
      Put_Number (Into, Used, Checked_Upto, Unsigned_32 (Text'Length));
   end Answer_Check;

   procedure Answer
     (Data : Byte_Array; L : Layout; Op : Operation; Text : String; Resume : Unsigned_32;
      Suggestions : Suggestion_Count; Into : out Answer_Bytes; Used : out Natural)
   is
   begin
      Into := [others => 0];
      case Op is
         when Define | Thesaurus | Suggest =>
            Answer_Entry (Data, L, Op, Text, Resume, Suggestions, Into, Used);
         when Check =>
            Answer_Check (Data, L, Text, Resume, Suggestions, Into, Used);
      end case;
   end Answer;

end Lexicon_Answers;
