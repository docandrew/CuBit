with CCL.Interfaces.Bible;
with CCL.Objects;
with CCL.Objects.Views;
with CCL.VM;
with CCL_Bible_Source;
with CuBit.Failures;
with Bible_Canon;
with Bible_Images;
with Bible_Parser;
with Bible_References;
with Bible_Search;
with Bible_Text;
with Bible_Translations;

package body CCL_Bible_Bindings is
   use Interfaces;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Host_Values.Value_Kind;
   use type CCL.Objects.Build_Result;
   use type Bible_Translations.Load_Status;
   use type Bible_Parser.Status;
   use type Bible_Search.Normalize_Status;
   use type Bible_Search.Search_Status;
   use type Bible_Canon.Book;
   use type CCL.Interfaces.Bible.Operation;
   use Bible_References;
   package BI renames CCL.Interfaces.Bible;
   package Views renames CCL.Objects.Views;
   package Failures renames CuBit.Failures;
   package Objects renames CCL.Objects;
   package Images renames Bible_Images;
   subtype Translation is Bible_Translations.Translation;

   Contracts : BI.Contracts;
   Editions : array (Translation) of Bible_Translations.Edition;
   --  Search's scratch (16 KiB), allocated by the first search and kept.
   type Work_Access is access Bible_Search.Work_Area;
   Work : Work_Access;

   function Handles (Binding : Unsigned_32) return Boolean is
     (Binding in BI.FIRST_BINDING .. BI.Binding_Of (BI.Operation'Last));

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
   begin
      BI.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Catalog.Resolve_Schema (Catalog, BI.PARSE_KEY, Contracts.Parse);
      CCL.Catalog.Resolve_Schema (Catalog, BI.VERSE_KEY, Contracts.Verse);
      CCL.Catalog.Resolve_Schema (Catalog, BI.PASSAGE_RESULT_KEY, Contracts.Passage_Result);
      CCL.Catalog.Resolve_Schema (Catalog, BI.SEARCH_KEY, Contracts.Search);
      CCL.Catalog.Resolve_Schema (Catalog, BI.REFERENCE_KEY, Contracts.Reference);
      CCL.Catalog.Resolve_Schema (Catalog, BI.PASSAGE_KEY, Contracts.Passage);
      CCL.Catalog.Resolve_Schema (Catalog, BI.VERSE_QUERY_KEY, Contracts.Verse_Query);
      CCL.Catalog.Resolve_Schema (Catalog, BI.PASSAGE_QUERY_KEY, Contracts.Passage_Query);
      CCL.Catalog.Resolve_Schema (Catalog, BI.SEARCH_QUERY_KEY, Contracts.Search_Query);
      Success := Objects.Is_Bound (Contracts.Parse) and then Objects.Is_Bound (Contracts.Verse)
        and then Objects.Is_Bound (Contracts.Passage_Result) and then Objects.Is_Bound (Contracts.Search)
        and then Objects.Is_Bound (Contracts.Reference) and then Objects.Is_Bound (Contracts.Passage)
        and then Objects.Is_Bound (Contracts.Verse_Query) and then Objects.Is_Bound (Contracts.Passage_Query)
        and then Objects.Is_Bound (Contracts.Search_Query);
      for Op in BI.Operation loop
         exit when not Success;
         CCL.Catalog.Resolve (Catalog, "bible." & BI.Name (Op), Resolved, Found);
         Success := Found;
         if Success then
            CCL.Catalog.Install (Grants, Resolved, BI.Binding_Of (Op), Grant);
            Success := Grant = CCL.Catalog.Grant_Added;
         end if;
      end loop;
   end Install;

   ---------------------------------------------------------------------------
   --  Translations, read once when first wanted.
   ---------------------------------------------------------------------------
   procedure Ensure (Which : Translation) is
      E : Bible_Translations.Edition renames Editions (Which);
   begin
      if E.Status = Bible_Translations.Not_Loaded then
         CCL_Bible_Source.Load (Which, E.Data, E.Status);
         if E.Status = Bible_Translations.Loaded then
            Bible_Translations.Open (E);
         elsif E.Status = Bible_Translations.Not_Loaded then
            E.Status := Bible_Translations.Read_Failed;
         end if;
      end if;
   end Ensure;

   function Ready (Which : Translation) return Boolean is (Editions (Which).Status = Bible_Translations.Loaded);

   function Unavailable (Which : Translation) return Failures.Failure is
     (Failures.Failed
        (Failures.Unavailable,
         "the " & Bible_Translations.Title (Which) & " is not available: " &
         Bible_Translations.Describe (Editions (Which).Status),
         CCL_Bible_Source.Unavailable_Remedy));

   function Text_Of (Which : Translation; Slice : Images.Text_Slice) return String is
      E : Bible_Translations.Edition renames Editions (Which);
      Result : String (1 .. Slice.Length);
   begin
      for I in Result'Range loop
         Result (I) := Character'Val (E.Data (Slice.First + I - 1));
      end loop;
      return Result;
   end Text_Of;

   --  A verse's text: its verse-text segments, spaced, in ASCII (CCL's
   --  strings are; Bible_Text.To_ASCII).
   function Verse_Text (Which : Translation; Row : Images.Image_Size) return String is
      E : Bible_Translations.Edition renames Editions (Which);
      V : constant Images.Verse_Record := Images.Read_Verse (E.Data.all, E.Layout, Row);
      function From (S : Natural) return String is
      begin
         if not V.Valid or else S >= V.First_Segment + V.Segments then
            return "";
         end if;
         declare
            Seg : constant Images.Segment_Record := Images.Read_Segment (E.Data.all, E.Layout, S);
            Rest : constant String := From (S + 1);
         begin
            if Seg.Valid and then Images.Is_Verse_Text (Seg.Kind) then
               return Text_Of (Which, Seg.Content) & (if Rest'Length > 0 then " " & Rest else "");
            end if;
            return Rest;
         end;
      end From;
   begin
      return Bible_Text.To_ASCII (From (V.First_Segment));
   end Verse_Text;

   ---------------------------------------------------------------------------
   --  Refusals: why a reference is not a verse of a translation.
   ---------------------------------------------------------------------------
   type Refusal is record
      Error : BI.Parse_Error := BI.Empty;
      Book_First, Book_Last : Natural := 0;   --  in Book_Text
      Value, Limit : Natural := 0;
   end record;

   --  What the parser's statuses are, as the CCL type's.
   function Error_Of (Outcome : Bible_Parser.Status) return BI.Parse_Error is
     (case Outcome is
         when Bible_Parser.Parsed | Bible_Parser.Empty_Text => BI.Empty,
         when Bible_Parser.Text_Too_Long => BI.Too_Long,
         when Bible_Parser.Unknown_Book => BI.Unknown_Book,
         when Bible_Parser.Ambiguous_Book => BI.Ambiguous_Book,
         when Bible_Parser.Malformed => BI.Malformed,
         when Bible_Parser.Number_Too_Large => BI.Too_Large,
         when Bible_Parser.Book_Not_In_Translation => BI.Not_In_Translation,
         when Bible_Parser.Chapter_Out_Of_Range => BI.No_Such_Chapter,
         when Bible_Parser.Verse_Out_Of_Range => BI.No_Such_Verse,
         when Bible_Parser.Reversed_Range => BI.Reversed);

   ---------------------------------------------------------------------------
   --  Building images.
   ---------------------------------------------------------------------------
   MAX_CELLS : constant := Objects.Maximum_Cells;
   MAX_TEXT  : constant := Objects.Maximum_Text_Bytes;
   MAX_LITERAL : constant := CCL.VM.MAX_LITERAL_TEXT;
   --  A verse in a list: its product, its reference (product, book,
   --  chapter, verse) and its text; and its literal's constructors.
   VERSE_CELLS : constant := 1 + 4 + 1;
   VERSE_LITERAL : constant := 48;
   --  A passage's or a search's fixed part, with room to spare.
   HEAD_CELLS : constant := 24;
   HEAD_LITERAL : constant := 160;

   function Literal_Of (Text : String) return Natural is
      Escapes : Natural := 0;
   begin
      for C of Text loop
         if C = '"' or else C = '\' then Escapes := Escapes + 1; end if;
      end loop;
      return Text'Length + 4 + Escapes;
   end Literal_Of;

   type Builder is record
      Image : Objects.Image;
      Step  : Objects.Build_Result := Objects.Added;
   end record;

   procedure Put (B : in out Builder; Cell : Objects.Cell) is
   begin
      if B.Step = Objects.Added then Objects.Append (B.Image, Cell, B.Step); end if;
   end Put;
   procedure Put_Text (B : in out Builder; Value : String) is
   begin
      if B.Step = Objects.Added then Objects.Append_Text (B.Image, Value, B.Step); end if;
   end Put_Text;
   procedure Put_Number (B : in out Builder; Value : Natural) is
   begin
      Put (B, Objects.Integer_Cell (Integer_64 (Value)));
   end Put_Number;
   procedure Put_Member (B : in out Builder; Position : Natural) is
   begin
      Put (B, Objects.Variant_Cell (Position + 1));
      Put (B, Objects.Unit_Cell);
   end Put_Member;
   procedure Put_Reference (B : in out Builder; Item : Reference) is
   begin
      Put (B, Objects.Product_Cell (BI.REFERENCE_FIELDS));
      Put_Text (B, Bible_Canon.Name (Item.Book));
      Put_Number (B, Item.Chapter);
      Put_Number (B, Item.Verse);
   end Put_Reference;
   procedure Put_Passage (B : in out Builder; Item : Passage) is
   begin
      Put (B, Objects.Product_Cell (BI.PASSAGE_FIELDS));
      Put_Reference (B, Item.From);
      Put_Reference (B, Item.To);
   end Put_Passage;
   procedure Put_Translation (B : in out Builder; Which : Translation) is
   begin
      Put_Member (B, Translation'Pos (Which));
   end Put_Translation;
   procedure Put_Verse (B : in out Builder; Which : Translation; Item : Reference) is
      E : Bible_Translations.Edition renames Editions (Which);
   begin
      Put (B, Objects.Product_Cell (BI.VERSE_FIELDS));
      Put_Reference (B, Item);
      Put_Text (B, Verse_Text (Which, Images.Verse_Row (E.Layout, E.Index, Item)));
   end Put_Verse;
   procedure Put_Refusal (B : in out Builder; Item : Refusal; Message, Book_Text : String) is
   begin
      Put (B, Objects.Variant_Cell (BI.Outcome_Choice'Pos (BI.Refused) + 1));
      Put (B, Objects.Product_Cell (BI.REFUSAL_FIELDS));
      Put_Member (B, BI.Parse_Error'Pos (Item.Error));
      Put_Text (B, Message);
      Put_Text (B, Book_Text);
      Put_Number (B, Item.Value);
      Put_Number (B, Item.Limit);
   end Put_Refusal;

   function Finish (B : Builder; Contract : Objects.Binding; Reply : out CCL.Host_Values.Call_Result)
     return Boolean is
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if B.Step = Objects.Added and then Objects.Validate (B.Image, Contract) then
         Reply := (Value => CCL.Host_Values.Object_Constant (B.Image), Success => True, Why => <>);
         return True;
      end if;
      return False;
   end Finish;

   procedure Too_Large (Reply : in out CCL.Host_Values.Call_Result) is
   begin
      Reply.Why := Failures.Failed (Failures.Exhausted, "the result does not fit one CCL value",
                                    "ask for a shorter passage");
   end Too_Large;

   ---------------------------------------------------------------------------
   --  Checking references against a translation.
   ---------------------------------------------------------------------------
   --  A reference from its parts (the book by any name the parser knows).
   procedure Check_Reference
     (Which : Translation; Book_Text : String; Chapter, Verse : Integer_64; Item : out Reference;
      Fault : out Refusal; Valid : out Boolean)
   is
      T : constant Versification := Editions (Which).Index.Table;
      Named : Bible_Canon.Book;
      Found, Ambiguous : Boolean;
   begin
      Item := (others => <>);
      Fault := (Book_First => 1, Book_Last => Book_Text'Length, others => <>);
      Valid := False;
      if Book_Text'Length > Bible_Parser.MAXIMUM_TEXT then
         Fault.Error := BI.Too_Long;
         return;
      end if;
      Bible_Parser.Find_Book (Book_Text, Named, Found, Ambiguous);
      if not Found then
         Fault.Error := (if Ambiguous then BI.Ambiguous_Book else BI.Unknown_Book);
         return;
      elsif not Has_Book (T, Named) then
         Fault.Error := BI.Not_In_Translation;
         return;
      elsif Chapter < 1 or else Chapter > Integer_64 (T.Chapters (Named)) then
         Fault := (Error => BI.No_Such_Chapter, Value => Natural (Integer_64'Max (0, Integer_64'Min
                     (Chapter, Integer_64 (Integer'Last)))), Limit => T.Chapters (Named), others => <>);
         return;
      end if;
      declare
         C : constant Chapter_Number := Chapter_Number (Chapter);
         Count : constant Verse_Count := Verses_In (T, Named, C);
      begin
         if Verse < 1 or else Verse > Integer_64 (Count) then
            Fault := (Error => BI.No_Such_Verse, Value => Natural (Integer_64'Max (0, Integer_64'Min
                        (Verse, Integer_64 (Integer'Last)))), Limit => Count, others => <>);
            return;
         end if;
         Item := (Named, C, Verse_Number (Verse));
         Valid := True;
      end;
   end Check_Reference;

   function Message_Of (Item : Refusal; Book_Text : String; Which : Translation; At_Reference : Reference)
     return String is
     (case Item.Error is
         when BI.Empty => "no reference was given",
         when BI.Too_Long => "that is too long to be a reference",
         when BI.Unknown_Book => "no book is called """ & Book_Text & """",
         when BI.Ambiguous_Book => """" & Book_Text & """ could be more than one book",
         when BI.Malformed => "expected a chapter and verse, such as 3:16",
         when BI.Too_Large => "that number is too large for a chapter or verse",
         when BI.Not_In_Translation => "that book is not in the " & Bible_Translations.Title (Which),
         when BI.No_Such_Chapter => "that book has" & Item.Limit'Image & " chapters in the " &
                                    Bible_Translations.Title (Which),
         when BI.No_Such_Verse => "that chapter has" & Item.Limit'Image & " verses in the " &
                                  Bible_Translations.Title (Which),
         when BI.Reversed => "that range ends before it starts",
         when BI.Omitted => "the " & Bible_Translations.Title (Which) & " has no text for " &
                            Image (At_Reference));

   ---------------------------------------------------------------------------
   --  The operations.
   ---------------------------------------------------------------------------
   procedure Refuse
     (Contract : Objects.Binding; Item : Refusal; Message, Book_Text : String;
      Reply : out CCL.Host_Values.Call_Result)
   is
      B : Builder := (Image => Objects.Empty (Contract), Step => Objects.Added);
   begin
      Put_Refusal (B, Item, Message, Book_Text);
      if not Finish (B, Contract, Reply) then
         Too_Large (Reply);
      end if;
   end Refuse;

   procedure Parse_Of (Text : String; Reply : out CCL.Host_Values.Call_Result) is
      Which : constant Translation := Bible_Translations.DEFAULT;
      Result : Bible_Parser.Result;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      Ensure (Which);
      if not Ready (Which) then
         Reply.Why := Unavailable (Which);
         return;
      end if;
      Bible_Parser.Parse (Text, Editions (Which).Index.Table, Result);
      if Result.Outcome = Bible_Parser.Parsed then
         declare
            B : Builder := (Image => Objects.Empty (Contracts.Parse), Step => Objects.Added);
         begin
            Put (B, Objects.Variant_Cell (1));
            Put_Passage (B, Result.Passage);
            if not Finish (B, Contracts.Parse, Reply) then
               Too_Large (Reply);
            end if;
         end;
      else
         declare
            Book_Text : constant String :=
              (if Result.Outcome in Bible_Parser.Unknown_Book | Bible_Parser.Ambiguous_Book
                 and then Result.Book_First in Text'Range and then Result.Book_Last in Text'Range
               then Text (Result.Book_First .. Result.Book_Last)
               elsif Result.Outcome >= Bible_Parser.Book_Not_In_Translation then Bible_Canon.Name (Result.Book)
               else "");
         begin
            Refuse (Contracts.Parse, (Error => Error_Of (Result.Outcome), Value => Result.Value,
                                      Limit => Result.Limit, others => <>),
                    Bible_Parser.Describe (Text, Result), Book_Text, Reply);
         end;
      end if;
   end Parse_Of;

   procedure Verse_Of (Which : Translation; Book_Text : String; Chapter, Verse : Integer_64;
                       Reply : out CCL.Host_Values.Call_Result)
   is
      Item : Reference;
      Fault : Refusal;
      Valid : Boolean;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      Ensure (Which);
      if not Ready (Which) then
         Reply.Why := Unavailable (Which);
         return;
      end if;
      Check_Reference (Which, Book_Text, Chapter, Verse, Item, Fault, Valid);
      if not Valid then
         Refuse (Contracts.Verse, Fault, Message_Of (Fault, Book_Text, Which, Item), Book_Text, Reply);
         return;
      end if;
      declare
         E : Bible_Translations.Edition renames Editions (Which);
         Row : constant Images.Image_Size := Images.Verse_Row (E.Layout, E.Index, Item);
      begin
         if Verse_Text (Which, Row)'Length = 0 then
            Fault := (Error => BI.Omitted, others => <>);
            Refuse (Contracts.Verse, Fault, Message_Of (Fault, Book_Text, Which, Item),
                    Bible_Canon.Name (Item.Book), Reply);
            return;
         end if;
      end;
      declare
         B : Builder := (Image => Objects.Empty (Contracts.Verse), Step => Objects.Added);
      begin
         Put (B, Objects.Variant_Cell (BI.Outcome_Choice'Pos (BI.Found) + 1));
         Put_Verse (B, Which, Item);
         if not Finish (B, Contracts.Verse, Reply) then
            Too_Large (Reply);
         end if;
      end;
   end Verse_Of;

   --  The passage's verses, as many as fit from its start.
   procedure Passage_Text_Of (Which : Translation; Item : Passage; Reply : out CCL.Host_Values.Call_Result) is
      T : constant Versification := Editions (Which).Index.Table;
      B : Builder := (Image => Objects.Empty (Contracts.Passage_Result), Step => Objects.Added);
      Total, Shown : Natural := 0;
      Cells : Natural := HEAD_CELLS;
      Bytes : Natural := 0;
      Literal : Natural := HEAD_LITERAL;
      Full : Boolean := False;
      E : Bible_Translations.Edition renames Editions (Which);
   begin
      --  Every verse of the range, and which of them fit.
      for C in Item.From.Chapter .. Item.To.Chapter loop
         for V in (if C = Item.From.Chapter then Item.From.Verse else 1) ..
                  (if C = Item.To.Chapter then Item.To.Verse else Verses_In (T, Item.From.Book, C))
         loop
            Total := Total + 1;
            if not Full then
               declare
                  Text : constant String :=
                    Verse_Text (Which, Images.Verse_Row (E.Layout, E.Index, (Item.From.Book, C, V)));
                  Need_Literal : constant Natural :=
                    VERSE_LITERAL + Literal_Of (Text) + Literal_Of (Bible_Canon.Name (Item.From.Book));
               begin
                  if Cells + VERSE_CELLS <= MAX_CELLS and then Bytes + Text'Length + Bible_Canon.MAXIMUM_NAME <= MAX_TEXT
                    and then Literal + Need_Literal <= MAX_LITERAL
                  then
                     Cells := Cells + VERSE_CELLS;
                     Bytes := Bytes + Text'Length + Bible_Canon.MAXIMUM_NAME;
                     Literal := Literal + Need_Literal;
                     Shown := Shown + 1;
                  else
                     Full := True;
                  end if;
               end;
            end if;
         end loop;
      end loop;
      Put (B, Objects.Variant_Cell (BI.Outcome_Choice'Pos (BI.Found) + 1));
      Put (B, Objects.Product_Cell (BI.PASSAGE_TEXT_FIELDS));
      Put_Passage (B, Item);
      Put_Translation (B, Which);
      Put (B, Objects.Sequence_Cell (Shown));
      declare
         Placed : Natural := 0;
      begin
         for C in Item.From.Chapter .. Item.To.Chapter loop
            for V in (if C = Item.From.Chapter then Item.From.Verse else 1) ..
                     (if C = Item.To.Chapter then Item.To.Verse else Verses_In (T, Item.From.Book, C))
            loop
               exit when Placed = Shown;
               Put_Verse (B, Which, (Item.From.Book, C, V));
               Placed := Placed + 1;
            end loop;
         end loop;
      end;
      Put_Number (B, Total);
      if not Finish (B, Contracts.Passage_Result, Reply) then
         Too_Large (Reply);
      end if;
   end Passage_Text_Of;

   procedure Passage_Of (Which : Translation; From_Book : String; From_Chapter, From_Verse : Integer_64;
                         To_Book : String; To_Chapter, To_Verse : Integer_64;
                         Reply : out CCL.Host_Values.Call_Result)
   is
      From, To : Reference;
      Fault : Refusal;
      Valid : Boolean;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      Ensure (Which);
      if not Ready (Which) then
         Reply.Why := Unavailable (Which);
         return;
      end if;
      Check_Reference (Which, From_Book, From_Chapter, From_Verse, From, Fault, Valid);
      if not Valid then
         Refuse (Contracts.Passage_Result, Fault, Message_Of (Fault, From_Book, Which, From), From_Book, Reply);
         return;
      end if;
      Check_Reference (Which, To_Book, To_Chapter, To_Verse, To, Fault, Valid);
      if not Valid then
         Refuse (Contracts.Passage_Result, Fault, Message_Of (Fault, To_Book, Which, To), To_Book, Reply);
         return;
      end if;
      if To.Book /= From.Book or else not Ordered ((From, To)) then
         Fault := (Error => BI.Reversed, others => <>);
         Refuse (Contracts.Passage_Result, Fault,
                 (if To.Book /= From.Book then "a passage stays within one book" else "that range ends before it starts"),
                 Bible_Canon.Name (From.Book), Reply);
         return;
      end if;
      Passage_Text_Of (Which, (From, To), Reply);
   end Passage_Of;

   procedure Read_Of (Text : String; Reply : out CCL.Host_Values.Call_Result) is
      Which : constant Translation := Bible_Translations.DEFAULT;
      Result : Bible_Parser.Result;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      Ensure (Which);
      if not Ready (Which) then
         Reply.Why := Unavailable (Which);
         return;
      end if;
      Bible_Parser.Parse (Text, Editions (Which).Index.Table, Result);
      if Result.Outcome = Bible_Parser.Parsed then
         Passage_Text_Of (Which, Result.Passage, Reply);
      else
         Refuse (Contracts.Passage_Result, (Error => Error_Of (Result.Outcome), Value => Result.Value,
                                            Limit => Result.Limit, others => <>),
                 Bible_Parser.Describe (Text, Result), "", Reply);
      end if;
   end Read_Of;

   procedure Search_Of (Which : Translation; Words : String; Reply : out CCL.Host_Values.Call_Result) is
      Q : Bible_Search.Query;
      Normalized : Bible_Search.Normalize_Status;
      Status : Bible_Search.Search_Status;
      Hits : Bible_Search.Hit_Array (1 .. MAX_CELLS / VERSE_CELLS);
      Found, Total : Natural;
   begin
      if Work = null then
         Work := new Bible_Search.Work_Area;
      end if;
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      Ensure (Which);
      if not Ready (Which) then
         Reply.Why := Unavailable (Which);
         return;
      end if;
      Bible_Search.Normalize (Words, Q, Normalized);
      if Normalized /= Bible_Search.Normalized then
         Reply.Why := Failures.Failed
           (Failures.Invalid_Argument, "search for one to" & Natural'Image (Bible_Search.MAXIMUM_TERMS) & " words",
            "(bible.search ""faith hope love"")");
         return;
      end if;
      declare
         E : Bible_Translations.Edition renames Editions (Which);
      begin
         Bible_Search.Search (E.Data.all, E.Layout, Q, False, Work.all, Hits, Found, Total, Status);
         if Status /= Bible_Search.Searched then
            Reply.Why := Failures.Failed (Failures.Device_Error, "the translation's index is damaged");
            return;
         end if;
         declare
            B : Builder := (Image => Objects.Empty (Contracts.Search), Step => Objects.Added);
            Cells : Natural := HEAD_CELLS;
            Bytes : Natural := Words'Length;
            Literal : Natural := HEAD_LITERAL + Literal_Of (Words);
            Shown : Natural := 0;
            Refs : array (1 .. Found) of Reference;
         begin
            for I in 1 .. Found loop
               declare
                  Ref : Reference;
                  Located : Boolean;
               begin
                  Images.Locate (E.Layout, E.Index, Hits (I), Ref, Located);
                  exit when not Located;
                  declare
                     Text : constant String := Verse_Text (Which, Hits (I));
                     Need_Literal : constant Natural :=
                       VERSE_LITERAL + Literal_Of (Text) + Literal_Of (Bible_Canon.Name (Ref.Book));
                  begin
                     exit when Cells + VERSE_CELLS > MAX_CELLS
                       or else Bytes + Text'Length + Bible_Canon.MAXIMUM_NAME > MAX_TEXT
                       or else Literal + Need_Literal > MAX_LITERAL;
                     Cells := Cells + VERSE_CELLS;
                     Bytes := Bytes + Text'Length + Bible_Canon.MAXIMUM_NAME;
                     Literal := Literal + Need_Literal;
                     Shown := Shown + 1;
                     Refs (Shown) := Ref;
                  end;
               end;
            end loop;
            Put (B, Objects.Product_Cell (BI.SEARCH_RESULT_FIELDS));
            Put_Text (B, Words);
            Put_Translation (B, Which);
            Put (B, Objects.Sequence_Cell (Shown));
            for I in 1 .. Shown loop
               Put_Verse (B, Which, Refs (I));
            end loop;
            Put_Number (B, Total);
            if not Finish (B, Contracts.Search, Reply) then
               Too_Large (Reply);
            end if;
         end;
      end;
   end Search_Of;

   ---------------------------------------------------------------------------
   --  Dispatch.
   ---------------------------------------------------------------------------
   procedure Invoke
     (Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      Op : BI.Operation;
      Object : Views.Snapshot;
      Captured : Boolean := False;
      function Root return Views.Cursor is (Views.Root (Object));
      function Field (At_Cursor : Views.Cursor; Index : Positive) return Views.Cursor is
        (Views.Field (Object, At_Cursor, Index));
      function Number (At_Cursor : Views.Cursor) return Integer_64 is
        (Objects.Integer_Of (Views.Scalar (Object, At_Cursor)));
      function Translation_At (At_Cursor : Views.Cursor) return Translation is
        (Translation'Val (Natural'Min (Natural (Views.Alternative (Object, At_Cursor)) - 1,
                                       Translation'Pos (Translation'Last))));
      procedure Capture (Contract : Objects.Binding) is
      begin
         if Argument.Kind = CCL.Host_Values.Object_Value then
            Views.Capture (Object, Contract, Argument.Object, Captured);
         end if;
      end Capture;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if not Handles (Binding) then return; end if;
      Op := BI.Operation'Val (Binding - BI.FIRST_BINDING);
      case Op is
         when BI.Parse | BI.Read | BI.Search =>
            if Argument.Kind /= CCL.Host_Values.Text_Value then return; end if;
            declare
               Text : constant String := Argument.Content.Data (1 .. Argument.Content.Length);
            begin
               case Op is
                  when BI.Parse => Parse_Of (Text, Reply);
                  when BI.Read => Read_Of (Text, Reply);
                  when others => Search_Of (Bible_Translations.DEFAULT, Text, Reply);
               end case;
            end;
         when BI.Verse | BI.Verse_In =>
            Capture (if Op = BI.Verse then Contracts.Reference else Contracts.Verse_Query);
            if not Captured then return; end if;
            declare
               Ref : constant Views.Cursor := (if Op = BI.Verse then Root else Field (Root, 1));
               Which : constant Translation :=
                 (if Op = BI.Verse then Bible_Translations.DEFAULT else Translation_At (Field (Root, 2)));
            begin
               Verse_Of (Which, Views.Text (Object, Field (Ref, 1)), Number (Field (Ref, 2)), Number (Field (Ref, 3)),
                         Reply);
            end;
         when BI.Passage | BI.Passage_In =>
            Capture (if Op = BI.Passage then Contracts.Passage else Contracts.Passage_Query);
            if not Captured then return; end if;
            declare
               Range_At : constant Views.Cursor := (if Op = BI.Passage then Root else Field (Root, 1));
               From : constant Views.Cursor := Field (Range_At, 1);
               To : constant Views.Cursor := Field (Range_At, 2);
               Which : constant Translation :=
                 (if Op = BI.Passage then Bible_Translations.DEFAULT else Translation_At (Field (Root, 2)));
            begin
               Passage_Of (Which, Views.Text (Object, Field (From, 1)), Number (Field (From, 2)),
                           Number (Field (From, 3)), Views.Text (Object, Field (To, 1)), Number (Field (To, 2)),
                           Number (Field (To, 3)), Reply);
            end;
         when BI.Search_In =>
            Capture (Contracts.Search_Query);
            if not Captured then return; end if;
            Search_Of (Translation_At (Field (Root, 2)), Views.Text (Object, Field (Root, 1)), Reply);
      end case;
   end Invoke;
end CCL_Bible_Bindings;
