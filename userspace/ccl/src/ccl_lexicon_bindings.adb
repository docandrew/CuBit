with CCL.Interfaces.Lexicon;
with CCL.Objects;
with CCL.Objects.Views;
with CCL.VM;
with CCL_Lexicon_Source;
with CuBit.Failures;
with Lexicon_Answers;
with Lexicon_Wire;

package body CCL_Lexicon_Bindings is
   use Interfaces;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Host_Values.Value_Kind;
   use type CCL.Objects.Build_Result;
   use type CCL_Lexicon_Source.Ask_Status;
   use type Lexicon_Wire.Item_Kind;
   use type Lexicon_Answers.Operation;
   use type CCL.Interfaces.Lexicon.Operation;
   package LX renames CCL.Interfaces.Lexicon;
   package LA renames Lexicon_Answers;
   package W renames Lexicon_Wire;
   package Views renames CCL.Objects.Views;
   package Failures renames CuBit.Failures;
   package Objects renames CCL.Objects;

   Contracts : LX.Contracts;

   function Handles (Binding : Unsigned_32) return Boolean is
     (Binding in LX.FIRST_BINDING .. LX.Binding_Of (LX.Operation'Last));

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
   begin
      LX.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Catalog.Resolve_Schema (Catalog, LX.DEFINITION_KEY, Contracts.Definition);
      CCL.Catalog.Resolve_Schema (Catalog, LX.THESAURUS_KEY, Contracts.Thesaurus);
      CCL.Catalog.Resolve_Schema (Catalog, LX.SYNONYMS_KEY, Contracts.Synonyms);
      CCL.Catalog.Resolve_Schema (Catalog, LX.SPELLINGS_KEY, Contracts.Spellings);
      CCL.Catalog.Resolve_Schema (Catalog, LX.MISSPELLINGS_KEY, Contracts.Misspellings);
      CCL.Catalog.Resolve_Schema (Catalog, LX.PAGE_KEY, Contracts.Page);
      Success := Objects.Is_Bound (Contracts.Definition) and then Objects.Is_Bound (Contracts.Thesaurus)
        and then Objects.Is_Bound (Contracts.Synonyms) and then Objects.Is_Bound (Contracts.Spellings)
        and then Objects.Is_Bound (Contracts.Misspellings) and then Objects.Is_Bound (Contracts.Page);
      for Op in LX.Operation loop
         exit when not Success;
         CCL.Catalog.Resolve (Catalog, "lexicon." & LX.Name (Op), Resolved, Found);
         Success := Found;
         if Success then
            CCL.Catalog.Install (Grants, Resolved, LX.Binding_Of (Op), Grant);
            Success := Grant = CCL.Catalog.Grant_Added;
         end if;
      end loop;
   end Install;

   ---------------------------------------------------------------------------
   --  Walking an answer: every item of every page, in order.
   ---------------------------------------------------------------------------
   type Walk_Status is (Walked, No_Answer, Unavailable, Failed);

   function Text_Of (Page : W.Answer_Bytes; Got : W.Item) return String is
      Result : String (1 .. Got.Length);
   begin
      for I in Result'Range loop
         Result (I) := Character'Val (Page (Page'First + Got.First + I - 1));
      end loop;
      return Result;
   end Text_Of;

   generic
      with procedure Visit (Page : W.Answer_Bytes; Got : W.Item; Stop : in out Boolean);
   procedure Walk (Op : LA.Operation; Text : String; Status : out Walk_Status);

   procedure Walk (Op : LA.Operation; Text : String; Status : out Walk_Status) is
      Resume : Unsigned_32 := 0;
      Page : CCL_Lexicon_Source.Page;
      Used, Position : Natural;
      Asked : CCL_Lexicon_Source.Ask_Status;
      Found, Damaged, More : Boolean;
      Stop : Boolean := False;
      Got : W.Item;
      Pages : Natural := 0;
      N : Unsigned_32;
   begin
      loop
         CCL_Lexicon_Source.Ask (Op, Text, Resume, LX.SUGGESTIONS, Page, Used, Asked);
         case Asked is
            when CCL_Lexicon_Source.Answered => null;
            when CCL_Lexicon_Source.Unavailable => Status := Unavailable; return;
            when CCL_Lexicon_Source.Failed => Status := Failed; return;
         end case;
         if Used = 0 then
            Status := (if Pages = 0 then No_Answer else Walked);
            return;
         end if;
         Pages := Pages + 1;
         More := False;
         Position := 0;
         loop
            W.Next (Page (0 .. Used - 1), Position, Found, Damaged, Got);
            if Damaged then
               Status := Failed;
               return;
            end if;
            exit when not Found;
            if Got.Kind in W.Continue_At | W.Checked_Upto then
               N := W.Number (Page (0 .. Used - 1), Got);
               if Got.Kind = W.Continue_At or else Natural (N) < Text'Length then
                  if N <= Resume then
                     Status := Failed;   --  no progress: never loop
                     return;
                  end if;
                  Resume := N;
                  More := True;
               end if;
            else
               Visit (Page (0 .. Used - 1), Got, Stop);
               if Stop then
                  Status := Walked;
                  return;
               end if;
            end if;
         end loop;
         exit when not More;
      end loop;
      Status := Walked;
   end Walk;

   ---------------------------------------------------------------------------
   --  What one result image holds: CCL.Objects' cells and text bytes.
   ---------------------------------------------------------------------------
   MAX_CELLS : constant := Objects.Maximum_Cells;
   MAX_TEXT  : constant := Objects.Maximum_Text_Bytes;
   subtype Cell_Count is Natural range 0 .. MAX_CELLS;
   subtype Text_Count is Natural range 0 .. MAX_TEXT;

   type Piece is record
      First  : Positive := 1;
      Length : Text_Count := 0;
   end record;
   type Kept_Item is record
      Kind  : W.Item_Kind := W.Synonym;
      Text  : Piece;
      Group : Cell_Count := 0;
   end record;
   type Kept_Items is array (1 .. MAX_CELLS) of Kept_Item;
   type Group_Record is record
      Part    : LX.Part_Of_Speech := LX.Noun;
      Meaning : Piece;
   end record;
   type Group_Array is array (1 .. MAX_CELLS) of Group_Record;
   type Piece_Array is array (1 .. LA.MAXIMUM_FORMS) of Piece;
   subtype Head_Count is Natural range 0 .. LA.MAXIMUM_FORMS;

   --  The parts of a result, and the budget they use.
   type Collection is record
      Store       : String (1 .. MAX_TEXT) := [others => ' '];
      Store_Used  : Text_Count := 0;
      Items       : Kept_Items;
      Item_Count  : Cell_Count := 0;
      Groups      : Group_Array;
      Group_Count : Cell_Count := 0;
      Heads       : Piece_Array;
      Heads_Found : Head_Count := 0;
      First_Total : Natural := 0;
      Query       : Piece;
      Cells       : Natural := 0;
      Bytes       : Natural := 0;
      Literal     : Natural := 0;
   end record;

   --  Cells of one group before its list elements: a Word_Sense is its
   --  product, its part (choice and unit), its definition and three list
   --  counts; a Synonym_Group the same with four lists.
   WORD_SENSE_CELLS    : constant := 1 + 2 + 1 + 3;
   SYNONYM_GROUP_CELLS : constant := 1 + 2 + 1 + 4;
   --  An entry's fixed cells: the choice, its product, the word, the groups'
   --  and the also list's counts, first and total (plus one per also word).
   ENTRY_CELLS : constant := 1 + 1 + 1 + 1 + 1 + 1 + 1;
   --  A synonym list: the choice, its product, the word, two list counts.
   SYNONYM_LIST_CELLS : constant := 1 + 1 + 1 + 2;
   --  A misspelling: its product, word, offset and suggestions' count.
   MISSPELLING_CELLS : constant := 1 + 1 + 1 + 1;

   --  A result shown in the console is written out as its CCL literal
   --  (CCL.VM.MAX_LITERAL_TEXT), so that is budgeted too, generously: a
   --  string is its text, quotes, a separator and escapes; a list at least
   --  "(list-of String) "; a group its constructor and part names.
   MAX_LITERAL : constant := CCL.VM.MAX_LITERAL_TEXT;
   LIST_LITERAL : constant := 18;
   GROUP_LITERAL : constant := 48 + 4 * LIST_LITERAL;
   ENTRY_LITERAL : constant := 64 + 2 * LIST_LITERAL;
   SYNONYM_LIST_LITERAL : constant := 48 + 2 * LIST_LITERAL;
   MISSPELLING_LITERAL : constant := 16 + 20 + LIST_LITERAL;

   function Literal_Of (Text : String) return Natural is
      Escapes : Natural := 0;
   begin
      for C of Text loop
         if C = '"' or else C = '\' then Escapes := Escapes + 1; end if;
      end loop;
      return Text'Length + 4 + Escapes;
   end Literal_Of;

   function Fits (C : Collection; Cells, Bytes, Literal : Natural) return Boolean is
     (C.Cells + Cells <= MAX_CELLS and then C.Bytes + Bytes <= MAX_TEXT
      and then C.Literal + Literal <= MAX_LITERAL);

   procedure Charge (C : in out Collection; Cells, Bytes, Literal : Natural) is
   begin
      C.Cells := C.Cells + Cells;
      C.Bytes := C.Bytes + Bytes;
      C.Literal := C.Literal + Literal;
   end Charge;

   --  Text into the store (a piece of it); Kept False when it is full.
   procedure Keep (C : in out Collection; Text : String; Result : out Piece; Kept : out Boolean) is
   begin
      Kept := Text'Length <= MAX_TEXT - C.Store_Used;
      Result := (First => C.Store_Used + 1, Length => 0);
      if Kept then
         C.Store (C.Store_Used + 1 .. C.Store_Used + Text'Length) := Text;
         Result.Length := Text'Length;
         C.Store_Used := C.Store_Used + Text'Length;
      end if;
   end Keep;

   function Text (C : Collection; P : Piece) return String is (C.Store (P.First .. P.First + P.Length - 1));

   function Part_Of (Page : W.Answer_Bytes; Got : W.Item) return LX.Part_Of_Speech is
     (if Got.Length = 1 and then Natural (Page (Page'First + Got.First)) <= LX.Part_Of_Speech'Pos (LX.Part_Of_Speech'Last)
      then LX.Part_Of_Speech'Val (Page (Page'First + Got.First)) else LX.Noun);

   ---------------------------------------------------------------------------
   --  Building images.
   ---------------------------------------------------------------------------
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
   procedure Put_Part (B : in out Builder; Part : LX.Part_Of_Speech) is
   begin
      Put (B, Objects.Variant_Cell (LX.Part_Of_Speech'Pos (Part) + 1));
      Put (B, Objects.Unit_Cell);
   end Put_Part;
   procedure Put_Choice (B : in out Builder; Choice : LX.Found_Choice) is
   begin
      Put (B, Objects.Variant_Cell (LX.Found_Choice'Pos (Choice) + 1));
   end Put_Choice;

   --  The kept items of group G (0: none) and kind K, as a list of Strings.
   procedure Put_List (B : in out Builder; C : Collection; G : Cell_Count; K : W.Item_Kind) is
      N : Natural := 0;
   begin
      for I in 1 .. C.Item_Count loop
         if C.Items (I).Group = G and then C.Items (I).Kind = K then N := N + 1; end if;
      end loop;
      Put (B, Objects.Sequence_Cell (N));
      for I in 1 .. C.Item_Count loop
         if C.Items (I).Group = G and then C.Items (I).Kind = K then
            Put_Text (B, Text (C, C.Items (I).Text));
         end if;
      end loop;
   end Put_List;

   --  Headwords after the first: the words the query may also be a form of.
   procedure Put_Also (B : in out Builder; C : Collection) is
   begin
      Put (B, Objects.Sequence_Cell (Natural'Max (C.Heads_Found, 1) - 1));
      for H in 2 .. C.Heads_Found loop
         Put_Text (B, Text (C, C.Heads (H)));
      end loop;
   end Put_Also;

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

   ---------------------------------------------------------------------------
   --  The operations.
   ---------------------------------------------------------------------------
   function Walk_Failure (Status : Walk_Status; Word : String) return Failures.Failure is
     (case Status is
         when Unavailable => Failures.Failed
           (Failures.Unavailable, CCL_Lexicon_Source.Unavailable_Reason, CCL_Lexicon_Source.Unavailable_Remedy),
         when No_Answer => Failures.Failed
           (Failures.Invalid_Argument, """" & Word & """ is not a word to look up",
            "give one word or phrase of 1 .." & Natural'Image (LX.MAXIMUM_WORD) & " bytes"),
         when Failed | Walked => Failures.Failed
           (Failures.Device_Error, "the lexicon service did not answer"));

   TOO_LARGE_REMEDY : constant String := "ask for less: a shorter text, or one entry page at a time";

   --  First pass: the query, the headwords and near spellings.
   procedure Survey (Op : LA.Operation; Word : String; C : in out Collection; Status : out Walk_Status) is
      In_First : Boolean := False;
      procedure Visit (Page : W.Answer_Bytes; Got : W.Item; Stop : in out Boolean) is
         Kept : Boolean;
         Kept_Piece : Piece;
      begin
         case Got.Kind is
            when W.Query =>
               Keep (C, Text_Of (Page, Got), Kept_Piece, Kept);
            C.Query := Kept_Piece;
            when W.Headword =>
               In_First := C.Heads_Found = 0;
               if C.Heads_Found < Head_Count'Last then
                  C.Heads_Found := C.Heads_Found + 1;
                  Keep (C, Text_Of (Page, Got), Kept_Piece, Kept);
            C.Heads (C.Heads_Found) := Kept_Piece;
               end if;
            when W.Sense_Total =>
               if In_First then C.First_Total := Natural (W.Number (Page, Got)); end if;
            when W.Suggestion =>
               if C.Heads_Found = 0 and then C.Item_Count < MAX_CELLS then
                  C.Item_Count := C.Item_Count + 1;
                  C.Items (C.Item_Count).Kind := W.Suggestion;
                  C.Items (C.Item_Count).Group := 0;
                  Keep (C, Text_Of (Page, Got), Kept_Piece, Kept);
            C.Items (C.Item_Count).Text := Kept_Piece;
               end if;
            when others =>
               null;
         end case;
         Stop := False;
      end Visit;
      procedure Run is new Walk (Visit);
   begin
      Run (Op, Word, Status);
   end Survey;

   --  Not_Found with the near spellings Survey kept.
   procedure Not_Found (C : Collection; Contract : Objects.Binding; Reply : out CCL.Host_Values.Call_Result) is
      B : Builder := (Image => Objects.Empty (Contract), Step => Objects.Added);
   begin
      Put_Choice (B, LX.Not_Found);
      Put (B, Objects.Product_Cell (LX.NEAR_SPELLINGS_FIELDS));
      Put_Text (B, Text (C, C.Query));
      Put_List (B, C, 0, W.Suggestion);
      if not Finish (B, Contract, Reply) then
         Reply.Why := Failures.Failed (Failures.Exhausted, "the near spellings do not fit one result");
      end if;
   end Not_Found;

   --  Second pass: the first headword's senses from First on, while they
   --  fit (Synonym_Group for Thesaurus, Word_Sense for Define).
   procedure Gather (Op : LA.Operation; Word : String; First : Positive; C : in out Collection;
                     Status : out Walk_Status) is
      Head : Natural := 0;
      Ordinal : Natural := 0;
      Taking : Boolean := False;
      Full : Boolean := False;
      Mark_Items : Cell_Count := 0;
      Mark_Store : Text_Count := 0;
      Mark_Cells, Mark_Bytes, Mark_Literal : Natural := 0;
      GROUP_CELLS : constant Natural := (if Op = LA.Thesaurus then SYNONYM_GROUP_CELLS else WORD_SENSE_CELLS);
      function Wanted (Kind : W.Item_Kind) return Boolean is
        (case Kind is
            when W.Example => Op = LA.Define,
            when W.Synonym | W.Antonym => True,
            when W.Similar | W.Broader => Op = LA.Thesaurus,
            when others => False);
      procedure Visit (Page : W.Answer_Bytes; Got : W.Item; Stop : in out Boolean) is
         Kept : Boolean;
         Kept_Piece : Piece;
         Piece_Of : Piece;
      begin
         case Got.Kind is
            when W.Headword =>
               Head := Head + 1;
               Taking := False;
               if Head > 1 then Stop := True; end if;
            when W.Sense_Start =>
               Ordinal := Ordinal + 1;
               Taking := Head = 1 and then Ordinal >= First;
               if Taking then
                  --  A whole group or none: remember where it starts.
                  Mark_Items := C.Item_Count;
                  Mark_Store := C.Store_Used;
                  Mark_Cells := C.Cells;
                  Mark_Bytes := C.Bytes;
                  Mark_Literal := C.Literal;
                  if not Fits (C, GROUP_CELLS, 0, GROUP_LITERAL) or else C.Group_Count = MAX_CELLS then
                     Full := True;
                     Stop := True;
                     return;
                  end if;
                  Charge (C, GROUP_CELLS, 0, GROUP_LITERAL);
                  C.Group_Count := C.Group_Count + 1;
                  C.Groups (C.Group_Count) := (Part => Part_Of (Page, Got), Meaning => (1, 0));
               end if;
            when W.Definition =>
               if Taking then
                  if not Fits (C, 0, Got.Length, Literal_Of (Text_Of (Page, Got))) then
                     Full := True;
                  else
                     Keep (C, Text_Of (Page, Got), Piece_Of, Kept);
                     C.Groups (C.Group_Count).Meaning := Piece_Of;
                     Charge (C, 0, Got.Length, Literal_Of (Text_Of (Page, Got)));
                  end if;
               end if;
            when others =>
               if Taking and then Wanted (Got.Kind) then
                  if not Fits (C, 1, Got.Length, Literal_Of (Text_Of (Page, Got))) or else C.Item_Count = MAX_CELLS then
                     Full := True;
                  else
                     Keep (C, Text_Of (Page, Got), Piece_Of, Kept);
                     C.Item_Count := C.Item_Count + 1;
                     C.Items (C.Item_Count) := (Kind => Got.Kind, Text => Piece_Of, Group => C.Group_Count);
                     Charge (C, 1, Got.Length, Literal_Of (Text_Of (Page, Got)));
                  end if;
               end if;
         end case;
         if Full then
            --  This group does not fit: drop it whole, and stop.
            if Taking then
               C.Item_Count := Mark_Items;
               C.Store_Used := Mark_Store;
               C.Cells := Mark_Cells;
               C.Bytes := Mark_Bytes;
               C.Literal := Mark_Literal;
               C.Group_Count := C.Group_Count - 1;
            end if;
            Stop := True;
         end if;
      end Visit;
      procedure Run is new Walk (Visit);
   begin
      Run (Op, Word, Status);
   end Gather;

   --  Define or Thesaurus, from sense First on.
   procedure Entry_Of (Op : LA.Operation; Word : String; First : Positive;
                       Reply : out CCL.Host_Values.Call_Result) is
      C : Collection;
      Status : Walk_Status;
      Contract : constant Objects.Binding :=
        (if Op = LA.Thesaurus then Contracts.Thesaurus else Contracts.Definition);
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      Survey (Op, Word, C, Status);
      if Status /= Walked then
         Reply.Why := Walk_Failure (Status, Word);
         return;
      elsif C.Heads_Found = 0 then
         Not_Found (C, Contract, Reply);
         return;
      end if;
      --  The fixed part: choice, product, word, groups list, also list (and
      --  its words), first and total.
      Charge (C, ENTRY_CELLS + (C.Heads_Found - 1), 0, ENTRY_LITERAL);
      for H in 1 .. C.Heads_Found loop
         Charge (C, 0, C.Heads (H).Length, Literal_Of (Text (C, C.Heads (H))));
      end loop;
      --  Survey's near spellings are not part of an entry.
      C.Item_Count := 0;
      Gather (Op, Word, First, C, Status);
      if Status /= Walked then
         Reply.Why := Walk_Failure (Status, Word);
         return;
      elsif C.Group_Count = 0 and then First <= C.First_Total then
         Reply.Why := Failures.Failed
           (Failures.Exhausted, "sense" & Natural'Image (First) & " of """ & Word & """ does not fit one result",
            TOO_LARGE_REMEDY);
         return;
      end if;
      declare
         B : Builder := (Image => Objects.Empty (Contract), Step => Objects.Added);
      begin
         Put_Choice (B, LX.Found);
         Put (B, Objects.Product_Cell (if Op = LA.Thesaurus then LX.THESAURUS_ENTRY_FIELDS
                                       else LX.WORD_ENTRY_FIELDS));
         Put_Text (B, Text (C, C.Heads (1)));
         Put (B, Objects.Sequence_Cell (C.Group_Count));
         for G in 1 .. C.Group_Count loop
            if Op = LA.Thesaurus then
               Put (B, Objects.Product_Cell (LX.SYNONYM_GROUP_FIELDS));
               Put_Part (B, C.Groups (G).Part);
               Put_Text (B, Text (C, C.Groups (G).Meaning));
               Put_List (B, C, G, W.Synonym);
               Put_List (B, C, G, W.Antonym);
               Put_List (B, C, G, W.Similar);
               Put_List (B, C, G, W.Broader);
            else
               Put (B, Objects.Product_Cell (LX.WORD_SENSE_FIELDS));
               Put_Part (B, C.Groups (G).Part);
               Put_Text (B, Text (C, C.Groups (G).Meaning));
               Put_List (B, C, G, W.Example);
               Put_List (B, C, G, W.Synonym);
               Put_List (B, C, G, W.Antonym);
            end if;
         end loop;
         Put_Also (B, C);
         Put_Number (B, First);
         Put_Number (B, C.First_Total);
         if not Finish (B, Contract, Reply) then
            Reply.Why := Failures.Failed (Failures.Exhausted, "the entry does not fit one result",
                                          TOO_LARGE_REMEDY);
         end if;
      end;
   end Entry_Of;

   --  Every synonym and antonym of the first headword, once each.
   procedure Synonyms_Of (Word : String; Reply : out CCL.Host_Values.Call_Result) is
      C : Collection;
      Status : Walk_Status;
      Full : Boolean := False;
      Head : Natural := 0;
      procedure Visit (Page : W.Answer_Bytes; Got : W.Item; Stop : in out Boolean) is
         Kept : Boolean;
         Kept_Piece : Piece;
         Value : constant String := (if Got.Kind in W.Synonym | W.Antonym then Text_Of (Page, Got) else "");
      begin
         if Got.Kind = W.Headword then
            Head := Head + 1;
            Stop := Head > 1;
         elsif Got.Kind in W.Synonym | W.Antonym then
            for I in 1 .. C.Item_Count loop
               if C.Items (I).Kind = Got.Kind and then Text (C, C.Items (I).Text) = Value then
                  return;
               end if;
            end loop;
            if not Fits (C, 1, Value'Length, Literal_Of (Value)) or else C.Item_Count = MAX_CELLS then
               Full := True;
               Stop := True;
               return;
            end if;
            C.Item_Count := C.Item_Count + 1;
            C.Items (C.Item_Count).Kind := Got.Kind;
            C.Items (C.Item_Count).Group := 0;
            Keep (C, Value, Kept_Piece, Kept);
            C.Items (C.Item_Count).Text := Kept_Piece;
            Charge (C, 1, Value'Length, Literal_Of (Value));
         end if;
      end Visit;
      procedure Run is new Walk (Visit);
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      Survey (LA.Define, Word, C, Status);
      if Status /= Walked then
         Reply.Why := Walk_Failure (Status, Word);
         return;
      elsif C.Heads_Found = 0 then
         Not_Found (C, Contracts.Synonyms, Reply);
         return;
      end if;
      C.Item_Count := 0;
      --  Choice, product, word, two lists.
      Charge (C, SYNONYM_LIST_CELLS, C.Heads (1).Length, SYNONYM_LIST_LITERAL + Literal_Of (Text (C, C.Heads (1))));
      Run (LA.Define, Word, Status);
      if Status /= Walked or else Full then
         Reply.Why := (if Full then Failures.Failed
                         (Failures.Exhausted, "the synonyms of """ & Word & """ do not fit one result",
                          "use (thesaurus-page (Page_Query """ & Word & """ 1)) for them group by group")
                       else Walk_Failure (Status, Word));
         return;
      end if;
      declare
         B : Builder := (Image => Objects.Empty (Contracts.Synonyms), Step => Objects.Added);
      begin
         Put_Choice (B, LX.Found);
         Put (B, Objects.Product_Cell (LX.SYNONYM_LIST_FIELDS));
         Put_Text (B, Text (C, C.Heads (1)));
         Put_List (B, C, 0, W.Synonym);
         Put_List (B, C, 0, W.Antonym);
         if not Finish (B, Contracts.Synonyms, Reply) then
            Reply.Why := Failures.Failed (Failures.Exhausted, "the synonyms do not fit one result");
         end if;
      end;
   end Synonyms_Of;

   --  Near spellings of a word (a record, found or not).
   procedure Suggestions_Of (Word : String; Reply : out CCL.Host_Values.Call_Result) is
      C : Collection;
      Status : Walk_Status;
      procedure Visit (Page : W.Answer_Bytes; Got : W.Item; Stop : in out Boolean) is
         Kept : Boolean;
         Kept_Piece : Piece;
      begin
         if Got.Kind = W.Query then
            Keep (C, Text_Of (Page, Got), Kept_Piece, Kept);
            C.Query := Kept_Piece;
         elsif Got.Kind = W.Suggestion and then C.Item_Count < MAX_CELLS then
            C.Item_Count := C.Item_Count + 1;
            C.Items (C.Item_Count).Kind := W.Suggestion;
            C.Items (C.Item_Count).Group := 0;
            Keep (C, Text_Of (Page, Got), Kept_Piece, Kept);
            C.Items (C.Item_Count).Text := Kept_Piece;
         end if;
         Stop := False;
      end Visit;
      procedure Run is new Walk (Visit);
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      Run (LA.Suggest, Word, Status);
      if Status /= Walked then
         Reply.Why := Walk_Failure (Status, Word);
         return;
      end if;
      declare
         B : Builder := (Image => Objects.Empty (Contracts.Spellings), Step => Objects.Added);
      begin
         Put (B, Objects.Product_Cell (LX.NEAR_SPELLINGS_FIELDS));
         Put_Text (B, Text (C, C.Query));
         Put_List (B, C, 0, W.Suggestion);
         if not Finish (B, Contracts.Spellings, Reply) then
            Reply.Why := Failures.Failed (Failures.Exhausted, "the near spellings do not fit one result");
         end if;
      end;
   end Suggestions_Of;

   --  The misspelt words of a text, each with its near spellings; or, for
   --  Known, whether there are none.
   procedure Check_Of (Text_In : String; Known : Boolean; Reply : out CCL.Host_Values.Call_Result) is
      C : Collection;
      Status : Walk_Status;
      Full : Boolean := False;
      --  Each misspelling is a group: its offset in Group_Record.Meaning.First
      --  (0-based) and its word as Meaning's text.
      Offsets : array (1 .. MAX_CELLS) of Natural := [others => 0];
      procedure Visit (Page : W.Answer_Bytes; Got : W.Item; Stop : in out Boolean) is
         Kept : Boolean;
         Kept_Piece : Piece;
         First : Unsigned_32;
         Length : Unsigned_16;
      begin
         if Got.Kind = W.Misspelling then
            W.Span (Page, Got, First, Length);
            if Natural (First) + Natural (Length) > Text_In'Length then
               return;   --  not in the text: ignore a malformed span
            end if;
            if not Fits (C, MISSPELLING_CELLS, Natural (Length), MISSPELLING_LITERAL + Natural (Length) + 4)
              or else C.Group_Count = MAX_CELLS
            then
               Full := True;
               Stop := True;
               return;
            end if;
            C.Group_Count := C.Group_Count + 1;
            Offsets (C.Group_Count) := Natural (First);
            Keep (C, Text_In (Text_In'First + Natural (First) .. Text_In'First + Natural (First) + Natural (Length) - 1),
                  Kept_Piece, Kept);
            C.Groups (C.Group_Count).Meaning := Kept_Piece;
            Charge (C, MISSPELLING_CELLS, Natural (Length), MISSPELLING_LITERAL + Natural (Length) + 4);
            Stop := Known;   --  one is enough to say no
         elsif Got.Kind = W.Suggestion and then C.Group_Count > 0 then
            if not Fits (C, 1, Got.Length, Literal_Of (Text_Of (Page, Got))) or else C.Item_Count = MAX_CELLS then
               Full := True;
               Stop := True;
               return;
            end if;
            C.Item_Count := C.Item_Count + 1;
            C.Items (C.Item_Count).Kind := W.Suggestion;
            C.Items (C.Item_Count).Group := C.Group_Count;
            Keep (C, Text_Of (Page, Got), Kept_Piece, Kept);
            C.Items (C.Item_Count).Text := Kept_Piece;
            Charge (C, 1, Got.Length, Literal_Of (Text_Of (Page, Got)));
         end if;
      end Visit;
      procedure Run is new Walk (Visit);
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      Charge (C, 1, 0, LIST_LITERAL);   --  the list's count
      Run (LA.Check, Text_In, Status);
      if Status = No_Answer then
         Status := Walked;   --  nothing to check is nothing misspelt
      end if;
      if Status /= Walked then
         Reply.Why := Walk_Failure (Status, Text_In);
         return;
      elsif Known then
         Reply := (Value => CCL.Host_Values.Boolean_Constant (C.Group_Count = 0), Success => True, Why => <>);
         return;
      elsif Full then
         Reply.Why := Failures.Failed (Failures.Exhausted, "the misspellings do not fit one result",
                                       TOO_LARGE_REMEDY);
         return;
      end if;
      declare
         B : Builder := (Image => Objects.Empty (Contracts.Misspellings), Step => Objects.Added);
      begin
         Put (B, Objects.Sequence_Cell (C.Group_Count));
         for G in 1 .. C.Group_Count loop
            Put (B, Objects.Product_Cell (LX.MISSPELLING_FIELDS));
            Put_Text (B, Text (C, C.Groups (G).Meaning));
            Put_Number (B, Offsets (G));
            Put_List (B, C, G, W.Suggestion);
         end loop;
         if not Finish (B, Contracts.Misspellings, Reply) then
            Reply.Why := Failures.Failed (Failures.Exhausted, "the misspellings do not fit one result",
                                          TOO_LARGE_REMEDY);
         end if;
      end;
   end Check_Of;

   procedure Invoke
     (Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      Op : LX.Operation;
      Object : Views.Snapshot;
      Captured : Boolean := False;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if not Handles (Binding) then return; end if;
      Op := LX.Operation'Val (Binding - LX.FIRST_BINDING);
      if Op in LX.Dict_Page | LX.Thesaurus_Page then
         if Argument.Kind = CCL.Host_Values.Object_Value then
            Views.Capture (Object, Contracts.Page, Argument.Object, Captured);
         end if;
         if Captured then
            declare
               Word : constant String := Views.Text (Object, Views.Field (Object, Views.Root (Object), 1));
               First : constant Integer_64 :=
                 Objects.Integer_Of (Views.Scalar (Object, Views.Field (Object, Views.Root (Object), 2)));
            begin
               if First < 1 or else First > Integer_64 (Integer'Last) then
                  Reply.Why := Failures.Failed
                    (Failures.Invalid_Argument, "a page starts at sense 1 or later",
                     "(Page_Query """ & Word & """ 1) is the first page");
               else
                  Entry_Of ((if Op = LX.Dict_Page then LA.Define else LA.Thesaurus), Word, Positive (First), Reply);
               end if;
            end;
         end if;
         return;
      end if;
      if Argument.Kind /= CCL.Host_Values.Text_Value then
         return;
      end if;
      declare
         Word : constant String := Argument.Content.Data (1 .. Argument.Content.Length);
      begin
         case Op is
            when LX.Dict => Entry_Of (LA.Define, Word, 1, Reply);
            when LX.Thesaurus => Entry_Of (LA.Thesaurus, Word, 1, Reply);
            when LX.Synonym => Synonyms_Of (Word, Reply);
            when LX.Suggest => Suggestions_Of (Word, Reply);
            when LX.Spell_Check => Check_Of (Word, False, Reply);
            when LX.Known => Check_Of (Word, True, Reply);
            when LX.Dict_Page | LX.Thesaurus_Page => null;
         end case;
      end;
   end Invoke;
end CCL_Lexicon_Bindings;
