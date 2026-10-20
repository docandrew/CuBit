with Ada.Unchecked_Deallocation;
with CuBit.UI.Widgets;

package body Dictionary_Views is
   use CuBit.UI;
   use type Controls.Control_ID;
   use type Controls.Pointer_Action;
   use type Lexicon_Wire.Item_Kind;
   package Ed renames CuBit.UI.Editor;
   package W renames Lexicon_Wire;
   package LA renames Lexicon_Answers;

   ---------------------------------------------------------------------------
   --  Geometry (logical pixels).
   ---------------------------------------------------------------------------
   TOOLBAR_HEIGHT : constant := 44;
   STATUS_HEIGHT  : constant := 26;
   CONTROL_HEIGHT : constant := 28;
   NAV_WIDTH      : constant := 34;
   GAP            : constant := 6;
   TAB_WIDTH      : constant := 104;
   SCROLLBAR_WIDTH : constant := 14;
   PLACEHOLDER_INSET : constant := 6;
   MARGIN         : constant := 32;     --  the page's left and right margins
   TOP_PAD        : constant := 22;
   BOTTOM_PAD     : constant := 28;
   NUMBER_WIDTH   : constant := 30;     --  "12." before a sense
   EXAMPLE_INDENT : constant := 14;     --  examples, under their sense
   LABEL_WIDTH    : constant := 84;     --  "Synonyms" before a list's words
   COUNT_GAP      : constant := 12;     --  headword to its sense count
   HEAD_ROW       : constant := 34;
   RULE_INSET     : constant := 8;      --  the rule under a headword, above the next row
   PART_GAP       : constant := 10;
   SECTION_GAP    : constant := 22;
   LIST_GAP       : constant := 2;
   ROW_EXTRA      : constant := 6;
   WHEEL_ROWS     : constant := 3;
   --  The store's first size; it doubles as entries need.
   FIRST_STORE_BYTES : constant := 16 * 1_024;
   FIRST_OPS : constant := 256;
   --  Near spellings asked for with an unknown word.
   SUGGESTIONS : constant := 8;
   --  Words offered on an empty page.
   SAMPLES : constant String := "cataract wharf serendipity accipitrine";

   function Row return Positive is (UI_Text_Height + ROW_EXTRA);

   procedure Free_Store is new Ada.Unchecked_Deallocation (W.Answer_Bytes, Store_Access);
   procedure Free_Ops is new Ada.Unchecked_Deallocation (Op_Array, Ops_Access);

   ---------------------------------------------------------------------------
   --  Text.
   ---------------------------------------------------------------------------
   function Trimmed (Value : Natural) return String is
      Image : constant String := Natural'Image (Value);
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Trimmed;

   function Label_Image (Label : Label_Name; Number : Natural) return String is
     (case Label is
         when No_Label => "",
         when Senses_Label => (if Number = 1 then "1 sense" else Trimmed (Number) & " senses"),
         when Synonyms_Label => "Synonyms",
         when Antonyms_Label => "Antonyms",
         when Related_Label => "Related",
         when Broader_Label => "Broader",
         when Narrower_Label => "Narrower",
         when Also_Label => "See also",
         when Suggestions_Label => "Did you mean",
         when Missing_Label => "No entry for",
         when Welcome_Label => "Type a word to look it up: its senses, examples, synonyms and antonyms.",
         when Welcome_Thesaurus_Label => "Type a word to find words with the same meaning, and the opposite.",
         when Try_Label => "Try",
         when Unavailable_Label => "The lexicon service is not available.",
         when Unavailable_Hint_Label =>
           "lexicon.svc reads Assets/cubit-lexicon/1/english.lexicon; this app needs the lexicon service.",
         when Not_A_Word_Label => "Type one word or phrase.",
         when Separator_Label => ",",
         when Noun_Label => "noun", when Verb_Label => "verb",
         when Adjective_Label => "adjective", when Adverb_Label => "adverb");

   function Store_Text (State : View_State; First, Length : Natural) return String is
      Result : String (1 .. Length);
   begin
      for I in Result'Range loop
         Result (I) := Character'Val (State.Store (First + I - 1));
      end loop;
      return Result;
   end Store_Text;

   function Op_Text (State : View_State; Item : Op) return String is
     (if Item.Label /= No_Label then Label_Image (Item.Label, Item.Number)
      elsif Item.Style = Number_Style then Trimmed (Item.Number) & "."
      else Store_Text (State, Item.First, Item.Length));

   function Emphasis (Style : Op_Style) return Boolean is
     (Style in Headword_Style | Part_Style | Missing_Style);
   function Measure (Style : Op_Style; Text : String) return Natural is
     (if Emphasis (Style) then Emphasis_Text_Width (Text) else UI_Text_Width (Text));

   ---------------------------------------------------------------------------
   --  The store and the layout.
   ---------------------------------------------------------------------------
   procedure Append_Bytes (State : in out View_State; Bytes : W.Answer_Bytes) is
   begin
      if State.Store = null then
         State.Store := new W.Answer_Bytes (0 .. FIRST_STORE_BYTES - 1);
      end if;
      if State.Store'Length - State.Store_Used < Bytes'Length then
         declare
            Size : Natural := State.Store'Length;
            Larger : Store_Access;
         begin
            while Size - State.Store_Used < Bytes'Length loop
               Size := 2 * Size;
            end loop;
            Larger := new W.Answer_Bytes (0 .. Size - 1);
            Larger (0 .. State.Store_Used - 1) := State.Store (0 .. State.Store_Used - 1);
            Free_Store (State.Store);
            State.Store := Larger;
         end;
      end if;
      State.Store (State.Store_Used .. State.Store_Used + Bytes'Length - 1) := Bytes;
      State.Store_Used := State.Store_Used + Bytes'Length;
   end Append_Bytes;

   procedure Add_Op (State : in out View_State; Item : Op) is
   begin
      if State.Ops = null then
         State.Ops := new Op_Array (1 .. FIRST_OPS);
      elsif State.Op_Count = State.Ops'Length then
         declare
            Larger : constant Ops_Access := new Op_Array (1 .. 2 * State.Ops'Length);
         begin
            Larger (1 .. State.Op_Count) := State.Ops (1 .. State.Op_Count);
            Free_Ops (State.Ops);
            State.Ops := Larger;
         end;
      end if;
      State.Op_Count := State.Op_Count + 1;
      State.Ops (State.Op_Count) := Item;
   end Add_Op;

   --  Every item of every page of Op on Word into the store.
   procedure Fetch (State : in out View_State; Operation : LA.Operation; Word : String) is
      Page : W.Answer_Bytes (0 .. PAGE_BYTES - 1);
      Used, Position, Start : Natural;
      Resume : Unsigned_32 := 0;
      Available, Present, Damaged, More : Boolean;
      Got : W.Item;
      Heads, Queries : Natural := 0;
   begin
      State.Store_Used := 0;
      State.Sense_Total := 0;
      loop
         Ask (Operation, Word, Resume, SUGGESTIONS, Page, Used, Available);
         if not Available then
            State.Status := Unavailable;
            State.Store_Used := 0;
            return;
         end if;
         More := False;
         Position := 0;
         loop
            Start := Position;
            W.Next (Page (0 .. Used - 1), Position, Present, Damaged, Got);
            exit when not Present;
            if Got.Kind = W.Continue_At then
               if W.Number (Page (0 .. Used - 1), Got) > Resume then
                  Resume := W.Number (Page (0 .. Used - 1), Got);
                  More := True;
               end if;
            else
               Append_Bytes (State, Page (Start .. Position - 1));
               if Got.Kind = W.Headword then Heads := Heads + 1; end if;
               if Got.Kind = W.Query then Queries := Queries + 1; end if;
               if Got.Kind = W.Sense_Start then State.Sense_Total := State.Sense_Total + 1; end if;
            end if;
         end loop;
         exit when not More or else Damaged;
      end loop;
      State.Status := (if Heads > 0 then Found elsif Queries > 0 then Not_Found else Not_A_Word);
   end Fetch;

   procedure Set_Asked (State : in out View_State; Word : String) is
      N : constant Natural := Natural'Min (Word'Length, Ed.MAX_TEXT_LENGTH);
   begin
      State.Asked.Length := N;
      State.Asked.Text (1 .. N) := Word (Word'First .. Word'First + N - 1);
   end Set_Asked;

   --  Show the entry for Word in the current mode (no history).
   procedure Show (State : in out View_State; Word : String) is
      Trimmed_Word : Natural := 0;
   begin
      for C of Word loop
         if C /= ' ' then Trimmed_Word := Trimmed_Word + 1; end if;
      end loop;
      Set_Asked (State, Word);
      State.Asked_Mode := State.Current;
      if Trimmed_Word = 0 then
         State.Status := Idle;
         State.Store_Used := 0;
         --  The sample words, as near-spelling items the layout links.
         declare
            From : Positive := SAMPLES'First;
         begin
            for I in SAMPLES'First .. SAMPLES'Last + 1 loop
               if I > SAMPLES'Last or else SAMPLES (I) = ' ' then
                  declare
                     Sample : constant String := SAMPLES (From .. I - 1);
                     Item : W.Answer_Bytes (0 .. W.ITEM_HEADER_BYTES + Sample'Length - 1);
                  begin
                     Item (0) := W.Item_Kind'Pos (W.Suggestion);
                     Item (1) := Unsigned_8 (Sample'Length mod 256);
                     Item (2) := Unsigned_8 (Sample'Length / 256);
                     for K in Sample'Range loop
                        Item (W.ITEM_HEADER_BYTES + K - Sample'First) := Character'Pos (Sample (K));
                     end loop;
                     Append_Bytes (State, Item);
                  end;
                  From := I + 1;
               end if;
            end loop;
         end;
      else
         Fetch (State, (if State.Current = Thesaurus_Mode then LA.Thesaurus else LA.Define), Word);
      end if;
      State.Laid_Out_Width := 0;
      State.Scroll := 0;
   end Show;

   procedure Set_Field (State : in out View_State; Word : String) is
      Accepted : Boolean;
   begin
      Ed.Initialize (State.Search, Word (Word'First .. Word'First + Natural'Min (Word'Length, Ed.MAX_TEXT_LENGTH) - 1),
                     Accepted);
      Ed.Move (State.Search, Ed.Move_End);
   end Set_Field;

   --  Record the shown word for Back, after the entry now shown.
   procedure Remember (State : in out View_State) is
      Word : constant String := Ed.Content (State.Search);
   begin
      if Word'Length = 0 then return; end if;
      if State.History_At > 0 and then
        State.History (State.History_At).Text (1 .. State.History (State.History_At).Length) = Word
      then
         return;
      end if;
      State.History_Used := State.History_At;   --  forward entries are dropped
      if State.History_Used = HISTORY_DEPTH then
         State.History (1 .. HISTORY_DEPTH - 1) := State.History (2 .. HISTORY_DEPTH);
         State.History_Used := HISTORY_DEPTH - 1;
      end if;
      State.History_Used := State.History_Used + 1;
      State.History (State.History_Used).Length := Natural'Min (Word'Length, Ed.MAX_TEXT_LENGTH);
      State.History (State.History_Used).Text (1 .. State.History (State.History_Used).Length) :=
        Word (Word'First .. Word'First + State.History (State.History_Used).Length - 1);
      State.History_At := State.History_Used;
   end Remember;

   procedure Go (State : in out View_State; To : History_Count) is
   begin
      if To in 1 .. State.History_Used then
         State.History_At := To;
         Set_Field (State, State.History (To).Text (1 .. State.History (To).Length));
         Show (State, Ed.Content (State.Search));
      end if;
   end Go;

   procedure Look_Up (State : in out View_State; Word : String) is
   begin
      Set_Field (State, Word);
      Show (State, Ed.Content (State.Search));
      Remember (State);
   end Look_Up;

   procedure Set_Mode (State : in out View_State; Value : Mode) is
   begin
      if Value /= State.Current then
         State.Current := Value;
         Show (State, Ed.Content (State.Search));
      end if;
   end Set_Mode;

   procedure Initialize (State : out View_State; Start : Mode) is
      Accepted : Boolean;
   begin
      State.Current := Start;
      Ed.Initialize (State.Search, "", Accepted);
      Show (State, "");
   end Initialize;

   ---------------------------------------------------------------------------
   --  Layout: the entry as operations, for the page width Width.
   ---------------------------------------------------------------------------
   procedure Lay_Out (State : in out View_State; Width : Positive) is
      Right : constant Natural := (if Width > 2 * MARGIN + 160 then Width - MARGIN else MARGIN + 160);
      Y : Natural := TOP_PAD;
      Heads : Natural := 0;
      Number : Natural := 0;
      Last_Part : Natural := Natural'Last;   --  none yet
      Sense_Open : Boolean := False;
      Sense_First : Natural := 0;            --  store position after its Sense_Start
      Head_Y : Natural := 0;
      Head_Right : Natural := 0;
      Query_First, Query_Length : Natural := 0;
      Has_Query : Boolean := False;

      --  Store text at First (Length bytes) from X0, wrapped at Right.
      procedure Flow (Style : Op_Style; First, Length : Natural; X0 : Natural; Quoted : Boolean := False) is
         T : constant String := Store_Text (State, First, Length);
         Quote : constant Natural := (if Quoted then UI_Text_Width ("""") else 0);
         Room : constant Natural := (if Right > X0 + 2 * Quote + 40 then Right - X0 - 2 * Quote else 40);
         Row_Start : Positive := 1;
         Last_Fit, Space : Natural;
         I : Positive;
      begin
         if Length = 0 then
            Y := Y + Row;
            return;
         end if;
         while Row_Start <= T'Last loop
            Last_Fit := 0;
            I := Row_Start;
            loop
               Space := I;
               while Space <= T'Last and then T (Space) /= ' ' loop Space := Space + 1; end loop;
               exit when Last_Fit > 0 and then Measure (Style, T (Row_Start .. Space - 1)) > Room;
               Last_Fit := Space - 1;
               exit when Space > T'Last;
               I := Space + 1;
            end loop;
            Add_Op (State, (Kind => Text_Op, Style => Style, X => X0 + Quote, Y => Y,
                            W => Measure (Style, T (Row_Start .. Last_Fit)),
                            First => First + Row_Start - 1, Length => Last_Fit - Row_Start + 1,
                            Open_Quote => Quoted and then Row_Start = 1,
                            Close_Quote => Quoted and then Last_Fit = T'Last, others => <>));
            Y := Y + Row;
            Row_Start := Last_Fit + 2;
         end loop;
      end Flow;

      --  The items of Kind between From and Upto as a labelled list of links.
      procedure Links_Of (Kind : W.Item_Kind; Label : Label_Name; From, Upto : Natural; X0 : Natural) is
         Position : Natural := From;
         Present, Damaged : Boolean;
         Got : W.Item;
         Count : Natural := 0;
         List_X : constant Natural := X0 + LABEL_WIDTH;
         X : Natural := List_X;
         Separator : constant Natural := UI_Text_Width (", ");
         Previous : Boolean := False;
      begin
         loop
            W.Next (State.Store (0 .. Upto - 1), Position, Present, Damaged, Got);
            exit when not Present;
            if Got.Kind = Kind then Count := Count + 1; end if;
         end loop;
         if Count = 0 then return; end if;
         Add_Op (State, (Kind => Text_Op, Style => Label_Style, X => X0, Y => Y,
                         W => UI_Text_Width (Label_Image (Label, 0)), Label => Label, others => <>));
         Position := From;
         loop
            W.Next (State.Store (0 .. Upto - 1), Position, Present, Damaged, Got);
            exit when not Present;
            if Got.Kind = Kind then
               declare
                  Word_W : constant Natural := UI_Text_Width (Store_Text (State, Got.First, Got.Length));
               begin
                  if Previous then
                     Add_Op (State, (Kind => Text_Op, Style => Label_Style, X => X, Y => Y, W => Separator,
                                     Label => Separator_Label, others => <>));
                     X := X + Separator;
                  end if;
                  if X > List_X and then X + Word_W > Right then
                     Y := Y + Row;
                     X := List_X;
                  end if;
                  Add_Op (State, (Kind => Link_Op, Style => Link_Style, X => X, Y => Y, W => Word_W,
                                  First => Got.First, Length => Got.Length, others => <>));
                  X := X + Word_W;
                  Previous := True;
               end;
            end if;
         end loop;
         Y := Y + Row + LIST_GAP;
      end Links_Of;

      --  The word lists of the sense from Sense_First up to Upto.
      procedure Close_Sense (Upto : Natural) is
         X0 : constant Natural := MARGIN + NUMBER_WIDTH;
      begin
         if not Sense_Open then return; end if;
         Sense_Open := False;
         Links_Of (W.Synonym, Synonyms_Label, Sense_First, Upto, X0);
         Links_Of (W.Antonym, Antonyms_Label, Sense_First, Upto, X0);
         if State.Asked_Mode = Thesaurus_Mode then
            Links_Of (W.Similar, Related_Label, Sense_First, Upto, X0);
            Links_Of (W.Broader, Broader_Label, Sense_First, Upto, X0);
            Links_Of (W.Narrower, Narrower_Label, Sense_First, Upto, X0);
         end if;
         Y := Y + PART_GAP / 2;
      end Close_Sense;

      function Part_Label (Part : Natural) return Label_Name is
        (case Part is when 0 => Noun_Label, when 1 => Verb_Label, when 2 => Adjective_Label,
                      when others => Adverb_Label);

      Position, Start : Natural := 0;
      Present, Damaged : Boolean;
      Got : W.Item;
   begin
      State.Op_Count := 0;
      case State.Status is
         when Unavailable =>
            Add_Op (State, (Style => Missing_Style, X => MARGIN, Y => Y, Label => Unavailable_Label, others => <>));
            Y := Y + HEAD_ROW;
            Add_Op (State, (Style => Muted_Body_Style, X => MARGIN, Y => Y, Label => Unavailable_Hint_Label,
                            others => <>));
            Y := Y + Row;
         when Not_A_Word =>
            Add_Op (State, (Style => Muted_Body_Style, X => MARGIN, Y => Y, Label => Not_A_Word_Label, others => <>));
            Y := Y + Row;
         when Idle =>
            Add_Op (State, (Style => Muted_Body_Style, X => MARGIN, Y => Y,
                            Label => (if State.Current = Thesaurus_Mode then Welcome_Thesaurus_Label
                                      else Welcome_Label), others => <>));
            Y := Y + Row + PART_GAP;
            Links_Of (W.Suggestion, Try_Label, 0, State.Store_Used, MARGIN);
         when Found | Not_Found =>
            loop
               Start := Position;
               W.Next (State.Store (0 .. State.Store_Used - 1), Position, Present, Damaged, Got);
               exit when not Present;
               case Got.Kind is
                  when W.Query =>
                     Query_First := Got.First;
                     Query_Length := Got.Length;
                     Has_Query := True;
                  when W.Headword =>
                     Close_Sense (Start);
                     if Heads > 0 then Y := Y + SECTION_GAP; end if;
                     Heads := Heads + 1;
                     Head_Y := Y;
                     Head_Right := MARGIN + Emphasis_Text_Width (Store_Text (State, Got.First, Got.Length));
                     Add_Op (State, (Kind => Text_Op, Style => Headword_Style, X => MARGIN, Y => Y,
                                     W => Head_Right - MARGIN, First => Got.First, Length => Got.Length,
                                     others => <>));
                     Add_Op (State, (Kind => Rule_Op, X => MARGIN, Y => Y + HEAD_ROW - RULE_INSET,
                                     W => Right - MARGIN, others => <>));
                     Y := Y + HEAD_ROW;
                     Number := 0;
                     Last_Part := Natural'Last;
                  when W.Sense_Total =>
                     Add_Op (State, (Style => Count_Style, X => Head_Right + COUNT_GAP, Y => Head_Y,
                                     Label => Senses_Label,
                                     Number => Natural (W.Number (State.Store (0 .. State.Store_Used - 1), Got)),
                                     others => <>));
                  when W.Sense_Start =>
                     Close_Sense (Start);
                     declare
                        Part : constant Natural :=
                          (if Got.Length = 1 then Natural (State.Store (Got.First)) else 0);
                     begin
                        if Part /= Last_Part then
                           if Last_Part /= Natural'Last then Y := Y + PART_GAP; end if;
                           Add_Op (State, (Style => Part_Style, X => MARGIN, Y => Y, Label => Part_Label (Part),
                                           others => <>));
                           Y := Y + Row;
                           Last_Part := Part;
                        end if;
                     end;
                     Number := Number + 1;
                     Sense_Open := True;
                     Sense_First := Position;
                  when W.Definition =>
                     Add_Op (State, (Style => Number_Style, X => MARGIN, Y => Y, Number => Number, others => <>));
                     Flow ((if State.Asked_Mode = Thesaurus_Mode then Muted_Body_Style else Body_Style),
                           Got.First, Got.Length, MARGIN + NUMBER_WIDTH);
                  when W.Example =>
                     if State.Asked_Mode = Dictionary_Mode then
                        Flow (Example_Style, Got.First, Got.Length, MARGIN + NUMBER_WIDTH + EXAMPLE_INDENT,
                              Quoted => True);
                     end if;
                  when others =>
                     null;
               end case;
            end loop;
            Close_Sense (State.Store_Used);
            if Heads = 0 and then Has_Query then
               Add_Op (State, (Style => Muted_Body_Style, X => MARGIN, Y => Y + 6, Label => Missing_Label,
                               others => <>));
               Add_Op (State, (Style => Missing_Style, X => MARGIN + UI_Text_Width ("No entry for "), Y => Y + 6,
                               First => Query_First, Length => Query_Length, others => <>));
               Y := Y + HEAD_ROW + PART_GAP;
               Links_Of (W.Suggestion, Suggestions_Label, 0, State.Store_Used, MARGIN);
            end if;
      end case;
      State.Content_Height := Y + BOTTOM_PAD;
      State.Laid_Out_Width := Width;
   end Lay_Out;

   ---------------------------------------------------------------------------
   --  Rendering.
   ---------------------------------------------------------------------------
   function Max_Scroll (State : View_State) return Natural is
     (if State.Content_Height > State.Content.h then State.Content_Height - State.Content.h else 0);

   function Over_Link (State : View_State; X, Y : Natural) return Natural is
   begin
      if not Point_In_Rect (X, Y, State.Content) then return 0; end if;
      for I in 1 .. State.Op_Count loop
         declare
            O : Op renames State.Ops (I);
            Top : constant Integer := State.Content.y + O.Y - State.Scroll;
         begin
            if O.Kind = Link_Op and then X >= State.Content.x + O.X and then X < State.Content.x + O.X + O.W
              and then Y >= Top and then Y < Top + Row
            then
               return I;
            end if;
         end;
      end loop;
      return 0;
   end Over_Link;

   procedure Render
     (State : in out View_State; C : CuBit.UI.Canvas; Bounds : CuBit.UI.Rect;
      UI : in out CuBit.UI.State.UI_State; Map : in out Controls.Control_Map)
   is
      Colors : constant Theme := Current_Theme;
      Toolbar : constant Rect := (Bounds.x, Bounds.y, Bounds.w, TOOLBAR_HEIGHT);
      Control_Y : constant Natural := Toolbar.y + (TOOLBAR_HEIGHT - CONTROL_HEIGHT) / 2;
      Tabs_X : constant Natural :=
        (if Bounds.w > 2 * TAB_WIDTH + 2 * GAP then Bounds.x + Bounds.w - 2 * TAB_WIDTH - 2 * GAP else 0);
      Field_X : constant Natural := Bounds.x + GAP + 2 * (NAV_WIDTH + GAP) + GAP;
      Field : constant Rect :=
        (Field_X, Control_Y, (if Tabs_X > Field_X + 3 * GAP then Tabs_X - Field_X - 3 * GAP else 1), CONTROL_HEIGHT);
      Status_Bar : constant Rect :=
        (Bounds.x, Bounds.y + (if Bounds.h > STATUS_HEIGHT then Bounds.h - STATUS_HEIGHT else 0), Bounds.w, STATUS_HEIGHT);
      Page : constant Rect :=
        (Bounds.x, Bounds.y + TOOLBAR_HEIGHT,
         (if Bounds.w > SCROLLBAR_WIDTH then Bounds.w - SCROLLBAR_WIDTH else 1),
         (if Bounds.h > TOOLBAR_HEIGHT + STATUS_HEIGHT then Bounds.h - TOOLBAR_HEIGHT - STATUS_HEIGHT else 1));
      Result : Widget_Result;
      Hovered : constant Natural := Over_Link (State, State.Hover_X, State.Hover_Y);
      Tab_Content : Canvas;
      Tab_Colors : Theme;
      Text : constant String := Ed.Content (State.Search);
   begin
      CuBit.UI.State.Begin_Frame (UI);
      Controls.Clear (Map);
      State.Content := Page;
      if State.Laid_Out_Width /= Page.w then
         Lay_Out (State, Page.w);
      end if;
      State.Scroll := Natural'Min (State.Scroll, Max_Scroll (State));

      --  The page.
      Fill_Rect (C, Page, Colors.field);
      declare
         PC : constant Canvas := With_Clip (C, Page);
      begin
         for I in 1 .. State.Op_Count loop
            declare
               O : Op renames State.Ops (I);
               Top : constant Integer := Page.y + O.Y - State.Scroll;
               X : constant Natural := Page.x + O.X;
            begin
               if O.Kind = Rule_Op then
                  if Top >= Page.y and then Top < Page.y + Page.h then
                     Fill_Rect (PC, (X, Natural (Top), O.W, 1), Colors.edge);
                  end if;
               elsif Top + Row > Page.y and then Top < Page.y + Page.h then
                  declare
                     T : constant String := Op_Text (State, O);
                     Y : constant Natural := Natural (Integer'Max (0, Top));
                     Hue : constant Color :=
                       (case O.Style is
                           when Headword_Style | Body_Style | Missing_Style => Colors.text,
                           when Part_Style | Link_Style => Colors.accent,
                           when Count_Style | Number_Style | Muted_Body_Style | Example_Style | Label_Style =>
                             Colors.muted);
                  begin
                     if Emphasis (O.Style) then
                        Draw_Emphasis_Text_Transparent (PC, X, Y, T, Hue);
                     else
                        if O.Open_Quote then
                           Draw_UI_Text_Transparent (PC, X - UI_Text_Width (""""), Y, """", Hue);
                        end if;
                        Draw_UI_Text_Transparent (PC, X, Y, T, Hue);
                        if O.Close_Quote then
                           Draw_UI_Text_Transparent (PC, X + O.W, Y, """", Hue);
                        end if;
                     end if;
                     if O.Kind = Link_Op and then I = Hovered then
                        Fill_Rect (PC, (X, Y + UI_Text_Height, O.W, 1), Colors.accent);
                     end if;
                  end;
               end if;
            end;
         end loop;
      end;
      Fill_Rect (C, (Page.x + Page.w, Page.y, SCROLLBAR_WIDTH, Page.h), Colors.field);
      CuBit.UI.Widgets.Vertical_Scrollbar
        (C, UI, Map, SCROLL_ID, (Page.x + Page.w, Page.y, SCROLLBAR_WIDTH, Page.h), Page, Colors,
         0, Max_Scroll (State), State.Scroll, Result, Positive'Max (1, Page.h), retainedInput => True);

      --  The toolbar: back, forward, the search field, the two modes.
      CuBit.UI.Widgets.Toolbar (C, Toolbar, Colors);
      CuBit.UI.Widgets.Navigation_Button
        (C, UI, Map, BACK_ID, (Bounds.x + GAP, Control_Y, NAV_WIDTH, CONTROL_HEIGHT), Toolbar, Colors,
         CuBit.UI.Widgets.Navigate_Back, "Back", State.History_At > 1, Result);
      CuBit.UI.Widgets.Navigation_Button
        (C, UI, Map, FORWARD_ID, (Bounds.x + 2 * GAP + NAV_WIDTH, Control_Y, NAV_WIDTH, CONTROL_HEIGHT), Toolbar,
         Colors, CuBit.UI.Widgets.Navigate_Forward, "Forward", State.History_At < State.History_Used, Result);
      Controls.Add (Map, SEARCH_ID, Field, Toolbar, Pointer_Text);
      Draw_Text_Edit_Field
        (C, Field, Colors, Text, Ed.Cursor (State.Search) - 1,
         Ed.Selection_First (State.Search) - 1, Ed.Selection_Last (State.Search) - 1, True, False);
      if Text'Length = 0 then
         Draw_UI_Text_Transparent
           (With_Clip (C, Field), Field.x + PLACEHOLDER_INSET, Field.y + (CONTROL_HEIGHT - UI_Text_Height) / 2,
            Fit_UI_Text ((if State.Current = Thesaurus_Mode then "Find synonyms" else "Look up a word"),
                         (if Field.w > 2 * PLACEHOLDER_INSET then Field.w - 2 * PLACEHOLDER_INSET else 0)),
            Colors.muted);
      end if;
      for M in Mode loop
         declare
            Tab_Box : constant Rect :=
              (Tabs_X + Mode'Pos (M) * (TAB_WIDTH + GAP), Control_Y, TAB_WIDTH, CONTROL_HEIGHT);
         begin
            CuBit.UI.Widgets.Tab
              (C, UI, Map, (if M = Dictionary_Mode then DICTIONARY_ID else THESAURUS_ID), Tab_Box, Toolbar, Colors,
               State.Current = M, Tab_Content, Tab_Colors, Result);
            Draw_UI_Text_Transparent
              (Tab_Content, Tab_Box.x + (TAB_WIDTH - UI_Text_Width (Title (M))) / 2,
               Tab_Box.y + (CONTROL_HEIGHT - UI_Text_Height) / 2, Title (M), Tab_Colors.text);
         end;
      end loop;

      --  The status bar, with the data's attribution.
      Draw_Status_Bar
        (C, Status_Bar, Colors,
         (case State.Status is
             when Idle => Title (State.Current),
             when Found => Label_Image (Senses_Label, State.Sense_Total),
             when Not_Found => "Not in the lexicon",
             when Unavailable => "Lexicon unavailable",
             when Not_A_Word => "Not a word"),
         "WordNet 3.0, Princeton University; SCOWL");
      CuBit.UI.State.Finish_Frame (UI);
   end Render;

   ---------------------------------------------------------------------------
   --  Events.
   ---------------------------------------------------------------------------
   procedure Handle
     (State : in out View_State; Item : Event; Map : in out Controls.Control_Map; Redraw : out Boolean)
   is
      Changed : Boolean := False;
      procedure Edited is
      begin
         Show (State, Ed.Content (State.Search));
      end Edited;
      procedure Scroll_By (Delta_Pixels : Integer) is
      begin
         State.Scroll := Natural (Integer'Max (0, Integer'Min (Integer (Max_Scroll (State)),
                                                              Integer (State.Scroll) + Delta_Pixels)));
      end Scroll_By;
   begin
      Redraw := True;
      case Item.Kind is
         when Resize =>
            State.Width := Item.X;
            State.Height := Item.Y;
            State.Laid_Out_Width := 0;
         when Text_Event =>
            if Item.Character_Value in ' ' .. '~' then
               Ed.Insert (State.Search, [1 => Item.Character_Value], Changed);
               if Changed then Edited; end if;
            end if;
         when Key_Event =>
            case Item.Key is
               when Enter => Remember (State);
               when Escape =>
                  Ed.Select_All (State.Search);
                  Ed.Backspace (State.Search, Changed);
                  if Changed then Edited; end if;
               when Backspace =>
                  Ed.Backspace (State.Search, Changed);
                  if Changed then Edited; end if;
               when Delete =>
                  Ed.Delete_Forward (State.Search, Changed);
                  if Changed then Edited; end if;
               when Left =>
                  if Item.Alt then
                     if State.History_At > 1 then Go (State, State.History_At - 1); end if;
                  else Ed.Move (State.Search, (if Item.Control then Ed.Move_Word_Left else Ed.Move_Left), Item.Shift);
                  end if;
               when Right =>
                  if Item.Alt then
                     if State.History_At < State.History_Used then Go (State, State.History_At + 1); end if;
                  else Ed.Move (State.Search, (if Item.Control then Ed.Move_Word_Right else Ed.Move_Right), Item.Shift);
                  end if;
               when Home =>
                  if Item.Control then State.Scroll := 0; else Ed.Move (State.Search, Ed.Move_Start, Item.Shift); end if;
               when End_Key =>
                  if Item.Control then State.Scroll := Max_Scroll (State);
                  else Ed.Move (State.Search, Ed.Move_End, Item.Shift);
                  end if;
               when Up => Scroll_By (-Row);
               when Down => Scroll_By (Row);
               when Page_Up => Scroll_By (-Integer (Natural'Max (Row, State.Content.h - Row)));
               when Page_Down => Scroll_By (Natural'Max (Row, State.Content.h - Row));
               when Tab =>
                  Set_Mode (State, (if State.Current = Dictionary_Mode then Thesaurus_Mode else Dictionary_Mode));
               when No_Key => Redraw := False;
            end case;
         when Wheel =>
            Scroll_By (-Item.Steps * WHEEL_ROWS * Row);
         when Pointer_Event =>
            declare
               Target : constant Controls.Control_ID := Controls.Hit (Map, Item.X, Item.Y);
               Link : constant Natural := Over_Link (State, Item.X, Item.Y);
               Was : constant Natural := Over_Link (State, State.Hover_X, State.Hover_Y);
            begin
               State.Hover_X := Item.X;
               State.Hover_Y := Item.Y;
               case Item.Action is
                  when Controls.Pointer_Press =>
                     State.Pressed_Link := Link;
                  when Controls.Pointer_Release =>
                     if Target = BACK_ID and then Controls.Take_Activated (Map, Target) then
                        if State.History_At > 1 then Go (State, State.History_At - 1); end if;
                     elsif Target = FORWARD_ID and then Controls.Take_Activated (Map, Target) then
                        if State.History_At < State.History_Used then Go (State, State.History_At + 1); end if;
                     elsif Target = DICTIONARY_ID and then Controls.Take_Activated (Map, Target) then
                        Set_Mode (State, Dictionary_Mode);
                     elsif Target = THESAURUS_ID and then Controls.Take_Activated (Map, Target) then
                        Set_Mode (State, Thesaurus_Mode);
                     elsif Link > 0 and then Link = State.Pressed_Link then
                        Remember (State);
                        Look_Up (State, Op_Text (State, State.Ops (Link)));
                     end if;
                     State.Pressed_Link := 0;
                  when Controls.Pointer_Move =>
                     Redraw := Link /= Was or else Target = SCROLL_ID;
                  when Controls.Pointer_Cancel =>
                     State.Pressed_Link := 0;
               end case;
            end;
      end case;
   end Handle;

   ---------------------------------------------------------------------------
   --  Queries.
   ---------------------------------------------------------------------------
   function Status (State : View_State) return Lookup_Status is (State.Status);
   function Query (State : View_State) return String is (Ed.Content (State.Search));
   function Current_Mode (State : View_State) return Mode is (State.Current);
   function Headword (State : View_State) return String is
   begin
      for I in 1 .. State.Op_Count loop
         if State.Ops (I).Style = Headword_Style and then State.Ops (I).Label = No_Label then
            return Op_Text (State, State.Ops (I));
         end if;
      end loop;
      return "";
   end Headword;
   function Senses (State : View_State) return Natural is (State.Sense_Total);
   function Links (State : View_State) return Natural is
      N : Natural := 0;
   begin
      for I in 1 .. State.Op_Count loop
         if State.Ops (I).Kind = Link_Op then N := N + 1; end if;
      end loop;
      return N;
   end Links;
   function Nth_Link (State : View_State; Index : Positive) return Natural is
      N : Natural := 0;
   begin
      for I in 1 .. State.Op_Count loop
         if State.Ops (I).Kind = Link_Op then
            N := N + 1;
            if N = Index then return I; end if;
         end if;
      end loop;
      return 0;
   end Nth_Link;
   function Link_Text (State : View_State; Index : Positive) return String is
     (if Nth_Link (State, Index) = 0 then "" else Op_Text (State, State.Ops (Nth_Link (State, Index))));
   function Link_Area (State : View_State; Index : Positive) return CuBit.UI.Rect is
      I : constant Natural := Nth_Link (State, Index);
   begin
      if I = 0 then return (others => 0); end if;
      declare
         O : Op renames State.Ops (I);
         Top : constant Integer := State.Content.y + O.Y - State.Scroll;
      begin
         if Top < State.Content.y or else Top + Row > State.Content.y + State.Content.h then
            return (others => 0);
         end if;
         return (State.Content.x + O.X, Natural (Top), O.W, Row);
      end;
   end Link_Area;
   function Can_Go_Back (State : View_State) return Boolean is (State.History_At > 1);
end Dictionary_Views;
