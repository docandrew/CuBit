with Ada.Unchecked_Deallocation;
with Interfaces;
with CuBit.UI_Metrics;
with CuBit.UI.Trees;
with CuBit.UI.Widgets;
with CuBit.Reading_Faces;
with Bible_Parser;
with Bible_Text;

package body Bible_Views is
   use CuBit.UI;
   use type Controls.Control_ID;
   use type Controls.Pointer_Action;
   use type Bible_Translations.Load_Status;
   use type Bible_Translations.Image_Access;
   use type Bible_Images.Segment_Kind;
   use type Bible_Parser.Status;
   use type Bible_Search.Normalize_Status;
   use type Bible_Search.Search_Status;
   use type Bible_Translations.Translation;
   package Ed renames CuBit.UI.Editor;
   package Images renames Bible_Images;
   package M renames CuBit.UI_Metrics;

   ---------------------------------------------------------------------------
   --  Geometry (logical pixels).
   ---------------------------------------------------------------------------
   CONTROL_HEIGHT  : constant := M.Control_Height;
   TOOLBAR_HEIGHT  : constant := CONTROL_HEIGHT + 2 * M.Space_M;
   STATUS_HEIGHT   : constant := M.Compact_Row_Height;
   NAV_WIDTH       : constant := CONTROL_HEIGHT + M.Space_S;
   GAP             : constant := 6;
   TAB_WIDTH       : constant := 3 * M.Base_Em + M.Space_M;
   CHAPTER_BUTTON_WIDTH : constant := 5 * M.Base_Em;
   FACE_BUTTON_WIDTH : constant := 9 * M.Base_Em;
   RED_WIDTH : constant := 7 * M.Base_Em;          --  its checkbox and label
   CHECKBOX_EDGE : constant := 16;
   SCROLLBAR_WIDTH : constant := 14;
   PLACEHOLDER_INSET : constant := 6;
   --  The sidebar: its default width and the narrowest each part may get.
   SIDEBAR_WIDTH   : constant := 15 * M.Base_Em;
   SIDEBAR_MINIMUM : constant := 10 * M.Base_Em;
   PAGE_MINIMUM    : constant := 20 * M.Base_Em;
   SPLITTER        : constant := 6;
   --  The reading column: at most READING_MEASURE wide (about 66 characters of
   --  the reading face, a book's measure), centred, with at least
   --  SIDE_MARGIN either side.
   READING_MEASURE         : constant := 34 * M.Base_Em;
   SIDE_MARGIN     : constant := 2 * M.Base_Em;
   TOP_PAD         : constant := 2 * M.Base_Em;
   BOTTOM_PAD      : constant := 3 * M.Base_Em;
   --  A reading line: the base em times 1.65.
   READING_LINE    : constant := 26;
   PARAGRAPH_INDENT : constant := 3 * M.Base_Em / 2;
   POETRY_INDENT   : constant := 2 * M.Base_Em;
   POETRY_HANG     : constant := M.Base_Em;
   STANZA_GAP      : constant := READING_LINE / 2;
   SECTION_GAP     : constant := READING_LINE;
   HEADING_GAP     : constant := 3 * M.Base_Em / 2;
   CAPTION_GAP     : constant := 4;
   --  A verse number: raised this far above the body's baseline, and this
   --  far from its first word.
   SUPERSCRIPT_RAISE : constant := 6;
   NUMBER_GAP      : constant := 3;
   --  Extra room before a verse number inside a line (a face's word space
   --  alone runs it into the verse before).
   NUMBER_LEAD     : constant := 2;
   TINT_PAD        : constant := 1;
   WHEEL_LINES     : constant := 3;
   --  Results: a reference and its verse, at most SNIPPET_LINES lines.
   RESULT_GAP      : constant := 10;
   SNIPPET_LINES   : constant := 2;
   --  Results laid out (the count says how many more there are).
   MAXIMUM_SHOWN   : constant := 200;
   --  The first sizes of the growable buffers; they double as needed.
   FIRST_OPS       : constant := 512;
   FIRST_STORE     : constant := 16 * 1_024;
   FIRST_NODES     : constant := 256;
   --  Searching starts at this many characters (one letter would match
   --  nearly every verse).
   MINIMUM_SEARCH  : constant := 2;
   ELLIPSIS        : constant String := [1 => Bible_Text.ELLIPSIS];
   MIDDLE_DOT      : constant Character := Character'Val (16#B7#);   --  in the reading face

   procedure Free_Ops is new Ada.Unchecked_Deallocation (Op_Array, Ops_Access);
   procedure Free_Store is new Ada.Unchecked_Deallocation (String, Store_Access);
   procedure Free_Nodes is new Ada.Unchecked_Deallocation (Node_Array, Nodes_Access);

   function Trimmed (Value : Natural) return String is
      Image : constant String := Natural'Image (Value);
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Trimmed;

   ---------------------------------------------------------------------------
   --  Translations.
   ---------------------------------------------------------------------------
   function Edition (State : View_State) return Bible_Translations.Edition is (State.Editions (State.Which));
   function Ready (State : View_State) return Boolean is
     (State.Editions (State.Which).Status = Bible_Translations.Loaded);
   function Table (State : View_State) return Versification is (State.Editions (State.Which).Index.Table);

   procedure Ensure (State : in out View_State; Which : Translation) is
      E : Bible_Translations.Edition renames State.Editions (Which);
   begin
      if E.Status = Bible_Translations.Not_Loaded then
         Load (Which, E.Data, E.Status);
         if E.Status = Bible_Translations.Loaded then
            Bible_Translations.Open (E);
         elsif E.Status = Bible_Translations.Not_Loaded then
            E.Status := Bible_Translations.Read_Failed;
         end if;
      end if;
   end Ensure;

   --  Bytes of the current image as text.
   function Text_Of (State : View_State; Slice : Images.Text_Slice) return String is
      E : Bible_Translations.Edition renames State.Editions (State.Which);
      Result : String (1 .. Slice.Length);
   begin
      for I in Result'Range loop
         Result (I) := Character'Val (E.Data (Slice.First + I - 1));
      end loop;
      return Result;
   end Text_Of;

   function About (State : View_State; Kind : Images.About_Kind) return String is
      E : Bible_Translations.Edition renames State.Editions (State.Which);
      Slice : Images.Text_Slice;
      Valid : Boolean;
   begin
      if E.Status /= Bible_Translations.Loaded then
         return "";
      end if;
      Images.Read_About (E.Data.all, E.Layout, Kind, Slice, Valid);
      return (if Valid then Text_Of (State, Slice) else "");
   end About;

   --  The verse's text in the face (its verse-text segments, spaced).
   function Verse_Face_Text (State : View_State; Row : Images.Image_Size) return String is
      E : Bible_Translations.Edition renames State.Editions (State.Which);
      V : constant Images.Verse_Record := Images.Read_Verse (E.Data.all, E.Layout, Row);
      function From (S : Natural) return String is
      begin
         if S >= V.First_Segment + V.Segments then
            return "";
         end if;
         declare
            Seg : constant Images.Segment_Record := Images.Read_Segment (E.Data.all, E.Layout, S);
         begin
            if Seg.Valid and then Images.Is_Verse_Text (Seg.Kind) then
               declare
                  Rest : constant String := From (S + 1);
               begin
                  return Bible_Text.Face_Text (Text_Of (State, Seg.Content)) &
                    (if Rest'Length > 0 then " " & Rest else "");
               end;
            end if;
            return From (S + 1);
         end;
      end From;
   begin
      return (if V.Valid then From (V.First_Segment) else "");
   end Verse_Face_Text;

   function Chapter_Title (Item : Book; Chapter : Chapter_Number; One_Chapter : Boolean) return String is
     (if One_Chapter then Name (Item)
      elsif Item = Psalms then "Psalm " & Trimmed (Chapter)
      else Name (Item) & " " & Trimmed (Chapter));

   ---------------------------------------------------------------------------
   --  The page's buffers.
   ---------------------------------------------------------------------------
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

   --  Text into the store; its first index.
   procedure Keep (State : in out View_State; Text : String; First : out Natural) is
   begin
      if State.Store = null then
         State.Store := new String (1 .. FIRST_STORE);
      end if;
      if State.Store'Length - State.Store_Used < Text'Length then
         declare
            Size : Natural := State.Store'Length;
            Larger : Store_Access;
         begin
            while Size - State.Store_Used < Text'Length loop
               Size := 2 * Size;
            end loop;
            Larger := new String (1 .. Size);
            Larger (1 .. State.Store_Used) := State.Store (1 .. State.Store_Used);
            Free_Store (State.Store);
            State.Store := Larger;
         end;
      end if;
      First := State.Store_Used + 1;
      State.Store (First .. First + Text'Length - 1) := Text;
      State.Store_Used := State.Store_Used + Text'Length;
   end Keep;

   function Op_Text (State : View_State; Item : Op) return String is
     (if Item.Length = 0 then "" else State.Store (Item.First .. Item.First + Item.Length - 1));

   function Face_Of (Style : Op_Style) return Text_Face is
     (case Style is
         when Body_Style | Red_Style | Verse_Number_Style | Heading_Style | Title_Style | Muted_Style |
              Caption_Style =>
            Reading_Face,
         when Section_Style | Reference_Style => Emphasis_Face,
         when Note_Style => Interface_Face);
   function Size_Of (Style : Op_Style) return Text_Size is
     (case Style is
         when Heading_Style => Heading_Size, when Verse_Number_Style | Caption_Style => Superscript_Size,
         when others => Body_Size);
   function Measure (Style : Op_Style; Text : String) return Natural is
     (Styled_Text_Width (Text, Face_Of (Style), Size_Of (Style)));

   ---------------------------------------------------------------------------
   --  Flowing text: words placed on lines from Left to Right, wrapped.
   ---------------------------------------------------------------------------
   type Flow is record
      Left, Right : Natural := 0;   --  the column
      X : Natural := 0;             --  the pen on the current line
      Y : Natural := 0;             --  the current line's top
      Line_Start : Natural := 0;    --  where a wrapped line starts
      Open_Line : Boolean := False; --  a line has words
      Line : Positive := READING_LINE;
   end record;

   procedure New_Line (State : in out View_State; F : in out Flow; Indent, Hang : Natural) is
   begin
      if F.Open_Line then
         F.Y := F.Y + F.Line;
         State.Lines := State.Lines + 1;
      end if;
      F.X := F.Left + Indent;
      F.Line_Start := F.Left + Hang;
      F.Open_Line := False;
   end New_Line;

   --  Place Text's words (face text) in Style; Verse tags the runs. A
   --  Prefix_Number is a verse number set before the first word, kept on
   --  its line. At most Max_Lines lines (0: no limit), the last ending in
   --  an ellipsis when cut.
   procedure Place_Words
     (State : in out View_State; F : in out Flow; Text : String; Style : Op_Style;
      Verse : Verse_Count := 0; Prefix_Number : Verse_Count := 0; Max_Lines : Natural := 0;
      Tint : Boolean := False; Join : Boolean := False)
   is
      Space : constant Natural := Measure (Style, " ");
      Number_Text : constant String := (if Prefix_Number > 0 then Trimmed (Prefix_Number) else "");
      Number_Width : constant Natural :=
        (if Prefix_Number > 0 then Measure (Verse_Number_Style, Number_Text) + NUMBER_GAP else 0);
      Run_X : Natural := 0;   --  the run being gathered
      Run_Open : Boolean := False;
      Run_Text_First, Run_Text_Last : Natural := 0;
      Lines_Used : Natural := 1;
      Stopped : Boolean := False;
      I : Natural := Text'First;
      Word_First, Word_Last : Natural;
      First_Word : Boolean := True;
      Body_Baseline : constant Natural := Styled_Baseline (Reading_Face, Body_Size);
      Small_Baseline : constant Natural := Styled_Baseline (Reading_Face, Superscript_Size);
      Text_Top : constant Natural := (READING_LINE - Styled_Line_Height (Reading_Face)) / 2;

      procedure Close_Run is
         First : Natural;
      begin
         if Run_Open then
            Keep (State, Text (Run_Text_First .. Run_Text_Last), First);
            if Tint then
               Add_Op (State, (Kind => Tint_Op, X => Run_X, Y => F.Y, W => F.X - Run_X, H => F.Line,
                               Verse => Verse, others => <>));
            end if;
            Add_Op (State, (Kind => Text_Op, Style => Style, X => Run_X, Y => F.Y + Text_Top,
                            W => F.X - Run_X, First => First, Length => Run_Text_Last - Run_Text_First + 1,
                            Verse => Verse, others => <>));
            Run_Open := False;
         end if;
      end Close_Run;
   begin
      while I <= Text'Last and then not Stopped loop
         --  The next word.
         while I <= Text'Last and then Bible_Text.Is_Break (Text (I)) loop
            I := I + 1;
         end loop;
         exit when I > Text'Last;
         Word_First := I;
         while I <= Text'Last and then not Bible_Text.Is_Break (Text (I)) loop
            I := I + 1;
         end loop;
         Word_Last := I - 1;
         declare
            Word : String renames Text (Word_First .. Word_Last);
            Word_Width : constant Natural := Measure (Style, Word);
            Lead : constant Natural := (if First_Word then Number_Width else 0);
            Before : constant Natural :=
              (if not F.Open_Line then 0
               elsif First_Word and then Prefix_Number > 0 then Space + NUMBER_LEAD
               elsif First_Word and then Join then 0
               else Space);
         begin
            if F.Open_Line and then F.X + Before + Lead + Word_Width > F.Right then
               --  Wrap.
               if Max_Lines > 0 and then Lines_Used = Max_Lines then
                  --  The last line: end it with an ellipsis.
                  Close_Run;
                  declare
                     First : Natural;
                  begin
                     Keep (State, ELLIPSIS, First);
                     Add_Op (State, (Kind => Text_Op, Style => Style, X => F.X + Space / 2,
                                     Y => F.Y + Text_Top, W => Measure (Style, ELLIPSIS), First => First,
                                     Length => ELLIPSIS'Length, Verse => Verse, others => <>));
                  end;
                  Stopped := True;
                  exit;
               end if;
               Close_Run;
               F.Y := F.Y + F.Line;
               State.Lines := State.Lines + 1;
               F.X := F.Line_Start;
               Lines_Used := Lines_Used + 1;
            elsif F.Open_Line then
               F.X := F.X + Before;
            end if;
            if First_Word and then Prefix_Number > 0 then
               Close_Run;
               declare
                  First : Natural;
               begin
                  Keep (State, Number_Text, First);
                  Add_Op (State, (Kind => Text_Op, Style => Verse_Number_Style, X => F.X,
                                  Y => F.Y + Text_Top + Body_Baseline - Natural'Min (Body_Baseline, Small_Baseline
                                    + SUPERSCRIPT_RAISE),
                                  W => Number_Width - NUMBER_GAP, First => First, Length => Number_Text'Length,
                                  Verse => Verse, others => <>));
               end;
               F.X := F.X + Number_Width;
            end if;
            if not Run_Open then
               Run_Open := True;
               Run_X := F.X;
               Run_Text_First := Word_First;
            end if;
            Run_Text_Last := Word_Last;
            F.X := F.X + Word_Width;
            F.Open_Line := True;
            First_Word := False;
         end;
      end loop;
      Close_Run;
   end Place_Words;

   ---------------------------------------------------------------------------
   --  Layout: the chapter.
   ---------------------------------------------------------------------------
   function Column_Left (Width : Natural) return Natural is
     (if Width > READING_MEASURE + 2 * SIDE_MARGIN then (Width - READING_MEASURE) / 2 else SIDE_MARGIN);
   function Column_Right (Width : Natural) return Natural is
     (if Width > READING_MEASURE + 2 * SIDE_MARGIN then (Width - READING_MEASURE) / 2 + READING_MEASURE
      elsif Width > 2 * SIDE_MARGIN + M.Base_Em then Width - SIDE_MARGIN
      else SIDE_MARGIN + M.Base_Em);

   procedure Caption_And_Heading (State : in out View_State; F : in out Flow; Text_Caption, Title : String) is
      First : Natural;
   begin
      Keep (State, Text_Caption, First);
      Add_Op (State, (Kind => Text_Op, Style => Caption_Style, X => F.Left, Y => F.Y,
                      W => Measure (Caption_Style, Text_Caption), First => First, Length => Text_Caption'Length,
                      others => <>));
      F.Y := F.Y + Styled_Line_Height (Reading_Face, Superscript_Size) + CAPTION_GAP;
      Keep (State, Title, First);
      Add_Op (State, (Kind => Text_Op, Style => Heading_Style, X => F.Left, Y => F.Y,
                      W => Measure (Heading_Style, Title), First => First, Length => Title'Length, others => <>));
      F.Y := F.Y + Styled_Line_Height (Reading_Face, Heading_Size);
      Add_Op (State, (Kind => Rule_Op, X => F.Left, Y => F.Y + M.Space_S, W => F.Right - F.Left, H => 1,
                      others => <>));
      F.Y := F.Y + HEADING_GAP;
   end Caption_And_Heading;

   procedure Lay_Out_Chapter (State : in out View_State; Width : Positive) is
      E : Bible_Translations.Edition renames State.Editions (State.Which);
      B : constant Book := State.Item.From.Book;
      C : constant Chapter_Number := State.Item.From.Chapter;
      T : constant Versification := E.Index.Table;
      F : Flow;
      Count : constant Verse_Count := (if C <= T.Chapters (B) then Verses_In (T, B, C) else 0);
      --  The testament and section, in capitals, a middle dot between.
      Caption : constant String :=
        (if Is_Deuterocanonical (B) then Testament_Title (Deuterocanon)
         else Testament_Title (Testament_Of (B)) & "  " & MIDDLE_DOT & "  " & Section_Title (Section_Of (B)));
      Upper : String := Caption;
   begin
      for I in Upper'Range loop
         if Upper (I) in 'a' .. 'z' then Upper (I) := Character'Val (Character'Pos (Upper (I)) - 32); end if;
      end loop;
      F.Left := Column_Left (Width);
      F.Right := Column_Right (Width);
      F.Y := TOP_PAD;
      Caption_And_Heading (State, F, Upper, Chapter_Title (B, C, T.Chapters (B) = 1));
      New_Line (State, F, 0, 0);
      for V in 1 .. Count loop
         declare
            Row : constant Images.Image_Size := Images.Verse_Row (E.Layout, E.Index, (B, C, V));
            VR : constant Images.Verse_Record := Images.Read_Verse (E.Data.all, E.Layout, Row);
            Numbered : Boolean := False;
            Tinted : constant Boolean :=
              State.Highlight and then Before_Or_Same (State.Item.From, (B, C, V))
              and then Before_Or_Same ((B, C, V), State.Item.To);
            Verse_Top : Natural := F.Y;
            Started : Boolean := False;
         begin
            if VR.Valid then
               for S in VR.First_Segment .. VR.First_Segment + VR.Segments - 1 loop
                  declare
                     Seg : constant Images.Segment_Record := Images.Read_Segment (E.Data.all, E.Layout, S);
                     Text : constant String := (if Seg.Valid then Bible_Text.Face_Text (Text_Of (State, Seg.Content))
                                                else "");
                  begin
                     if Seg.Valid then
                        case Seg.Kind is
                           when Images.Section_Heading | Images.Major_Heading =>
                              New_Line (State, F, 0, 0);
                              F.Y := F.Y + (if F.Y > TOP_PAD then SECTION_GAP / 2 else 0);
                              Place_Words (State, F, Text, Section_Style);
                              New_Line (State, F, 0, 0);
                              F.Y := F.Y + SECTION_GAP / 4;
                           when Images.Psalm_Title =>
                              New_Line (State, F, 0, 0);
                              Place_Words (State, F, Text, Title_Style);
                              New_Line (State, F, 0, 0);
                              F.Y := F.Y + STANZA_GAP;
                           when Images.Prologue =>
                              New_Line (State, F, PARAGRAPH_INDENT, 0);
                              Place_Words (State, F, Text, Muted_Style);
                              New_Line (State, F, 0, 0);
                              F.Y := F.Y + STANZA_GAP;
                           when Images.Prose | Images.Paragraph | Images.Flush_Paragraph | Images.Poetry_1 |
                                Images.Poetry_2 | Images.Poetry_3 =>
                              if Seg.Stanza_Break and then F.Open_Line then
                                 New_Line (State, F, 0, 0);
                                 F.Y := F.Y + STANZA_GAP;
                              end if;
                              case Seg.Kind is
                                 when Images.Paragraph => New_Line (State, F, PARAGRAPH_INDENT, 0);
                                 when Images.Flush_Paragraph => New_Line (State, F, 0, 0);
                                 when Images.Poetry_1 => New_Line (State, F, POETRY_INDENT, POETRY_INDENT + POETRY_HANG);
                                 when Images.Poetry_2 =>
                                    New_Line (State, F, 2 * POETRY_INDENT, 2 * POETRY_INDENT + POETRY_HANG);
                                 when Images.Poetry_3 =>
                                    New_Line (State, F, 3 * POETRY_INDENT, 3 * POETRY_INDENT + POETRY_HANG);
                                 when others => null;   --  prose continues the line
                              end case;
                              if not Started then
                                 Verse_Top := F.Y;
                                 Started := True;
                              end if;
                              if Seg.Words_Of_Christ and then State.Red then
                                 State.Red_Count := State.Red_Count + 1;
                              end if;
                              Place_Words (State, F, Text,
                                           (if Seg.Words_Of_Christ and then State.Red then Red_Style
                                            else Body_Style),
                                           Verse => V, Prefix_Number => (if Numbered then 0 else V), Tint => Tinted,
                                           Join => Seg.Joined and then Seg.Kind = Images.Prose);
                              Numbered := True;
                        end case;
                     end if;
                  end;
               end loop;
            end if;
            if State.Scroll_To_Verse = V then
               --  The verse a third of the way down the page, or the page's
               --  top (its heading) when the verse is in view from there.
               declare
                  Lead : constant Natural :=
                    (if State.Content.h > 3 * READING_LINE then State.Content.h / 3 else 2 * READING_LINE);
               begin
                  State.Scroll :=
                    (if Verse_Top + 3 * READING_LINE <= State.Content.h then 0
                     elsif Verse_Top > Lead then Verse_Top - Lead else 0);
               end;
               State.Scroll_To_Verse := 0;
            end if;
         end;
      end loop;
      New_Line (State, F, 0, 0);
      State.Content_Height := F.Y + BOTTOM_PAD;
   end Lay_Out_Chapter;

   ---------------------------------------------------------------------------
   --  Layout: search results, or the reference the field names.
   ---------------------------------------------------------------------------
   procedure Lay_Out_Results (State : in out View_State; Width : Positive) is
      E : Bible_Translations.Edition renames State.Editions (State.Which);
      F : Flow;
      Query : constant String := State.Query_Text (1 .. State.Query_Length);
      procedure Note (Text : String; Style : Op_Style := Note_Style) is
      begin
         New_Line (State, F, 0, 0);
         Place_Words (State, F, Text, Style);
         New_Line (State, F, 0, 0);
      end Note;
      procedure Reference_Row (Ref : Reference; Target : Natural; Snippet : String) is
         Label : constant String := Image (Ref);
         First : Natural;
      begin
         New_Line (State, F, 0, 0);
         Keep (State, Label, First);
         Add_Op (State, (Kind => Link_Op, Style => Reference_Style, X => F.Left,
                         Y => F.Y + (READING_LINE - Styled_Line_Height (Emphasis_Face)) / 2,
                         W => Measure (Reference_Style, Label), H => READING_LINE,
                         First => First, Length => Label'Length, Target => Target, others => <>));
         F.Y := F.Y + READING_LINE;
         F.X := F.Left;
         F.Line_Start := F.Left;
         F.Open_Line := False;
         Place_Words (State, F, Snippet, Body_Style, Max_Lines => SNIPPET_LINES);
         New_Line (State, F, 0, 0);
         F.Y := F.Y + RESULT_GAP;
      end Reference_Row;
   begin
      F.Left := Column_Left (Width);
      F.Right := Column_Right (Width);
      F.Y := TOP_PAD;
      if State.Reference_Hint then
         declare
            Row : constant Images.Image_Size := Images.Verse_Row (E.Layout, E.Index, State.Hint.From);
         begin
            Note ("Press Enter to open " & Image (Table (State), State.Hint) & ".");
            Reference_Row (State.Hint.From, 0, Verse_Face_Text (State, Row));
         end;
      elsif not State.Searched then
         declare
            Parsed : Bible_Parser.Result;
         begin
            Bible_Parser.Parse (Query, Table (State), Parsed);
            Note (Bible_Parser.Describe (Query, Parsed));
         end;
      elsif State.Total = 0 then
         Note ("No verse in the " & Bible_Translations.Title (State.Which) & " has every word of """ &
               Query & """.");
      else
         Note (Trimmed (State.Total) & (if State.Total = 1 then " verse has " else " verses have ") &
               (if State.Whole_Words then "" else "words beginning ") & """" & Query &
               """" & (if State.Whole_Words then "" else " (Enter: whole words)") & (if State.Total > MAXIMUM_SHOWN then "; the first" & Natural'Image (MAXIMUM_SHOWN) &
                         " are shown. Add a word to narrow them." else "."));
         F.Y := F.Y + RESULT_GAP;
         for I in 1 .. Natural'Min (State.Found, MAXIMUM_SHOWN) loop
            declare
               Ref : Reference;
               Found : Boolean;
            begin
               Images.Locate (E.Layout, E.Index, State.Hits (I), Ref, Found);
               if Found then
                  Reference_Row (Ref, I, Verse_Face_Text (State, State.Hits (I)));
               end if;
            end;
         end loop;
      end if;
      State.Content_Height := F.Y + BOTTOM_PAD;
   end Lay_Out_Results;

   procedure Lay_Out (State : in out View_State; Width : Positive) is
   begin
      State.Op_Count := 0;
      State.Store_Used := 0;
      State.Lines := 0;
      State.Red_Count := 0;
      if not Ready (State) then
         declare
            F : Flow;
         begin
            F.Left := Column_Left (Width);
            F.Right := Column_Right (Width);
            F.Y := TOP_PAD;
            Place_Words (State, F, "The " & Bible_Translations.Title (State.Which) & " is not available: " &
                         Bible_Translations.Describe (Edition (State).Status) & ".", Note_Style);
            New_Line (State, F, 0, 0);
            Place_Words (State, F, "Its text is read from Assets/" & Bible_Translations.PACKAGE_DIRECTORY &
                         Bible_Translations.File_Name (State.Which) & ".", Note_Style);
            New_Line (State, F, 0, 0);
            State.Content_Height := F.Y + BOTTOM_PAD;
         end;
      elsif State.Current = Results then
         Lay_Out_Results (State, Width);
      else
         Lay_Out_Chapter (State, Width);
      end if;
      State.Laid_Out_Width := Width;
   end Lay_Out;

   ---------------------------------------------------------------------------
   --  The sidebar tree.
   ---------------------------------------------------------------------------
   procedure Add_Node (State : in out View_State; Item : Node) is
   begin
      if State.Nodes = null then
         State.Nodes := new Node_Array (1 .. FIRST_NODES);
      elsif State.Node_Count = State.Nodes'Length then
         declare
            Larger : constant Nodes_Access := new Node_Array (1 .. 2 * State.Nodes'Length);
         begin
            Larger (1 .. State.Node_Count) := State.Nodes (1 .. State.Node_Count);
            Free_Nodes (State.Nodes);
            State.Nodes := Larger;
         end;
      end if;
      State.Node_Count := State.Node_Count + 1;
      State.Nodes (State.Node_Count) := Item;
   end Add_Node;

   function Has_Section (T : Versification; S : Section) return Boolean is
     (for some B in First_Book (S) .. Last_Book (S) => Has_Book (T, B));
   function Has_Testament (T : Versification; P : Testament) return Boolean is
     (for some S in First_Section (P) .. Last_Section (P) => Has_Section (T, S));

   function Book_Depth (P : Testament) return Natural is (if First_Section (P) = Last_Section (P) then 1 else 2);

   procedure Rebuild_Nodes (State : in out View_State) is
      T : constant Versification := Table (State);
      type Follows is array (0 .. 4) of Boolean;
      Sibling_Follows : Follows := [others => False];
   begin
      State.Node_Count := 0;
      State.Tree_Stale := False;
      if not Ready (State) then
         return;
      end if;
      for P in Testament loop
         if Has_Testament (T, P) then
            Add_Node (State, (Kind => Testament_Node, Part => P, Depth => 0, Has_Children => True,
                              Expanded => State.Testament_Open (P), others => <>));
            if State.Testament_Open (P) then
               for S in First_Section (P) .. Last_Section (P) loop
                  if Has_Section (T, S) then
                     --  A testament of one section (the Apocrypha) lists its
                     --  books directly.
                     if First_Section (P) /= Last_Section (P) then
                        Add_Node (State, (Kind => Section_Node, Part => P, Group => S, Depth => 1,
                                          Has_Children => True, Expanded => State.Section_Open (S), others => <>));
                     end if;
                     if State.Section_Open (S) or else First_Section (P) = Last_Section (P) then
                        for B in First_Book (S) .. Last_Book (S) loop
                           if Has_Book (T, B) then
                              Add_Node (State, (Kind => Book_Node, Part => P, Group => S, Item => B,
                                                Depth => (if First_Section (P) = Last_Section (P) then 1 else 2),
                                                Has_Children => True, Expanded => State.Book_Open (B),
                                                others => <>));
                              if State.Book_Open (B) then
                                 for C in 1 .. T.Chapters (B) loop
                                    if Verses_In (T, B, C) > 0 then
                                       declare
                                          Open_Verses : constant Boolean :=
                                            B = State.Item.From.Book and then State.Chapter_Open = C;
                                       begin
                                          Add_Node (State, (Kind => Chapter_Node, Part => P, Group => S, Item => B,
                                                            Chapter => C, Depth => Book_Depth (P) + 1,
                                                            Has_Children => True,
                                                            Expanded => Open_Verses, others => <>));
                                          if Open_Verses then
                                             for V in 1 .. Verses_In (T, B, C) loop
                                                Add_Node (State, (Kind => Verse_Node, Part => P, Group => S, Item => B,
                                                                  Chapter => C, Verse => V,
                                                                  Depth => Book_Depth (P) + 2,
                                                                  others => <>));
                                             end loop;
                                          end if;
                                       end;
                                    end if;
                                 end loop;
                              end if;
                           end if;
                        end loop;
                     end if;
                  end if;
               end loop;
            end if;
         end if;
      end loop;
      --  Which rows are the last of their siblings (for the branch lines).
      for I in reverse 1 .. State.Node_Count loop
         declare
            D : constant Natural := State.Nodes (I).Depth;
         begin
            State.Nodes (I).Last_Sibling := not Sibling_Follows (D);
            Sibling_Follows (D) := True;
            for Deeper in D + 1 .. Follows'Last loop
               Sibling_Follows (Deeper) := False;
            end loop;
         end;
      end loop;
   end Rebuild_Nodes;

   function Node_Label (Item : Node) return String is
     (case Item.Kind is
         when Testament_Node => Testament_Title (Item.Part),
         when Section_Node => Section_Title (Item.Group),
         when Book_Node => Name (Item.Item),
         when Chapter_Node => (if Item.Item = Psalms then "Psalm " else "Chapter ") & Trimmed (Item.Chapter),
         when Verse_Node => "Verse " & Trimmed (Item.Verse));

   --  A node's identity, for the tree's selection.
   function Node_Key (Item : Node) return Natural is
     (case Item.Kind is
         when Testament_Node => 1 + Testament'Pos (Item.Part),
         when Section_Node => 10 + Section'Pos (Item.Group),
         when Book_Node => 100 + Book'Pos (Item.Item),
         when Chapter_Node => 1_000 + Book'Pos (Item.Item) * 256 + Item.Chapter,
         when Verse_Node => 100_000 + (Book'Pos (Item.Item) * 256 + Item.Chapter) * 256 + Item.Verse);

   function Selected_Key (State : View_State) return Natural is
     (if State.Highlight and then State.Item.From = State.Item.To
      then Node_Key ((Kind => Verse_Node, Item => State.Item.From.Book, Chapter => State.Item.From.Chapter,
                      Verse => State.Item.From.Verse, others => <>))
      else Node_Key ((Kind => Chapter_Node, Item => State.Item.From.Book, Chapter => State.Item.From.Chapter,
                      others => <>)));

   --  Open the sidebar down to the passage shown.
   procedure Reveal (State : in out View_State) is
      B : constant Book := State.Item.From.Book;
   begin
      if not State.Book_Open (B) then
         State.Section_Open := [others => False];
         State.Book_Open := [others => False];
         State.Chapter_Open := 0;
      end if;
      State.Testament_Open (Testament_Of (B)) := True;
      State.Section_Open (Section_Of (B)) := True;
      State.Book_Open (B) := True;
      State.Tree_Stale := True;
      State.Tree_Follow := True;
   end Reveal;

   ---------------------------------------------------------------------------
   --  Navigation.
   ---------------------------------------------------------------------------
   procedure Remember (State : in out View_State) is
      Here : constant Place := (State.Which, State.Item);
   begin
      if State.History_At > 0 and then State.History (State.History_At) = Here then
         return;
      end if;
      State.History_Used := State.History_At;   --  forward entries are dropped
      if State.History_Used = HISTORY_DEPTH then
         State.History (1 .. HISTORY_DEPTH - 1) := State.History (2 .. HISTORY_DEPTH);
         State.History_Used := HISTORY_DEPTH - 1;
      end if;
      State.History_Used := State.History_Used + 1;
      State.History (State.History_Used) := Here;
      State.History_At := State.History_Used;
   end Remember;

   --  Show Item (a passage of the current translation) without recording it.
   procedure Show (State : in out View_State; Item : Passage; Highlight : Boolean) is
   begin
      State.Item := Item;
      State.Highlight := Highlight;
      State.Current := (if Ready (State) then Reading else Unavailable);
      State.Scroll := 0;
      State.Scroll_To_Verse := (if Highlight then Item.From.Verse else 0);
      State.Laid_Out_Width := 0;
      Reveal (State);
   end Show;

   function Is_Whole_Chapter (State : View_State; Item : Passage) return Boolean is
     (Item.From.Book = Item.To.Book and then Item.From.Chapter = Item.To.Chapter and then Item.From.Verse = 1
      and then Item.To.Chapter <= Table (State).Chapters (Item.To.Book)
      and then Item.To.Verse = Verses_In (Table (State), Item.To.Book, Item.To.Chapter));

   procedure Open (State : in out View_State; Item : Passage) is
   begin
      if not Ready (State) or else not Exists (Table (State), Item) then
         return;
      end if;
      Show (State, Item, Highlight => not Is_Whole_Chapter (State, Item));
      Remember (State);
   end Open;

   --  The first chapter with verses of B, from C on.
   function Whole_Chapter (State : View_State; B : Book; C : Chapter_Number) return Passage is
     ((B, C, 1), (B, C, Natural'Max (1, Verses_In (Table (State), B, C))));

   --  The next (Forward) or previous chapter with verses, across books.
   procedure Step_Chapter (State : in out View_State; Forward : Boolean) is
      T : constant Versification := Table (State);
      B : Book := State.Item.From.Book;
      C : Natural := State.Item.From.Chapter;
   begin
      if not Ready (State) then
         return;
      end if;
      loop
         if Forward then
            if C < T.Chapters (B) then
               C := C + 1;
            elsif B = Book'Last then
               return;
            else
               B := Book'Succ (B);
               C := 1;
            end if;
         else
            if C > 1 then
               C := C - 1;
            elsif B = Book'First then
               return;
            else
               B := Book'Pred (B);
               C := T.Chapters (B);
            end if;
         end if;
         exit when C in 1 .. T.Chapters (B) and then Verses_In (T, B, C) > 0;
      end loop;
      Open (State, Whole_Chapter (State, B, C));
   end Step_Chapter;

   procedure Go (State : in out View_State; To : History_Count) is
   begin
      if To in 1 .. State.History_Used then
         State.History_At := To;
         State.Which := State.History (To).Which;
         Ensure (State, State.Which);
         Show (State, State.History (To).Item,
               Highlight => Ready (State) and then not Is_Whole_Chapter (State, State.History (To).Item));
      end if;
   end Go;

   procedure Set_Translation (State : in out View_State; Which : Translation) is
      Keep_Item : constant Passage := State.Item;
   begin
      if Which = State.Which and then Ready (State) then
         return;
      end if;
      State.Which := Which;
      Ensure (State, Which);
      State.Tree_Stale := True;
      State.Laid_Out_Width := 0;
      if not Ready (State) then
         State.Current := Unavailable;
         return;
      end if;
      --  The same passage where this translation has it, else its chapter,
      --  else the start of the Bible.
      if Exists (Table (State), Keep_Item) then
         Show (State, Keep_Item, State.Highlight);
      elsif Keep_Item.From.Chapter <= Table (State).Chapters (Keep_Item.From.Book)
        and then Verses_In (Table (State), Keep_Item.From.Book, Keep_Item.From.Chapter) > 0
      then
         Show (State, Whole_Chapter (State, Keep_Item.From.Book, Keep_Item.From.Chapter), False);
      else
         Show (State, Whole_Chapter (State, Genesis, 1), False);
      end if;
      if State.Query_Length > 0 then
         Enter_Text (State, State.Query_Text (1 .. State.Query_Length));
      end if;
      Remember (State);
      Save (Bible_Translations.Translation_Preference, Bible_Translations.Abbreviation (Which));
   end Set_Translation;

   function Face_Name return String is
     (if CuBit.Reading_Faces.Chosen = 0 then "" else CuBit.Reading_Faces.Name (CuBit.Reading_Faces.Chosen));

   procedure Set_Face (State : in out View_State; Name : String) is
      Found : constant CuBit.Reading_Faces.Face_Count := CuBit.Reading_Faces.Find (Name);
   begin
      if Found > 0 then
         CuBit.Reading_Faces.Choose (Found);
         State.Laid_Out_Width := 0;   --  every width changes
         Save (Bible_Translations.Reading_Face_Preference, Name);
      end if;
   end Set_Face;

   procedure Set_Red_Letters (State : in out View_State; On : Boolean) is
   begin
      if On /= State.Red then
         State.Red := On;
         State.Laid_Out_Width := 0;
      end if;
      Save (Bible_Translations.Red_Letters_Preference,
            (if On then Bible_Translations.RED_LETTERS_ON else Bible_Translations.RED_LETTERS_OFF));
   end Set_Red_Letters;
   function Red_Letters (State : View_State) return Boolean is (State.Red);
   function Red_Runs (State : View_State) return Natural is (State.Red_Count);

   --  How light a background is: its luma (ITU-R BT.601), 0 .. 255.
   function Luma (Value : Color) return Natural is
      use Interfaces;
   begin
      return (299 * Natural (Shift_Right (Value, 16) and 16#FF#) + 587 * Natural (Shift_Right (Value, 8) and 16#FF#)
              + 114 * Natural (Value and 16#FF#)) / 1_000;
   end Luma;
   LIGHT_PAGE : constant := 128;

   --  On light pages a deep crimson, on dark ones a soft rose.
   DEEP_RED  : constant Color := 16#A3_1A_1A#;
   LIGHT_RED : constant Color := 16#F2_8C_8C#;
   function Red_Letter_Color (Page : CuBit.UI.Color) return CuBit.UI.Color is
     (if Luma (Page) >= LIGHT_PAGE then DEEP_RED else LIGHT_RED);

   procedure Next_Face (State : in out View_State) is
      Count : constant CuBit.Reading_Faces.Face_Count := CuBit.Reading_Faces.Count;
      Now : constant CuBit.Reading_Faces.Face_Count := CuBit.Reading_Faces.Chosen;
   begin
      if Count > 1 then
         Set_Face (State, CuBit.Reading_Faces.Name (if Now >= Count then 1 else Now + 1));
      end if;
   end Next_Face;

   ---------------------------------------------------------------------------
   --  The field: a reference, or words to search for.
   ---------------------------------------------------------------------------
   procedure Edited (State : in out View_State; Whole_Words : Boolean := False) is
      Text : constant String := Ed.Content (State.Search);
      Parsed : Bible_Parser.Result;
      Q : Bible_Search.Query;
      Normalized : Bible_Search.Normalize_Status;
      Status : Bible_Search.Search_Status;
      Letters : Natural := 0;
   begin
      State.Query_Length := Natural'Min (Text'Length, State.Query_Text'Length);
      State.Query_Text (1 .. State.Query_Length) := Text (Text'First .. Text'First + State.Query_Length - 1);
      State.Reference_Hint := False;
      State.Searched := False;
      State.Found := 0;
      State.Total := 0;
      State.Laid_Out_Width := 0;
      State.Scroll := 0;
      for C of Text loop
         if C /= ' ' then Letters := Letters + 1; end if;
      end loop;
      if Letters = 0 or else not Ready (State) then
         State.Current := (if Ready (State) then Reading else Unavailable);
         return;
      end if;
      State.Current := Results;
      Bible_Parser.Parse (Text, Table (State), Parsed);
      if Parsed.Outcome = Bible_Parser.Parsed then
         State.Reference_Hint := True;
         State.Hint := Parsed.Passage;
         return;
      elsif Parsed.Outcome in Bible_Parser.Book_Not_In_Translation | Bible_Parser.Chapter_Out_Of_Range |
                              Bible_Parser.Verse_Out_Of_Range | Bible_Parser.Reversed_Range
      then
         return;   --  a reference, wrong: Lay_Out_Results says why
      end if;
      if Letters < MINIMUM_SEARCH then
         State.Current := Reading;
         return;
      end if;
      Bible_Search.Normalize (Text, Q, Normalized);
      if Normalized /= Bible_Search.Normalized then
         State.Searched := True;
         return;
      end if;
      if State.Hits = null then
         State.Hits := new Bible_Search.Hit_Array (1 .. MAXIMUM_SHOWN);
         State.Work := new Bible_Search.Work_Area;
      end if;
      declare
         E : Bible_Translations.Edition renames State.Editions (State.Which);
      begin
         Bible_Search.Search (E.Data.all, E.Layout, Q, Prefix_Last => not Whole_Words, Work => State.Work.all,
                              Hits => State.Hits.all, Found => State.Found, Total => State.Total, Status => Status);
      end;
      State.Searched := True;
      State.Whole_Words := Whole_Words;
      if Status /= Bible_Search.Searched then
         State.Found := 0;
         State.Total := 0;
      end if;
   end Edited;

   procedure Set_Field (State : in out View_State; Text : String) is
      Accepted : Boolean;
   begin
      Ed.Initialize (State.Search, Text (Text'First .. Text'First + Natural'Min (Text'Length, Ed.MAX_TEXT_LENGTH) - 1),
                     Accepted);
      Ed.Move (State.Search, Ed.Move_End);
   end Set_Field;

   procedure Clear_Field (State : in out View_State) is
   begin
      Set_Field (State, "");
      Edited (State);
   end Clear_Field;

   procedure Enter_Text (State : in out View_State; Text : String) is
   begin
      Set_Field (State, Text);
      Edited (State);
      if State.Reference_Hint then
         Clear_Field (State);
         Open (State, State.Hint);
      end if;
   end Enter_Text;

   procedure Initialize (State : out View_State) is
      Accepted : Boolean;
      Saved_Face : constant String := Saved (Bible_Translations.Reading_Face_Preference);
      Saved_Translation : constant String := Saved (Bible_Translations.Translation_Preference);
      Saved_Red : constant String := Saved (Bible_Translations.Red_Letters_Preference);
   begin
      State.Red := Saved_Red /= Bible_Translations.RED_LETTERS_OFF;
      State.Which := Bible_Translations.DEFAULT;
      --  The reader's remembered face and translation, where still offered.
      if CuBit.Reading_Faces.Find (Saved_Face) > 0 then
         CuBit.Reading_Faces.Choose (CuBit.Reading_Faces.Find (Saved_Face));
      end if;
      for T in Translation loop
         if Bible_Translations.Abbreviation (T) = Saved_Translation then
            State.Which := T;
         end if;
      end loop;
      Ed.Initialize (State.Search, "", Accepted);
      Ensure (State, State.Which);
      if Ready (State) then
         --  The Gospel of John, chapter 1, for a first look.
         Show (State, Whole_Chapter (State, John, 1), False);
         Remember (State);
      else
         State.Current := Unavailable;
      end if;
   end Initialize;

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
              and then Y >= Top and then Y < Top + Integer (O.H)
            then
               return I;
            end if;
         end;
      end loop;
      return 0;
   end Over_Link;

   function Tint (Colors : Theme) return Color is
      use Interfaces;
      function Mix (Shift : Natural) return Unsigned_32 is
        ((Shift_Right (Colors.field, Shift) and 16#FF#) * 3 / 4 + (Shift_Right (Colors.accent, Shift) and 16#FF#) / 4);
   begin
      return Shift_Left (Mix (16), 16) or Shift_Left (Mix (8), 8) or Mix (0);
   end Tint;

   procedure Render
     (State : in out View_State; C : CuBit.UI.Canvas; Bounds : CuBit.UI.Rect;
      UI : in out CuBit.UI.State.UI_State; Map : in out Controls.Control_Map)
   is
      Colors : constant Theme := Current_Theme;
      Toolbar : constant Rect := (Bounds.x, Bounds.y, Bounds.w, TOOLBAR_HEIGHT);
      Control_Y : constant Natural := Toolbar.y + (TOOLBAR_HEIGHT - CONTROL_HEIGHT) / 2;
      Tabs_Width : constant Natural := 3 * TAB_WIDTH + 2 * GAP;
      Tabs_X : constant Natural := (if Bounds.w > Tabs_Width + GAP then Bounds.x + Bounds.w - Tabs_Width - GAP else 0);
      Face_X : constant Natural := (if Tabs_X > FACE_BUTTON_WIDTH + GAP then Tabs_X - FACE_BUTTON_WIDTH - GAP else 0);
      Red_X : constant Natural := (if Face_X > RED_WIDTH + GAP then Face_X - RED_WIDTH - GAP else 0);
      Buttons_X : constant Natural :=
        (if Red_X > 2 * CHAPTER_BUTTON_WIDTH + 3 * GAP then Red_X - 2 * CHAPTER_BUTTON_WIDTH - 3 * GAP else 0);
      Field_X : constant Natural := Bounds.x + GAP + 2 * (NAV_WIDTH + GAP) + GAP;
      Field : constant Rect :=
        (Field_X, Control_Y, (if Buttons_X > Field_X + 2 * GAP then Buttons_X - Field_X - 2 * GAP else 1),
         CONTROL_HEIGHT);
      Status_Bar : constant Rect :=
        (Bounds.x, Bounds.y + (if Bounds.h > STATUS_HEIGHT then Bounds.h - STATUS_HEIGHT else 0), Bounds.w,
         STATUS_HEIGHT);
      Body_Box : constant Rect :=
        (Bounds.x, Bounds.y + TOOLBAR_HEIGHT, Bounds.w,
         (if Bounds.h > TOOLBAR_HEIGHT + STATUS_HEIGHT then Bounds.h - TOOLBAR_HEIGHT - STATUS_HEIGHT else 1));
      Sidebar, Page_Box : Rect;
      Result : Widget_Result;
      Hovered : constant Natural := Over_Link (State, State.Hover_X, State.Hover_Y);
      Text : constant String := Ed.Content (State.Search);
      Tab_Content : Canvas;
      Tab_Colors : Theme;
   begin
      CuBit.UI.State.Begin_Frame (UI);
      Controls.Clear (Map);
      if State.Split = 0 then
         State.Split := SIDEBAR_WIDTH;
      end if;

      --  The body: the sidebar and the page, split.
      CuBit.UI.Widgets.Split_Pane
        (C, UI, Map, SPLIT_ID, Body_Box, Body_Box, Colors, vertical => True, position => State.Split,
         first => Sidebar, second => Page_Box, splitterSize => SPLITTER, minFirst => SIDEBAR_MINIMUM,
         minSecond => PAGE_MINIMUM, retainedInput => True);

      --  The page.
      declare
         Page : constant Rect :=
           (Page_Box.x, Page_Box.y, (if Page_Box.w > SCROLLBAR_WIDTH then Page_Box.w - SCROLLBAR_WIDTH else 1),
            Page_Box.h);
      begin
         State.Content := Page;
         if State.Laid_Out_Width /= Page.w then
            Lay_Out (State, Page.w);
         end if;
         State.Scroll := Natural'Min (State.Scroll, Max_Scroll (State));
         Fill_Rect (C, Page, Colors.field);
         declare
            PC : constant Canvas := With_Clip (C, Page);
            Tint_Color : constant Color := Tint (Colors);
         begin
            --  Tints first, then everything over them.
            for Pass in 1 .. 2 loop
               for I in 1 .. State.Op_Count loop
                  declare
                     O : Op renames State.Ops (I);
                     Top : constant Integer := Page.y + O.Y - State.Scroll;
                     X : constant Natural := Page.x + O.X;
                     Height : constant Natural :=
                       (case O.Kind is
                           when Text_Op | Link_Op => Styled_Line_Height (Face_Of (O.Style), Size_Of (O.Style)),
                           when Rule_Op | Tint_Op => O.H);
                  begin
                     if Top + Integer (Height) > Page.y and then Top < Page.y + Page.h then
                        declare
                           Y : constant Natural := Natural (Integer'Max (0, Top));
                        begin
                           case O.Kind is
                              when Tint_Op =>
                                 if Pass = 1 then
                                    Fill_Rect (PC, (Natural'Max (Page.x, X - TINT_PAD), Y, O.W + 2 * TINT_PAD, O.H),
                                               Tint_Color);
                                 end if;
                              when Rule_Op =>
                                 if Pass = 2 and then Top >= Page.y then
                                    Fill_Rect (PC, (X, Y, O.W, 1), Colors.edge);
                                 end if;
                              when Text_Op | Link_Op =>
                                 if Pass = 2 then
                                    Draw_Styled_Text_Transparent
                                      (PC, X, Y, Op_Text (State, O),
                                       (case O.Style is
                                           when Body_Style | Heading_Style | Section_Style => Colors.text,
                           when Red_Style => Red_Letter_Color (Colors.field),
                                           when Reference_Style => Colors.accent,
                                           when Verse_Number_Style | Caption_Style | Title_Style | Muted_Style |
                                                Note_Style => Colors.muted),
                                       Face_Of (O.Style), Size_Of (O.Style));
                                    if O.Kind = Link_Op and then I = Hovered then
                                       Fill_Rect (PC, (X, Y + Styled_Line_Height (Face_Of (O.Style)), O.W, 1),
                                                  Colors.accent);
                                    end if;
                                 end if;
                           end case;
                        end;
                     end if;
                  end;
               end loop;
            end loop;
         end;
         Fill_Rect (C, (Page.x + Page.w, Page.y, SCROLLBAR_WIDTH, Page.h), Colors.field);
         CuBit.UI.Widgets.Vertical_Scrollbar
           (C, UI, Map, PAGE_SCROLL_ID, (Page.x + Page.w, Page.y, SCROLLBAR_WIDTH, Page.h), Page, Colors,
            0, Max_Scroll (State), State.Scroll, Result, Positive'Max (1, Page.h), retainedInput => True);
      end;

      --  The sidebar.
      declare
         Frame_Content : Rect;
         Rows_Box : Rect;
         Row_Height : constant Positive := CuBit.UI.Trees.TREE_ROW_HEIGHT;
         Visible : Natural;
         Selected : Natural := Selected_Key (State);
         Branches : Interfaces.Unsigned_64;
         Drawn : Natural := 0;
         use type Interfaces.Unsigned_64;
         type Continues is array (0 .. 4) of Boolean;
         Continue_At : Continues := [others => False];
      begin
         if State.Tree_Stale then
            Rebuild_Nodes (State);
         end if;
         CuBit.UI.Trees.View_Frame (C, Sidebar, Colors, False, Frame_Content);
         Rows_Box := (Frame_Content.x, Frame_Content.y,
                      (if Frame_Content.w > SCROLLBAR_WIDTH then Frame_Content.w - SCROLLBAR_WIDTH else 1),
                      Frame_Content.h);
         State.Tree_Box := Rows_Box;
         Visible := Rows_Box.h / Row_Height;
         --  After a move, scroll the passage's row into the middle of view.
         if State.Tree_Follow and then Visible > 0 then
            State.Tree_Follow := False;
            for I in 1 .. State.Node_Count loop
               if Node_Key (State.Nodes (I)) = Selected then
                  if I <= State.Tree_Scroll or else I > State.Tree_Scroll + Visible then
                     State.Tree_Scroll := (if I > Visible / 2 then I - Visible / 2 else 0);
                  end if;
                  exit;
               end if;
            end loop;
         end if;
         if State.Node_Count > Visible then
            State.Tree_Scroll := Natural'Min (State.Tree_Scroll, State.Node_Count - Visible);
         else
            State.Tree_Scroll := 0;
         end if;
         Fill_Rect (C, Frame_Content, Colors.field);
         CuBit.UI.Widgets.Vertical_Scrollbar
           (C, UI, Map, TREE_SCROLL_ID,
            (Frame_Content.x + Rows_Box.w, Frame_Content.y, SCROLLBAR_WIDTH, Frame_Content.h), Rows_Box, Colors,
            0, (if State.Node_Count > Visible then State.Node_Count - Visible else 0), State.Tree_Scroll, Result,
            Positive'Max (1, Visible), retainedInput => True);
         for I in 1 .. State.Node_Count loop
            declare
               N : Node renames State.Nodes (I);
            begin
               --  Branch lines of the ancestors still to continue.
               --  Bit L: the ancestor at depth L + 1 has a sibling below.
               Branches := 0;
               if N.Depth >= 2 then
                  for Level in 0 .. Natural'Min (N.Depth, Continues'Last) - 2 loop
                     if Continue_At (Level + 1) then
                        Branches := Branches or Interfaces.Shift_Left (1, Level);
                     end if;
                  end loop;
               end if;
               if I > State.Tree_Scroll and then Drawn < Visible then
                  declare
                     Row : constant Rect := (Rows_Box.x, Rows_Box.y + Drawn * Row_Height, Rows_Box.w, Row_Height);
                  begin
                     CuBit.UI.Trees.Tree_Item
                       (C, UI, Map, FIRST_ROW_ID + Drawn, Row, Rows_Box, Colors, Node_Label (N), Node_Key (N),
                        Selected, N.Depth, N.Expanded, N.Has_Children,
                        (case N.Kind is
                            when Testament_Node | Section_Node => CuBit.UI.Trees.Folder_Icon,
                            when others => CuBit.UI.Trees.No_Icon),
                        True, N.Last_Sibling, Branches, Result, retainedInput => True);
                  end;
                  Drawn := Drawn + 1;
               end if;
               if N.Depth <= Continues'Last then
                  Continue_At (N.Depth) := not N.Last_Sibling;
               end if;
            end;
         end loop;
      end;

      --  The toolbar: back, forward, the field, the chapter steps, the
      --  translations.
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
            Fit_UI_Text ("Go to a passage (John 3:16) or search for words",
                         (if Field.w > 2 * PLACEHOLDER_INSET then Field.w - 2 * PLACEHOLDER_INSET else 0)),
            Colors.muted);
      end if;
      if Buttons_X > 0 then
         CuBit.UI.Widgets.Button
           (C, UI, Map, PREVIOUS_ID, (Buttons_X, Control_Y, CHAPTER_BUTTON_WIDTH, CONTROL_HEIGHT), Toolbar, Colors,
            "Previous", Result, retainedInput => True, quiet => True);
         CuBit.UI.Widgets.Button
           (C, UI, Map, NEXT_ID, (Buttons_X + CHAPTER_BUTTON_WIDTH + GAP, Control_Y, CHAPTER_BUTTON_WIDTH,
                                   CONTROL_HEIGHT), Toolbar, Colors,
            "Next", Result, retainedInput => True, quiet => True);
      end if;
      --  The words of Christ in red: a checkbox and its label.
      if Red_X > 0 then
         declare
            Checked : Boolean := State.Red;
            Box : constant Rect := (Red_X, Control_Y + (CONTROL_HEIGHT - CHECKBOX_EDGE) / 2, CHECKBOX_EDGE,
                                    CHECKBOX_EDGE);
         begin
            CuBit.UI.Widgets.Checkbox (C, UI, Map, RED_ID, Box, Toolbar, Colors, Checked, Result);
            Draw_UI_Text_Transparent
              (C, Red_X + CHECKBOX_EDGE + M.Space_S, Control_Y + (CONTROL_HEIGHT - UI_Text_Height) / 2,
               Fit_UI_Text ("Red letters", RED_WIDTH - CHECKBOX_EDGE - M.Space_S), Colors.text);
            if Checked /= State.Red then
               Set_Red_Letters (State, Checked);
               CuBit.UI.State.Request_Followup_Render (UI);
            end if;
         end;
      end if;
      --  The reading face: its name; a click is the next face offered.
      if Face_X > 0 and then CuBit.Reading_Faces.Count > 1 then
         CuBit.UI.Widgets.Button
           (C, UI, Map, FACE_ID, (Face_X, Control_Y, FACE_BUTTON_WIDTH, CONTROL_HEIGHT), Toolbar, Colors,
            Fit_UI_Text (Face_Name, FACE_BUTTON_WIDTH - 2 * PLACEHOLDER_INSET), Result,
            retainedInput => True, quiet => True);
      end if;
      if Tabs_X > 0 then
         for T in Translation loop
            declare
               Tab_Box : constant Rect :=
                 (Tabs_X + Translation'Pos (T) * (TAB_WIDTH + GAP), Control_Y, TAB_WIDTH, CONTROL_HEIGHT);
               Label : constant String := Bible_Translations.Abbreviation (T);
            begin
               CuBit.UI.Widgets.Tab
                 (C, UI, Map, FIRST_TAB_ID + Translation'Pos (T), Tab_Box, Toolbar, Colors, State.Which = T,
                  Tab_Content, Tab_Colors, Result);
               Draw_UI_Text_Transparent
                 (Tab_Content, Tab_Box.x + (TAB_WIDTH - UI_Text_Width (Label)) / 2,
                  Tab_Box.y + (CONTROL_HEIGHT - UI_Text_Height) / 2, Label, Tab_Colors.text);
            end;
         end loop;
      end if;

      Draw_Status_Bar (C, Status_Bar, Colors, Status_Text (State),
                       (if Ready (State) then Bible_Translations.Title (State.Which) & "  |  Public Domain"
                        else ""));
      CuBit.UI.State.Finish_Frame (UI);
   end Render;

   ---------------------------------------------------------------------------
   --  Events.
   ---------------------------------------------------------------------------
   procedure Activate_Node (State : in out View_State; Row : Positive) is
   begin
      if State.Tree_Stale then
         Rebuild_Nodes (State);
      end if;
      if Row > State.Node_Count then
         return;
      end if;
      declare
         N : constant Node := State.Nodes (Row);
      begin
         case N.Kind is
            when Testament_Node =>
               State.Testament_Open (N.Part) := not State.Testament_Open (N.Part);
            when Section_Node =>
               State.Section_Open (N.Group) := not State.Section_Open (N.Group);
            when Book_Node =>
               if N.Item = State.Item.From.Book then
                  State.Book_Open (N.Item) := not State.Book_Open (N.Item);
               else
                  for C in 1 .. Table (State).Chapters (N.Item) loop
                     if Verses_In (Table (State), N.Item, C) > 0 then
                        State.Chapter_Open := 0;
                        Open (State, Whole_Chapter (State, N.Item, C));
                        exit;
                     end if;
                  end loop;
               end if;
            when Chapter_Node =>
               if N.Item = State.Item.From.Book and then N.Chapter = State.Item.From.Chapter
                 and then not State.Highlight
               then
                  State.Chapter_Open := (if State.Chapter_Open = N.Chapter then 0 else N.Chapter);
               else
                  State.Chapter_Open := 0;
                  Open (State, Whole_Chapter (State, N.Item, N.Chapter));
               end if;
            when Verse_Node =>
               Open (State, ((N.Item, N.Chapter, N.Verse), (N.Item, N.Chapter, N.Verse)));
               State.Chapter_Open := N.Chapter;
         end case;
         State.Tree_Stale := True;
      end;
   end Activate_Node;

   procedure Handle
     (State : in out View_State; Item : Event; Map : in out Controls.Control_Map; Redraw : out Boolean)
   is
      Changed : Boolean := False;
      procedure Scroll_By (Delta_Pixels : Integer) is
      begin
         State.Scroll := Natural (Integer'Max (0, Integer'Min (Integer (Max_Scroll (State)),
                                                              Integer (State.Scroll) + Delta_Pixels)));
      end Scroll_By;
      procedure Field_Changed is
      begin
         if Changed then Edited (State); end if;
      end Field_Changed;
      procedure Back is
      begin
         if State.History_At > 1 then Go (State, State.History_At - 1); end if;
      end Back;
      procedure Forward is
      begin
         if State.History_At < State.History_Used then Go (State, State.History_At + 1); end if;
      end Forward;
      Page : constant Natural := Natural'Max (READING_LINE, State.Content.h - READING_LINE);
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
               Field_Changed;
            end if;
         when Key_Event =>
            case Item.Key is
               when Enter =>
                  if State.Current = Results and then State.Reference_Hint then
                     declare
                        Target : constant Passage := State.Hint;
                     begin
                        Clear_Field (State);
                        Open (State, Target);
                     end;
                  elsif State.Current = Results then
                     Edited (State, Whole_Words => True);
                  end if;
               when Escape =>
                  Clear_Field (State);
               when Backspace =>
                  Ed.Backspace (State.Search, Changed);
                  Field_Changed;
               when Delete =>
                  Ed.Delete_Forward (State.Search, Changed);
                  Field_Changed;
               when Left =>
                  if Item.Alt then Back;
                  else Ed.Move (State.Search, (if Item.Control then Ed.Move_Word_Left else Ed.Move_Left), Item.Shift);
                  end if;
               when Right =>
                  if Item.Alt then Forward;
                  else Ed.Move (State.Search, (if Item.Control then Ed.Move_Word_Right else Ed.Move_Right), Item.Shift);
                  end if;
               when Home =>
                  if Item.Control then State.Scroll := 0; else Ed.Move (State.Search, Ed.Move_Start, Item.Shift); end if;
               when End_Key =>
                  if Item.Control then State.Scroll := Max_Scroll (State);
                  else Ed.Move (State.Search, Ed.Move_End, Item.Shift);
                  end if;
               when Up => Scroll_By (-READING_LINE);
               when Down => Scroll_By (READING_LINE);
               when Page_Up =>
                  if Item.Control then Step_Chapter (State, Forward => False); else Scroll_By (-Integer (Page)); end if;
               when Page_Down =>
                  if Item.Control then Step_Chapter (State, Forward => True); else Scroll_By (Page); end if;
               when Tab =>
                  Set_Translation
                    (State, (if Item.Shift then (if State.Which = Translation'First then Translation'Last
                                                 else Translation'Pred (State.Which))
                             else (if State.Which = Translation'Last then Translation'First
                                   else Translation'Succ (State.Which))));
               when Next_Face_Key => Next_Face (State);
               when Red_Letters_Key => Set_Red_Letters (State, not State.Red);
               when No_Key => Redraw := False;
            end case;
         when Wheel =>
            if Point_In_Rect (State.Hover_X, State.Hover_Y, State.Tree_Box) then
               State.Tree_Scroll := Natural (Integer'Max (0, Integer (State.Tree_Scroll) - Item.Steps * WHEEL_LINES));
            else
               Scroll_By (-Item.Steps * WHEEL_LINES * READING_LINE);
            end if;
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
                        Back;
                     elsif Target = FORWARD_ID and then Controls.Take_Activated (Map, Target) then
                        Forward;
                     elsif Target = PREVIOUS_ID and then Controls.Take_Activated (Map, Target) then
                        Step_Chapter (State, Forward => False);
                     elsif Target = FACE_ID and then Controls.Take_Activated (Map, Target) then
                        Next_Face (State);
                     elsif Target = NEXT_ID and then Controls.Take_Activated (Map, Target) then
                        Step_Chapter (State, Forward => True);
                     elsif Target in FIRST_TAB_ID .. FIRST_TAB_ID + Translation'Pos (Translation'Last)
                       and then Controls.Take_Activated (Map, Target)
                     then
                        Set_Translation (State, Translation'Val (Target - FIRST_TAB_ID));
                     elsif Target >= FIRST_ROW_ID and then Controls.Take_Activated (Map, Target) then
                        Activate_Node (State, State.Tree_Scroll + Natural (Target - FIRST_ROW_ID) + 1);
                     elsif Link > 0 and then Link = State.Pressed_Link then
                        declare
                           O : constant Op := State.Ops (Link);
                        begin
                           if O.Target = 0 and then State.Reference_Hint then
                              declare
                                 Hint : constant Passage := State.Hint;
                              begin
                                 Clear_Field (State);
                                 Open (State, Hint);
                              end;
                           elsif O.Target in 1 .. State.Found then
                              declare
                                 Ref : Reference;
                                 Found : Boolean;
                                 E : Bible_Translations.Edition renames State.Editions (State.Which);
                              begin
                                 Images.Locate (E.Layout, E.Index, State.Hits (O.Target), Ref, Found);
                                 if Found then
                                    Open (State, (Ref, Ref));
                                 end if;
                              end;
                           end if;
                        end;
                     end if;
                     State.Pressed_Link := 0;
                  when Controls.Pointer_Move =>
                     Redraw := Link /= Was or else Target in PAGE_SCROLL_ID | TREE_SCROLL_ID | SPLIT_ID
                       or else Target >= FIRST_ROW_ID;
                  when Controls.Pointer_Cancel =>
                     State.Pressed_Link := 0;
               end case;
            end;
      end case;
   end Handle;

   ---------------------------------------------------------------------------
   --  Queries.
   ---------------------------------------------------------------------------
   function Mode (State : View_State) return Page_Mode is (State.Current);
   function Current_Translation (State : View_State) return Translation is (State.Which);
   function Shown (State : View_State) return Passage is (State.Item);
   function Heading (State : View_State) return String is
     (if State.Current = Reading and then Ready (State)
        and then State.Item.From.Chapter <= Table (State).Chapters (State.Item.From.Book)
      then Chapter_Title (State.Item.From.Book, State.Item.From.Chapter,
                          Table (State).Chapters (State.Item.From.Book) = 1)
      else "");
   function Status_Text (State : View_State) return String is
     (case State.Current is
         when Unavailable => Bible_Translations.Title (State.Which) & " unavailable",
         when Results =>
           (if State.Reference_Hint then Image (Table (State), State.Hint)
            elsif State.Searched then Trimmed (State.Total) & (if State.Total = 1 then " verse" else " verses")
            else "Not a reference"),
         when Reading => Image (Table (State), State.Item));
   function Result_Count (State : View_State) return Natural is (State.Found);
   function Result_Reference (State : View_State; Index : Positive) return Reference is
      Ref : Reference;
      Found : Boolean;
      E : Bible_Translations.Edition renames State.Editions (State.Which);
   begin
      if Index > State.Found or else not Ready (State) then
         return (others => <>);
      end if;
      Images.Locate (E.Layout, E.Index, State.Hits (Index), Ref, Found);
      return Ref;
   end Result_Reference;
   function Query (State : View_State) return String is (Ed.Content (State.Search));
   function Verse_Area (State : View_State; Verse : Verse_Number) return CuBit.UI.Rect is
   begin
      for I in 1 .. State.Op_Count loop
         declare
            O : Op renames State.Ops (I);
            Top : constant Integer := State.Content.y + O.Y - State.Scroll;
         begin
            if O.Style = Verse_Number_Style and then O.Verse = Verse then
               if Top < State.Content.y or else Top + READING_LINE > State.Content.y + State.Content.h then
                  return (others => 0);
               end if;
               return (State.Content.x + O.X, Natural (Top), O.W, READING_LINE);
            end if;
         end;
      end loop;
      return (others => 0);
   end Verse_Area;
   function Result_Area (State : View_State; Index : Positive) return CuBit.UI.Rect is
   begin
      for I in 1 .. State.Op_Count loop
         declare
            O : Op renames State.Ops (I);
            Top : constant Integer := State.Content.y + O.Y - State.Scroll;
         begin
            if O.Kind = Link_Op and then O.Target = Index then
               if Top < State.Content.y or else Top + Integer (O.H) > State.Content.y + State.Content.h then
                  return (others => 0);
               end if;
               return (State.Content.x + O.X, Natural (Top), O.W, O.H);
            end if;
         end;
      end loop;
      return (others => 0);
   end Result_Area;
   function Tree_Rows (State : View_State) return Natural is (State.Node_Count);
   function Tree_Row_Label (State : View_State; Row : Positive) return String is
     (if Row <= State.Node_Count then Node_Label (State.Nodes (Row)) else "");
   function Laid_Out_Lines (State : View_State) return Natural is (State.Lines);
   function Verse_Any_Red (State : View_State; Verse : Verse_Number) return Boolean is
     (for some I in 1 .. State.Op_Count =>
        State.Ops (I).Kind = Text_Op and then State.Ops (I).Verse = Verse and then State.Ops (I).Style = Red_Style);
   function Verse_All_Red (State : View_State; Verse : Verse_Number) return Boolean is
     (Verse_Any_Red (State, Verse)
      and then (for all I in 1 .. State.Op_Count =>
                  (if State.Ops (I).Kind = Text_Op and then State.Ops (I).Verse = Verse
                     and then State.Ops (I).Style in Body_Style | Red_Style
                   then State.Ops (I).Style = Red_Style)));
end Bible_Views;
