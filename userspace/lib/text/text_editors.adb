with Ada.Unchecked_Deallocation;
with CuBit.UI; use CuBit.UI;
with Text_Cursors;
with Text_Layout;
with Text_Marks;
with Text_UTF8;
with Text_Search;
use type Text_UTF8.Code_Point;

package body Text_Editors is
   use Text_Documents;
   use type Text_Syntax.Token_Kind;
   use type Text_Syntax.Language;
   use type Text_Marks.Mark_Kind;

   TEXT_PAD : constant := 6;
   GUTTER_PAD : constant := 10;
   MINIMUM_DIGITS : constant := 3;
   GLYPH_TOP : constant := 1;
   CARET_WIDTH : constant := 2;
   END_MARK_WIDTH : constant := 6;
   SCROLL_MARGIN_COLUMNS : constant := 4;
   WHEEL_ROWS : constant := 3;
   MINIMAP_LINE_HEIGHT : constant := 2;
   RULER_MARK_HEIGHT : constant := 3;
   RULER_SELECTIONS : constant := 2_000;
   FNV_OFFSET : constant Unsigned_64 := 16#CBF2_9CE4_8422_2325#;
   FNV_PRIME : constant Unsigned_64 := 16#0000_0100_0000_01B3#;

   --  A segment's rows: its place and where each row starts.
   type Segment_Rows is record
      Place : Segment_Place;
      Count : Positive := 1;
      Starts : Text_Layout.Row_Starts;
   end record;

   type Wrap_Entry is record
      Revision : Unsigned_64 := 0;
      Document : Text_Documents.Document_Access;
      Line : Line_Count := 0;
      Index : Natural := 0;
      Width : Column_Count := 0;
      Tab : Tab_Width := 1;
      Rows : aliased Segment_Rows;
   end record;
   --  A few segments: a frame walks forward while revealing the cursor
   --  walks back, and neither should evict the other's segment.
   CACHED_SEGMENTS : constant := 8;
   type Wrap_Entries is array (1 .. CACHED_SEGMENTS) of Wrap_Entry;
   type Wrap_Cache is record
      Entries : Wrap_Entries;
      Next : Positive range 1 .. CACHED_SEGMENTS := 1;
   end record;
   type Rows_Ref is access constant Segment_Rows;

   procedure Free is new Ada.Unchecked_Deallocation (Row_Array, Row_Array_Access);
   procedure Free_Cache is new Ada.Unchecked_Deallocation (Wrap_Cache, Wrap_Cache_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Editor, Editor_Access);


   function Palette_For (Colors : CuBit.UI.Theme; Dark : Boolean) return Palette is
      use Text_Syntax;
   begin
      if Dark then
         return (Background => Colors.field, Text => Colors.text, Gutter_Background => 16#1C242A#,
                 Gutter_Text => 16#6E7C86#, Gutter_Current => Colors.text, Current_Line => 16#28323A#,
                 Selection => 16#2F5A78#, Selection_Unfocused => 16#34444F#, Caret => 16#8CC8FF#,
                 Match => 16#5C4E1C#, Occurrence => 16#7A8C99#, Ruler_Background => 16#1C242A#, Ruler_View => 16#34424C#,
                 Ruler_Caret => 16#8CC8FF#, Error => 16#F07A6E#, Warning => 16#E8C15A#, Information => 16#7FB2FF#,
                 Invalid => 16#F07A6E#,
                 Tokens => [Plain => Colors.text, Keyword => 16#7FB2FF#, Type_Name => 16#5FD3C8#,
                            Number => 16#D7A6F2#, String_Text => 16#A5D98F#, Character_Text => 16#A5D98F#,
                            Comment => 16#7D8B96#, Preprocessor => 16#E3A27A#, Attribute => 16#F2B166#,
                            Operator => 16#C0C8CE#, Label => 16#E6C07B#, Heading => 16#7FB2FF#,
                            Emphasis => 16#D7A6F2#, Strong => 16#FF8FA3#, Code => 16#A5D98F#, Link => 16#8CC8FF#,
                            Quote => 16#93A1AB#, List_Marker => 16#F2B166#, Escape => 16#F2B166#]);
      else
         return (Background => Colors.field, Text => Colors.text, Gutter_Background => 16#F1F3F4#,
                 Gutter_Text => 16#98A2A8#, Gutter_Current => Colors.text, Current_Line => 16#FFF9E0#,
                 Selection => 16#C3DDF7#, Selection_Unfocused => 16#DDE3E8#, Caret => 16#1060C0#,
                 Match => 16#FFE680#, Occurrence => 16#8E9AA4#, Ruler_Background => 16#EEF1F3#, Ruler_View => 16#D5DCE1#,
                 Ruler_Caret => 16#1060C0#, Error => 16#D0302A#, Warning => 16#C98A00#, Information => 16#1A6FD0#,
                 Invalid => 16#D0302A#,
                 Tokens => [Plain => Colors.text, Keyword => 16#1D4FB8#, Type_Name => 16#0F7A72#,
                            Number => 16#9A3FB5#, String_Text => 16#2E7D32#, Character_Text => 16#2E7D32#,
                            Comment => 16#7D8790#, Preprocessor => 16#A0522D#, Attribute => 16#B85C00#,
                            Operator => 16#55606A#, Label => 16#8A5300#, Heading => 16#1D4FB8#,
                            Emphasis => 16#6B3FA0#, Strong => 16#B0003A#, Code => 16#2E7D32#, Link => 16#1060C0#,
                            Quote => 16#6A7680#, List_Marker => 16#B85C00#, Escape => 16#B85C00#]);
      end if;
   end Palette_For;

   function Create return Editor_Access is
      E : constant Editor_Access := new Editor;
   begin
      E.Rows := new Row_Array;
      E.Wrapped := new Wrap_Cache;
      return E;
   end Create;

   procedure Destroy (E : in out Editor_Access) is
   begin
      if E /= null then
         Free (E.Rows);
         Free_Cache (E.Wrapped);
         Free (E);
      end if;
   end Destroy;

   procedure Attach (E : in out Editor; D : Text_Documents.Document_Access) is
   begin
      E.Document := D;
      E.Top := 1;
      E.Top_Segment := 0;
      E.Top_Row := 1;
      E.Column := 0;
      E.Everything := True;
   end Attach;

   function Document_Of (E : Editor) return Text_Documents.Document_Access is (E.Document);

   procedure Set_Area (E : in out Editor; Area : CuBit.UI.Rect) is
   begin
      if Area /= E.Bounds then
         E.Bounds := Area;
         E.Everything := True;
      end if;
   end Set_Area;

   function Area (E : Editor) return CuBit.UI.Rect is (E.Bounds);
   procedure Set_Wrap (E : in out Editor; On : Boolean) is
   begin
      E.Wrap := On;
      E.Column := 0;
      E.Top_Segment := 0;
      E.Top_Row := 1;
      E.Everything := True;
   end Set_Wrap;
   function Wrap_On (E : Editor) return Boolean is (E.Wrap);
   procedure Set_Minimap (E : in out Editor; On : Boolean) is
   begin
      E.Minimap := On;
      E.Everything := True;
   end Set_Minimap;
   function Minimap_On (E : Editor) return Boolean is (E.Minimap);
   procedure Set_Tab (E : in out Editor; Tab : Tab_Width) is
   begin
      E.Tab := Tab;
      E.Everything := True;
   end Set_Tab;

   function Digits_Of (N : Line_Count) return Positive is
      Value : Line_Count := N;
      Result : Positive := 1;
   begin
      while Value >= 10 loop
         Value := Value / 10;
         Result := Result + 1;
      end loop;
      return Result;
   end Digits_Of;

   function Gutter_Width (E : Editor) return Natural is
     (Positive'Max (MINIMUM_DIGITS, Digits_Of (Line_Total (E.Document.all))) * CELL_WIDTH + GUTTER_PAD);
   function Side_Width (E : Editor) return Natural is
     (RULER_WIDTH + (if E.Minimap then MINIMAP_WIDTH else 0));
   function Text_X (E : Editor) return Natural is (E.Bounds.x + Gutter_Width (E) + TEXT_PAD);
   function Text_Width (E : Editor) return Natural is
     (Natural'Max (0, E.Bounds.w - Gutter_Width (E) - TEXT_PAD - Side_Width (E)));
   function Text_Columns (E : Editor) return Positive is
     (Positive'Max (1, Natural'Min (MAXIMUM_CELLS, Text_Width (E) / CELL_WIDTH)));
   function Page_Rows (E : Editor) return Positive is
     (Positive'Max (1, Natural'Min (MAXIMUM_ROWS, E.Bounds.h / ROW_HEIGHT)));
   function Visible_Slots (E : Editor) return Natural is
     (Natural'Min (MAXIMUM_ROWS, (E.Bounds.h + ROW_HEIGHT - 1) / ROW_HEIGHT));

   procedure Set_Show_Matches (E : in out Editor; On : Boolean) is
   begin
      E.Show_Matches := On;
   end Set_Show_Matches;

   function Settings (E : Editor) return Text_Documents.Layout_Settings is
     ((Tab => E.Tab, Wrap => (if E.Wrap then Text_Columns (E) else 0), Window_Columns => Text_Columns (E)));

   function Top_Line (E : Editor) return Line_Number is (E.Top);
   function First_Column (E : Editor) return Column_Count is (E.Column);
   function Rows_Painted (E : Editor) return Natural is (E.Painted);
   function Rows_Shown (E : Editor) return Natural is (E.Shown);
   function Guessed_Highlight (E : Editor) return Boolean is (E.Guessed);

   procedure Invalidate (E : in out Editor) is
   begin
      E.Everything := True;
   end Invalidate;

   ---------------------------------------------------------------------------
   --  Rows: (line, segment, row) in document order.

   type Row_Id is record
      Line : Line_Number := 1;
      Segment : Natural := 0;
      Row : Positive := 1;
   end record;

   function "<" (A, B : Row_Id) return Boolean is
     (A.Line < B.Line or else (A.Line = B.Line and then (A.Segment < B.Segment
                                                          or else (A.Segment = B.Segment and then A.Row < B.Row))));



   --  The rows of segment Index of line L, in the editor's cache (valid
   --  until the next call): no copy of the row table.
   function Rows_Of (E : Editor; L : Line_Number; Index : Natural) return Rows_Ref is
      D : Document renames E.Document.all;
      S : constant Layout_Settings := Settings (E);
      Width : constant Column_Count := Effective_Wrap (D, S, L);
      Cache : Wrap_Cache renames E.Wrapped.all;
   begin
      for K in Cache.Entries'Range loop
         declare
            C : Wrap_Entry renames Cache.Entries (K);
         begin
            if C.Revision = Revision (D) and then C.Document = E.Document and then C.Line = L
              and then C.Index = Index and then C.Width = Width and then C.Tab = S.Tab
            then
               return C.Rows'Access;
            end if;
         end;
      end loop;
      declare
         C : Wrap_Entry renames Cache.Entries (Cache.Next);
      begin
         Cache.Next := Cache.Next mod CACHED_SEGMENTS + 1;
         C.Rows.Place := Segment (D, L, Index);
         C.Rows.Starts (1) := 1;
         C.Rows.Count := 1;
         if Width > 0 then
            declare
               Text : constant String := Slice (D, C.Rows.Place.First, C.Rows.Place.Last);
            begin
               Text_Layout.Wrap (Text, Width, S.Tab, C.Rows.Starts, C.Rows.Count);
            end;
         end if;
         C.Revision := Revision (D);
         C.Document := E.Document;
         C.Line := L;
         C.Index := Index;
         C.Width := Width;
         C.Tab := S.Tab;
         return C.Rows'Access;
      end;
   end Rows_Of;

   procedure Row_Bounds (Rows : Segment_Rows; Row : Positive; First, Last : out Byte_Offset) is
      R : constant Positive := Positive'Min (Row, Rows.Count);
   begin
      First := Rows.Place.First + Byte_Count (Rows.Starts (R) - 1);
      Last := (if R < Rows.Count then Rows.Place.First + Byte_Count (Rows.Starts (R + 1) - 1) else Rows.Place.Last);
   end Row_Bounds;

   procedure Next_Row (E : Editor; Id : in out Row_Id; Rows : Segment_Rows; Found : out Boolean) is
      D : Document renames E.Document.all;
   begin
      Found := True;
      if Id.Row < Rows.Count then
         Id.Row := Id.Row + 1;
      elsif Id.Segment + 1 < Segment_Count (D, Id.Line) then
         Id := (Id.Line, Id.Segment + 1, 1);
      elsif Id.Line < Line_Total (D) then
         Id := (Id.Line + 1, 0, 1);
      else
         Found := False;
      end if;
   end Next_Row;

   procedure Previous_Row (E : Editor; Id : in out Row_Id; Found : out Boolean) is
      D : Document renames E.Document.all;
   begin
      Found := True;
      if Id.Row > 1 then
         Id.Row := Id.Row - 1;
      elsif Id.Segment > 0 then
         Id := (Id.Line, Id.Segment - 1, Rows_Of (E, Id.Line, Id.Segment - 1).Count);
      elsif Id.Line > 1 then
         declare
            Last_Segment : constant Natural := Segment_Count (D, Id.Line - 1) - 1;
         begin
            Id := (Id.Line - 1, Last_Segment, Rows_Of (E, Id.Line - 1, Last_Segment).Count);
         end;
      else
         Found := False;
      end if;
   end Previous_Row;

   function Top_Id (E : Editor) return Row_Id is
      D : Document renames E.Document.all;
      L : constant Line_Number := Line_Number'Min (E.Top, Line_Total (D));
      Segment_Index : constant Natural := Natural'Min (E.Top_Segment, Segment_Count (D, L) - 1);
   begin
      return (L, Segment_Index, E.Top_Row);
   end Top_Id;

   procedure Set_Top (E : in out Editor; Id : Row_Id) is
   begin
      E.Top := Id.Line;
      E.Top_Segment := Id.Segment;
      E.Top_Row := Id.Row;
   end Set_Top;

   --  The row holding Offset (a caret at a soft break belongs to the later row).
   function Row_Of_Offset (E : Editor; Offset : Byte_Offset) return Row_Id is
      D : Document renames E.Document.all;
      Place : constant Segment_Place := Segment_Of (D, Offset);
      Rows : constant Rows_Ref := Rows_Of (E, Place.Line, Place.Index);
      Within : constant Natural := Natural (Offset - Byte_Offset'Min (Offset, Place.First)) + 1;
      R : Positive := 1;
   begin
      while R < Rows.Count and then Rows.Starts (R + 1) <= Within loop
         R := R + 1;
      end loop;
      return (Place.Line, Place.Index, R);
   end Row_Of_Offset;

   procedure Scroll_Rows (E : in out Editor; Count : Integer) is
      Id : Row_Id := Top_Id (E);
      Found : Boolean := True;
   begin
      if Count > 0 then
         --  Not past the point where the last row is at the bottom.
         for Step in 1 .. Count loop
            declare
               Probe : Row_Id := Id;
               Ahead : Natural := 0;
            begin
               --  Stop when fewer than a page of rows remains below.
               while Ahead < Page_Rows (E) loop
                  Next_Row (E, Probe, Rows_Of (E, Probe.Line, Probe.Segment).all, Found);
                  exit when not Found;
                  Ahead := Ahead + 1;
               end loop;
               exit when Ahead < Page_Rows (E);
               Next_Row (E, Id, Rows_Of (E, Id.Line, Id.Segment).all, Found);
               exit when not Found;
            end;
         end loop;
      else
         for Step in 1 .. -Count loop
            Previous_Row (E, Id, Found);
            exit when not Found;
         end loop;
      end if;
      Set_Top (E, Id);
   end Scroll_Rows;

   procedure Scroll_To_Line (E : in out Editor; L : Line_Number) is
      Target : constant Line_Number := Line_Number'Min (L, Line_Total (E.Document.all));
      Third : constant Line_Count := Line_Count (Page_Rows (E) / 3);
   begin
      Set_Top (E, ((if Target > Third then Target - Third else 1), 0, 1));
   end Scroll_To_Line;

   procedure Reveal_Primary (E : in out Editor) is
      D : Document renames E.Document.all;
      Head : constant Byte_Offset := Primary (D).Head;
      Target : constant Row_Id := Row_Of_Offset (E, Head);
      Top : constant Row_Id := Top_Id (E);
      Found : Boolean := True;
   begin
      if Target < Top then
         Set_Top (E, Target);
      else
         --  The last row shown, a page below the top.
         declare
            Bottom : Row_Id := Top;
         begin
            for Step in 1 .. Page_Rows (E) - 1 loop
               exit when Bottom = Target;
               Next_Row (E, Bottom, Rows_Of (E, Bottom.Line, Bottom.Segment).all, Found);
               exit when not Found;
            end loop;
            if Bottom < Target then
               declare
                  New_Top : Row_Id := Target;
               begin
                  for Step in 1 .. Page_Rows (E) - 1 loop
                     Previous_Row (E, New_Top, Found);
                     exit when not Found;
                  end loop;
                  Set_Top (E, New_Top);
               end;
            end if;
         end;
      end if;
      --  Columns, without wrap.
      if not E.Wrap and then Effective_Wrap (D, Settings (E), Target.Line) = 0 then
         declare
            Start : constant Byte_Offset := Line_Start (D, Target.Line);
            Text : constant String := Slice (D, Start, Head);
            Column : constant Column_Count :=
              Text_Layout.Column_Of (Text, Text'Last + 1, E.Tab);
            Columns : constant Positive := Text_Columns (E);
         begin
            if Column < E.Column then
               E.Column := (if Column > SCROLL_MARGIN_COLUMNS then Column - SCROLL_MARGIN_COLUMNS else 0);
            elsif Column + SCROLL_MARGIN_COLUMNS >= E.Column + Columns then
               E.Column := Column + SCROLL_MARGIN_COLUMNS - Columns + 1;
            end if;
         end;
      end if;
   end Reveal_Primary;

   ---------------------------------------------------------------------------
   --  Building a row's cells.

   function Intersects (A, B : Rect) return Boolean is
     (not Is_Empty (A) and then not Is_Empty (B) and then A.x < B.x + B.w and then B.x < A.x + A.w
      and then A.y < B.y + B.h and then B.y < A.y + A.h);

   function Mix (H : Unsigned_64; V : Unsigned_64) return Unsigned_64 is ((H xor V) * FNV_PRIME);

   --  A row's hash: everything its pixels depend on.
   procedure Rehash (E : Editor; R : in out Row_View) is
      H : Unsigned_64 := FNV_OFFSET;
   begin
      H := Mix (H, Unsigned_64 (R.Line) * Boolean'Pos (R.Lead));
      H := Mix (H, Boolean'Pos (R.Lead) + 2 * Boolean'Pos (R.Current) + 4 * Boolean'Pos (R.Caret_After)
                   + 8 * Boolean'Pos (R.Selected_End) + 16 * Boolean'Pos (E.Focused));
      H := Mix (H, Unsigned_64 (R.Cells));
      for K in 1 .. R.Cells loop
         declare
            F : constant Cell_Flags := R.Flags (K);
         begin
            H := Mix (H, Unsigned_64 (Character'Pos (R.Chars (K)))
                         + 256 * Unsigned_64 (Text_Syntax.Token_Kind'Pos (R.Kinds (K)))
                         + 65_536 * Unsigned_64 (Boolean'Pos (F.Selected) + 2 * Boolean'Pos (F.Match)
                                                 + 4 * Boolean'Pos (F.Caret) + 8 * Boolean'Pos (F.Boxed)
                                                 + 16 * F.Squiggle + 64 * Boolean'Pos (F.Occurrence)));
         end;
      end loop;
      --  Never 0 or BLANK_ROW (the buffer records' markers).
      R.Hash := (if H = 0 or else H = Unsigned_64'Last then 1 else H);
   end Rehash;

   procedure Build_Row
     (E : in out Editor; Slot : Row_Slot; Id : Row_Id; Rows : Segment_Rows; Primary_Line : Line_Number;
      Selection_Start : Positive)
   is
      D : Document renames E.Document.all;
      R : Row_View renames E.Rows (Slot);
      First, Last : Byte_Offset;
      Skip : constant Column_Count := (if E.Wrap or else Rows.Count > 1 or else Id.Segment > 0 then 0 else E.Column);
      Columns : constant Positive := Text_Columns (E);
      Spans : Text_Syntax.Span_Array;
      Span_Count : Text_Syntax.Span_Count := 0;
      Exact : Boolean := True;
      Span_Base : Natural := 0;   --  the row's start, from its segment's first byte
   begin
      Row_Bounds (Rows, Id.Row, First, Last);
      R.Line := Id.Line;
      R.Segment := Id.Segment;
      R.Row := Id.Row;
      R.First := First;
      R.Last := Last;
      R.Lead := Id.Segment = 0 and then Id.Row = 1;
      R.Final := Id.Row = Rows.Count and then Id.Segment + 1 >= Segment_Count (D, Id.Line);
      R.Current := Id.Line = Primary_Line;
      R.Cells := 0;
      R.Caret_After := False;
      R.Selected_End := False;
      if Id.Segment = 0 then
         declare
            Whole : constant String := Slice (D, Rows.Place.First, Rows.Place.Last);
         begin
            Line_Spans (D, Id.Line, Whole, Spans, Span_Count, Exact);
            if not Exact then
               E.Guessed := True;
            end if;
            Span_Base := Natural (First - Rows.Place.First);
         end;
      end if;
      declare
         Bytes : constant String := Slice (D, First, Last);
         I : Positive := 1;
         Column : Column_Count := 0;
         Span : Natural := 1;
         Selection_Index : Natural := Selection_Start;
         Count : constant Natural := Selection_Count (D);
         Kind : Text_Syntax.Token_Kind;
      begin
         while I <= Bytes'Length loop
            declare
               --  Printable ASCII before a byte that cannot extend it (a
               --  combining mark or joiner is never ASCII): one cell, one
               --  byte, without decoding.
               Plain_Byte : constant Boolean := Bytes (I) in ' ' .. '~'
                 and then (I = Bytes'Length or else Character'Pos (Bytes (I + 1)) < 16#80#);
               Size : constant Positive := (if Plain_Byte then 1 else Text_UTF8.Cluster_Size (Bytes, I));
               Offset : constant Byte_Offset := First + Byte_Count (I - 1);
               Code : Text_UTF8.Code_Point;
               Cells : Natural;
               Valid : Boolean;
               Shown_Char : Character;
               Boxed : Boolean := False;
               Selected : Boolean := False;
            begin
               if Plain_Byte then
                  Cells := 1;
                  Shown_Char := Bytes (I);
               elsif Bytes (I) = ASCII.HT then
                  Cells := E.Tab - Column mod E.Tab;
                  Shown_Char := ' ';
               else
                  Text_UTF8.Cluster_Face (Bytes, I, Code, Cells, Valid);
                  if not Valid then
                     Boxed := True;
                     Shown_Char := ' ';
                  elsif Code < 16#20# or else Code = 16#7F# then
                     Boxed := True;
                     Shown_Char := ' ';
                  elsif Code <= 16#FF# then
                     Shown_Char := Character'Val (Natural (Code));
                  else
                     Boxed := True;
                     Shown_Char := ' ';
                  end if;
               end if;
               --  Its token.
               Kind := Text_Syntax.Plain;
               while Span <= Span_Count
                 and then Spans (Span).First + Spans (Span).Length - 1 < Span_Base + I
               loop
                  Span := Span + 1;
               end loop;
               if Span <= Span_Count and then Spans (Span).First <= Span_Base + I then
                  Kind := Spans (Span).Kind;
               end if;
               --  Selected when a selection covers its first byte.
               while Selection_Index <= Count
                 and then Text_Cursors.Last_Of (Selection (D, Selection_Index)) <= Offset
               loop
                  Selection_Index := Selection_Index + 1;
               end loop;
               if Selection_Index <= Count then
                  Selected := Text_Cursors.First_Of (Selection (D, Selection_Index)) <= Offset;
               end if;
               for J in 0 .. Cells - 1 loop
                  if Column + J >= Skip and then Column + J - Skip < Columns then
                     declare
                        K : constant Cell_Index := Column + J - Skip + 1;
                     begin
                        R.Chars (K) := (if J = 0 then Shown_Char else ' ');
                        R.Kinds (K) := Kind;
                        R.Flags (K) := (Selected => Selected, Boxed => Boxed and then J = 0, others => <>);
                        R.Cells := Natural'Max (R.Cells, K);
                     end;
                  end if;
               end loop;
               Column := Column + Cells;
               I := I + Size;
            end;
         end loop;
      end;
      --  Carets: each selection's head within the row.
      for K in Selection_Start .. Selection_Count (D) loop
         declare
            S : constant Text_Cursors.Selection := Selection (D, K);
         begin
            exit when Text_Cursors.First_Of (S) > Last;
            if S.Head >= First and then (S.Head < Last or else (S.Head = Last and then (R.Final or else Last = First)))
            then
               if S.Head = Last then
                  R.Caret_After := True;
               else
                  declare
                     Text : constant String := Slice (D, First, S.Head);
                     Column : constant Column_Count := Text_Layout.Column_Of (Text, Text'Last + 1, E.Tab);
                  begin
                     if Column >= Skip and then Column - Skip < Columns then
                        R.Flags (Column - Skip + 1).Caret := True;
                     end if;
                  end;
               end if;
            end if;
            if R.Final and then Text_Cursors.First_Of (S) <= Last and then Text_Cursors.Last_Of (S) > Last then
               R.Selected_End := True;
            end if;
         end;
      end loop;
      --  A caret after the last cell, in view.
      if R.Caret_After then
         declare
            Text : constant String := Slice (D, First, Last);
            Column : constant Column_Count := Text_Layout.Column_Of (Text, Text'Last + 1, E.Tab);
         begin
            if Column < Skip or else Column - Skip >= Columns then
               R.Caret_After := False;
            elsif Column - Skip < R.Cells then
               R.Flags (Column - Skip + 1).Caret := True;
               R.Caret_After := False;
            elsif Column - Skip > R.Cells then
               --  Pad to the caret (a line shorter than the scroll).
               for K in R.Cells + 1 .. Column - Skip loop
                  R.Chars (K) := ' ';
                  R.Kinds (K) := Text_Syntax.Plain;
                  R.Flags (K) := (others => <>);
               end loop;
               R.Cells := Column - Skip;
            end if;
         end;
      end if;
      --  Search matches.
      --  Occurrences of the identifier under the cursor (whole tokens).
      if E.Word_Length > 0 and then Last > First then
         declare
            Bytes : constant String := Slice (D, First, Last);
            Word_Settings : constant Text_Search.Options :=
              (Case_Sensitive => not E.Word_Folded, Whole_Word => True, Regular => False);
            From : Positive := 1;
            Found : Boolean;
            At_Index : Positive;
         begin
            loop
               exit when From > Bytes'Length;
               Text_Search.Find_Literal (Bytes, E.Word (1 .. E.Word_Length), From, Word_Settings, ' ', ' ',
                                         Found, At_Index);
               exit when not Found;
               declare
                  From_Column : constant Column_Count := Text_Layout.Column_Of (Bytes, At_Index, E.Tab);
                  To_Column : constant Column_Count :=
                    Text_Layout.Column_Of (Bytes, Natural'Min (At_Index + E.Word_Length, Bytes'Last + 1), E.Tab);
               begin
                  for C in From_Column .. To_Column - 1 loop
                     if C >= Skip and then C - Skip < R.Cells then
                        R.Flags (C - Skip + 1).Occurrence := True;
                     end if;
                  end loop;
               end;
               From := At_Index + E.Word_Length;
            end loop;
         end;
      end if;
      if E.Show_Matches and then Search_Pattern (D)'Length > 0 and then Last > First then
         declare
            Bytes : constant String := Slice (D, First, Last);
            procedure Visit (Match_First : Positive; Match_Length : Natural) is
               From : constant Column_Count := Text_Layout.Column_Of (Bytes, Match_First, E.Tab);
               To : constant Column_Count :=
                 Text_Layout.Column_Of (Bytes, Natural'Min (Match_First + Match_Length, Bytes'Last + 1), E.Tab);
            begin
               for C in From .. To - 1 loop
                  if C >= Skip and then C - Skip < R.Cells then
                     R.Flags (C - Skip + 1).Match := True;
                  end if;
               end loop;
            end Visit;
            procedure Each is new Matches_In (Visit);
         begin
            Each (D, Bytes);
         end;
      end if;
      --  Diagnostics.
      for K in 1 .. Mark_Count (D) loop
         declare
            M : constant Text_Marks.Mark := Mark (D, K);
         begin
            exit when M.First > Last;
            if M.Kind = Text_Marks.Diagnostic and then M.Last > First and then M.First < Last then
               declare
                  Bytes : constant String := Slice (D, First, Last);
                  From : constant Column_Count :=
                    Text_Layout.Column_Of (Bytes, Natural (Byte_Offset'Max (M.First, First) - First) + 1, E.Tab);
                  To : constant Column_Count :=
                    Text_Layout.Column_Of (Bytes, Natural (Byte_Offset'Min (M.Last, Last) - First) + 1, E.Tab);
               begin
                  for C in From .. Column_Count'Max (From + 1, To) - 1 loop
                     if C >= Skip and then C - Skip < R.Cells then
                        R.Flags (C - Skip + 1).Squiggle := Text_Marks.Severity'Pos (M.Level);
                     end if;
                  end loop;
               end;
            end if;
         end;
      end loop;
      Rehash (E, R);
   end Build_Row;

   --  The first selection that ends at or after Offset (they are sorted).
   function First_Selection_From (D : Document; Offset : Byte_Offset) return Positive is
      Low : Positive := 1;
      High : Natural := Selection_Count (D);
   begin
      while Low <= High loop
         declare
            Middle : constant Positive := (Low + High) / 2;
         begin
            if Text_Cursors.Last_Of (Selection (D, Middle)) < Offset then
               Low := Middle + 1;
            else
               High := Middle - 1;
            end if;
         end;
      end loop;
      return Low;
   end First_Selection_From;

   procedure Clear_Occurrence (E : in out Editor) is
   begin
      E.Word_Length := 0;
   end Clear_Occurrence;

   procedure Refresh_Occurrence (E : in out Editor) is
      D : Document renames E.Document.all;
   begin
      E.Word_Length := 0;
      E.Word_Folded := Language (D) = Text_Syntax.Ada_Source;
      declare
         P : constant Text_Cursors.Selection := Primary (D);
      begin
         if Text_Cursors.Is_Empty (P) and then Selection_Count (D) = 1 then
            declare
               Place : constant Segment_Place := Segment_Of (D, P.Head);
               Text : constant String := Slice (D, Place.First, Place.Last);
               Here : constant Natural := Natural (P.Head - Place.First);
               function Ident (C : Character) return Boolean is (C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_');
               Low : Natural := Here;
               High : Natural := Here;
            begin
               while Low >= 1 and then Ident (Text (Low)) loop
                  Low := Low - 1;
               end loop;
               while High < Text'Last and then Ident (Text (High + 1)) loop
                  High := High + 1;
               end loop;
               if High - Low in 2 .. E.Word'Length and then Text (Low + 1) not in '0' .. '9' then
                  E.Word_Length := High - Low;
                  E.Word (1 .. E.Word_Length) := Text (Low + 1 .. High);
               end if;
            end;
         end if;
      end;
   end Refresh_Occurrence;

   function Row_Width (R : Row_View) return Content_Width is
     (Content_Width'Min (Content_Width'Last,
        R.Cells * CELL_WIDTH + (if R.Selected_End then END_MARK_WIDTH elsif R.Caret_After then CARET_WIDTH else 0)));
   --  Drawn over the current line's background (which spans the row).
   function Is_Lit (E : Editor; R : Row_View) return Boolean is (R.Current and then E.Focused);

   procedure Prepare (E : in out Editor; Colors : Palette; Focused : Boolean; Damage : out CuBit.UI.Rect) is
      D : Document renames E.Document.all;
      Id : Row_Id := Top_Id (E);
      Found : Boolean := True;
      Slots : constant Natural := Visible_Slots (E);
      Primary_Line : constant Line_Number := Line_Of (D, Primary (D).Head);
      Chrome : Unsigned_64 := FNV_OFFSET;
   begin
      if Colors /= E.Colors or else Focused /= E.Focused then
         E.Everything := True;
      end if;
      E.Colors := Colors;
      E.Focused := Focused;
      E.Guessed := False;
      E.Shown := 0;
      Set_Top (E, Id);
      if Slots > 0 then
         declare
            Rows : Rows_Ref := Rows_Of (E, Id.Line, Id.Segment);
            First_Offset : Byte_Offset;
            Ignored : Byte_Offset;
            Start : Positive;
         begin
            Row_Bounds (Rows.all, Id.Row, First_Offset, Ignored);
            Start := First_Selection_From (D, First_Offset);
            for Slot in 1 .. Slots loop
               --  Build_Row may wrap other segments (spans read the first):
               --  take the rows again from the cache when needed.
               Rows := Rows_Of (E, Id.Line, Id.Segment);
               Build_Row (E, Slot, Id, Rows.all, Primary_Line, Start);
               E.Shown := Slot;
               --  Move the selection cursor on past this row.
               while Start <= Selection_Count (D)
                 and then Text_Cursors.Last_Of (Selection (D, Start)) < E.Rows (Slot).Last
               loop
                  Start := Start + 1;
               end loop;
               Next_Row (E, Id, Rows.all, Found);
               exit when not Found;
               Rows := Rows_Of (E, Id.Line, Id.Segment);
            end loop;
         end;
      end if;
      --  Occurrences show only when the word is there at least twice in
      --  view: an outline around the lone word under the caret is noise.
      declare
         Runs : Natural := 0;
      begin
         for Slot in 1 .. E.Shown loop
            for K in 1 .. E.Rows (Slot).Cells loop
               if E.Rows (Slot).Flags (K).Occurrence and then (K = 1 or else not E.Rows (Slot).Flags (K - 1).Occurrence)
               then
                  Runs := Runs + 1;
               end if;
            end loop;
            exit when Runs >= 2;
         end loop;
         if Runs = 1 then
            for Slot in 1 .. E.Shown loop
               declare
                  R : Row_View renames E.Rows (Slot);
                  Had : Boolean := False;
               begin
                  for K in 1 .. R.Cells loop
                     Had := Had or else R.Flags (K).Occurrence;
                     R.Flags (K).Occurrence := False;
                  end loop;
                  if Had then
                     Rehash (E, R);
                  end if;
               end;
            end loop;
         end if;
      end;
      --  The gutter's width, the ruler and the minimap follow the document
      --  and the view: any change repaints them (and the rows, when the
      --  gutter's width moves the text).
      Chrome := Mix (Chrome, Unsigned_64 (Gutter_Width (E)));
      if Chrome /= E.Previous_Chrome then
         E.Everything := True;
      end if;
      E.Chrome_Hash := Chrome;
      --  Which slots changed.
      Damage := (others => 0);
      for Slot in 1 .. Natural'Max (E.Shown, E.Previous_Shown) loop
         declare
            Hash : constant Unsigned_64 := (if Slot <= E.Shown then E.Rows (Slot).Hash else 0);
            Width : constant Content_Width := (if Slot <= E.Shown then Row_Width (E.Rows (Slot)) else 0);
            Lit : constant Boolean := Slot <= E.Shown and then Is_Lit (E, E.Rows (Slot));
            Row_Left : constant Natural := Text_X (E) - E.Bounds.x;
            Whole : constant Natural := Natural'Max (0, E.Bounds.w - Side_Width (E));
         begin
            if E.Everything or else Slot > E.Previous_Shown or else Hash /= E.Previous (Slot) then
               E.Dirty (Slot) := 1;
               declare
                  Full : constant Boolean := E.Everything or else Slot > E.Previous_Shown or else Slot > E.Shown
                    or else Lit or else E.Previous_Lit (Slot);
                  Paint : constant Content_Width := Content_Width'Max (Width, E.Previous_Width (Slot));
               begin
                  Damage := Union_Rect (Damage, (E.Bounds.x, E.Bounds.y + (Slot - 1) * ROW_HEIGHT,
                                                 (if Full then Whole else Natural'Min (Whole, Row_Left + Paint)),
                                                 ROW_HEIGHT));
               end;
            else
               E.Dirty (Slot) := 0;
            end if;
         end;
      end loop;
      if E.Everything then
         Damage := E.Bounds;
      elsif not Is_Empty (Damage) or else E.Previous_Shown /= E.Shown then
         --  The ruler and minimap show the view and the cursors.
         Damage := Union_Rect (Damage, (E.Bounds.x + E.Bounds.w - Side_Width (E), E.Bounds.y, Side_Width (E),
                                        E.Bounds.h));
      end if;
   end Prepare;

   ---------------------------------------------------------------------------
   --  Drawing.

   --  Over the gutter's background: opaque, cached glyph cells.
   procedure Draw_Number (C : Canvas; Right, Y : Natural; Value : Line_Count; Ink, Under : Color) is
      Image : constant String := Line_Count'Image (Value);
      Text : constant String := Image (Image'First + 1 .. Image'Last);
   begin
      Draw_Code_Text (C, Right - Text'Length * CELL_WIDTH, Y + GLYPH_TOP, Text, Ink, Under);
   end Draw_Number;

   --  Full: the whole row; otherwise up to Paint past the text's left edge
   --  (the rest of the row is background in the target already).
   procedure Draw_Row (E : Editor; C : Canvas; Slot : Row_Slot; Full : Boolean; Paint : Content_Width) is
      R : Row_View renames E.Rows (Slot);
      P : Palette renames E.Colors;
      D : Document renames E.Document.all;
      Y : constant Natural := E.Bounds.y + (Slot - 1) * ROW_HEIGHT;
      Gutter : constant Natural := Gutter_Width (E);
      X0 : constant Natural := Text_X (E);
      Text_Box : constant Rect := (E.Bounds.x + Gutter, Y, Natural'Max (0, E.Bounds.w - Gutter - Side_Width (E)), ROW_HEIGHT);
      Inner : constant Canvas := With_Clip (C, Text_Box);
      Background : constant Color := (if R.Current and then E.Focused then P.Current_Line else P.Background);
   begin
      Fill_Rect (C, (E.Bounds.x, Y, Gutter, ROW_HEIGHT), P.Gutter_Background);
      if R.Lead then
         Draw_Number (C, E.Bounds.x + Gutter - GUTTER_PAD / 2, Y, R.Line,
                      (if R.Current then P.Gutter_Current else P.Gutter_Text), P.Gutter_Background);
         --  Gutter annotations on this line.
         for K in 1 .. Mark_Count (D) loop
            declare
               M : constant Text_Marks.Mark := Mark (D, K);
            begin
               exit when M.First > R.Last + 1;
               if M.Kind = Text_Marks.Annotation and then M.First >= R.First and then M.First <= R.Last then
                  Fill_Rect (C, (E.Bounds.x + 2, Y + ROW_HEIGHT / 2 - 3, 6, 6),
                             (case M.Glyph is when Text_Marks.Breakpoint => P.Error,
                                              when Text_Marks.Handler_Link => P.Information,
                                              when others => P.Warning));
               elsif M.Kind = Text_Marks.Diagnostic and then M.First >= R.First and then M.First <= R.Last then
                  Fill_Rect (C, (E.Bounds.x + 2, Y + 3, 3, ROW_HEIGHT - 6),
                             (case M.Level is when Text_Marks.Error => P.Error, when Text_Marks.Warning => P.Warning,
                                              when others => P.Information));
               end if;
            end;
         end loop;
      end if;
      if Full then
         Fill_Rect (C, Text_Box, Background);
      else
         --  Right of the old and new content the row is background already.
         Fill_Rect (C, (Text_Box.x, Y, Natural'Min (Text_Box.w, X0 - Text_Box.x + Paint), ROW_HEIGHT),
                    Background);
      end if;
      --  Backgrounds: matches, then selections over them.
      for K in 1 .. R.Cells loop
         if R.Flags (K).Match and then not R.Flags (K).Selected then
            Fill_Rect (Inner, (X0 + (K - 1) * CELL_WIDTH, Y + 1, CELL_WIDTH, ROW_HEIGHT - 2), P.Match);
         elsif R.Flags (K).Selected then
            Fill_Rect (Inner, (X0 + (K - 1) * CELL_WIDTH, Y, CELL_WIDTH, ROW_HEIGHT),
                       (if E.Focused then P.Selection else P.Selection_Unfocused));
         end if;
      end loop;
      if R.Selected_End then
         Fill_Rect (Inner, (X0 + R.Cells * CELL_WIDTH, Y, END_MARK_WIDTH, ROW_HEIGHT),
                    (if E.Focused then P.Selection else P.Selection_Unfocused));
      end if;
      --  Text in runs of one ink over one background, drawn opaque: the
      --  toolkit copies pre-blended glyph cells (its composite cache)
      --  instead of blending every pixel. Each cell's background is what
      --  was filled under it above.
      declare
         function Under (K : Positive) return Color is
           (if R.Flags (K).Selected then (if E.Focused then P.Selection else P.Selection_Unfocused)
            elsif R.Flags (K).Match then P.Match
            else Background);
         K : Natural := 1;
      begin
         while K <= R.Cells loop
            declare
               Kind : constant Text_Syntax.Token_Kind := R.Kinds (K);
               Ground : constant Color := Under (K);
               Stop : Natural := K;
            begin
               while Stop < R.Cells and then R.Kinds (Stop + 1) = Kind and then Under (Stop + 1) = Ground loop
                  Stop := Stop + 1;
               end loop;
               declare
                  Run : constant String := String (R.Chars (K .. Stop));
                  Blank : Boolean := True;
               begin
                  for Ch of Run loop
                     if Ch /= ' ' then
                        Blank := False;
                        exit;
                     end if;
                  end loop;
                  if not Blank then
                     Draw_Code_Text (Inner, X0 + (K - 1) * CELL_WIDTH, Y + GLYPH_TOP, Run, P.Tokens (Kind), Ground);
                  end if;
               end;
               K := Stop + 1;
            end;
         end loop;
      end;
      --  Occurrences of the word under the cursor: an outline around each
      --  run, never a fill (fills are selections and search matches).
      declare
         function Outlined (K : Positive) return Boolean is
           (R.Flags (K).Occurrence and then not R.Flags (K).Selected and then not R.Flags (K).Match);
         K : Natural := 1;
      begin
         while K <= R.Cells loop
            if Outlined (K) then
               declare
                  Stop : Natural := K;
               begin
                  while Stop < R.Cells and then Outlined (Stop + 1) loop
                     Stop := Stop + 1;
                  end loop;
                  Stroke_Rect (Inner, (X0 + (K - 1) * CELL_WIDTH, Y, (Stop - K + 1) * CELL_WIDTH, ROW_HEIGHT),
                               P.Occurrence, P.Occurrence);
                  K := Stop + 1;
               end;
            else
               K := K + 1;
            end if;
         end loop;
      end;
      --  Boxes, squiggles and carets.
      for K in 1 .. R.Cells loop
         declare
            X : constant Natural := X0 + (K - 1) * CELL_WIDTH;
            F : constant Cell_Flags := R.Flags (K);
         begin
            if F.Boxed then
               Stroke_Rect (Inner, (X + 1, Y + 3, CELL_WIDTH - 2, ROW_HEIGHT - 6), P.Invalid, P.Invalid);
            end if;
            if F.Squiggle > 0 then
               declare
                  Ink : constant Color :=
                    (case F.Squiggle is when 3 => P.Error, when 2 => P.Warning, when others => P.Information);
               begin
                  for DX in 0 .. CELL_WIDTH - 1 loop
                     Fill_Rect (Inner, (X + DX, Y + ROW_HEIGHT - 3 + ((X + DX) / 2) mod 2, 1, 1), Ink);
                  end loop;
               end;
            end if;
            if F.Caret then
               Fill_Rect (Inner, (X, Y + 1, CARET_WIDTH, ROW_HEIGHT - 2),
                          (if E.Focused then P.Caret else P.Gutter_Text));
            end if;
         end;
      end loop;
      if R.Caret_After then
         Fill_Rect (Inner, (X0 + R.Cells * CELL_WIDTH, Y + 1, CARET_WIDTH, ROW_HEIGHT - 2),
                    (if E.Focused then P.Caret else P.Gutter_Text));
      end if;
   end Draw_Row;

   procedure Draw_Side (E : Editor; C : Canvas) is
      P : Palette renames E.Colors;
      D : Document renames E.Document.all;
      Ruler : constant Rect := (E.Bounds.x + E.Bounds.w - RULER_WIDTH, E.Bounds.y, RULER_WIDTH, E.Bounds.h);
      Lines : constant Line_Count := Line_Total (D);
      function Y_Of (L : Line_Count) return Natural is
        (Ruler.y + Natural (Long_Long_Integer (L - 1) * Long_Long_Integer (Natural'Max (1, Ruler.h - RULER_MARK_HEIGHT))
                            / Long_Long_Integer (Line_Count'Max (1, Lines))));
      Last_Shown : constant Line_Count := (if E.Shown > 0 then E.Rows (E.Shown).Line else E.Top);
   begin
      Fill_Rect (C, Ruler, P.Ruler_Background);
      Fill_Rect (C, (Ruler.x, Ruler.y, 1, Ruler.h), P.Gutter_Text);
      declare
         Top_Y : constant Natural := Y_Of (E.Top);
         Bottom_Y : constant Natural := Natural'Max (Y_Of (Last_Shown + 1), Top_Y + RULER_MARK_HEIGHT);
      begin
         Fill_Rect (C, (Ruler.x + 1, Top_Y, RULER_WIDTH - 1, Bottom_Y - Top_Y), P.Ruler_View);
      end;
      for K in 1 .. Mark_Count (D) loop
         declare
            M : constant Text_Marks.Mark := Mark (D, K);
         begin
            if M.Kind = Text_Marks.Diagnostic then
               Fill_Rect (C, (Ruler.x + RULER_WIDTH / 2, Y_Of (Line_Of (D, M.First)), RULER_WIDTH / 2, RULER_MARK_HEIGHT),
                          (case M.Level is when Text_Marks.Error => P.Error, when Text_Marks.Warning => P.Warning,
                                           when others => P.Information));
            end if;
         end;
      end loop;
      declare
         Count : constant Natural := Selection_Count (D);
         Step : constant Positive := Positive'Max (1, Count / RULER_SELECTIONS);
         K : Natural := 1;
      begin
         while K <= Count loop
            Fill_Rect (C, (Ruler.x + 2, Y_Of (Line_Of (D, Selection (D, K).Head)), RULER_WIDTH / 2 - 2, 2),
                       (if K = Primary_Index (D) then P.Caret else P.Ruler_Caret));
            K := K + Step;
         end loop;
      end;
      --  The minimap: the lines around the view, two pixels each, one per
      --  character, in their tokens' colors.
      if E.Minimap then
         declare
            Map : constant Rect := (Ruler.x - MINIMAP_WIDTH, E.Bounds.y, MINIMAP_WIDTH, E.Bounds.h);
            Capacity : constant Line_Count := Line_Count (Natural'Max (1, Map.h / MINIMAP_LINE_HEIGHT));
            Shown_Lines : constant Line_Count := Line_Count'Max (1, Last_Shown - E.Top + 1);
            First : constant Line_Number :=
              (if Lines <= Capacity then 1
               else Line_Number'Max (1, Line_Number'Min (Lines - Capacity + 1,
                                                         E.Top - (Capacity - Shown_Lines) / 2)));
            Spans : Text_Syntax.Span_Array;
            Span_Count : Text_Syntax.Span_Count;
            Exact : Boolean;
         begin
            Fill_Rect (C, Map, P.Background);
            Fill_Rect (C, (Map.x, Map.y + Natural (E.Top - First) * MINIMAP_LINE_HEIGHT, Map.w,
                           Natural (Shown_Lines) * MINIMAP_LINE_HEIGHT), P.Ruler_View);
            for L in First .. Line_Number'Min (Lines, First + Capacity - 1) loop
               declare
                  Place : constant Segment_Place := Segment (D, L, 0);
                  Text : constant String := Slice (D, Place.First, Place.Last);
                  Y : constant Natural := Map.y + Natural (L - First) * MINIMAP_LINE_HEIGHT;
                  Span : Natural := 1;
                  Column : Natural := 0;
               begin
                  Line_Spans (E.Document.all, L, Text, Spans, Span_Count, Exact);
                  for I in Text'Range loop
                     exit when Column >= MINIMAP_WIDTH - 2;
                     while Span <= Span_Count and then Spans (Span).First + Spans (Span).Length - 1 < I loop
                        Span := Span + 1;
                     end loop;
                     if Text (I) = ASCII.HT then
                        Column := Column + E.Tab;
                     elsif not Text_UTF8.Is_Continuation (Text (I)) then
                        if Text (I) /= ' ' then
                           Fill_Rect (C, (Map.x + 1 + Column, Y, 1, MINIMAP_LINE_HEIGHT - 1),
                                      P.Tokens ((if Span <= Span_Count and then Spans (Span).First <= I
                                                 then Spans (Span).Kind else Text_Syntax.Plain)));
                        end if;
                        Column := Column + 1;
                     end if;
                  end loop;
               end;
            end loop;
         end;
      end if;
   end Draw_Side;

   --  A row slot's hash in a buffer record when it shows no line.
   BLANK_ROW : constant Unsigned_64 := Unsigned_64'Last;

   --  The record of the buffer C draws into: found by its address, or the
   --  least recently drawn one, emptied.
   function Buffer_For (E : in out Editor; C : CuBit.UI.Canvas) return Buffer_Index is
      use type System.Address;
      Oldest : Buffer_Index := Buffer_Index'First;
   begin
      E.Draws := E.Draws + 1;
      for B in Buffer_Index loop
         declare
            R : Buffer_Record renames E.Buffers (B);
         begin
            if R.Target = C.addr and then R.Pitch = C.pitch then
               if R.Bounds /= E.Bounds then
                  R.Valid := False;
                  R.Bounds := E.Bounds;
               end if;
               R.Stamp := E.Draws;
               return B;
            end if;
            if R.Stamp < E.Buffers (Oldest).Stamp then
               Oldest := B;
            end if;
         end;
      end loop;
      E.Buffers (Oldest) := (Target => C.addr, Pitch => C.pitch, Bounds => E.Bounds, Valid => False,
                             Stamp => E.Draws, others => <>);
      return Oldest;
   end Buffer_For;

   procedure Draw (E : in out Editor; C : CuBit.UI.Canvas) is
      Clip : constant Rect := (if C.clipEnabled then C.clip else E.Bounds);
      Gutter : constant Natural := Gutter_Width (E);
      Row_Left : constant Natural := Text_X (E) - E.Bounds.x;
      Whole : constant Natural := Natural'Max (0, E.Bounds.w - Side_Width (E));
      B : Buffer_Index;
   begin
      E.Painted := 0;
      if E.Everything then
         --  Colors, focus or the gutter changed: no buffer shows these rows.
         for K in Buffer_Index loop
            E.Buffers (K).Valid := False;
         end loop;
      end if;
      B := Buffer_For (E, C);
      declare
         R : Buffer_Record renames E.Buffers (B);
      begin
         for Slot in 1 .. Visible_Slots (E) loop
            declare
               Row_Box : constant Rect := (E.Bounds.x, E.Bounds.y + (Slot - 1) * ROW_HEIGHT, E.Bounds.w, ROW_HEIGHT);
               Shown : constant Boolean := Slot <= E.Shown;
               Hash : constant Unsigned_64 := (if Shown then E.Rows (Slot).Hash else BLANK_ROW);
               Width : constant Content_Width := (if Shown then Row_Width (E.Rows (Slot)) else 0);
               Lit : constant Boolean := Shown and then Is_Lit (E, E.Rows (Slot));
            begin
               --  Only rows this buffer shows differently: it keeps every
               --  other pixel.
               if Intersects (Row_Box, Clip) and then (not R.Valid or else R.Hashes (Slot) /= Hash) then
                  declare
                     Full : constant Boolean := not R.Valid or else R.Hashes (Slot) = 0 or else not Shown
                       or else Lit or else R.Lits (Slot);
                     Paint : constant Content_Width := Content_Width'Max (Width, R.Widths (Slot));
                     Reach : constant Natural := (if Full then Whole else Natural'Min (Whole, Row_Left + Paint));
                     Covered : constant Boolean :=
                       Clip.x <= Row_Box.x and then Clip.x + Clip.w >= Row_Box.x + Reach
                       and then Clip.y <= Row_Box.y and then Clip.y + Clip.h >= Row_Box.y + ROW_HEIGHT;
                  begin
                     if Shown then
                        Draw_Row (E, C, Slot, Full, Paint);
                        E.Painted := E.Painted + 1;
                     else
                        Fill_Rect (C, (E.Bounds.x, Row_Box.y, Gutter, ROW_HEIGHT), E.Colors.Gutter_Background);
                        Fill_Rect (C, (E.Bounds.x + Gutter, Row_Box.y, Whole - Natural'Min (Whole, Gutter), ROW_HEIGHT),
                                   E.Colors.Background);
                     end if;
                     --  A row the clip cut is only partly in this buffer.
                     R.Hashes (Slot) := (if Covered then Hash else 0);
                     R.Widths (Slot) := Width;
                     R.Lits (Slot) := Lit;
                  end;
               end if;
            end;
         end loop;
         if not R.Valid then
            --  Rows outside the clip were not drawn: unknown in this buffer.
            for Slot in 1 .. Visible_Slots (E) loop
               declare
                  Row_Box : constant Rect := (E.Bounds.x, E.Bounds.y + (Slot - 1) * ROW_HEIGHT, E.Bounds.w, ROW_HEIGHT);
               begin
                  if not Intersects (Row_Box, Clip) then
                     R.Hashes (Slot) := 0;
                  end if;
               end;
            end loop;
            R.Valid := True;
         end if;
      end;
      Draw_Side (E, C);
      for Slot in 1 .. E.Shown loop
         E.Previous (Slot) := E.Rows (Slot).Hash;
         E.Previous_Width (Slot) := Row_Width (E.Rows (Slot));
         E.Previous_Lit (Slot) := Is_Lit (E, E.Rows (Slot));
         E.Dirty (Slot) := 0;
      end loop;
      for Slot in E.Shown + 1 .. Visible_Slots (E) loop
         E.Previous_Width (Slot) := 0;
         E.Previous_Lit (Slot) := False;
      end loop;
      E.Previous_Shown := E.Shown;
      E.Previous_Chrome := E.Chrome_Hash;
      E.Everything := False;
   end Draw;

   procedure Invalidate_Area (E : in out Editor; Area : CuBit.UI.Rect) is
   begin
      if Is_Empty (Area) then
         return;
      end if;
      for Slot in 1 .. Visible_Slots (E) loop
         if Intersects ((E.Bounds.x, E.Bounds.y + (Slot - 1) * ROW_HEIGHT, E.Bounds.w, ROW_HEIGHT), Area) then
            for B in Buffer_Index loop
               E.Buffers (B).Hashes (Slot) := 0;
            end loop;
         end if;
      end loop;
   end Invalidate_Area;

   ---------------------------------------------------------------------------
   --  Pointer.

   function Offset_At (E : Editor; X, Y : Natural) return Byte_Offset is
      D : Document renames E.Document.all;
      Slot : constant Natural := (if Y > E.Bounds.y then (Y - E.Bounds.y) / ROW_HEIGHT + 1 else 1);
   begin
      if E.Shown = 0 then
         return 0;
      elsif Slot > E.Shown then
         return Length (D);
      end if;
      declare
         R : Row_View renames E.Rows (Slot);
         Bytes : constant String := Slice (D, R.First, R.Last);
         Skip : constant Column_Count :=
           (if E.Wrap or else R.Segment > 0 or else not (R.Lead and then R.Final) then 0 else E.Column);
         Column : constant Column_Count := (if X > Text_X (E) then (X - Text_X (E)) / CELL_WIDTH else 0) + Skip;
         Index : Positive;
      begin
         if Bytes'Length = 0 then
            return R.First;
         end if;
         Index := Text_Layout.Index_At_Column (Bytes, Column, E.Tab, Nearest => True);
         if Index > Bytes'Last and then not R.Final then
            Index := Text_Layout.Previous_Boundary (Bytes, Index);
         end if;
         return R.First + Byte_Count (Index - 1);
      end;
   end Offset_At;

   procedure Caret_Position (E : Editor; Offset : Byte_Offset; X, Y : out Natural; Shown : out Boolean) is
      D : Document renames E.Document.all;
   begin
      X := 0;
      Y := 0;
      Shown := False;
      for Slot in 1 .. E.Shown loop
         declare
            R : Row_View renames E.Rows (Slot);
         begin
            if Offset >= R.First and then (Offset < R.Last or else (Offset = R.Last and then R.Final)) then
               declare
                  Text : constant String := Slice (D, R.First, Offset);
                  Column : constant Column_Count := Text_Layout.Column_Of (Text, Text'Last + 1, E.Tab);
                  Skip : constant Column_Count := (if E.Wrap then 0 else E.Column);
               begin
                  X := Text_X (E) + (if Column > Skip then Column - Skip else 0) * CELL_WIDTH;
                  Y := E.Bounds.y + (Slot - 1) * ROW_HEIGHT;
                  Shown := True;
                  return;
               end;
            end if;
         end;
      end loop;
   end Caret_Position;

   procedure Scroll_To_Ratio (E : in out Editor; Y : Natural) is
      Lines : constant Line_Count := Line_Total (E.Document.all);
      Within : constant Natural := (if Y > E.Bounds.y then Y - E.Bounds.y else 0);
      Target : constant Line_Count :=
        Line_Count (Long_Long_Integer (Within) * Long_Long_Integer (Lines) / Long_Long_Integer (Natural'Max (1, E.Bounds.h))) + 1;
   begin
      Scroll_To_Line (E, Line_Number'Max (1, Line_Number'Min (Lines, Target)));
   end Scroll_To_Ratio;

   procedure Pointer_Press
     (E : in out Editor; X, Y : Natural; Clicks : Positive; Control, Shift, Alt : Boolean)
   is
      D : Document renames E.Document.all;
      Side_X : constant Natural := E.Bounds.x + E.Bounds.w - Side_Width (E);
   begin
      if X >= Side_X then
         Scroll_To_Ratio (E, Y);
         E.Dragging := False;
         return;
      end if;
      declare
         O : constant Byte_Offset := Offset_At (E, X, Y);
      begin
         if X < E.Bounds.x + Gutter_Width (E) then
            --  The gutter selects the line.
            Set_Caret (D, O);
            Select_Line (D);
            E.Dragging := False;
            return;
         end if;
         E.Column_Drag := False;
         if Clicks = 2 then
            Set_Caret (D, O);
            Select_Word_Or_Next (D);
         elsif Clicks >= 3 then
            Set_Caret (D, O);
            Select_Line (D);
         elsif Alt then
            Set_Caret (D, O);
            E.Column_Drag := True;
         elsif Control then
            Add_Selection (D, O, O);
         elsif Shift then
            Set_Caret (D, O, Extend => True);
         else
            Set_Caret (D, O);
         end if;
         E.Press_Offset := O;
         E.Dragging := Clicks = 1;
      end;
   end Pointer_Press;

   procedure Pointer_Drag (E : in out Editor; X, Y : Natural) is
      D : Document renames E.Document.all;
   begin
      if not E.Dragging then
         return;
      end if;
      if Y < E.Bounds.y then
         Scroll_Rows (E, -1);
      elsif Y >= E.Bounds.y + E.Bounds.h then
         Scroll_Rows (E, 1);
      end if;
      declare
         O : constant Byte_Offset := Offset_At (E, X, Natural'Min (Natural'Max (Y, E.Bounds.y),
                                                                    E.Bounds.y + Natural'Max (1, E.Bounds.h) - 1));
      begin
         if E.Column_Drag then
            Select_Column_Box (D, E.Press_Offset, O, Settings (E));
         else
            Set_Caret (D, O, Extend => True);
         end if;
      end;
   end Pointer_Drag;

   procedure Pointer_Release (E : in out Editor) is
   begin
      E.Dragging := False;
      E.Column_Drag := False;
   end Pointer_Release;

   procedure Wheel (E : in out Editor; Steps : Integer) is
   begin
      Scroll_Rows (E, -(Steps * WHEEL_ROWS));
   end Wheel;
end Text_Editors;
