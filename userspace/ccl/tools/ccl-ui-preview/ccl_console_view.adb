with CCL.Completions;
with CCL.Hints;
with CCL.Highlighting;
with CCL.Image_Store;
with CCL.Literal_Tables;
with CCL.Presentations;
with CCL.Types;
with CCL.Types.Shapes;
with CCL.Units;

package body CCL_Console_View is
   use type CCL.Interfaces.Console.Notation;
   use type CCL.Language.Views.Surface;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;
   use type CCL.Language.Interpretation_Status;
   use type CCL.Language.Static_Type;
   use type CCL.Highlighting.Token_Class;
   use type CCL.Highlighting.Form_State;
   use type CCL.Presentations.Form;
   package Edit renames CuBit.UI.Editor;
   package HL renames CCL.Highlighting;

   ---------------------------------------------------------------------------
   --  The console's own surfaces, one palette per theme (console.theme):
   --  Midnight, the terminal's dark identity, and Daylight. Every colour is
   --  named here; drawing reads the current theme's.
   ---------------------------------------------------------------------------
   type Palette is record
      Background : Color;
      Header_Top : Color;
      Header_Bottom : Color;
      Card : Color;
      Card_Hover : Color;
      Dock : Color;
      Edge : Color;
      Ink : Color;
      Muted : Color;
      Faint : Color;
      Accent : Color;
      Good : Color;
      Danger : Color;
      Selection : Color;
      Popup : Color;
      Popup_Selected : Color;
      Tooltip : Color;
      Form_Color : Color;
      Operator_Color : Color;
      Host_Color : Color;
      Type_Color : Color;
      Call_Color : Color;
      Number_Color : Color;
      Boolean_Color : Color;
      String_Color : Color;
      Comment_Color : Color;
   end record;
   PALETTES : constant array (CCL.Interfaces.Console.Theme) of Palette :=
     [CCL.Interfaces.Console.Midnight =>
        (
         Background => 16#0A0E16#,
         Header_Top => 16#141B2B#,
         Header_Bottom => 16#0D121D#,
         Card => 16#111725#,
         Card_Hover => 16#16203A#,
         Dock => 16#0E1420#,
         Edge => 16#22304A#,
         Ink => 16#D8E1F0#,
         Muted => 16#71809C#,
         Faint => 16#3A4761#,
         Accent => 16#5CC8FF#,
         Good => 16#7EE0B5#,
         Danger => 16#FF6B7A#,
         Selection => 16#1F3A5F#,
         Popup => 16#151D2E#,
         Popup_Selected => 16#22416A#,
         Tooltip => 16#1B2438#,
         Form_Color => 16#C792EA#,
         Operator_Color => 16#89DDFF#,
         Host_Color => 16#5CC8FF#,
         Type_Color => 16#FFCB6B#,
         Call_Color => 16#82AAFF#,
         Number_Color => 16#F78C6C#,
         Boolean_Color => 16#FF9CAC#,
         String_Color => 16#C3E88D#,
         Comment_Color => 16#56637E#),
      CCL.Interfaces.Console.Daylight =>
        (
         Background => 16#F4F6FA#,
         Header_Top => 16#FFFFFF#,
         Header_Bottom => 16#EEF2F8#,
         Card => 16#FFFFFF#,
         Card_Hover => 16#EAF2FF#,
         Dock => 16#F8FAFD#,
         Edge => 16#CBD5E3#,
         Ink => 16#1C2433#,
         Muted => 16#5D6B82#,
         Faint => 16#A9B5C7#,
         Accent => 16#0B7BD6#,
         Good => 16#1F9D6B#,
         Danger => 16#D23B4E#,
         Selection => 16#CFE3FF#,
         Popup => 16#FFFFFF#,
         Popup_Selected => 16#D9E9FF#,
         Tooltip => 16#F1F5FB#,
         Form_Color => 16#8E44C8#,
         Operator_Color => 16#0E7C9C#,
         Host_Color => 16#0B7BD6#,
         Type_Color => 16#A86A00#,
         Call_Color => 16#2F5FD0#,
         Number_Color => 16#C25A1E#,
         Boolean_Color => 16#C2345C#,
         String_Color => 16#3D8A16#,
         Comment_Color => 16#8693A8#)];
   Current_Theme : CCL.Interfaces.Console.Theme := CCL.Interfaces.Console.Midnight;
   function BACKGROUND return Color is (PALETTES (Current_Theme).Background);
   function HEADER_TOP return Color is (PALETTES (Current_Theme).Header_Top);
   function HEADER_BOTTOM return Color is (PALETTES (Current_Theme).Header_Bottom);
   function CARD return Color is (PALETTES (Current_Theme).Card);
   function CARD_HOVER return Color is (PALETTES (Current_Theme).Card_Hover);
   function DOCK return Color is (PALETTES (Current_Theme).Dock);
   function EDGE return Color is (PALETTES (Current_Theme).Edge);
   function INK return Color is (PALETTES (Current_Theme).Ink);
   function MUTED return Color is (PALETTES (Current_Theme).Muted);
   function FAINT return Color is (PALETTES (Current_Theme).Faint);
   function ACCENT return Color is (PALETTES (Current_Theme).Accent);
   function GOOD return Color is (PALETTES (Current_Theme).Good);
   function DANGER return Color is (PALETTES (Current_Theme).Danger);
   function SELECTION return Color is (PALETTES (Current_Theme).Selection);
   function POPUP return Color is (PALETTES (Current_Theme).Popup);
   function POPUP_SELECTED return Color is (PALETTES (Current_Theme).Popup_Selected);
   function TOOLTIP return Color is (PALETTES (Current_Theme).Tooltip);
   function FORM_COLOR return Color is (PALETTES (Current_Theme).Form_Color);
   function OPERATOR_COLOR return Color is (PALETTES (Current_Theme).Operator_Color);
   function HOST_COLOR return Color is (PALETTES (Current_Theme).Host_Color);
   function TYPE_COLOR return Color is (PALETTES (Current_Theme).Type_Color);
   function CALL_COLOR return Color is (PALETTES (Current_Theme).Call_Color);
   function NUMBER_COLOR return Color is (PALETTES (Current_Theme).Number_Color);
   function BOOLEAN_COLOR return Color is (PALETTES (Current_Theme).Boolean_Color);
   function STRING_COLOR return Color is (PALETTES (Current_Theme).String_Color);
   function COMMENT_COLOR return Color is (PALETTES (Current_Theme).Comment_Color);

   --  Parentheses take their colour from their depth.
   type Rainbow_Index is mod 6;
   RAINBOW : constant array (Rainbow_Index) of Color :=
     [16#FFD866#, 16#C792EA#, 16#5CC8FF#, 16#7EE0B5#, 16#F78C6C#, 16#FF9CAC#];

   function Mark_Color (Item : HL.Mark) return Color is
     (case Item.Class is
         when HL.Whitespace | HL.Name => INK,
         when HL.Comment => COMMENT_COLOR,
         when HL.Delimiter => RAINBOW (Rainbow_Index'Mod (Item.Depth)),
         when HL.Mismatch | HL.Unterminated => DANGER,
         when HL.Special_Form => FORM_COLOR,
         when HL.Operator => OPERATOR_COLOR,
         when HL.Host_Operation => HOST_COLOR,
         when HL.Type_Name => TYPE_COLOR,
         when HL.Call_Name => CALL_COLOR,
         when HL.Number => NUMBER_COLOR,
         when HL.Boolean_Literal => BOOLEAN_COLOR,
         when HL.Text_Literal => STRING_COLOR);

   ---------------------------------------------------------------------------
   --  Geometry
   ---------------------------------------------------------------------------
   function Column_Width return Positive is (Positive'Max (1, Code_Text_Width ("M")));
   function Line_Height return Positive is (Code_Text_Height + 3);
   HEADER_HEIGHT  : constant := 34;
   STATUS_HEIGHT  : constant := 24;
   MARGIN         : constant := 14;
   CARD_PADDING   : constant := 8;
   CARD_GAP       : constant := 8;
   GUTTER         : constant := 4;    --  the status bar on a card's left edge
   PROMPT_COLUMNS : constant := 3;    --  "> " plus breathing room
   MAXIMUM_INPUT_ROWS : constant := 12;
   WHEEL_ROWS     : constant := 3;
   TOOLTIP_PADDING : constant := 7;
   POPUP_COLUMNS  : constant := 44;

   type Geometry is record
      Header, Transcript, Dock, Input, Status : Rect;
      Columns : Positive := 1;    --  input text columns
      Card_Columns : Positive := 1;
   end record;

   --  Rows of Text wrapped at Columns, counting line breaks.
   function Row_Count (Text : String; Columns : Positive) return Positive is
      Rows : Positive := 1;
      Column : Natural := 0;
   begin
      for C of Text loop
         if C = ASCII.LF then
            Rows := Rows + 1; Column := 0;
         elsif Column = Columns then
            Rows := Rows + 1; Column := 1;
         else
            Column := Column + 1;
         end if;
      end loop;
      return Rows;
   end Row_Count;

   --  Where the cursor before character Position (1-based, up to
   --  Text'Length + 1) sits in the wrapped text.
   procedure Locate
     (Text : String; Columns : Positive; Position : Positive;
      Row, Column : out Natural)
   is
   begin
      Row := 0; Column := 0;
      for I in 1 .. Natural'Min (Position - 1, Text'Length) loop
         if Text (Text'First + I - 1) = ASCII.LF then
            Row := Row + 1; Column := 0;
         elsif Column = Columns then
            Row := Row + 1; Column := 1;
         else
            Column := Column + 1;
         end if;
      end loop;
      if Column = Columns and then Position <= Text'Length and then
        Text (Text'First + Position - 1) /= ASCII.LF
      then
         Row := Row + 1; Column := 0;
      end if;
   end Locate;

   --  The cursor position nearest to (Row, Column) in the wrapped text.
   function Position_At
     (Text : String; Columns : Positive; Row, Column : Natural) return Positive
   is
      R, C : Natural := 0;
   begin
      for I in 1 .. Text'Length loop
         if R = Row and then (C >= Column or else Text (Text'First + I - 1) = ASCII.LF) then
            return I;
         elsif R > Row then
            return I - 1;
         end if;
         if Text (Text'First + I - 1) = ASCII.LF then
            R := R + 1; C := 0;
         elsif C = Columns then
            R := R + 1; C := 1;
         else
            C := C + 1;
         end if;
      end loop;
      return Text'Length + 1;
   end Position_At;

   function Layout (Bounds : Rect; Input : String) return Geometry is
      G : Geometry;
      CW : constant Positive := Column_Width;
      Inner : constant Natural := (if Bounds.w > 2 * MARGIN then Bounds.w - 2 * MARGIN else 1);
      Rows : Positive;
      Dock_Height : Natural;
   begin
      G.Columns := Positive'Max (8, (if Inner > PROMPT_COLUMNS * CW + 2 * CARD_PADDING
                                     then (Inner - PROMPT_COLUMNS * CW - 2 * CARD_PADDING) / CW else 1));
      G.Card_Columns := Positive'Max (8, (if Inner > GUTTER + 2 * CARD_PADDING + 4 * CW
                                          then (Inner - GUTTER - 2 * CARD_PADDING) / CW - 2 else 1));
      Rows := Positive'Min (MAXIMUM_INPUT_ROWS, Row_Count (Input, G.Columns));
      Dock_Height := Rows * Line_Height + 2 * CARD_PADDING + STATUS_HEIGHT + 2;
      G.Header := (Bounds.x, Bounds.y, Bounds.w, Natural'Min (HEADER_HEIGHT, Bounds.h));
      Dock_Height := Natural'Min (Dock_Height, (if Bounds.h > HEADER_HEIGHT then Bounds.h - HEADER_HEIGHT else 0));
      G.Dock := (Bounds.x, Bounds.y + Bounds.h - Dock_Height, Bounds.w, Dock_Height);
      G.Transcript := (Bounds.x, Bounds.y + G.Header.h, Bounds.w,
                       (if G.Dock.y > Bounds.y + G.Header.h then G.Dock.y - Bounds.y - G.Header.h else 0));
      G.Input := (Bounds.x + MARGIN + PROMPT_COLUMNS * CW + CARD_PADDING, G.Dock.y + CARD_PADDING + 2,
                  G.Columns * CW, Rows * Line_Height);
      G.Status := (Bounds.x + MARGIN, G.Dock.y + Dock_Height - STATUS_HEIGHT,
                   Inner, STATUS_HEIGHT);
      return G;
   end Layout;

   function Image (Value : Natural) return String is
     (Natural'Image (Value) (2 .. Natural'Image (Value)'Last));

   --  1234567 -> "1,234,567"
   function Grouped (Value : Natural) return String is
      Digits_Image : constant String := Image (Value);
   begin
      if Digits_Image'Length <= 3 then return Digits_Image; end if;
      return Grouped (Value / 1000) & "," & Digits_Image (Digits_Image'Last - 2 .. Digits_Image'Last);
   end Grouped;

   function Elapsed_Seconds (Milliseconds : Interfaces.Unsigned_64) return String is
     (Natural'Image (Natural (Interfaces.Unsigned_64'Min (Milliseconds / 1_000, 3_600))) & "s");

   function Elapsed_Image (Milliseconds : Interfaces.Unsigned_64) return String is
     (if Milliseconds = 0 then "<1 ms"
      elsif Milliseconds < 10_000 then Image (Natural (Milliseconds)) & " ms"
      else Image (Natural (Interfaces.Unsigned_64'Min (Milliseconds / 1000, 999_999))) & " s");

   function Signature_Image (S : CCL.Catalog.Completion.Suggestion) return String
     renames CCL.Completions.Signature_Image;

   ---------------------------------------------------------------------------
   --  Lifecycle
   ---------------------------------------------------------------------------
   procedure Initialize
     (State : out View_State; Catalog : CCL.Catalog.Interface_Catalog)
   is
      Accepted : Boolean;
   begin
      CCL.Sessions.Initialize (State.Session, Catalog);
      State.Meta := [others => <>];
      Edit.Initialize (State.Input, "", Accepted);
      Edit.Initialize (State.Draft, "", Accepted);
      State.Recalled := 0;
      State.Visible_Interfaces := CCL.Catalog.Length (Catalog);
      State.Scroll := 0;
      State.Scroll_Limit := 0;
      State.Page := 1;
      State.Columns := 80;
      State.Hit_Total := 0;
      State.Pointer_X := 0;
      State.Pointer_Y := 0;
      State.Pointer_Known := False;
      State.Hovered := 0;
      State.Selecting := False;
      State.Candidate_Total := 0;
      State.Matches_Beyond := False;
      State.Selected := 1;
      State.Popup_Open := False;
      State.Prefix_Length := 0;
      State.Signature_Visible := False;
   end Initialize;

   function Example (Index : Example_Index) return String is
     (case Index is
         when 1 => "(+ 20 22)",
         when 2 => "(sort (list 5 3 9 1))",
         when 3 => "(upper (concat ""cubit "" ""console""))",
         when 4 => "(clock.monotonic-ms)",
         when 5 => "(length (logs.recent ""desktop""))",
         when 6 => ":env");

   ---------------------------------------------------------------------------
   --  Completion
   ---------------------------------------------------------------------------
   --  Completion is CCL.Completions' (shared with the Observatory); the
   --  view keeps only which candidate is selected and whether the list shows.
   procedure Refresh_Completion (State : in out View_State; Explicit : Boolean) is
      Text : constant String := Edit.Content (State.Input);
      Cursor : constant Positive := Edit.Cursor (State.Input);
      Found : CCL.Completions.Result;
   begin
      State.Candidate_Total := 0;
      State.Matches_Beyond := False;
      State.Signature_Visible := False;
      State.Prefix_Length := 0;
      if Edit.Selection_First (State.Input) /= Edit.Selection_Last (State.Input) then
         State.Popup_Open := False;
         return;
      end if;
      CCL.Sessions.Complete_At
        (State.Session, Text (Text'First .. Text'First + Cursor - 2),
         (if Cursor <= Text'Length then Text (Text'First + Cursor - 1) else ' '), Found);
      State.Prefix_Length := Found.Prefix_Length;
      State.Matches_Beyond := Found.Beyond;
      State.Signature := Found.Signature;
      State.Signature_Visible := Found.Signature_Visible;
      State.Signature_Origin := Found.Signature_Origin;
      for I in 1 .. Found.Count loop
         State.Candidates (I) :=
           (Suggestion => Found.Candidates (I).Suggestion,
            Origin => (case Found.Candidates (I).Origin is
                          when CCL.Completions.Host_Operation => Host_Candidate,
                          when CCL.Completions.Builtin => Builtin_Candidate,
                          when CCL.Completions.Form => Form_Candidate));
      end loop;
      State.Candidate_Total := Found.Count;
      State.Popup_Open := State.Candidate_Total > 0 and then (Explicit or else State.Prefix_Length > 0);
      if State.Selected > State.Candidate_Total then State.Selected := 1; end if;
   end Refresh_Completion;

   procedure Accept_Candidate (State : in out View_State; Index : Positive) is
      Changed : Boolean;
   begin
      if Index > State.Candidate_Total then return; end if;
      declare
         S : CCL.Catalog.Completion.Suggestion renames State.Candidates (Index).Suggestion;
         Text : constant String := Edit.Content (State.Input);
         Cursor : constant Positive := Edit.Cursor (State.Input);
         Follows_Space : constant Boolean :=
           Cursor <= Text'Length and then Text (Cursor) in ' ' | ')' | ASCII.LF;
      begin
         if S.Length > State.Prefix_Length then
            Edit.Insert (State.Input, S.Name (State.Prefix_Length + 1 .. S.Length), Changed);
         end if;
         if not Follows_Space then Edit.Insert (State.Input, " ", Changed); end if;
      end;
      State.Popup_Open := False;
      State.Recalled := 0;
   end Accept_Candidate;

   ---------------------------------------------------------------------------
   --  Events
   ---------------------------------------------------------------------------
   function Hit_At (State : View_State; X, Y : Natural) return Hit_Count is
   begin
      --  Later regions are drawn on top (popup, tooltip), so search backwards.
      for I in reverse 1 .. State.Hit_Total loop
         if Point_In_Rect (X, Y, State.Hits (I).Area) then return I; end if;
      end loop;
      return 0;
   end Hit_At;

   procedure Set_Notice (State : in out View_State; Text : String) is
   begin
      State.Notice_Length := Natural'Min (Text'Length, State.Notice'Length);
      State.Notice (1 .. State.Notice_Length) := Text (Text'First .. Text'First + State.Notice_Length - 1);
   end Set_Notice;

   procedure Set_Input (State : in out View_State; Text : String) is
      Accepted : Boolean;
   begin
      Edit.Initialize (State.Input, Text, Accepted);
      State.Recalled := 0;
   end Set_Input;

   --  A result as source that reads back as the same value.
   function Result_Source (Outcome : CCL.Language.Interpretation_Result) return String is
      Value : constant String := CCL.Sessions.Result_Value_Image (Outcome);
      Quoted : String (1 .. 2 * Value'Length + 2);
      Last : Natural := 1;
   begin
      if CCL.Sessions.Result_Type (Outcome) /= CCL.Language.String_Type or else
        Outcome.Has_List or else Outcome.Has_Literal
      then
         return Value;
      end if;
      Quoted (1) := '"';
      for C of Value loop
         if C in '"' | '\' then
            Last := Last + 1; Quoted (Last) := '\';
         end if;
         Last := Last + 1;
         Quoted (Last) := (if C = ASCII.LF then 'n' else C);
         if C = ASCII.LF then Quoted (Last - 1 .. Last) := "\n"; end if;
      end loop;
      Last := Last + 1; Quoted (Last) := '"';
      return Quoted (1 .. Last);
   end Result_Source;

   --  The CCL that a table click writes. A row is bound as "row"; the
   --  query wraps the entry's own source, so it reads the same data again.
   ROW_NAME : constant String := "row";
   function Field_Access (Entry_Value : CCL.Sessions.Submission; Column : Positive) return String is
     ("(field " & ROW_NAME & " " &
      CCL.Types.Image (Entry_Value.Outcome.Literal_Shape.Fields (Column).Identifier) & ")");
   function Row_Query
     (Operation : String; Entry_Value : CCL.Sessions.Submission; Body_Text : String) return String
   is
   begin
      return "(" & Operation & " (fn ((" & ROW_NAME & " " &
        CCL.Types.Image (Entry_Value.Outcome.Literal_Shape.Row_Type) & ")) " & Body_Text & ") " &
        Entry_Value.Source (1 .. Entry_Value.Source_Length) & ")";
   end Row_Query;

   function Surface_Of (Value : CCL.Interfaces.Console.Notation) return CCL.Language.Views.Surface is
     (CCL.Language.Views.Surface'Val (CCL.Interfaces.Console.Notation'Pos (Value)));

   --  BASIC text as a cell shows it: without the marker line that tells
   --  the session it is BASIC (the console's notation says so instead).
   function Without_Marker (Text : String) return String is
      Marker : constant String := CCL.Language.Views.BASIC_MARKER;
      First : Natural;
   begin
      if Text'Length < Marker'Length or else
        Text (Text'First .. Text'First + Marker'Length - 1) /= Marker
      then
         return Text;
      end if;
      First := Text'First + Marker'Length;
      while First <= Text'Last and then Text (First) in ASCII.LF | ASCII.CR | ' ' loop
         First := First + 1;
      end loop;
      return Text (First .. Text'Last);
   end Without_Marker;

   --  Text as the session reads it in the console's notation: BASIC typed
   --  without its marker line gets one (commands are the same in both).
   function For_Session (State : View_State; Text : String) return String is
     (if State.Notation = CCL.Interfaces.Console.Basic and then Text'Length > 0 and then
         Text (Text'First) /= ':' and then
         CCL.Language.Views.Detect (Text) = CCL.Language.Views.Lisp
      then CCL.Language.Views.BASIC_MARKER & ASCII.LF & Text else Text);

   --  What an entry runs. Places (docs/ccl-places.md): :cd and :ls are
   --  written as the CCL they mean, which the transcript then shows; other
   --  text runs as typed (For_Session).
   function Program_For (State : View_State; Text : String) return String is
      Name : constant String :=
        (if Text'Length > 4 and then Text (Text'First .. Text'First + 3) = ":cd "
         then Text (Text'First + 4 .. Text'Last) else "");
      Plain_Name : constant Boolean :=
        (for all C of Name => C in ' ' .. '~' and then C not in '"' | '\');
   begin
      if Text = ":ls" then
         return "(fs.list here)";
      elsif Text = ":ps" then
         return "(proc.list)";
      elsif Text = ":cd" then
         return "(define here (fs.home))";
      elsif Name'Length > 0 and then Plain_Name then
         return (if Name = ".." then "(define here (fs.up here))"
                 else "(define here (fs.enter (Child here """ & Name & """)))");
      end if;
      return For_Session (State, Text);
   end Program_For;

   --  Each cell's source in the current notation, converted again only when
   --  the cell or the notation changes.
   procedure Update_Shown (State : in out View_State) is
      Entry_Value : CCL.Sessions.Submission;
      Found, Converted : Boolean;
   begin
      for I in 1 .. CCL.Sessions.Length (State.Session) loop
         CCL.Sessions.Recall (State.Session, I, Entry_Value, Found);
         if Found then
            declare
               Source : constant String := Entry_Value.Source (1 .. Entry_Value.Source_Length);
               S : Shown_Source renames State.Shown (I);
            begin
               if not (S.Valid and then S.Into = State.Notation and then
                       S.Source (1 .. S.Source_Length) = Source)
               then
                  S.Valid := True;
                  S.Into := State.Notation;
                  S.Source_Length := Source'Length;
                  S.Source (1 .. Source'Length) := Source;
                  CCL.Sessions.View_Source
                    (State.Session, Source, Surface_Of (State.Notation), S.Shown, Converted);
                  if not Converted then
                     S.Shown.Length := Source'Length;
                     S.Shown.Data (1 .. Source'Length) := Source;
                  end if;
               end if;
            end;
         end if;
      end loop;
   end Update_Shown;

   --  The cell's source as the transcript shows it.
   function Displayed
     (State : View_State; Index : CCL.Sessions.History_Index;
      Entry_Value : CCL.Sessions.Submission) return String
   is
      S : Shown_Source renames State.Shown (Index);
   begin
      if S.Valid and then S.Into = State.Notation and then
        S.Source (1 .. S.Source_Length) = Entry_Value.Source (1 .. Entry_Value.Source_Length)
      then
         return Without_Marker (S.Shown.Data (1 .. S.Shown.Length));
      end if;
      return Without_Marker (Entry_Value.Source (1 .. Entry_Value.Source_Length));
   end Displayed;

   function Notation (State : View_State) return CCL.Interfaces.Console.Notation is (State.Notation);

   function Cell_Source (State : View_State; Index : Positive) return String is
      Entry_Value : CCL.Sessions.Submission;
      Found : Boolean;
   begin
      if Index > CCL.Sessions.Maximum_History then return ""; end if;
      CCL.Sessions.Recall (State.Session, Index, Entry_Value, Found);
      return (if Found then Displayed (State, Index, Entry_Value) else "");
   end Cell_Source;

   procedure Set_Notation (State : in out View_State; Value : CCL.Interfaces.Console.Notation) is
      Text : constant String := Edit.Content (State.Input);
      Shown : CCL.Language.Views.Text;
      Converted : Boolean;
   begin
      if Value = State.Notation then return; end if;
      --  The input switches too, when it reads as a whole entry.
      if Text'Length > 0 then
         CCL.Sessions.View_Source
           (State.Session, For_Session (State, Text), Surface_Of (Value), Shown, Converted);
         if Converted then Set_Input (State, Without_Marker (Shown.Data (1 .. Shown.Length))); end if;
      end if;
      State.Notation := Value;
      Update_Shown (State);
      Set_Notice (State, (if Value = CCL.Interfaces.Console.Basic then "BASIC notation" else "Lisp notation") &
                  "  |  the same cells, written the other way");
   end Set_Notation;

   function Theme return CCL.Interfaces.Console.Theme is (Current_Theme);
   procedure Set_Theme (Value : CCL.Interfaces.Console.Theme) is
   begin
      Current_Theme := Value;
   end Set_Theme;

   function Statistics (State : View_State) return CCL.Interfaces.Console.Statistics is
      Result : CCL.Interfaces.Console.Statistics;
      Count : constant CCL.Sessions.History_Count := CCL.Sessions.Length (State.Session);
      function Ms (Value : Interfaces.Unsigned_64) return Natural is
        (if Value > Interfaces.Unsigned_64 (Natural'Last) then Natural'Last else Natural (Value));
   begin
      Result.Cells := Count;
      for I in 1 .. Count loop
         if State.Meta (I).Live then
            Result.Live := Result.Live + 1;
            Result.Live_Runs := Natural'Min (Natural'Last - State.Meta (I).Runs, Result.Live_Runs) +
                                State.Meta (I).Runs;
         end if;
         Result.Slowest_Ms := Natural'Max (Result.Slowest_Ms, Ms (State.Meta (I).Elapsed_Ms));
      end loop;
      if Count > 0 then Result.Last_Ms := Ms (State.Meta (Count).Elapsed_Ms); end if;
      return Result;
   end Statistics;

   procedure Handle
     (State : in out View_State; Event : View_Event;
      Submitted, Redraw : out Boolean)
   is
      Changed : Boolean;
      Text : constant String := Edit.Content (State.Input);
      Cursor : constant Positive := Edit.Cursor (State.Input);
      Count : constant CCL.Sessions.History_Count := CCL.Sessions.Length (State.Session);
      Row, Column : Natural;
      Edited : Boolean := False;

      procedure Recall is
         Entry_Value : CCL.Sessions.Submission;
         Found : Boolean;
      begin
         CCL.Sessions.Recall (State.Session, State.Recalled, Entry_Value, Found);
         if Found and then not Entry_Value.Source_Truncated then
            Edit.Initialize (State.Input, Displayed (State, State.Recalled, Entry_Value), Changed);
         end if;
      end Recall;

      --  :watch [seconds] and :unwatch: presentation commands, handled here
         --  and never recorded (they change no session state).
      function Watch_Command return Boolean is
         WATCH : constant String := ":watch";
         UNWATCH : constant String := ":unwatch";
         DEFAULT_SECONDS : constant := 1;
         MAXIMUM_SECONDS : constant := 3_600;
         Seconds : Natural := 0;
         Live : Natural := 0;
      begin
         if Text = UNWATCH then
            for M of State.Meta loop
               M.Live := False;
            end loop;
            Set_Notice (State, "Live cells stopped");
            return True;
         elsif Text'Length < WATCH'Length or else Text (Text'First .. Text'First + WATCH'Length - 1) /= WATCH or else
           (Text'Length > WATCH'Length and then Text (Text'First + WATCH'Length) /= ' ')
         then
            return False;
         end if;
         for C of Text (Text'First + WATCH'Length .. Text'Last) loop
            if C in '0' .. '9' then
               Seconds := Natural'Min (MAXIMUM_SECONDS + 1, Seconds * 10 + (Character'Pos (C) - Character'Pos ('0')));
            elsif C /= ' ' then
               Set_Notice (State, ":watch takes whole seconds, as in :watch 5");
               return True;
            end if;
         end loop;
         if Seconds = 0 then Seconds := DEFAULT_SECONDS; end if;
         for M of State.Meta loop
            if M.Live then Live := Live + 1; end if;
         end loop;
         if Count = 0 then
            Set_Notice (State, "Nothing to watch yet: run an expression first");
         elsif Seconds > MAXIMUM_SECONDS then
            Set_Notice (State, "At most one hour between runs");
         elsif not State.Meta (Count).Live and then Live = MAXIMUM_LIVE_CELLS then
            Set_Notice (State, "At most" & Natural'Image (MAXIMUM_LIVE_CELLS) & " live cells; :unwatch stops them");
         else
            State.Meta (Count).Live := True;
            State.Meta (Count).Interval_Ms := Interfaces.Unsigned_64 (Seconds) * 1_000;
            State.Meta (Count).Due_Ms := Now_Ms + State.Meta (Count).Interval_Ms;
            Set_Notice (State, "Entry" & Natural'Image (Count) & " is live, every" & Natural'Image (Seconds) &
                        " s  |  :unwatch or click LIVE to stop");
         end if;
         return True;
      end Watch_Command;

      procedure Submit is
         Outcome : CCL.Language.Interpretation_Result;
         Before : constant CCL.Sessions.History_Count := Count;
         Started : constant Interfaces.Unsigned_64 := Now_Ms;
         Finished : Interfaces.Unsigned_64;
         After : CCL.Sessions.History_Count;
      begin
         if Edit.Length (State.Input) = 0 then return; end if;
         if Watch_Command then
            Set_Input (State, "");
            return;
         end if;
         --  :lisp and :basic switch the notation, like console.notation.
         if Text = ":lisp" or else Text = ":basic" then
            Set_Input (State, "");
            Set_Notation (State, (if Text = ":basic" then CCL.Interfaces.Console.Basic
                                  else CCL.Interfaces.Console.Lisp));
            return;
         end if;
         Execute (State.Session, Program_For (State, Text), CCL.Sessions.Default_Fuel, Outcome);
         Finished := Now_Ms;
         After := CCL.Sessions.Length (State.Session);
         if After > 0 then
            if After = Before then
               --  The session evicted its oldest entry; keep the meta aligned.
               State.Meta (1 .. CCL.Sessions.Maximum_History - 1) :=
                 State.Meta (2 .. CCL.Sessions.Maximum_History);
            end if;
            State.Meta (After) :=
              (Elapsed_Ms => (if Finished >= Started then Finished - Started else 0),
               Fuel_Used => CCL.Sessions.Default_Fuel -
                 Natural'Min (Outcome.Fuel_Remaining, CCL.Sessions.Default_Fuel),
               others => <>);
         end if;
         Set_Input (State, "");
         State.Scroll := 0;
         State.Popup_Open := False;
         Submitted := True;
      end Submit;

      procedure Move_Row (Delta_Rows : Integer) is
      begin
         Locate (Text, State.Columns, Cursor, Row, Column);
         Edit.Place_Cursor
           (State.Input,
            Position_At (Text, State.Columns, Natural (Integer (Row) + Delta_Rows), Column),
            Event.Shift);
      end Move_Row;

      procedure Line_Edge (To_End : Boolean) is
      begin
         Locate (Text, State.Columns, Cursor, Row, Column);
         Edit.Place_Cursor
           (State.Input,
            Position_At (Text, State.Columns, Row, (if To_End then Natural'Last else 0)),
            Event.Shift);
      end Line_Edge;

      procedure Scroll_By (Pixels : Integer) is
      begin
         State.Scroll := Natural'Max (0, Natural'Min (State.Scroll_Limit, Integer (State.Scroll) + Pixels));
      end Scroll_By;

      procedure Place_From_Pointer (Extend : Boolean) is
         G_Input : Rect := (others => 0);
      begin
         for I in 1 .. State.Hit_Total loop
            if State.Hits (I).Kind = Input_Hit then G_Input := State.Hits (I).Area; end if;
         end loop;
         if Is_Empty (G_Input) then return; end if;
         Edit.Place_Cursor
           (State.Input,
            Position_At (Text, State.Columns,
              (if Event.Y > G_Input.y then (Event.Y - G_Input.y) / Line_Height else 0),
              (if Event.X > G_Input.x then (Event.X - G_Input.x + Column_Width / 2) / Column_Width else 0)),
            Extend);
      end Place_From_Pointer;
   begin
      Submitted := False;
      Redraw := True;
      if Event.Kind in Text_Input | Backspace | Delete | Escape then
         State.Notice_Length := 0;
      end if;
      case Event.Kind is
         when No_Event => Redraw := False;
         when Text_Input =>
            if Event.Character_Value in ' ' .. '~' then
               Edit.Insert (State.Input, [1 => Event.Character_Value], Changed);
               Edited := True;
            end if;
         when Backspace => Edit.Backspace (State.Input, Changed); Edited := True;
         when Delete => Edit.Delete_Forward (State.Input, Changed); Edited := True;
         when Left | Right =>
            Edit.Move (State.Input,
              (if Event.Kind = Left then
                 (if Event.Control then Edit.Move_Word_Left else Edit.Move_Left)
               else (if Event.Control then Edit.Move_Word_Right else Edit.Move_Right)),
              Event.Shift);
            Edited := True;
         when Home | End_Key =>
            if Event.Control then
               Edit.Move (State.Input, (if Event.Kind = Home then Edit.Move_Start else Edit.Move_End), Event.Shift);
            else
               Line_Edge (To_End => Event.Kind = End_Key);
            end if;
            Edited := True;
         when Up | Down =>
            Locate (Text, State.Columns, Cursor, Row, Column);
            if State.Popup_Open then
               if Event.Kind = Up then
                  State.Selected := (if State.Selected = 1 then State.Candidate_Total else State.Selected - 1);
               else
                  State.Selected := (if State.Selected >= State.Candidate_Total then 1 else State.Selected + 1);
               end if;
            elsif Event.Kind = Up and then Row > 0 then
               Move_Row (-1);
            elsif Event.Kind = Down and then Row + 1 < Row_Count (Text, State.Columns) then
               Move_Row (1);
            elsif Event.Kind = Up and then Count > 0 then
               if State.Recalled = 0 then
                  State.Draft := State.Input;
                  State.Recalled := Count;
               elsif State.Recalled > 1 then
                  State.Recalled := State.Recalled - 1;
               end if;
               Recall;
            elsif Event.Kind = Down and then State.Recalled > 0 then
               if State.Recalled < Count then
                  State.Recalled := State.Recalled + 1;
                  Recall;
               else
                  State.Recalled := 0;
                  State.Input := State.Draft;
               end if;
            end if;
         when Page_Up => Scroll_By (State.Page);
         when Page_Down => Scroll_By (-State.Page);
         when Wheel_Up => Scroll_By (WHEEL_ROWS * Line_Height);
         when Wheel_Down => Scroll_By (-WHEEL_ROWS * Line_Height);
         when Toggle_Notation =>
            Set_Notation (State, (if State.Notation = CCL.Interfaces.Console.Lisp
                                  then CCL.Interfaces.Console.Basic else CCL.Interfaces.Console.Lisp));
            Redraw := True;
         when Enter =>
            if State.Popup_Open and then not Event.Shift then
               Accept_Candidate (State, State.Selected);
               Edited := True;
            elsif Event.Shift or else
              (not Event.Control and then HL.Balance (Text) = HL.Open_Forms)
            then
               --  Continue the form on a new line, indented by its depth.
               Edit.Insert (State.Input, ASCII.LF &
                 String'(1 .. 2 * Natural'Min (HL.Open_Depth (Text (Text'First .. Text'First + Cursor - 2)), 16) => ' '),
                 Changed);
               Edited := True;
            else
               Submit;
            end if;
         when Run => Submit;
         when Tab =>
            if State.Popup_Open then
               Accept_Candidate (State, State.Selected);
            else
               Refresh_Completion (State, Explicit => True);
               if State.Candidate_Total = 1 then
                  Accept_Candidate (State, 1);
               elsif State.Candidate_Total = 0 then
                  Edit.Insert (State.Input, "  ", Changed);
               end if;
            end if;
            Edited := True;
         when Complete => Refresh_Completion (State, Explicit => True);
         when Escape =>
            if State.Popup_Open then
               State.Popup_Open := False;
            else
               Set_Input (State, "");
               State.Signature_Visible := False;
            end if;
         when Select_All => Edit.Select_All (State.Input);
         when Pointer_Move =>
            State.Pointer_X := Event.X; State.Pointer_Y := Event.Y;
            State.Pointer_Known := True;
            if State.Selecting then
               Place_From_Pointer (Extend => True);
            else
               declare
                  Now_Hovered : constant Hit_Count := Hit_At (State, Event.X, Event.Y);
               begin
                  Redraw := Now_Hovered /= State.Hovered;
                  State.Hovered := Now_Hovered;
               end;
            end if;
         when Pointer_Down =>
            State.Pointer_X := Event.X; State.Pointer_Y := Event.Y;
            declare
               Target : constant Hit_Count := Hit_At (State, Event.X, Event.Y);
               Entry_Value : CCL.Sessions.Submission;
               Found : Boolean;
            begin
               if Target = 0 then
                  State.Popup_Open := False;
               else
                  declare
                     H : constant Hit := State.Hits (Target);
                  begin
                     case H.Kind is
                        when Example_Hit => Set_Input (State, Example (H.Item));
                        when Source_Hit | Name_Hit | Badge_Hit =>
                           CCL.Sessions.Recall (State.Session, H.Item, Entry_Value, Found);
                           if Found and then not Entry_Value.Source_Truncated then
                              Set_Input (State, Displayed (State, H.Item, Entry_Value));
                           end if;
                        when Result_Hit =>
                           CCL.Sessions.Recall (State.Session, H.Item, Entry_Value, Found);
                           if Found and then Entry_Value.Outcome.Status = CCL.Language.Succeeded then
                              Edit.Insert (State.Input, Result_Source (Entry_Value.Outcome), Changed);
                           end if;
                        when Cell_Hit =>
                           CCL.Sessions.Recall (State.Session, H.Item, Entry_Value, Found);
                           if Found and then H.Last <= Entry_Value.Outcome.Literal.Length then
                              if Event.Control and then H.Column > 0 then
                                 --  Drill down: keep the rows whose field equals this cell.
                                 Set_Input (State, Row_Query
                                   ("where", Entry_Value,
                                    "(= " & Field_Access (Entry_Value, H.Column) & " " &
                                    Entry_Value.Outcome.Literal.Data (H.First .. H.Last) & ")"));
                              else
                                 Edit.Insert (State.Input, Entry_Value.Outcome.Literal.Data (H.First .. H.Last), Changed);
                              end if;
                           end if;
                        when Header_Hit =>
                           --  Sort the rows by this field.
                           CCL.Sessions.Recall (State.Session, H.Item, Entry_Value, Found);
                           if Found then
                              Set_Input (State, Row_Query
                                ("sort-by", Entry_Value, Field_Access (Entry_Value, H.Column)));
                           end if;
                        when Completion_Hit => Accept_Candidate (State, H.Item);
                        when Live_Hit =>
                           State.Meta (H.Item).Live := False;
                           Set_Notice (State, "Entry" & Natural'Image (H.Item) & " is no longer live");
                        when Input_Hit =>
                           Place_From_Pointer (Extend => Event.Shift);
                           State.Selecting := True;
                        when No_Hit => null;
                     end case;
                  end;
               end if;
               Edited := True;
            end;
         when Pointer_Drag =>
            if State.Selecting then Place_From_Pointer (Extend => True); end if;
         when Pointer_Up => State.Selecting := False;
      end case;
      if Edited and then Event.Kind not in Up | Down then
         if Event.Kind in Text_Input | Backspace | Delete then State.Recalled := 0; end if;
         Refresh_Completion (State, Explicit => False);
      end if;
   end Handle;

   ---------------------------------------------------------------------------
   --  Drawing
   ---------------------------------------------------------------------------
   procedure Add_Hit (State : in out View_State; Item : Hit) is
   begin
      if State.Hit_Total < Maximum_Hits and then not Is_Empty (Item.Area) then
         State.Hit_Total := State.Hit_Total + 1;
         State.Hits (State.Hit_Total) := Item;
      end if;
   end Add_Hit;

   --  Draws Text wrapped at Columns from (X, Y), coloured by its marks (or
   --  in Plain when Plain_Only), skipping rows outside [Top, Bottom). Host
   --  operation names become Name_Hit regions for entry Item when Item > 0.
   procedure Draw_Text_Block
     (State : in out View_State; C : Canvas; X : Natural; Y : Integer;
      Text : String; Columns : Positive; Background : Color;
      Top, Bottom : Integer; Plain : Color := INK; Plain_Only : Boolean := False;
      Item : Natural := 0)
   is
      Marks : HL.Mark_Map (1 .. Text'Length);
      CW : constant Positive := Column_Width;
      LH : constant Positive := Line_Height;
      Row, Column : Natural := 0;
      Run_First : Positive := 1;
      Run_Column : Natural := 0;
      Run_Length : Natural := 0;
      Run_Color : Color := Plain;
      Run_Class : HL.Token_Class := HL.Whitespace;

      procedure Flush is
         Row_Y : constant Integer := Y + Row * LH;
      begin
         if Run_Length > 0 and then Row_Y >= Top - LH and then Row_Y < Bottom and then Row_Y >= 0 then
            Draw_Code_Text (C, X + Run_Column * CW, Natural (Row_Y),
              Text (Text'First + Run_First - 1 .. Text'First + Run_First + Run_Length - 2),
              Run_Color, Background);
            if Item > 0 and then Run_Class = HL.Host_Operation then
               Add_Hit (State, (Name_Hit, (X + Run_Column * CW, Natural (Row_Y), Run_Length * CW, LH),
                                Item, Run_First, Run_First + Run_Length - 1, 0));
            end if;
         end if;
         Run_Length := 0;
      end Flush;
   begin
      if Plain_Only then
         Marks := [others => (HL.Name, 0)];
      else
         HL.Classify (Text, Marks);
      end if;
      for I in 1 .. Text'Length loop
         declare
            Ch : constant Character := Text (Text'First + I - 1);
            Shade : constant Color := (if Plain_Only then Plain else Mark_Color (Marks (I)));
         begin
            if Ch = ASCII.LF then
               Flush;
               Row := Row + 1; Column := 0;
            else
               if Column = Columns then
                  Flush;
                  Row := Row + 1; Column := 0;
               end if;
               if Run_Length > 0 and then (Shade /= Run_Color or else Marks (I).Class /= Run_Class) then
                  Flush;
               end if;
               if Run_Length = 0 then
                  Run_First := I; Run_Column := Column; Run_Color := Shade; Run_Class := Marks (I).Class;
               end if;
               Run_Length := Run_Length + 1;
               Column := Column + 1;
            end if;
         end;
      end loop;
      Flush;
   end Draw_Text_Block;

   --  A rounded-feeling pill: fill, 1px border, label centred.
   procedure Draw_Pill (C : Canvas; Area : Rect; Label : String; Ink, Border, Fill : Color) is
   begin
      Fill_Rect (C, Area, Fill);
      Fill_Rect (C, (Area.x + 1, Area.y, Area.w - 2, 1), Border);
      Fill_Rect (C, (Area.x + 1, Area.y + Area.h - 1, Area.w - 2, 1), Border);
      Fill_Rect (C, (Area.x, Area.y + 1, 1, Area.h - 2), Border);
      Fill_Rect (C, (Area.x + Area.w - 1, Area.y + 1, 1, Area.h - 2), Border);
      Draw_UI_Text_Transparent (C, Area.x + (Area.w - UI_Text_Width (Label)) / 2,
        Area.y + (Area.h - UI_Text_Height) / 2, Label, Ink);
   end Draw_Pill;

   --  A zigzag under one character cell: the diagnostic's position.
   procedure Draw_Squiggle (C : Canvas; X, Y, Width : Natural) is
   begin
      for I in 0 .. Width - 1 loop
         Set_Pixel (C, X + I, Y + (if (I / 2) mod 2 = 0 then 0 else 1), DANGER);
      end loop;
   end Draw_Squiggle;

   function Pill_Width (Label : String) return Natural is (UI_Text_Width (Label) + 16);

   ---------------------------------------------------------------------------
   --  Tables: a record, or a list of them, laid out under its field names.
   ---------------------------------------------------------------------------
   MAXIMUM_TABLE_ROWS : constant := 20;   --  rows a card shows
   COLUMN_GAP : constant := 2;            --  characters between columns
   MINIMUM_COLUMN : constant := 4;
   type Width_Array is array (CCL.Types.Component_Index) of Natural;
   type Table_View is record
      Cells : CCL.Literal_Tables.Table;
      Shape : CCL.Types.Shapes.Row_Shape;
      Shown : Natural := 0;
      Widths : Width_Array := [others => 0];
   end record;

   --  The result as a table fitting Columns characters, when it has rows.
   --  A table cell as shown: its literal, in its column type's unit
   --  (CCL.Units): sizes, times and modes as people read them.
   function Cell_Text
     (Outcome : CCL.Language.Interpretation_Result; View : Table_View; Row, Column : Positive)
      return String
   is
      Cell : constant CCL.Literal_Tables.Span := View.Cells.Cells (Row, Column);
      Literal : constant String := Outcome.Literal.Data (1 .. Outcome.Literal.Length);
   begin
      if Cell.First not in Literal'Range or else Cell.Last not in Literal'Range then return ""; end if;
      return CCL.Units.Humanize
        (CCL.Units.Unit_Of (CCL.Types.Image (View.Shape.Fields (Column).Type_Name)),
         Literal (Cell.First .. Cell.Last));
   end Cell_Text;

   procedure Tabulate
     (Outcome : CCL.Language.Interpretation_Result; Columns : Positive;
      View : out Table_View; Found : out Boolean)
   is
      Shown : CCL.Presentations.Presentation;
      Total : Natural := 0;
   begin
      View := (others => <>);
      CCL.Presentations.Describe (Outcome, Shown);
      Found := Shown.Kind = CCL.Presentations.Table;
      if not Found then return; end if;
      View.Shape := Shown.Shape;
      View.Cells := Shown.Cells;
      View.Shown := Natural'Min (View.Cells.Rows, MAXIMUM_TABLE_ROWS);
      for C in 1 .. View.Shape.Count loop
         View.Widths (C) := Natural'Max (MINIMUM_COLUMN, View.Shape.Fields (C).Identifier.Length);
         for R in 1 .. View.Shown loop
            View.Widths (C) := Natural'Max (View.Widths (C), Cell_Text (Outcome, View, R, C)'Length);
         end loop;
         Total := Total + View.Widths (C) + COLUMN_GAP;
      end loop;
      --  Narrow the widest column until the table fits.
      while Total > Columns loop
         declare
            Widest : CCL.Types.Component_Index := 1;
         begin
            for C in 2 .. View.Shape.Count loop
               if View.Widths (C) > View.Widths (Widest) then Widest := C; end if;
            end loop;
            exit when View.Widths (Widest) <= MINIMUM_COLUMN;
            View.Widths (Widest) := View.Widths (Widest) - 1;
            Total := Total - 1;
         end;
      end loop;
   end Tabulate;

   ---------------------------------------------------------------------------
   --  Pictures: an Image result, drawn from its stored pixels.
   ---------------------------------------------------------------------------
   MAXIMUM_PICTURE_HEIGHT : constant := 280;
   MAXIMUM_PICTURE_SCALE : constant := 8;
   PICTURE_FRAME : constant := 1;

   --  The size an image is shown at within Available pixels of width:
   --  whole-number enlargement while it fits, else reduction.
   procedure Picture_Size
     (Width, Height, Available : Natural; Shown_Width, Shown_Height : out Natural;
      Maximum_Height : Positive := MAXIMUM_PICTURE_HEIGHT)
   is
      Scale : Positive := 1;
   begin
      Shown_Width := 0;
      Shown_Height := 0;
      if Width = 0 or else Height = 0 then return; end if;
      while Scale < MAXIMUM_PICTURE_SCALE and then Width * (Scale + 1) <= Available and then
        Height * (Scale + 1) <= Maximum_Height
      loop
         Scale := Scale + 1;
      end loop;
      Shown_Width := Width * Scale;
      Shown_Height := Height * Scale;
      if Shown_Width > Available then
         Shown_Height := Natural'Max (1, Shown_Height * Available / Shown_Width);
         Shown_Width := Available;
      end if;
      if Shown_Height > Maximum_Height then
         Shown_Width := Natural'Max (1, Shown_Width * Maximum_Height / Shown_Height);
         Shown_Height := Maximum_Height;
      end if;
   end Picture_Size;

   --  Galleries: thumbnails in rows, each in a box, captioned.
   THUMB_WIDTH : constant := 160;
   THUMB_HEIGHT : constant := 120;
   THUMB_GAP : constant := 12;
   function Thumbs_Per_Row (Available : Natural) return Positive is
     (Positive'Max (1, (Available + THUMB_GAP) / (THUMB_WIDTH + THUMB_GAP)));
   function Gallery_Rows (Item : CCL.Presentations.Presentation; Columns : Positive) return Positive is
      Per_Row : constant Positive := Thumbs_Per_Row (Columns * Column_Width);
      Lines : constant Natural := (Item.Picture_Count + Per_Row - 1) / Per_Row;
   begin
      return Positive'Max (1, (Lines * (THUMB_HEIGHT + Line_Height + THUMB_GAP) + Line_Height - 1) / Line_Height);
   end Gallery_Rows;

   --  A picture's rows in a card: the image, then its caption.
   function Picture_Rows (Item : CCL.Presentations.Presentation; Columns : Positive) return Positive is
      Shown_Width, Shown_Height : Natural;
   begin
      Picture_Size (Item.Image_Width, Item.Image_Height, Columns * Column_Width, Shown_Width, Shown_Height);
      return (Shown_Height + 2 * PICTURE_FRAME + Line_Height - 1) / Line_Height + 1;
   end Picture_Rows;

   --  Rows the result area of a card takes.
   function Result_Rows
     (Outcome : CCL.Language.Interpretation_Result; Columns : Positive) return Positive
   is
      View : Table_View;
      Found : Boolean;
      Shown : CCL.Presentations.Presentation;
   begin
      CCL.Presentations.Describe (Outcome, Shown);
      if Shown.Kind = CCL.Presentations.Picture then
         return Picture_Rows (Shown, Columns);
      elsif Shown.Kind = CCL.Presentations.Gallery then
         return Gallery_Rows (Shown, Columns);
      end if;
      Tabulate (Outcome, Columns, View, Found);
      if Found then
         return 1 + View.Shown + (if View.Cells.Total > View.Shown then 1 else 0);
      end if;
      return Row_Count (CCL.Sessions.Result_Value_Image (Outcome), Columns);
   end Result_Rows;

   procedure Draw_Frame
     (State : in out View_State; Canvas : CuBit.UI.Canvas;
      Bounds : CuBit.UI.Rect)
   is
      Text : constant String := Edit.Content (State.Input);
      G : constant Geometry := Layout (Bounds, Text);
      C : constant CuBit.UI.Canvas := With_Clip (Canvas, Bounds);
      CW : constant Positive := Column_Width;
      LH : constant Positive := Line_Height;
      Count : constant CCL.Sessions.History_Count := CCL.Sessions.Length (State.Session);
      Card_X : constant Natural := Bounds.x + MARGIN;
      Card_W : constant Natural := (if Bounds.w > 2 * MARGIN then Bounds.w - 2 * MARGIN else 1);
      Text_X : constant Natural := Card_X + GUTTER + CARD_PADDING;
      Previous_Hover : constant Hit := (if State.Hovered in 1 .. State.Hit_Total
                                        then State.Hits (State.Hovered) else (others => <>));

      --  Source columns beside the type badge, which sits at the top right.
      function Source_Columns (Outcome : CCL.Language.Interpretation_Result) return Positive is
        (Positive'Max (8, G.Card_Columns -
           (if CCL.Sessions.Result_Type_Image (Outcome)'Length = 0 then 0
            else Pill_Width (CCL.Sessions.Result_Type_Image (Outcome)) / CW + 2)));

      function Card_Height (I : CCL.Sessions.History_Index; Entry_Value : CCL.Sessions.Submission)
        return Positive is
        (2 * CARD_PADDING +
         LH * (Row_Count (Displayed (State, I, Entry_Value),
                          Source_Columns (Entry_Value.Outcome)) +
               Result_Rows (Entry_Value.Outcome, Positive'Max (1, G.Card_Columns - 2))) + 4);

      procedure Draw_Header is
         Stats : constant String :=
           Image (Natural (State.Visible_Interfaces)) &
           (if State.Visible_Interfaces = 1 then " interface  " else " interfaces  ") &
           Image (CCL.Sessions.Kept_Definitions (State.Session)) & " definitions  " &
           Image (CCL.Sessions.Kept_Values (State.Session)) & " values  " &
           "fuel " & Grouped (CCL.Sessions.Default_Fuel) & "  " &
           (if State.Notation = CCL.Interfaces.Console.Basic then "BASIC" else "Lisp");
         Brand_X : constant Natural := Bounds.x + MARGIN;
         Y : constant Natural := G.Header.y + (G.Header.h - UI_Text_Height) / 2;
      begin
         Fill_Vertical_Gradient (C, G.Header, HEADER_TOP, HEADER_BOTTOM);
         Fill_Rect (C, (G.Header.x, G.Header.y + G.Header.h - 1, G.Header.w, 1), EDGE);
         --  The mark: a small accent block, then the name.
         Fill_Rect (C, (Brand_X, G.Header.y + 10, 4, G.Header.h - 20), ACCENT);
         Draw_UI_Text_Transparent (C, Brand_X + 12, Y, "CCL", ACCENT);
         Draw_UI_Text_Transparent (C, Brand_X + 12 + UI_Text_Width ("CCL "), Y, "console", INK);
         if Bounds.w > UI_Text_Width (Stats) + 260 then
            Draw_UI_Text_Transparent
              (C, Bounds.x + Bounds.w - MARGIN - UI_Text_Width (Stats), Y, Stats, MUTED);
         end if;
      end Draw_Header;

      procedure Draw_Welcome (Top : Natural) is
         Y : Natural := Top + 18;
      begin
         Draw_UI_Text_Transparent (C, Card_X, Y, "Everything CCL, at your fingertips.", INK);
         Y := Y + UI_Text_Height + 6;
         Draw_UI_Text_Transparent (C, Card_X, Y,
           "Type an expression and press Enter. Click an example to try it.", MUTED);
         Y := Y + UI_Text_Height + 14;
         for I in Example_Index loop
            declare
               Sample : constant String := Example (I);
               Area : constant Rect := (Card_X, Y, Natural'Min (Card_W, (Sample'Length + 4) * CW), LH + 8);
               Hovered : constant Boolean :=
                 Previous_Hover.Kind = Example_Hit and then Previous_Hover.Item = I;
            begin
               exit when Y + Area.h > G.Transcript.y + G.Transcript.h;
               Fill_Rect (C, Area, (if Hovered then CARD_HOVER else CARD));
               Fill_Rect (C, (Area.x, Area.y, 2, Area.h), (if Hovered then ACCENT else FAINT));
               Draw_Text_Block (State, C, Area.x + 2 * CW, Y + 4, Sample, Card_W / CW,
                 (if Hovered then CARD_HOVER else CARD), Area.y, Area.y + Area.h);
               Add_Hit (State, (Example_Hit, Area, I, 0, 0, 0));
               Y := Y + Area.h + 6;
            end;
         end loop;
         Y := Y + 12;
         if Y + 3 * (UI_Text_Height + 4) < G.Transcript.y + G.Transcript.h then
            Draw_UI_Text_Transparent (C, Card_X, Y,
              "Enter runs a complete form; an open form continues on a new line.", MUTED);
            Y := Y + UI_Text_Height + 4;
            Draw_UI_Text_Transparent (C, Card_X, Y,
              "Tab or Ctrl+Space completes. Up recalls history. Click a past entry to edit it,", MUTED);
            Y := Y + UI_Text_Height + 4;
            Draw_UI_Text_Transparent (C, Card_X, Y,
              "or its result to insert the value. :env :reset :ps :cd :ls :files :save :load", MUTED);
         end if;
      end Draw_Welcome;

      procedure Draw_Transcript is
         TC : constant CuBit.UI.Canvas := With_Clip (C, G.Transcript);
         Top : constant Integer := G.Transcript.y;
         Bottom : constant Integer := G.Transcript.y + G.Transcript.h;
         Heights : array (1 .. CCL.Sessions.Maximum_History) of Natural := [others => 0];
         Total : Natural := CARD_GAP;
         Y : Integer;
         Entry_Value : CCL.Sessions.Submission;
         Found : Boolean;

         --  Header (field names), then the rows, zebra-striped, each cell a
         --  clickable value; numbers align right, long cells end in "~".
         procedure Draw_Table
           (C : CuBit.UI.Canvas; X : Natural; Y : Integer;
            Outcome : CCL.Language.Interpretation_Result; View : Table_View;
            Fill : Color; Item : Positive)
         is
            Total_Width : Natural := 0;
            function Column_X (Column : CCL.Types.Component_Index) return Natural is
               Offset : Natural := 0;
            begin
               for K in 1 .. Column - 1 loop
                  Offset := Offset + View.Widths (K) + COLUMN_GAP;
               end loop;
               return X + Offset * CW;
            end Column_X;
            function Visible (Row_Y : Integer) return Boolean is
              (Row_Y >= Top and then Row_Y + LH <= Bottom and then Row_Y >= 0);
         begin
            for K in 1 .. View.Shape.Count loop
               Total_Width := Total_Width + View.Widths (K) + COLUMN_GAP;
            end loop;
            if Visible (Y) then
               for K in 1 .. View.Shape.Count loop
                  declare
                     Field_Name : constant String := CCL.Types.Image (View.Shape.Fields (K).Identifier);
                     Shown_Name : constant String :=
                       Field_Name (Field_Name'First ..
                         Field_Name'First + Natural'Min (Field_Name'Length, View.Widths (K)) - 1);
                     Name_X : constant Natural :=
                       (if View.Shape.Fields (K).Numeric
                        then Column_X (K) + (View.Widths (K) - Shown_Name'Length) * CW
                        else Column_X (K));
                  begin
                     Draw_UI_Text_Transparent (C, Name_X, Natural (Y) + 1, Shown_Name, MUTED);
                     Add_Hit (State, (Header_Hit, (Column_X (K), Natural (Y), View.Widths (K) * CW, LH),
                                      Item, 0, 0, K));
                  end;
               end loop;
               Fill_Rect (C, (X, Natural (Y) + LH - 2, Natural'Max (1, Total_Width * CW), 1), EDGE);
            end if;
            for R in 1 .. View.Shown loop
               declare
                  Row_Y : constant Integer := Y + R * LH;
                  Shade : constant Color := (if R mod 2 = 0 then CARD_HOVER else Fill);
               begin
                  if Visible (Row_Y) then
                     if R mod 2 = 0 then
                        Fill_Rect (C, (X, Natural (Row_Y), Natural'Max (1, Total_Width * CW), LH), Shade);
                     end if;
                     for K in 1 .. View.Shape.Count loop
                        declare
                           Cell : constant CCL.Literal_Tables.Span := View.Cells.Cells (R, K);
                           Human : constant String := Cell_Text (Outcome, View, R, K);
                           Length : constant Natural := Human'Length;
                           Fits : constant Boolean := Length <= View.Widths (K);
                           Shown : constant Natural := (if Fits then Length else View.Widths (K) - 1);
                           Text : constant String :=
                             Human (Human'First .. Human'First + Shown - 1) &
                             (if Fits then "" else "~");
                           Cell_X : constant Natural :=
                             (if View.Shape.Fields (K).Numeric and then Fits
                              then Column_X (K) + (View.Widths (K) - Length) * CW
                              else Column_X (K));
                        begin
                           Draw_Text_Block (State, C, Cell_X, Row_Y, Text, Positive'Max (1, Text'Length),
                             Shade, Top, Bottom);
                           Add_Hit (State, (Cell_Hit, (Column_X (K), Natural (Row_Y), View.Widths (K) * CW, LH),
                                            Item, Cell.First, Cell.Last, K));
                        end;
                     end loop;
                  end if;
               end;
            end loop;
            if View.Cells.Total > View.Shown and then Visible (Y + (View.Shown + 1) * LH) then
               Draw_UI_Text_Transparent (C, X, Natural (Y + (View.Shown + 1) * LH) + 1,
                 "+" & Image (View.Cells.Total - View.Shown) & " more rows", MUTED);
            end if;
         end Draw_Table;

         --  The image at (X, Y), framed, then a caption; expired pixels are
         --  said to be so, never shown as something else.
         procedure Draw_Picture
           (C : CuBit.UI.Canvas; X : Natural; Y : Integer;
            Item : CCL.Presentations.Presentation; Available : Positive; Entry_Index : Positive)
         is
            Shown_Width, Shown_Height : Natural;
            Known : constant Boolean := CCL.Image_Store.Known (Item.Image);
            Image_Top : constant Integer := Y + PICTURE_FRAME;
            Caption_Y : Integer;
         begin
            Picture_Size (Item.Image_Width, Item.Image_Height, Available, Shown_Width, Shown_Height);
            Caption_Y := Image_Top + Shown_Height + PICTURE_FRAME + 2;
            if Shown_Width = 0 then return; end if;
            for DY in 0 .. Shown_Height - 1 loop
               declare
                  Row_Y : constant Integer := Image_Top + DY;
                  Source_Y : constant Natural := DY * Item.Image_Height / Shown_Height;
               begin
                  if Row_Y >= Top and then Row_Y < Bottom and then Row_Y >= 0 then
                     if not Known then
                        Fill_Rect (C, (X, Natural (Row_Y), Shown_Width, 1),
                          (if (DY / 6) mod 2 = 0 then CARD_HOVER else CARD));
                     else
                        declare
                           Run_Start : Natural := 0;
                           Run_Colour : Color := CCL.Image_Store.Pixel_At (Item.Image, 0, Source_Y);
                        begin
                           --  Runs of one colour as single fills.
                           for DX in 1 .. Shown_Width loop
                              declare
                                 Colour : constant Color :=
                                   (if DX < Shown_Width
                                    then CCL.Image_Store.Pixel_At (Item.Image, DX * Item.Image_Width / Shown_Width, Source_Y)
                                    else not Run_Colour);
                              begin
                                 if Colour /= Run_Colour then
                                    Fill_Rect (C, (X + Run_Start, Natural (Row_Y), DX - Run_Start, 1),
                                               Run_Colour);
                                    Run_Start := DX;
                                    Run_Colour := Colour;
                                 end if;
                              end;
                           end loop;
                        end;
                     end if;
                  end if;
               end;
            end loop;
            if Image_Top >= Top and then Image_Top + Shown_Height <= Bottom and then Y >= 0 then
               Stroke_Rect (C, (X - PICTURE_FRAME, Natural (Y), Shown_Width + 2 * PICTURE_FRAME,
                                Shown_Height + 2 * PICTURE_FRAME), EDGE, EDGE);
               Add_Hit (State, (Result_Hit, (X, Natural (Image_Top), Shown_Width, Shown_Height),
                                Entry_Index, 0, 0, 0));
            end if;
            if Caption_Y >= Top and then Caption_Y + LH <= Bottom then
               Draw_UI_Text_Transparent (C, X, Natural (Caption_Y),
                 (if Known then Image (Item.Image_Width) & " x " & Image (Item.Image_Height) & " pixels" &
                    (if Shown_Width /= Item.Image_Width then
                       "  |  shown at " & Image (Shown_Width) & " x " & Image (Shown_Height) else "")
                  else "image expired: no longer in this process's image store"),
                 (if Known then MUTED else DANGER));
            end if;
         end Draw_Picture;

         --  Each picture fitted into a thumbnail box; a click inserts it.
         procedure Draw_Gallery
           (C : CuBit.UI.Canvas; X : Natural; Y : Integer;
            Item : CCL.Presentations.Presentation; Available : Positive; Entry_Index : Positive)
         is
            Per_Row : constant Positive := Thumbs_Per_Row (Available);
         begin
            for K in 1 .. Item.Picture_Count loop
               declare
                  P : CCL.Presentations.Picture_Entry renames Item.Pictures (K);
                  Box_X : constant Natural := X + ((K - 1) mod Per_Row) * (THUMB_WIDTH + THUMB_GAP);
                  Box_Y : constant Integer := Y + ((K - 1) / Per_Row) * (THUMB_HEIGHT + LH + THUMB_GAP);
                  Shown_Width, Shown_Height : Natural;
                  Known : constant Boolean := CCL.Image_Store.Known (P.Image);
               begin
                  Picture_Size (P.Width, P.Height, THUMB_WIDTH, Shown_Width, Shown_Height, THUMB_HEIGHT);
                  if Box_Y >= Top and then Box_Y + THUMB_HEIGHT + LH <= Bottom and then Shown_Width > 0 then
                     Fill_Rect (C, (Box_X, Natural (Box_Y), THUMB_WIDTH, THUMB_HEIGHT), BACKGROUND);
                     declare
                        Left : constant Natural := Box_X + (THUMB_WIDTH - Shown_Width) / 2;
                        Up : constant Natural := Natural (Box_Y) + (THUMB_HEIGHT - Shown_Height) / 2;
                     begin
                        for DY in 0 .. Shown_Height - 1 loop
                           for DX in 0 .. Shown_Width - 1 loop
                              Set_Pixel (C, Left + DX, Up + DY,
                                (if Known then CCL.Image_Store.Pixel_At
                                   (P.Image, DX * P.Width / Shown_Width, DY * P.Height / Shown_Height)
                                 elsif (DY / 6) mod 2 = 0 then CARD_HOVER else CARD));
                           end loop;
                        end loop;
                     end;
                     Stroke_Rect (C, (Box_X, Natural (Box_Y), THUMB_WIDTH, THUMB_HEIGHT), EDGE, EDGE);
                     Draw_UI_Text_Transparent (C, Box_X, Natural (Box_Y) + THUMB_HEIGHT + 2,
                       (if Known then Image (P.Width) & " x " & Image (P.Height) else "expired"),
                       (if Known then MUTED else DANGER));
                     --  The whole element, "(Image w h id)": from before its first
                     --  field to after its last. Column 0: an element, not a field.
                     Add_Hit (State, (Cell_Hit, (Box_X, Natural (Box_Y), THUMB_WIDTH, THUMB_HEIGHT),
                                      Entry_Index,
                                      Natural'Max (1, Item.Cells.Cells (K, 1).First - (Item.Shape.Row_Type.Length + 2)),
                                      Item.Cells.Cells (K, Item.Shape.Count).Last + 1, 0));
                  end if;
               end;
            end loop;
         end Draw_Gallery;
      begin
         Fill_Rect (C, G.Transcript, BACKGROUND);
         if Count = 0 then
            State.Scroll_Limit := 0;
            Draw_Welcome (G.Transcript.y);
            return;
         end if;
         for I in 1 .. Count loop
            CCL.Sessions.Recall (State.Session, I, Entry_Value, Found);
            Heights (I) := (if Found then Card_Height (I, Entry_Value) else 0);
            Total := Total + Heights (I) + CARD_GAP;
         end loop;
         State.Scroll_Limit := (if Total > G.Transcript.h then Total - G.Transcript.h else 0);
         State.Scroll := Natural'Min (State.Scroll, State.Scroll_Limit);
         --  Top-anchored while it fits; otherwise the newest entry sits just
         --  above the input unless scrolled back.
         Y := Top + CARD_GAP - (State.Scroll_Limit - State.Scroll);
         for I in 1 .. Count loop
            if Y + Heights (I) > Top and then Y < Bottom then
               CCL.Sessions.Recall (State.Session, I, Entry_Value, Found);
               if Found then
                  declare
                     Outcome : CCL.Language.Interpretation_Result renames Entry_Value.Outcome;
                     Source : constant String := Displayed (State, I, Entry_Value);
                     Ok : constant Boolean := Outcome.Status = CCL.Language.Succeeded;
                     Hovered : constant Boolean :=
                       Previous_Hover.Kind in Source_Hit | Result_Hit | Name_Hit | Badge_Hit and then
                       Previous_Hover.Item = I;
                     Fill : constant Color := (if Hovered then CARD_HOVER else CARD);
                     Wrap : constant Positive := Source_Columns (Outcome);
                     Source_Rows : constant Positive := Row_Count (Source, Wrap);
                     Result_Y : constant Integer := Y + CARD_PADDING + Source_Rows * LH + 4;
                     Type_Label : constant String := CCL.Sessions.Result_Type_Image (Outcome);
                     Value : constant String := CCL.Sessions.Result_Value_Image (Outcome);
                     Cost : constant String :=
                       Elapsed_Image (State.Meta (I).Elapsed_Ms) & "  " &
                       Grouped (State.Meta (I).Fuel_Used) & " fuel";
                     Card_Top : constant Natural := Natural (Integer'Max (Top, Y));
                     Table : Table_View;
                     Tabular : Boolean;
                     Shown : CCL.Presentations.Presentation;
                     Card_Bottom : constant Natural := Natural (Integer'Min (Bottom, Y + Heights (I)));
                  begin
                     if Card_Bottom > Card_Top then
                        Fill_Rect (TC, (Card_X, Card_Top, Card_W, Card_Bottom - Card_Top), Fill);
                        Fill_Rect (TC, (Card_X, Card_Top, GUTTER, Card_Bottom - Card_Top),
                          (if Ok then GOOD else DANGER));
                     end if;
                     --  Source, highlighted.
                     Draw_Text_Block (State, TC, Text_X, Y + CARD_PADDING, Source, Wrap,
                       Fill, Top, Bottom, Item => I);
                     if Card_Bottom > Card_Top then
                        Add_Hit (State, (Source_Hit,
                          (Card_X, Natural'Max (Card_Top, Natural (Integer'Max (0, Y))),
                           Card_W, Natural'Max (0, Integer'Min (Card_Bottom, Result_Y) -
                                     Integer'Max (Card_Top, Y))), I, 0, 0, 0));
                     end if;
                     --  A diagnostic's position, underlined in the source.
                     if not Ok and then Outcome.Diagnostic_Position in 1 .. Source'Length then
                        declare
                           Row, Column : Natural;
                           Mark_Y : Integer;
                        begin
                           Locate (Source, Wrap, Outcome.Diagnostic_Position, Row, Column);
                           Mark_Y := Y + CARD_PADDING + (Row + 1) * LH - 2;
                           if Mark_Y in Top .. Bottom - 2 then
                              Draw_Squiggle (TC, Text_X + Column * CW, Mark_Y, CW);
                           end if;
                        end;
                     end if;
                     --  The result, its type as a badge, and what it cost.
                     if Result_Y >= Top - LH and then Result_Y < Bottom and then Result_Y >= 0 then
                        Draw_Code_Text (TC, Text_X, Natural (Result_Y), (if Ok then "=" else "!"),
                          (if Ok then ACCENT else DANGER), Fill);
                     end if;
                     CCL.Presentations.Describe (Outcome, Shown);
                     Tabulate (Outcome, Positive'Max (1, G.Card_Columns - 2), Table, Tabular);
                     if Shown.Kind = CCL.Presentations.Picture then
                        Draw_Picture (TC, Text_X + 2 * CW, Result_Y, Shown,
                          Positive'Max (1, G.Card_Columns - 2) * CW, I);
                     elsif Shown.Kind = CCL.Presentations.Gallery then
                        Draw_Gallery (TC, Text_X + 2 * CW, Result_Y, Shown,
                          Positive'Max (1, G.Card_Columns - 2) * CW, I);
                     elsif Tabular then
                        Draw_Table (TC, Text_X + 2 * CW, Result_Y, Outcome, Table, Fill, I);
                     else
                     Draw_Text_Block (State, TC, Text_X + 2 * CW, Result_Y, Value,
                       Positive'Max (1, G.Card_Columns - 2), Fill, Top, Bottom,
                       Plain => (if Ok then STRING_COLOR else DANGER),
                       Plain_Only => not Ok or else
                         (CCL.Sessions.Result_Type (Outcome) = CCL.Language.String_Type and then
                          not Outcome.Has_List and then not Outcome.Has_Literal));
                     end if;
                     if Ok and then not Tabular and then
                       Shown.Kind not in CCL.Presentations.Picture | CCL.Presentations.Gallery and then
                       Result_Y + LH > Top and then Result_Y < Bottom
                     then
                        Add_Hit (State, (Result_Hit,
                          (Text_X, Natural (Integer'Max (Top, Result_Y)),
                           Card_W - (Text_X - Card_X),
                           Natural (Integer'Max (1, Integer'Min (Card_Bottom, Y + Heights (I)) -
                                                    Integer'Max (Top, Result_Y)))), I, 0, 0, 0));
                     end if;
                     declare
                        Badge_W : constant Natural := Pill_Width (Type_Label);
                        Meta : constant String :=
                          (if Tabular and then Table.Shape.Many
                           then Grouped (Table.Cells.Total) &
                                (if Table.Cells.Total = 1 then " row  " else " rows  ")
                           else "") & Cost;
                        Meta_W : constant Natural := UI_Text_Width (Meta);
                        Used_Columns : Natural := 0;
                        Right : constant Natural := Card_X + Card_W - CARD_PADDING;
                        Badge_Y : constant Integer := Y + CARD_PADDING;
                     begin
                        if State.Meta (I).Live and then Badge_Y >= Top and then Badge_Y + LH <= Bottom then
                           declare
                              Label : constant String := "LIVE" &
                                (if State.Meta (I).Interval_Ms = 0 then ""
                                 else Elapsed_Seconds (State.Meta (I).Interval_Ms)) &
                                "  x" & Image (State.Meta (I).Runs);
                              Live_W : constant Natural := Pill_Width (Label);
                              Live_X : constant Natural :=
                                (if Right > Badge_W + Live_W + 6 then Right - Badge_W - Live_W - 6 else Card_X);
                           begin
                              Draw_Pill (TC, (Live_X, Natural (Badge_Y), Live_W, LH), Label, GOOD, GOOD, Fill);
                              Add_Hit (State, (Live_Hit, (Live_X, Natural (Badge_Y), Live_W, LH), I, 0, 0, 0));
                           end;
                        end if;
                        if Type_Label'Length > 0 and then Badge_Y >= Top and then
                          Badge_Y + LH <= Bottom and then Right > Badge_W
                        then
                           Draw_Pill (TC, (Right - Badge_W, Natural (Badge_Y), Badge_W, LH),
                             Type_Label, TYPE_COLOR, FAINT, Fill);
                           Add_Hit (State, (Badge_Hit, (Right - Badge_W, Natural (Badge_Y), Badge_W, LH), I, 0, 0, 0));
                        end if;
                        if Tabular then
                           for K in 1 .. Table.Shape.Count loop
                              Used_Columns := Used_Columns + Table.Widths (K) + COLUMN_GAP;
                           end loop;
                        else
                           Used_Columns := Value'Length;
                        end if;
                        if Result_Y >= Top and then Result_Y + LH <= Bottom and then Right > Meta_W and then
                          Used_Columns * CW + 2 * CW + Meta_W + 3 * CW < Card_W
                        then
                           Draw_UI_Text_Transparent (TC, Right - Meta_W, Natural (Result_Y) + 1, Meta, MUTED);
                        end if;
                     end;
                  end;
               end if;
            end if;
            Y := Y + Heights (I) + CARD_GAP;
         end loop;
         --  How far back the view is, when it is not following the newest.
         if State.Scroll > 0 and then G.Transcript.h > 40 then
            declare
               Label : constant String := "scrolled back - End or Page Down to return";
               W : constant Natural := Pill_Width (Label);
            begin
               Draw_Pill (TC, (G.Transcript.x + (G.Transcript.w - W) / 2,
                 G.Transcript.y + G.Transcript.h - LH - 8, W, LH + 2), Label, ACCENT, ACCENT, POPUP);
            end;
         end if;
         --  A thin scroll indicator on the right edge.
         if State.Scroll_Limit > 0 and then G.Transcript.h > 20 then
            declare
               Track : constant Natural := G.Transcript.h - 8;
               Thumb : constant Natural := Natural'Max (16, Track * G.Transcript.h / (Total + 1));
               Offset : constant Natural :=
                 (Track - Natural'Min (Thumb, Track)) * (State.Scroll_Limit - State.Scroll) / State.Scroll_Limit;
            begin
               Fill_Rect (TC, (Bounds.x + Bounds.w - 5, G.Transcript.y + 4 + Offset, 3,
                 Natural'Min (Thumb, Track)), FAINT);
            end;
         end if;
      end Draw_Transcript;

      procedure Draw_Dock is
         Marks : HL.Mark_Map (1 .. Text'Length);
         Cursor : constant Positive := Edit.Cursor (State.Input);
         Selection_First : constant Positive := Edit.Selection_First (State.Input);
         Selection_Last : constant Positive := Edit.Selection_Last (State.Input);
         Rows : constant Positive := Row_Count (Text, G.Columns);
         Cursor_Row, Cursor_Column : Natural;
         First_Row : Natural := 0;
         Visible_Rows : constant Positive := Positive'Max (1, G.Input.h / LH);
         State_Of_Form : constant HL.Form_State := HL.Balance (Text);
         Partner : Natural := 0;

         function Cell (Position : Positive) return Rect is
            R, Col : Natural;
         begin
            Locate (Text, G.Columns, Position, R, Col);
            if R < First_Row or else R >= First_Row + Visible_Rows then return (others => 0); end if;
            return (G.Input.x + Col * CW, G.Input.y + (R - First_Row) * LH, CW, LH);
         end Cell;
      begin
         Fill_Rect (C, G.Dock, DOCK);
         Fill_Rect (C, (G.Dock.x, G.Dock.y, G.Dock.w, 1), EDGE);
         HL.Classify (Text, Marks);
         Locate (Text, G.Columns, Cursor, Cursor_Row, Cursor_Column);
         if Cursor_Row >= Visible_Rows then First_Row := Cursor_Row - Visible_Rows + 1; end if;
         --  The prompt: accent while the entry is complete, muted while open.
         Draw_Code_Text (C, Bounds.x + MARGIN + CW / 2, G.Input.y, ">",
           (if State_Of_Form = HL.Open_Forms then MUTED
            elsif State_Of_Form = HL.Malformed then DANGER else ACCENT), DOCK);
         for R in 1 .. Natural'Min (Rows, Visible_Rows) - 1 loop
            Draw_Code_Text (C, Bounds.x + MARGIN + CW / 2, G.Input.y + R * LH, ".", FAINT, DOCK);
         end loop;
         --  The partner of a parenthesis at or just before the caret.
         for Probe in reverse Natural'Max (1, Cursor - 1) .. Natural'Min (Cursor, Text'Length) loop
            if Marks (Probe).Class = HL.Delimiter then
               declare
                  Depth : constant HL.Nesting_Depth := Marks (Probe).Depth;
                  Opening : constant Boolean := Text (Text'First + Probe - 1) = '(';
                  Scan : Natural := Probe;
               begin
                  loop
                     Scan := (if Opening then Scan + 1 else Scan - 1);
                     exit when Scan = 0 or else Scan > Text'Length;
                     if Marks (Scan).Class = HL.Delimiter and then Marks (Scan).Depth = Depth and then
                       Text (Text'First + Scan - 1) /= Text (Text'First + Probe - 1)
                     then
                        Partner := Scan;
                        exit;
                     end if;
                  end loop;
                  if Partner > 0 then
                     Stroke_Rect (C, Cell (Probe), FAINT, FAINT);
                     Stroke_Rect (C, Cell (Partner), FAINT, FAINT);
                  end if;
               end;
               exit;
            end if;
         end loop;
         --  Text, selection and caret.
         declare
            IC : constant CuBit.UI.Canvas := With_Clip (C, G.Input);
         begin
            declare
               R, Col : Natural := 0;
            begin
               for I in 1 .. Text'Length loop
                  declare
                     Selected : constant Boolean := I >= Selection_First and then I < Selection_Last;
                     Ch : constant Character := Text (Text'First + I - 1);
                  begin
                     if Ch = ASCII.LF then
                        R := R + 1; Col := 0;
                     else
                        if Col = G.Columns then R := R + 1; Col := 0; end if;
                        if R >= First_Row and then R < First_Row + Visible_Rows then
                           Draw_Code_Text (IC, G.Input.x + Col * CW, G.Input.y + (R - First_Row) * LH + 1,
                             [1 => Ch], Mark_Color (Marks (I)), (if Selected then SELECTION else DOCK));
                        end if;
                        Col := Col + 1;
                     end if;
                  end;
               end loop;
            end;
            if Text'Length = 0 then
               Draw_Code_Text (IC, G.Input.x, G.Input.y + 1, "an expression, or :env", FAINT, DOCK);
            end if;
            --  Ghost text: the rest of the selected completion.
            if State.Popup_Open and then State.Selected <= State.Candidate_Total then
               declare
                  S : CCL.Catalog.Completion.Suggestion renames State.Candidates (State.Selected).Suggestion;
                  Caret : constant Rect := Cell (Cursor);
               begin
                  if S.Length > State.Prefix_Length and then not Is_Empty (Caret) and then
                    (Cursor > Text'Length or else Text (Text'First + Cursor - 1) = ASCII.LF)
                  then
                     Draw_Code_Text (IC, Caret.x, Caret.y + 1,
                       S.Name (State.Prefix_Length + 1 .. S.Length), FAINT, DOCK);
                  end if;
               end;
            end if;
            declare
               Caret : constant Rect := Cell (Cursor);
            begin
               if not Is_Empty (Caret) then
                  Fill_Rect (IC, (Caret.x, Caret.y + 1, 2, LH - 2), ACCENT);
               end if;
            end;
         end;
         Add_Hit (State, (Input_Hit, (G.Input.x, G.Input.y, G.Input.w, G.Input.h), 0, 0, 0, 0));
         --  Status: what Enter will do, or the call being written.
         declare
            Y : constant Natural := G.Status.y + (G.Status.h - UI_Text_Height) / 2;
            Depth : constant Natural := HL.Open_Depth (Text);
            Hint : constant String :=
              (if State.Notice_Length > 0 then State.Notice (1 .. State.Notice_Length)
               elsif State.Popup_Open and then State.Selected <= State.Candidate_Total then
                 "Enter or Tab accepts " &
                 State.Candidates (State.Selected).Suggestion.Name
                   (1 .. State.Candidates (State.Selected).Suggestion.Length) &
                 "  |  Up/Down choose  |  Esc closes"
               elsif State.Signature_Visible
               then CCL.Completions.Describe (State.Signature, State.Signature_Origin)
               else (case State_Of_Form is
                       when HL.Empty => "Enter runs  |  Shift+Enter new line  |  Tab completes  |  Up recalls",
                       when HL.Complete => "Enter runs",
                       when HL.Open_Forms => Image (Depth) & " open form" &
                         (if Depth = 1 then "" else "s") & "  |  Enter continues  |  Ctrl+Enter runs anyway",
                       when HL.Malformed => "unbalanced  |  Enter shows the diagnostic"));
            Position : constant String :=
              "Ln " & Image (Cursor_Row + 1) & ", Col " & Image (Cursor_Column + 1) &
              "  |  " & Image (Text'Length) & "/" & Image (Edit.MAX_TEXT_LENGTH);
         begin
            Fill_Rect (C, (G.Status.x, G.Status.y, G.Status.w, 1), EDGE);
            Draw_UI_Text_Transparent (C, G.Status.x, Y + 1, Hint,
              (if State.Signature_Visible then HOST_COLOR
               elsif State_Of_Form = HL.Malformed then DANGER else MUTED));
            if G.Status.w > UI_Text_Width (Hint) + UI_Text_Width (Position) + 24 then
              Draw_UI_Text_Transparent
                (C, G.Status.x + G.Status.w - UI_Text_Width (Position), Y + 1, Position, FAINT);
            end if;
         end;
      end Draw_Dock;

      procedure Draw_Popup is
         Caret_Row, Caret_Column : Natural;
         Rows : constant Natural := State.Candidate_Total + (if State.Matches_Beyond then 1 else 0);
         Detail : constant Boolean :=
           State.Selected <= State.Candidate_Total and then
           State.Candidates (State.Selected).Origin = Host_Candidate;
         H : constant Natural := Rows * LH + (if Detail then LH + 6 else 0) + 8;
         W : constant Natural := POPUP_COLUMNS * CW;
         X, Y : Natural;
      begin
         if not State.Popup_Open or else State.Candidate_Total = 0 then return; end if;
         Locate (Text, G.Columns, Edit.Cursor (State.Input), Caret_Row, Caret_Column);
         X := Natural'Min (G.Input.x + Natural'Max (0, Caret_Column - Integer (State.Prefix_Length)) * CW,
                           (if Bounds.x + Bounds.w > W + MARGIN then Bounds.x + Bounds.w - W - MARGIN else Bounds.x));
         Y := (if G.Dock.y > H + Bounds.y + 4 then G.Dock.y - H - 4 else Bounds.y);
         Fill_Rect (C, (X + 3, Y + 3, W, H), BACKGROUND);   --  shadow
         Fill_Rect (C, (X, Y, W, H), POPUP);
         Stroke_Rect (C, (X, Y, W, H), EDGE, EDGE);
         for I in 1 .. State.Candidate_Total loop
            declare
               Item : Candidate renames State.Candidates (I);
               Row_Y : constant Natural := Y + 4 + (I - 1) * LH;
               Fill : constant Color := (if I = State.Selected then POPUP_SELECTED else POPUP);
               Name : constant String := Item.Suggestion.Name (1 .. Item.Suggestion.Length);
               Tag : constant String :=
                 (case Item.Origin is
                     when Host_Candidate => "service",
                     when Builtin_Candidate => "built-in",
                     when Form_Candidate => "form");
            begin
               Fill_Rect (C, (X + 2, Row_Y, W - 4, LH), Fill);
               Draw_Code_Text (C, X + CW, Row_Y + 1, Name (Name'First .. Name'First + State.Prefix_Length - 1),
                 ACCENT, Fill);
               Draw_Code_Text (C, X + CW + State.Prefix_Length * CW, Row_Y + 1,
                 Name (Name'First + State.Prefix_Length .. Name'Last),
                 (case Item.Origin is
                     when Host_Candidate => HOST_COLOR,
                     when Builtin_Candidate => OPERATOR_COLOR,
                     when Form_Candidate => FORM_COLOR), Fill);
               Draw_UI_Text_Transparent (C, X + W - CW - UI_Text_Width (Tag), Row_Y + 2, Tag, MUTED);
               Add_Hit (State, (Completion_Hit, (X + 2, Row_Y, W - 4, LH), I, 0, 0, 0));
            end;
         end loop;
         if State.Matches_Beyond then
            Draw_UI_Text_Transparent (C, X + CW, Y + 4 + State.Candidate_Total * LH + 2,
              "more - keep typing to narrow", MUTED);
         end if;
         if Detail then
            Fill_Rect (C, (X + 2, Y + H - LH - 6, W - 4, 1), EDGE);
            Draw_Code_Text (C, X + CW, Y + H - LH - 2,
              CCL.Completions.Describe
                (State.Candidates (State.Selected).Suggestion,
                 (case State.Candidates (State.Selected).Origin is
                     when Host_Candidate => CCL.Completions.Host_Operation,
                     when Builtin_Candidate => CCL.Completions.Builtin,
                     when Form_Candidate => CCL.Completions.Form)), INK, POPUP);
         end if;
      end Draw_Popup;

      procedure Draw_Tooltip is
         H : Hit;
         Entry_Value : CCL.Sessions.Submission;
         Found : Boolean;
         Operation : CCL.Catalog.Resolved_Operation;
      begin
         if State.Hovered not in 1 .. State.Hit_Total or else not State.Pointer_Known then return; end if;
         H := State.Hits (State.Hovered);
         CCL.Sessions.Recall (State.Session, Natural'Max (1, Natural'Min (H.Item, CCL.Sessions.Maximum_History)),
           Entry_Value, Found);
         declare
            Name : constant String :=
              (if H.Kind = Name_Hit and then Found and then
                  H.Last <= Displayed (State, H.Item, Entry_Value)'Length
               then Displayed (State, H.Item, Entry_Value) (H.First .. H.Last) else "");
            function Described return String is
               S : CCL.Catalog.Completion.Suggestion;
               Known : Boolean;
            begin
               CCL.Sessions.Describe (State.Session, Name, Operation, Known);
               if not Known and then CCL.Hints.Hint (Name)'Length > 0 then
                  return CCL.Hints.Hint (Name);
               elsif not Known or else Name'Length > S.Name'Length then
                  return Name & ": not in this session's catalog";
               end if;
               S.Name (1 .. Name'Length) := Name;
               S.Length := Name'Length;
               S.Contract := Operation;
               return Signature_Image (S);
            end Described;
            function Cell_Tip return String is
               Shape : CCL.Types.Shapes.Row_Shape renames Entry_Value.Outcome.Literal_Shape;
               Maximum_Value : constant := 72;
            begin
               if not Found or else H.Column not in 1 .. Shape.Count or else
                 H.Last > Entry_Value.Outcome.Literal.Length
               then
                  return "";
               end if;
               declare
                  Value : constant String := Entry_Value.Outcome.Literal.Data (H.First .. H.Last);
               begin
                  return CCL.Types.Image (Shape.Fields (H.Column).Identifier) & " : " &
                    CCL.Types.Image (Shape.Fields (H.Column).Type_Name) & " = " &
                    (if Value'Length > Maximum_Value
                     then Value (Value'First .. Value'First + Maximum_Value - 1) & "~" else Value) &
                    "  |  click to insert";
               end;
            end Cell_Tip;
            Tip : constant String :=
              (case H.Kind is
                  when Cell_Hit =>
                    (if H.Column = 0 then "Click to insert this image"
                     else Cell_Tip & "  |  Ctrl+click: rows with this value"),
                  when Header_Hit => "Click to sort the rows by this field",
                  when Live_Hit => "Re-runs on its own; click to stop",
                  when Name_Hit => Described,
                  when Badge_Hit => "Result type " & CCL.Sessions.Result_Type_Image (Entry_Value.Outcome) &
                                    "  |  click to edit the entry",
                  when Source_Hit => "Click to edit this entry again",
                  when Result_Hit => "Click to insert this value at the caret",
                  when Example_Hit => "Click to put this in the input",
                  when others => "");
            W : constant Natural := UI_Text_Width (Tip) + 2 * TOOLTIP_PADDING;
            Height : constant Natural := UI_Text_Height + 2 * TOOLTIP_PADDING - 2;
            X : constant Natural := Natural'Min (State.Pointer_X + 14,
              (if Bounds.x + Bounds.w > W + 4 then Bounds.x + Bounds.w - W - 4 else Bounds.x));
            Y : constant Natural :=
              (if State.Pointer_Y + 22 + Height < Bounds.y + Bounds.h then State.Pointer_Y + 22
               elsif State.Pointer_Y > Height + 8 then State.Pointer_Y - Height - 8 else State.Pointer_Y);
         begin
            if Tip'Length = 0 then return; end if;
            Fill_Rect (C, (X + 2, Y + 2, W, Height), BACKGROUND);
            Fill_Rect (C, (X, Y, W, Height), TOOLTIP);
            Stroke_Rect (C, (X, Y, W, Height), EDGE, EDGE);
            Draw_UI_Text_Transparent (C, X + TOOLTIP_PADDING, Y + TOOLTIP_PADDING - 1, Tip,
              (if H.Kind = Name_Hit then HOST_COLOR else INK));
         end;
      end Draw_Tooltip;
   begin
      State.Hit_Total := 0;
      State.Columns := G.Columns;
      State.Page := Positive'Max (Line_Height, (G.Transcript.h * 3) / 4);
      Fill_Rect (C, Bounds, BACKGROUND);
      Draw_Header;
      Draw_Transcript;
      Draw_Dock;
      Draw_Popup;
      --  The hover was found in the previous frame's regions; find it again
      --  in this frame's so the tooltip and its target agree.
      if State.Pointer_Known then
         State.Hovered := Hit_At (State, State.Pointer_X, State.Pointer_Y);
         if State.Hovered > 0 and then State.Hits (State.Hovered).Kind in Input_Hit | Completion_Hit then
            null;
         elsif State.Hovered > 0 then
            Draw_Tooltip;
         end if;
      end if;
   end Draw_Frame;

   procedure Draw
     (State : in out View_State; Canvas : CuBit.UI.Canvas;
      Bounds : CuBit.UI.Rect) is
   begin
      Update_Shown (State);
      Draw_Frame (State, Canvas, Bounds);
   end Draw;

   procedure Refresh (State : in out View_State; Redraw : out Boolean) is
      Count : constant CCL.Sessions.History_Count := CCL.Sessions.Length (State.Session);
      Outcome : CCL.Language.Interpretation_Result;
      Reevaluated : Boolean;
      Now : Interfaces.Unsigned_64 := Now_Ms;
   begin
      Redraw := False;
      for I in 1 .. Count loop
         declare
            M : Entry_Meta renames State.Meta (I);
         begin
            if M.Live and then Now >= M.Due_Ms then
               Reevaluate (State.Session, I, CCL.Sessions.Default_Fuel, Outcome, Reevaluated);
               declare
                  Finished : constant Interfaces.Unsigned_64 := Now_Ms;
               begin
                  if Reevaluated then
                     M.Runs := M.Runs + 1;
                     M.Elapsed_Ms := (if Finished >= Now then Finished - Now else 0);
                     M.Fuel_Used := CCL.Sessions.Default_Fuel -
                       Natural'Min (Outcome.Fuel_Remaining, CCL.Sessions.Default_Fuel);
                     --  From completion: a slow cell never queues up runs. A
                     --  cell with no period runs only when elements arrive.
                     M.Due_Ms := (if M.Interval_Ms = 0 then Interfaces.Unsigned_64'Last
                                  else Finished + M.Interval_Ms);
                  else
                     M.Live := False;
                     Set_Notice (State, "Entry" & Natural'Image (I) &
                                 " cannot be live: only a plain expression re-runs");
                  end if;
                  Now := Finished;
               end;
               Redraw := True;
            end if;
         end;
      end loop;
   end Refresh;

   procedure Follow (State : in out View_State; Source : String) is
      Outcome : CCL.Language.Interpretation_Result;
      Before : constant CCL.Sessions.History_Count := CCL.Sessions.Length (State.Session);
      Started : constant Interfaces.Unsigned_64 := Now_Ms;
      Finished : Interfaces.Unsigned_64;
      After : CCL.Sessions.History_Count;
      Live_Count : Natural := 0;
   begin
      Execute (State.Session, Source, CCL.Sessions.Default_Fuel, Outcome);
      Finished := Now_Ms;
      After := CCL.Sessions.Length (State.Session);
      if After = 0 then return; end if;
      if After = Before then
         State.Meta (1 .. CCL.Sessions.Maximum_History - 1) :=
           State.Meta (2 .. CCL.Sessions.Maximum_History);
      end if;
      for M of State.Meta loop
         if M.Live then Live_Count := Live_Count + 1; end if;
      end loop;
      State.Meta (After) :=
        (Elapsed_Ms => (if Finished >= Started then Finished - Started else 0),
         Fuel_Used => CCL.Sessions.Default_Fuel -
           Natural'Min (Outcome.Fuel_Remaining, CCL.Sessions.Default_Fuel),
         Live => Live_Count < MAXIMUM_LIVE_CELLS, Interval_Ms => 0,
         Due_Ms => Interfaces.Unsigned_64'Last, others => <>);
      State.Scroll := 0;
   end Follow;

   procedure Update_Session (State : in out View_State; Redraw : out Boolean) is
   begin
      Act (State.Session, Redraw);
   end Update_Session;

   function Latest_Source (State : View_State) return String is
     (if CCL.Sessions.Length (State.Session) = 0 then ""
      else Cell_Source (State, CCL.Sessions.Length (State.Session)));

   function Next_Deadline (State : View_State) return Interfaces.Unsigned_64 is
      Earliest : Interfaces.Unsigned_64 := 0;
   begin
      for I in 1 .. CCL.Sessions.Length (State.Session) loop
         if State.Meta (I).Live and then (Earliest = 0 or else State.Meta (I).Due_Ms < Earliest) then
            Earliest := State.Meta (I).Due_Ms;
         end if;
      end loop;
      return Earliest;
   end Next_Deadline;

   function Holds_Stream (State : View_State; Handle : CCL.Streams.Handle) return Boolean is
     (CCL.Sessions.Holds_Stream (State.Session, Handle));

   procedure Note_Arrival (State : in out View_State) is
   begin
      for I in 1 .. CCL.Sessions.Length (State.Session) loop
         if State.Meta (I).Live then
            --  Due at once (0 would mean "not scheduled").
            State.Meta (I).Due_Ms := 1;
         end if;
      end loop;
   end Note_Arrival;

   function Live_Runs (State : View_State; Index : Positive) return Natural is
     (if Index <= CCL.Sessions.Maximum_History and then State.Meta (Index).Live
      then State.Meta (Index).Runs else 0);

   function Latest_Result (State : View_State) return String is
      Latest : CCL.Sessions.Submission;
      Found : Boolean := False;
   begin
      if CCL.Sessions.Length (State.Session) > 0 then
         CCL.Sessions.Recall (State.Session, CCL.Sessions.Length (State.Session), Latest, Found);
      end if;
      return (if Found then CCL.Sessions.Result_Image (Latest.Outcome) else "");
   end Latest_Result;

   function Input_Text (State : View_State) return String is (Edit.Content (State.Input));

   function Pointer_Style (State : View_State) return CuBit.UI.Pointer_Cursor_Style is
     (if State.Hovered in 1 .. State.Hit_Total and then State.Hits (State.Hovered).Kind = Input_Hit
      then Pointer_Text else Pointer_Default);
   function Region
     (State : View_State; Kind : Region_Kind; Index : Positive) return CuBit.UI.Rect
   is
      Wanted : constant Hit_Kind :=
        (case Kind is
            when Entry_Source => Source_Hit, when Entry_Result => Result_Hit,
            when Entry_Name => Name_Hit, when Result_Type => Badge_Hit,
            when Example => Example_Hit, when Input => Input_Hit,
            when Suggestion => Completion_Hit, when Table_Cell => Cell_Hit,
            when Table_Header => Header_Hit, when Live_Mark => Live_Hit);
      Seen : Natural := 0;
   begin
      for I in 1 .. State.Hit_Total loop
         if State.Hits (I).Kind = Wanted then
            Seen := Seen + 1;
            if Seen = Index then return State.Hits (I).Area; end if;
         end if;
      end loop;
      return (others => 0);
   end Region;

   function Suggestions (State : View_State) return Natural is
     (if State.Popup_Open then State.Candidate_Total else 0);

   function Suggestion_Name (State : View_State; Index : Positive) return String is
     (if State.Popup_Open and then Index <= State.Candidate_Total
      then State.Candidates (Index).Suggestion.Name (1 .. State.Candidates (Index).Suggestion.Length)
      else "");
end CCL_Console_View;
