with Ada.Unchecked_Deallocation;
with Agent_Loop;
with Agent_Missions;
with Agent_Reports;

package body Agent_App_View is
   use CuBit.UI;
   use Interfaces;
   use type Agent_Loop.Phase;
   use type Agent_Loop.Stop;
   package Editor renames CuBit.UI.Editor;

   --  Layout, in logical pixels.
   HEADER_HEIGHT : constant := 40;
   INPUT_HEIGHT : constant := 44;
   PANEL_WIDTH : constant := 300;
   GUTTER : constant := 12;
   ROW_GAP : constant := 4;
   CARD_STRIPE : constant := 4;
   BUTTON_WIDTH : constant := 112;
   BUTTON_HEIGHT : constant := 26;
   METER_HEIGHT : constant := 12;
   SECTION_GAP : constant := 14;
   --  The transcript's first allocation; it doubles as text arrives.
   FIRST_TRANSCRIPT_BYTES : constant := 16_384;
   --  Rows one Page_Up / wheel notch scrolls.
   PAGE_ROWS : constant := 12;
   WHEEL_ROWS : constant := 3;
   NANO_PER_CENT : constant := 10_000_000;
   CENTS_PER_DOLLAR : constant := 100;
   MS_PER_SECOND : constant := 1_000;

   procedure Free is new Ada.Unchecked_Deallocation (String, Text_Access);

   function Image (Value : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   function Dollars (Nano : Unsigned_64) return String is
      Cents : constant Unsigned_64 := Nano / NANO_PER_CENT;
      Part : constant Unsigned_64 := Cents mod CENTS_PER_DOLLAR;
   begin
      return "$" & Image (Cents / CENTS_PER_DOLLAR) & "." & (if Part < 10 then "0" else "") & Image (Part);
   end Dollars;

   function Starts (Text, Prefix : String) return Boolean is
     (Text'Length >= Prefix'Length and then Text (Text'First .. Text'First + Prefix'Length - 1) = Prefix);

   function Style_Of (Line : String) return Line_Style is
     (if Starts (Line, "[tool ") then Tool_Card
      elsif Starts (Line, "  -> ") then Tool_Result
      elsif Starts (Line, "[stop ") then Stop_Line
      elsif Starts (Line, "[compacted: ") then Compaction_Line
      else Plain_Line);

   procedure Put (Into : out String; Length : out Natural; Text : String) is
      N : constant Natural := Natural'Min (Text'Length, Into'Length);
   begin
      Into := [others => ' '];
      Into (Into'First .. Into'First + N - 1) := Text (Text'First .. Text'First + N - 1);
      Length := N;
   end Put;

   procedure Initialize (State : out View_State) is
      Accepted : Boolean;
   begin
      Editor.Initialize (State.Prompt_Field, "", Accepted);
      Editor.Initialize (State.Key_Field, "", Accepted, Editor.Sensitive);
      State.Mode := Prompt_Mode;
      State.Transcript := null;
      State.Transcript_Length := 0;
      State.Scroll_Rows := 0;
      State.Key_Stored := False;
      State.Key_Note_Length := 0;
      State.Status_Length := 0;
      State.Mission := (others => <>);
      State.Tools_Length := 0;
      State.Elapsed_Ms := 0;
   end Initialize;

   function Mode (State : View_State) return Input_Mode is (State.Mode);

   procedure Set_Mode (State : in out View_State; Mode : Input_Mode) is
   begin
      if Mode = Prompt_Mode then
         Editor.Wipe (State.Key_Field);
      end if;
      State.Mode := Mode;
   end Set_Mode;

   function Prompt (State : View_State) return String is (Editor.Content (State.Prompt_Field));
   procedure Clear_Prompt (State : in out View_State) is
   begin
      Editor.Wipe (State.Prompt_Field);
   end Clear_Prompt;
   function Key_Text (State : View_State) return String is (Editor.Content (State.Key_Field));
   procedure Wipe_Key (State : in out View_State) is
   begin
      Editor.Wipe (State.Key_Field);
   end Wipe_Key;

   procedure Set_Key_Status (State : in out View_State; Stored : Boolean; Note : String) is
   begin
      State.Key_Stored := Stored;
      Put (State.Key_Note, State.Key_Note_Length, Note);
   end Set_Key_Status;

   procedure Set_Status (State : in out View_State; Text : String) is
   begin
      Put (State.Status, State.Status_Length, Text);
   end Set_Status;

   procedure Append_Transcript (State : in out View_State; Text : String) is
   begin
      if Text'Length = 0 then
         return;
      end if;
      if State.Transcript = null or else State.Transcript_Length + Text'Length > State.Transcript'Length then
         declare
            Capacity : Natural :=
              (if State.Transcript = null then FIRST_TRANSCRIPT_BYTES else State.Transcript'Length);
            Grown : Text_Access;
         begin
            while Capacity < State.Transcript_Length + Text'Length loop
               Capacity := 2 * Capacity;
            end loop;
            Grown := new String (1 .. Capacity);
            if State.Transcript /= null then
               Grown (1 .. State.Transcript_Length) := State.Transcript (1 .. State.Transcript_Length);
               Free (State.Transcript);
            end if;
            State.Transcript := Grown;
         end;
      end if;
      State.Transcript (State.Transcript_Length + 1 .. State.Transcript_Length + Text'Length) := Text;
      State.Transcript_Length := State.Transcript_Length + Text'Length;
   end Append_Transcript;

   function Transcript_Length (State : View_State) return Natural is (State.Transcript_Length);

   procedure Set_Mission
     (State : in out View_State; Item : CCL_Agent_Bindings.Mission_View; Tools : String;
      Now_Ms : Unsigned_64) is
   begin
      State.Mission := Item;
      Put (State.Tools, State.Tools_Length, Tools);
      State.Elapsed_Ms := (if Item.Found and then Now_Ms > Item.Started_Ms then Now_Ms - Item.Started_Ms else 0);
   end Set_Mission;

   ---------------------------------------------------------------------------
   --  Geometry.
   ---------------------------------------------------------------------------
   type Geometry is record
      Header, Transcript, Panel, Input, Field, Key_Button : Rect;
   end record;

   function Layout (Bounds : Rect) return Geometry is
      G : Geometry;
      Panel_W : constant Natural := (if Bounds.w > 2 * PANEL_WIDTH then PANEL_WIDTH else Bounds.w / 3);
      Body_H : constant Natural :=
        (if Bounds.h > HEADER_HEIGHT + INPUT_HEIGHT then Bounds.h - HEADER_HEIGHT - INPUT_HEIGHT else 0);
   begin
      G.Header := (Bounds.x, Bounds.y, Bounds.w, HEADER_HEIGHT);
      G.Panel := (Bounds.x + Bounds.w - Panel_W, Bounds.y + HEADER_HEIGHT, Panel_W, Body_H);
      G.Transcript := (Bounds.x, Bounds.y + HEADER_HEIGHT, Bounds.w - Panel_W, Body_H);
      G.Input := (Bounds.x, Bounds.y + HEADER_HEIGHT + Body_H, Bounds.w, INPUT_HEIGHT);
      G.Field := (G.Input.x + GUTTER + 96, G.Input.y + (INPUT_HEIGHT - BUTTON_HEIGHT) / 2,
                  (if G.Input.w > 2 * GUTTER + 96 then G.Input.w - 2 * GUTTER - 96 else 0), BUTTON_HEIGHT);
      G.Key_Button := ((if Bounds.w > BUTTON_WIDTH + GUTTER then Bounds.x + Bounds.w - BUTTON_WIDTH - GUTTER
                        else Bounds.x),
                       Bounds.y + (HEADER_HEIGHT - BUTTON_HEIGHT) / 2, BUTTON_WIDTH, BUTTON_HEIGHT);
      return G;
   end Layout;

   function Row_Height return Positive is (Code_Text_Height + ROW_GAP);
   function Columns (Width : Natural) return Positive is
     (Positive'Max (1, (if Width > 2 * GUTTER then (Width - 2 * GUTTER) / Positive'Max (1, Code_Text_Width ("M"))
                        else 1)));

   --  Every wrapped row of the transcript, in order: Visit (line, first,
   --  last, style) for each.
   generic
      with procedure Visit (Line_First, Row_First : Positive; Row_Last : Natural; Style : Line_Style);
   procedure For_Each_Row (State : View_State; Width : Natural);

   procedure For_Each_Row (State : View_State; Width : Natural) is
      Cols : constant Positive := Columns (Width);
      First : Positive := 1;
   begin
      if State.Transcript = null then
         return;
      end if;
      declare
         Text : String renames State.Transcript.all;
      begin
      while First <= State.Transcript_Length loop
         declare
            Last : Natural := First - 1;
         begin
            while Last < State.Transcript_Length and then Text (Last + 1) /= ASCII.LF loop
               Last := Last + 1;
            end loop;
            declare
               Style : constant Line_Style := Style_Of (Text (First .. Last));
               Row : Positive := First;
            begin
               if Last < First then
                  Visit (First, First, First - 1, Style);
               end if;
               while Row <= Last loop
                  Visit (First, Row, Natural'Min (Last, Row + Cols - 1), Style);
                  Row := Row + Cols;
               end loop;
            end;
            First := Last + 2;
         end;
      end loop;
      end;
   end For_Each_Row;

   function Row_Count (State : View_State; Width : Natural) return Natural is
      Total : Natural := 0;
      procedure Count (Line_First, Row_First : Positive; Row_Last : Natural; Style : Line_Style) is
         pragma Unreferenced (Line_First, Row_First, Row_Last, Style);
      begin
         Total := Total + 1;
      end Count;
      procedure Walk is new For_Each_Row (Count);
   begin
      Walk (State, Width);
      return Total;
   end Row_Count;

   ---------------------------------------------------------------------------
   --  Input.
   ---------------------------------------------------------------------------
   procedure Insert_Pasted (State : in out View_State; Text : String; Changed : out Boolean) is
      Clean : String (Text'Range);
      Last : Natural := Text'First - 1;
   begin
      --  One line: control characters (a pasted newline) are dropped.
      for C of Text loop
         if C in ' ' .. '~' then
            Last := Last + 1;
            Clean (Last) := C;
         end if;
      end loop;
      if State.Mode = Key_Mode then
         Editor.Insert (State.Key_Field, Clean (Clean'First .. Last), Changed);
      else
         Editor.Insert (State.Prompt_Field, Clean (Clean'First .. Last), Changed);
      end if;
      Clean := [others => ' '];
   end Insert_Pasted;

   procedure Handle (State : in out View_State; Event : View_Event; Bounds : Rect; Result : out Action) is
      G : constant Geometry := Layout (Bounds);
      Changed : Boolean := False;
      procedure Edit (Field : in out Editor.Edit_State) is
      begin
         case Event.Kind is
            when Text_Input =>
               if Event.Character_Value in ' ' .. '~' then
                  Editor.Insert (Field, [1 => Event.Character_Value], Changed);
               end if;
            when Backspace => Editor.Backspace (Field, Changed);
            when Delete => Editor.Delete_Forward (Field, Changed);
            when Left => Editor.Move (Field, Editor.Move_Left); Changed := True;
            when Right => Editor.Move (Field, Editor.Move_Right); Changed := True;
            when Home => Editor.Move (Field, Editor.Move_Start); Changed := True;
            when End_Key => Editor.Move (Field, Editor.Move_End); Changed := True;
            when Select_All => Editor.Select_All (Field); Changed := True;
            when others => null;
         end case;
      end Edit;
      Rows : constant Natural := Row_Count (State, G.Transcript.w);
      Visible : constant Natural := (if G.Transcript.h > 2 * GUTTER then (G.Transcript.h - 2 * GUTTER) / Row_Height
                                     else 0);
      Most_Scroll : constant Natural := (if Rows > Visible then Rows - Visible else 0);
   begin
      Result := Nothing;
      case Event.Kind is
         when No_Event =>
            null;
         when Set_Key =>
            State.Mode := Key_Mode;
            Result := Redraw;
         when Pointer_Down =>
            if Point_In_Rect (Event.X, Event.Y, G.Key_Button) then
               if State.Mode = Key_Mode then
                  Set_Mode (State, Prompt_Mode);
               else
                  State.Mode := Key_Mode;
               end if;
               Result := Redraw;
            end if;
         when Enter =>
            if State.Mode = Key_Mode then
               if Editor.Length (State.Key_Field) > 0 then
                  Result := Store_Key;
               end if;
            elsif Editor.Length (State.Prompt_Field) > 0 then
               Result := Ask;
            end if;
         when Escape =>
            if State.Mode = Key_Mode then
               Set_Mode (State, Prompt_Mode);
               Result := Redraw;
            elsif State.Mission.Found and then State.Mission.Running then
               Result := Cancel_Mission;
            end if;
         when Page_Up | Wheel_Up =>
            State.Scroll_Rows := Natural'Min
              (Most_Scroll, State.Scroll_Rows + (if Event.Kind = Page_Up then PAGE_ROWS else WHEEL_ROWS));
            Result := Redraw;
         when Page_Down | Wheel_Down =>
            State.Scroll_Rows := State.Scroll_Rows -
              Natural'Min (State.Scroll_Rows, (if Event.Kind = Page_Down then PAGE_ROWS else WHEEL_ROWS));
            Result := Redraw;
         when others =>
            if State.Mode = Key_Mode then
               Edit (State.Key_Field);
            else
               Edit (State.Prompt_Field);
            end if;
            if Changed then
               Result := Redraw;
            end if;
      end case;
   end Handle;

   ---------------------------------------------------------------------------
   --  Drawing.
   ---------------------------------------------------------------------------
   procedure Draw_Transcript (State : View_State; C : Canvas; Area : Rect; Colors : Theme) is
      Rows : constant Natural := Row_Count (State, Area.w);
      Height : constant Positive := Row_Height;
      Visible : constant Natural := (if Area.h > 2 * GUTTER then (Area.h - 2 * GUTTER) / Height else 0);
      Last_Row : constant Natural := (if Rows > State.Scroll_Rows then Rows - State.Scroll_Rows else 0);
      First_Row : constant Natural := (if Last_Row > Visible then Last_Row - Visible + 1 else 1);
      Index : Natural := 0;
      Clip : constant Canvas := With_Clip (C, Area);
      procedure Draw_Row (Line_First, Row_First : Positive; Row_Last : Natural; Style : Line_Style) is
         Y : Natural;
         Row_Box : Rect;
         Ink : Color := Colors.text;
         Text : String renames State.Transcript.all;
      begin
         Index := Index + 1;
         if Index < First_Row or else Index > Last_Row then
            return;
         end if;
         Y := Area.y + GUTTER + (Index - First_Row) * Height;
         Row_Box := (Area.x + GUTTER / 2, Y - ROW_GAP / 2, (if Area.w > GUTTER then Area.w - GUTTER else 0), Height);
         case Style is
            when Tool_Card | Tool_Result =>
               Fill_Rect (Clip, Row_Box, Colors.face);
               Fill_Rect (Clip, (Row_Box.x, Row_Box.y, CARD_STRIPE, Row_Box.h),
                          (if Style = Tool_Card then Colors.accent else Colors.edge));
               Ink := (if Style = Tool_Card then Colors.text else Colors.muted);
               if Style = Tool_Result and then Text (Line_First .. Row_Last)'Length > 7
                 and then Starts (Text (Line_First .. Row_Last), "  -> ok")
               then
                  Ink := Colors.good;
               elsif Style = Tool_Result then
                  Ink := Colors.danger;
               end if;
            when Stop_Line =>
               Ink := (if Starts (Text (Line_First .. Row_Last), "[stop completed]") then Colors.good
                       else Colors.danger);
            when Compaction_Line =>
               Fill_Rect (Clip, Row_Box, Colors.edge);
               Ink := Colors.muted;
            when Plain_Line =>
               null;
         end case;
         if Row_Last >= Row_First then
            Draw_Code_Text_Transparent (Clip, Area.x + GUTTER, Y, Text (Row_First .. Row_Last), Ink);
         end if;
      end Draw_Row;
      procedure Walk is new For_Each_Row (Draw_Row);
   begin
      Fill_Rect (C, Area, Colors.field);
      if State.Transcript_Length = 0 then
         Draw_UI_Text_Transparent
           (Clip, Area.x + GUTTER, Area.y + GUTTER,
            (if State.Key_Stored then "Ask about this machine: logs, processes, files in the mission's place."
             else "Set the Anthropic API key first (it goes to secrets.svc; the agent never sees it)."),
            Colors.muted);
         return;
      end if;
      Walk (State, Area.w);
   end Draw_Transcript;

   procedure Draw_Panel (State : View_State; C : Canvas; Area : Rect; Colors : Theme) is
      Clip : constant Canvas := With_Clip (C, Area);
      X : constant Natural := Area.x + GUTTER;
      W : constant Natural := (if Area.w > 2 * GUTTER then Area.w - 2 * GUTTER else 0);
      Y : Natural := Area.y + GUTTER;
      Line : constant Positive := UI_Text_Height + ROW_GAP;
      M : CCL_Agent_Bindings.Mission_View renames State.Mission;
      procedure Heading (Text : String) is
      begin
         Draw_Emphasis_Text_Transparent (Clip, X, Y, Text, Colors.text);
         Y := Y + Line;
      end Heading;
      procedure Item (Label, Value : String; Ink : Color := Colors.text) is
      begin
         Draw_UI_Text_Transparent (Clip, X, Y, Label, Colors.muted);
         Draw_UI_Text_Fitted (Clip, (X + 88, Y, (if W > 88 then W - 88 else 0), Line), X + 88, Y, Value, Ink,
                              Colors.face);
         Y := Y + Line;
      end Item;
      procedure Meter (Label : String; Used, Limit : Unsigned_64; Shown : String) is
         Scale : constant Unsigned_64 := Unsigned_64'Max (1, Limit / Unsigned_64 (Natural'Last) + 1);
      begin
         Item (Label, Shown, (if Limit > 0 and then Used >= Limit then Colors.danger else Colors.text));
         Draw_Progress_Bar (Clip, (X, Y, W, METER_HEIGHT), Colors, 0,
                            Natural (Limit / Scale), Natural (Unsigned_64'Min (Used, Limit) / Scale));
         Y := Y + METER_HEIGHT + ROW_GAP;
      end Meter;
      function Phase_Text return String is
        (if not M.Found then "idle: no mission yet"
         elsif not M.Running then "finished: " & Agent_Reports.Stop_Name (M.Stop)
         elsif M.Asking then "asking the model"
         elsif M.Phase = Agent_Loop.Running_Tools then "running tools"
         else "working");
      Spent : constant Agent_Loop.Spent := M.Spent;
      Budget : constant Agent_Loop.Budget := M.Budget;
   begin
      Fill_Rect (C, Area, Colors.face);
      Fill_Rect (C, (Area.x, Area.y, 1, Area.h), Colors.edge);
      Heading ("Authority");
      Item ("Mission", Agent_Missions.Profile_Name (M.Mission) & " (read-only)");
      Item ("Model", (if M.Found then CCL_Agent_Bindings.Image (M.Model) else Agent_Missions.Diagnose_Model));
      Item ("Tools", (if State.Tools_Length > 0 then State.Tools (1 .. State.Tools_Length)
                      else "logs, processes, files (read)"));
      Item ("Place", (if M.Found and then M.Place_Count > 0 then CCL_Agent_Bindings.Image (M.First_Place)
                      else "(none)"));
      Item ("Approvals", "none: Diagnose changes nothing", Colors.muted);
      Y := Y + SECTION_GAP;
      Heading ("Budget");
      if M.Found then
         Meter ("Tokens", Unsigned_64 (Agent_Loop.Tokens_Of (Spent)), Unsigned_64 (Budget.Tokens),
                Image (Unsigned_64 (Agent_Loop.Tokens_Of (Spent))) & " / " & Image (Unsigned_64 (Budget.Tokens)));
         Meter ("Cost", Unsigned_64 (Spent.Cost), Unsigned_64 (Budget.Cost),
                Dollars (Unsigned_64 (Spent.Cost)) & " / " & Dollars (Unsigned_64 (Budget.Cost)));
         Meter ("Time", State.Elapsed_Ms, Unsigned_64 (Budget.Wall_Ms),
                Image (State.Elapsed_Ms / MS_PER_SECOND) & " s / " &
                Image (Unsigned_64 (Budget.Wall_Ms) / MS_PER_SECOND) & " s");
         Meter ("Tool calls", Unsigned_64 (Spent.Tool_Calls), Unsigned_64 (Budget.Tool_Calls),
                Image (Unsigned_64 (Spent.Tool_Calls)) & " / " & Image (Unsigned_64 (Budget.Tool_Calls)));
      else
         Draw_UI_Text_Transparent (Clip, X, Y, "set per mission when you ask", Colors.muted);
         Y := Y + Line;
      end if;
      Y := Y + SECTION_GAP;
      Heading ("Status");
      Item ("State", Phase_Text,
            (if M.Found and then not M.Running and then M.Stop /= Agent_Loop.Completed then Colors.danger
             else Colors.text));
      if M.Found then
         Item ("Turns", Image (Unsigned_64 (Spent.Turns)) & ", refused calls " &
                 Image (Unsigned_64 (Spent.Refused_Calls)));
         Item ("Context", Image (Unsigned_64 (M.History_Bytes)) & " bytes, compacted " &
                 Image (Unsigned_64 (M.Compactions)) & "x");
         if M.Last_Tool.Length > 0 then
            Item ("Last tool", CCL_Agent_Bindings.Image (M.Last_Tool));
         end if;
         if M.Detail.Length > 0 then
            Draw_UI_Text_Fitted (Clip, (X, Y, W, Line), X, Y, CCL_Agent_Bindings.Image (M.Detail),
                                 Colors.danger, Colors.face);
            Y := Y + Line;
         end if;
         Item ("Record", (if M.Recorded then "session " & CCL_Agent_Bindings.Image (M.Session_Name)
                          else "not recorded"), Colors.muted);
      end if;
      if State.Status_Length > 0 then
         Draw_UI_Text_Fitted (Clip, (X, Y, W, Line), X, Y, State.Status (1 .. State.Status_Length),
                              Colors.muted, Colors.face);
         Y := Y + Line;
      end if;
      Y := Y + SECTION_GAP;
      Heading ("API key");
      Item ("Secret", (if State.Key_Stored then "stored in secrets.svc" else "not set"),
            (if State.Key_Stored then Colors.good else Colors.danger));
      if State.Key_Note_Length > 0 then
         Draw_UI_Text_Fitted (Clip, (X, Y, W, Line), X, Y, State.Key_Note (1 .. State.Key_Note_Length),
                              Colors.muted, Colors.face);
      end if;
   end Draw_Panel;

   procedure Draw (State : View_State; Canvas : CuBit.UI.Canvas; Bounds : Rect) is
      Colors : constant Theme := Current_Theme;
      G : constant Geometry := Layout (Bounds);
      Text_Y : constant Natural := G.Header.y + (HEADER_HEIGHT - UI_Text_Height) / 2;
      Masked : constant Boolean := State.Mode = Key_Mode;
      --  The key never leaves its field to be drawn: the masked field shows
      --  Secret_Mask for any non-empty text, so a stand-in is enough.
      Field_Text : constant String :=
        (if Masked then (if Editor.Length (State.Key_Field) > 0 then "*" else "")
         else Editor.Content (State.Prompt_Field));
      Caret : constant Natural := (if Masked then Field_Text'Length else Editor.Cursor (State.Prompt_Field) - 1);
      From : constant Natural := (if Masked then Caret else Editor.Selection_First (State.Prompt_Field) - 1);
      To : constant Natural := (if Masked then Caret else Editor.Selection_Last (State.Prompt_Field) - 1);
   begin
      Fill_Rect (Canvas, Bounds, Colors.panel);
      --  Header: what this is, who acts, and the key.
      Fill_Rect (Canvas, G.Header, Colors.titleBar);
      Fill_Rect (Canvas, (G.Header.x, G.Header.y + G.Header.h - 1, G.Header.w, 1), Colors.menuRule);
      Draw_Emphasis_Text_Transparent (Canvas, G.Header.x + GUTTER + 4, Text_Y, "Agent", Colors.text);
      Draw_UI_Text_Transparent
        (Canvas, G.Header.x + GUTTER + 4 + Emphasis_Text_Width ("Agent") + GUTTER, Text_Y,
         Agent_Missions.Profile_Name (State.Mission.Mission) & " mission, " &
         (if State.Mission.Found then CCL_Agent_Bindings.Image (State.Mission.Model)
          else Agent_Missions.Diagnose_Model) &
         (if State.Mission.Found and then State.Mission.Running then "  -  acting (Esc stops it)" else ""),
         (if State.Mission.Found and then State.Mission.Running then Colors.accent else Colors.muted));
      Draw_Button (Canvas, G.Key_Button, Colors,
                   (if State.Mode = Key_Mode then Button_Active else Button_Normal),
                   (if State.Key_Stored then "Replace key" else "Set API key"));
      Draw_Transcript (State, Canvas, G.Transcript, Colors);
      Draw_Panel (State, Canvas, G.Panel, Colors);
      --  The input line: the prompt, or the key's masked entry.
      Fill_Rect (Canvas, G.Input, Colors.face);
      Fill_Rect (Canvas, (G.Input.x, G.Input.y, G.Input.w, 1), Colors.edge);
      Draw_UI_Text_Transparent
        (Canvas, G.Input.x + GUTTER, G.Input.y + (INPUT_HEIGHT - UI_Text_Height) / 2,
         (if State.Mode = Key_Mode then "API key:" else "Ask:"),
         (if State.Mode = Key_Mode then Colors.accent else Colors.text));
      Draw_Text_Edit_Field
        (Canvas, G.Field, Colors, Field_Text, Caret, From, To, True, False,
         suggestion => (if Field_Text'Length > 0 then ""
                        elsif State.Mode = Key_Mode then "paste or type the key, Enter stores it, Esc cancels"
                        else "Enter asks; Esc stops a running mission"),
         masked => Masked);
   end Draw;
end Agent_App_View;
