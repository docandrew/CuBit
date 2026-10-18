with CuBit.Appearance;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Editor;
with CuBit.UI.Icons;
with CuBit.UI.Widgets;
with Text_Cursors;
with Text_Layout;
with Text_Modal;
with Text_Search;
with Text_Syntax;

package body Text_Shells is
   use Text_Commands;
   use Text_Documents;
   use type Text_Editors.Editor_Access;
   use type Text_Hosts.File_Status;
   use type CuBit.UI.Editor.Movement;
   use type Text_Syntax.Language;
   use type Text_Search.Options;

   --  Layout, in logical pixels.
   TOOLBAR_HEIGHT : constant := 34;
   TOOL_SIZE : constant := 26;
   TOOL_GAP : constant := 2;
   TOOL_GROUP_GAP : constant := 10;
   MARGIN : constant := 4;
   TAB_HEIGHT : constant := 24;
   MAXIMUM_TAB_WIDTH : constant := 220;
   FIND_HEIGHT : constant := 30;
   FIELD_HEIGHT : constant := 22;
   FIELD_WIDTH : constant := 260;
   OPTION_WIDTH : constant := 30;
   STATUS_HEIGHT : constant := 22;
   PALETTE_WIDTH : constant := 720;
   PALETTE_TOP : constant := 60;
   PALETTE_SHOWN : constant := 14;
   PALETTE_ROW_HEIGHT : constant := 22;
   SPLIT_GAP : constant := 3;
   PANE_HEADER_HEIGHT : constant := 20;
   --  Work per pump.
   CHUNK_BYTES : constant := 1_048_576;
   CHUNKS_PER_PUMP : constant := 8;
   FIND_BUDGET : constant := 8 * 1_048_576;
   COUNT_BUDGET : constant := 8 * 1_048_576;
   HIGHLIGHT_BUDGET : constant := 8_192;
   PASTE_CAPACITY : constant := 16 * 1_048_576;
   MESSAGE_LIFETIME_US : constant := 6_000_000;
   --  The occurrence highlight follows the cursor once it rests this long.
   OCCURRENCE_REST_US : constant := 250_000;
   DOUBLE_CLICK_MS : constant := 400;
   DOUBLE_CLICK_SLOP : constant := 4;
   MICROSECONDS_PER_MILLISECOND : constant := 1_000;
   MAXIMUM_MESSAGE : constant := 240;
   MAXIMUM_PALETTE_ROWS : constant := 4_096;
   MAXIMUM_SYMBOLS : constant := 4_096;
   SYMBOL_SCAN_LINES : constant := 200_000;
   MAXIMUM_SYMBOL_NAME : constant := 80;
   FNV_OFFSET : constant Unsigned_64 := 16#CBF2_9CE4_8422_2325#;
   FNV_PRIME : constant Unsigned_64 := 16#0000_0100_0000_01B3#;

   type Tool is
     (Tool_New, Tool_Open, Tool_Save, Tool_Undo, Tool_Redo, Tool_Find, Tool_Palette, Tool_Split, Tool_Wrap,
      Tool_Minimap);
   Tool_Command : constant array (Tool) of Command :=
     [Tool_New => New_Document, Tool_Open => Open, Tool_Save => Save, Tool_Undo => Undo, Tool_Redo => Redo,
      Tool_Find => Find, Tool_Palette => Command_Palette, Tool_Split => Split_View, Tool_Wrap => Toggle_Wrap,
      Tool_Minimap => Toggle_Minimap];
   Tool_Icon : constant array (Tool) of CuBit.UI.Icons.Icon :=
     [Tool_New => CuBit.UI.Icons.New_Tab, Tool_Open => CuBit.UI.Icons.Folder,
      Tool_Save => CuBit.UI.Icons.Document_File, Tool_Undo => CuBit.UI.Icons.Go_Back,
      Tool_Redo => CuBit.UI.Icons.Go_Forward, Tool_Find => CuBit.UI.Icons.Search,
      Tool_Palette => CuBit.UI.Icons.Properties, Tool_Split => CuBit.UI.Icons.Add_Pane,
      Tool_Wrap => CuBit.UI.Icons.Columns, Tool_Minimap => CuBit.UI.Icons.Drawer];

   type Tab is record
      Doc : Document_Access;
      Loading : Boolean := False;
      Loaded : Byte_Count := 0;
      Started_Us : Unsigned_64 := 0;
      Close_Armed : Boolean := False;
   end record;
   type Tab_Array is array (Tab_Index) of Tab;

   type Palette_Mode is (Commands_Mode, Line_Mode, Anything_Mode, Open_Mode, Argument_Mode);
   type Focus_Kind is (Editor_Focus, Find_Focus, Replace_Focus);
   type Row_Kind is (Command_Row, Name_Row, Symbol_Row, Line_Row, Call_Row);
   type Palette_Entry is record
      Kind : Row_Kind := Command_Row;
      Item : Command := No_Command;
      Index : Natural := 0;   --  a granted name's index, a symbol's index, a line
      Score : Integer := 0;
   end record;
   type Palette_Array is array (1 .. MAXIMUM_PALETTE_ROWS) of Palette_Entry;
   type Symbol is record
      Line : Line_Number := 1;
      Name : String (1 .. MAXIMUM_SYMBOL_NAME) := [others => ' '];
      Length : Natural := 0;
   end record;
   type Symbol_Array is array (1 .. MAXIMUM_SYMBOLS) of Symbol;
   type Editor_Pair is array (1 .. 2) of Text_Editors.Editor_Access;
   type Area_Pair is array (1 .. 2) of Rect;
   type Save_Phase is (Idle, Writing, Committing);

   type Shell_State is limited record
      Host : access Host_Type;
      Tab_List : Tab_Array;
      Count : Tab_Count := 0;
      Current : Tab_Count := 0;
      Editors : Editor_Pair := [others => null];
      Split_On : Boolean := False;
      Active : Positive range 1 .. 2 := 1;
      Dark : Boolean := False;
      Colors : Text_Editors.Palette;
      Edit_Bytes : Byte_Count := 0;
      Now : Unsigned_64 := 0;
      Bounds : Rect := (others => 0);
      Dirty : Rect := (others => 0);
      Everything : Boolean := True;
      Toolbar_Area, Tabs_Area, Find_Area, Status_Area, Palette_Area : Rect := (others => 0);
      Find_Box, Replace_Box : Rect := (others => 0);
      Editor_Areas : Area_Pair := [others => (others => 0)];
      Header_Areas : Area_Pair := [others => (others => 0)];
      Toolbar_Hash, Tabs_Hash, Status_Hash, Find_Hash, Palette_Hash : Unsigned_64 := 0;
      Drawn_Toolbar, Drawn_Tabs, Drawn_Status, Drawn_Find, Drawn_Palette : Unsigned_64 := 1;
      Prepared : Boolean := False;
      Message : String (1 .. MAXIMUM_MESSAGE) := [others => ' '];
      Message_Length : Natural := 0;
      Message_Until : Unsigned_64 := 0;
      Quit : Boolean := False;
      --  Find.
      Find_Visible, Replace_Visible : Boolean := False;
      Focus : Focus_Kind := Editor_Focus;
      Find_Field, Replace_Field : CuBit.UI.Editor.Edit_State;
      Options : Text_Search.Options;
      Finding : Boolean := False;
      Find_Forward : Boolean := True;
      Match_Total : Natural := 0;
      Count_Done : Boolean := True;
      Search_Bad : Boolean := False;
      --  Palette.
      Palette_On : Boolean := False;
      Mode : Palette_Mode := Commands_Mode;
      Palette_Field : CuBit.UI.Editor.Edit_State;
      Pending : Command := No_Command;
      Rows : Palette_Array;
      Row_Count : Natural := 0;
      Selected : Natural := 0;
      Symbols : Symbol_Array;
      Symbol_Count : Natural := 0;
      --  Modal keys.
      Modal : Boolean := False;
      Modal_State : Text_Modal.State;
      Describe : Boolean := False;
      --  Pointer.
      Last_Press_Ms : Unsigned_64 := 0;
      Last_Press_X, Last_Press_Y : Natural := 0;
      Clicks : Positive := 1;
      Pressed_Editor : Natural := 0;
      Hot_Tool : Natural := 0;
      --  Background work.
      Load_Tab : Tab_Count := 0;
      Save_Tab : Tab_Count := 0;
      Phase : Save_Phase := Idle;
      Frozen : Snapshot_Access;
      Save_Offset : Byte_Count := 0;
      Paste_Pending : Boolean := False;
      Occurrence_Due : Unsigned_64 := Unsigned_64'Last;
      Paste_Buffer : access String;
      Chunk : access String;
      --  Statistics.
      Last_Render_Us, Last_Pump_Us, Worst_Render_Us : Unsigned_64 := 0;
      Overlay : Boolean := False;
      Painted : Natural := 0;
   end record;

   function Mix (H : Unsigned_64; Text : String) return Unsigned_64 is
      Result : Unsigned_64 := H;
   begin
      for C of Text loop
         Result := (Result xor Unsigned_64 (Character'Pos (C))) * FNV_PRIME;
      end loop;
      return (Result xor Unsigned_64 (Text'Length)) * FNV_PRIME;
   end Mix;

   function Intersects (A, B : Rect) return Boolean is
     (not Is_Empty (A) and then not Is_Empty (B) and then A.x < B.x + B.w and then B.x < A.x + A.w
      and then A.y < B.y + B.h and then B.y < A.y + A.h);

   function Image (N : Long_Long_Integer) return String is
      Text : constant String := Long_Long_Integer'Image (N);
   begin
      return (if N < 0 then Text else Text (Text'First + 1 .. Text'Last));
   end Image;

   function Typed (Text : String) return Event is
      Result : Event := (Kind => Text_Event, others => <>);
   begin
      Result.Text (1 .. Text'Length) := Text;
      Result.Text_Length := Text'Length;
      return Result;
   end Typed;

   procedure Say (S : in out Shell_State; Text : String) is
      Length : constant Natural := Natural'Min (Text'Length, MAXIMUM_MESSAGE);
   begin
      S.Message (1 .. Length) := Text (Text'First .. Text'First + Length - 1);
      S.Message_Length := Length;
      S.Message_Until := S.Now + MESSAGE_LIFETIME_US;
   end Say;

   function Doc (S : Shell_State) return Document_Access is
     (if S.Current = 0 then null else S.Tab_List (S.Current).Doc);
   function Editor (S : Shell_State) return Text_Editors.Editor_Access is (S.Editors (S.Active));

   ---------------------------------------------------------------------------
   --  Setup and tabs.

   procedure Initialize
     (S : in out Shell; Host : not null access Host_Type; Dark : Boolean; Edit_Bytes : Byte_Count)
   is
   begin
      S.State := new Shell_State;
      S.State.Host := Host;
      S.State.Edit_Bytes := Edit_Bytes;
      S.State.Editors (1) := Text_Editors.Create;
      S.State.Editors (2) := Text_Editors.Create;
      S.State.Paste_Buffer := new String (1 .. PASTE_CAPACITY);
      S.State.Chunk := new String (1 .. CHUNK_BYTES);
      Set_Dark (S, Dark);
   end Initialize;

   procedure Set_Dark (S : in out Shell; Dark : Boolean) is
   begin
      S.State.Dark := Dark;
      S.State.Colors := Text_Editors.Palette_For (Current_Theme, Dark);
      S.State.Everything := True;
   end Set_Dark;

   procedure Show_Tab (S : in out Shell_State; T : Tab_Index) is
   begin
      S.Current := T;
      Text_Editors.Attach (S.Editors (1).all, S.Tab_List (T).Doc);
      Text_Editors.Attach (S.Editors (2).all, S.Tab_List (T).Doc);
      S.Active := 1;
      S.Everything := True;
   end Show_Tab;

   function Add_Tab (S : in out Shell_State; D : Document_Access) return Boolean is
   begin
      if S.Count >= MAXIMUM_TABS then
         Say (S, "Too many tabs: close one first");
         return False;
      end if;
      S.Count := S.Count + 1;
      S.Tab_List (S.Count) := (Doc => D, others => <>);
      Show_Tab (S, S.Count);
      return True;
   end Add_Tab;

   procedure New_State_Document (St : in out Shell_State; Name : String) is
      D : Document_Access := Create (Limits_For (0, St.Edit_Bytes), Name);
      Ok : Boolean;
   begin
      Begin_Load (D.all, 0);
      Load_Chunk (D.all, "", Ok);
      End_Load (D.all);
      if not Add_Tab (St, D) then
         Destroy (D);
      end if;
   end New_State_Document;

   procedure New_Document (S : in out Shell; Name : String := "untitled.txt") is
   begin
      New_State_Document (S.State.all, Name);
   end New_Document;

   procedure Open_State (St : in out Shell_State; Name : String);

   procedure Open (S : in out Shell; Name : String) is
   begin
      Open_State (S.State.all, Name);
   end Open;

   procedure Open_State (St : in out Shell_State; Name : String) is
      Size : Byte_Count;
      Status : Text_Hosts.File_Status;
   begin
      if St.Load_Tab /= 0 then
         Say (St, "Still opening " & Display_Name (St.Host.all, Text_Documents.Name (St.Tab_List (St.Load_Tab).Doc.all)));
         return;
      end if;
      --  Already open: show it.
      for T in 1 .. St.Count loop
         if Text_Documents.Name (St.Tab_List (T).Doc.all) = Name then
            Show_Tab (St, T);
            return;
         end if;
      end loop;
      Start_Load (St.Host.all, Name, Size, Status);
      case Status is
         when Text_Hosts.Done | Text_Hosts.Pending => null;
         when Text_Hosts.Not_Granted => Say (St, Display_Name (St.Host.all, Name) & ": not granted"); return;
         when Text_Hosts.Not_Found => Say (St, Display_Name (St.Host.all, Name) & ": not found"); return;
         when Text_Hosts.Too_Large => Say (St, Display_Name (St.Host.all, Name) & ": too large"); return;
         when others => Say (St, Display_Name (St.Host.all, Name) & ": cannot open"); return;
      end case;
      declare
         D : Document_Access := Create (Limits_For (Size, St.Edit_Bytes), Name);
      begin
         Begin_Load (D.all, Size);
         if Add_Tab (St, D) then
            St.Load_Tab := St.Count;
            St.Tab_List (St.Count).Loading := True;
            St.Tab_List (St.Count).Started_Us := St.Now;
         else
            Destroy (D);
         end if;
      end;
   end Open_State;

   procedure Close_Tab (S : in out Shell_State; T : Tab_Index) is
   begin
      if S.Load_Tab = T or else S.Save_Tab = T then
         Say (S, "Busy: wait for the load or save to finish");
         return;
      end if;
      Destroy (S.Tab_List (T).Doc);
      if T < S.Count then
         S.Tab_List (T .. S.Count - 1) := S.Tab_List (T + 1 .. S.Count);
      end if;
      if S.Load_Tab > T then
         S.Load_Tab := S.Load_Tab - 1;
      end if;
      if S.Save_Tab > T then
         S.Save_Tab := S.Save_Tab - 1;
      end if;
      S.Count := S.Count - 1;
      if S.Count = 0 then
         S.Current := 0;
         Text_Editors.Attach (S.Editors (1).all, null);
         Text_Editors.Attach (S.Editors (2).all, null);
         S.Split_On := False;
      else
         Show_Tab (S, Natural'Min (T, S.Count));
      end if;
      S.Everything := True;
   end Close_Tab;

   procedure Set_Clock (S : in out Shell; Now_Us : Unsigned_64) is
   begin
      S.State.Now := Now_Us;
   end Set_Clock;

   ---------------------------------------------------------------------------
   --  Search state.

   function Field_Text (F : CuBit.UI.Editor.Edit_State) return String is (CuBit.UI.Editor.Content (F));

   procedure Apply_Search (S : in out Shell_State) is
      Status : Search_Status;
      D : constant Document_Access := Doc (S);
   begin
      if D = null then
         return;
      end if;
      Set_Search (D.all, Field_Text (S.Find_Field), S.Options, Status);
      S.Search_Bad := Status = Bad_Pattern;
      S.Match_Total := 0;
      S.Count_Done := Status /= Ready;
      S.Finding := False;
      Text_Editors.Invalidate (S.Editors (1).all);
      Text_Editors.Invalidate (S.Editors (2).all);
   end Apply_Search;

   procedure Run_Find (S : in out Shell_State; Forward : Boolean) is
      Status : Search_Status;
      D : constant Document_Access := Doc (S);
   begin
      if D = null then
         return;
      end if;
      if Search_Pattern (D.all) /= Field_Text (S.Find_Field) or else Search_Settings (D.all) /= S.Options then
         Apply_Search (S);
      end if;
      Find (D.all, Forward, FIND_BUDGET, Status);
      case Status is
         when Found =>
            S.Finding := False;
            Text_Editors.Reveal_Primary (Editor (S).all);
         when Searching =>
            S.Finding := True;
            S.Find_Forward := Forward;
         when Not_Found =>
            S.Finding := False;
            Say (S, "Not found: " & Field_Text (S.Find_Field));
         when Bad_Pattern =>
            Say (S, "Not a valid pattern");
         when others =>
            S.Finding := False;
      end case;
   end Run_Find;

   ---------------------------------------------------------------------------
   --  Palette.

   function Lower (C : Character) return Character is
     (if C in 'A' .. 'Z' then Character'Val (Character'Pos (C) + 32) else C);

   --  A fuzzy score: every query character in order (case-folded), more
   --  for runs and word starts; Integer'First when it does not match.
   function Score (Query, Text : String) return Integer is
      Q : Natural := Query'First;
      Result : Integer := 0;
      Previous_Match : Natural := 0;
   begin
      if Query'Length = 0 then
         return 0;
      end if;
      for I in Text'Range loop
         exit when Q > Query'Last;
         if Lower (Text (I)) = Lower (Query (Q)) then
            Result := Result + 1;
            if Previous_Match = I - 1 and then I > Text'First then
               Result := Result + 4;
            end if;
            if I = Text'First or else Text (I - 1) in ' ' | '-' | '.' | '_' | '/' then
               Result := Result + 6;
            end if;
            Previous_Match := I;
            Q := Q + 1;
         end if;
      end loop;
      return (if Q > Query'Last then Result - Text'Length / 16 else Integer'First);
   end Score;

   type Word_List is array (Positive range <>) of access constant String;
   Ada_Words : constant Word_List :=
     [new String'("procedure "), new String'("function "), new String'("package body "), new String'("package "),
      new String'("type "), new String'("subtype "), new String'("task "), new String'("protected ")];
   Rust_Words : constant Word_List :=
     [new String'("pub fn "), new String'("fn "), new String'("struct "), new String'("pub struct "),
      new String'("enum "), new String'("trait "), new String'("impl ")];

   --  Exact, prefix and substring matches of the query in a name.
   function Name_Bonus (Query, Text : String) return Integer is
      function Same (A, B : String) return Boolean is
        (A'Length = B'Length and then (for all K in 0 .. A'Length - 1 => Lower (A (A'First + K)) = Lower (B (B'First + K))));
   begin
      if Same (Query, Text) then
         return 1_000;
      elsif Query'Length <= Text'Length and then Same (Query, Text (Text'First .. Text'First + Query'Length - 1)) then
         return 500;
      end if;
      for I in Text'First .. Text'Last - Query'Length + 1 loop
         if Same (Query, Text (I .. I + Query'Length - 1)) then
            return 200;
         end if;
      end loop;
      return 0;
   end Name_Bonus;

   --  Which characters of Text the query matched (as Score walks it).
   type Match_Marks is array (Positive range <>) of Boolean;
   function Matched (Query, Text : String) return Match_Marks is
      Result : Match_Marks (Text'Range) := [others => False];
      Q : Natural := Query'First;
   begin
      for I in Text'Range loop
         exit when Q > Query'Last;
         if Lower (Text (I)) = Lower (Query (Q)) then
            Result (I) := True;
            Q := Q + 1;
         end if;
      end loop;
      return (if Q > Query'Last then Result else [Text'Range => False]);
   end Matched;

   function Symbol_Text (S : Shell_State; K : Positive) return String is
     (S.Symbols (K).Name (1 .. S.Symbols (K).Length));

   procedure Collect_Symbols (S : in out Shell_State) is
      D : constant Document_Access := Doc (S);
      function Starts (Text, Word : String) return Boolean is
        (Text'Length > Word'Length and then Text (Text'First .. Text'First + Word'Length - 1) = Word);
      function Identifier_At (Text : String; From : Positive) return String is
         Stop : Natural := From - 1;
      begin
         while Stop < Text'Last and then (Text (Stop + 1) in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.'
                                          or else Character'Pos (Text (Stop + 1)) >= 16#80#)
         loop
            Stop := Stop + 1;
         end loop;
         return Text (From .. Stop);
      end Identifier_At;
      procedure Add (L : Line_Number; Name : String) is
      begin
         if Name'Length > 0 and then S.Symbol_Count < MAXIMUM_SYMBOLS then
            S.Symbol_Count := S.Symbol_Count + 1;
            S.Symbols (S.Symbol_Count).Line := L;
            S.Symbols (S.Symbol_Count).Length := Natural'Min (Name'Length, MAXIMUM_SYMBOL_NAME);
            S.Symbols (S.Symbol_Count).Name (1 .. S.Symbols (S.Symbol_Count).Length) :=
              Name (Name'First .. Name'First + S.Symbols (S.Symbol_Count).Length - 1);
         end if;
      end Add;
   begin
      S.Symbol_Count := 0;
      if D = null then
         return;
      end if;
      for L in 1 .. Line_Number'Min (Line_Total (D.all), SYMBOL_SCAN_LINES) loop
         declare
            Place : constant Segment_Place := Segment (D.all, L, 0);
            Raw : constant String := Slice (D.all, Place.First, Byte_Offset'Min (Place.Last, Place.First + 200));
            Indent : Natural := 0;
         begin
            while Indent < Raw'Length and then Raw (Raw'First + Indent) in ' ' | ASCII.HT loop
               Indent := Indent + 1;
            end loop;
            declare
               T : constant String := Raw (Raw'First + Indent .. Raw'Last);
            begin
               case Language (D.all) is
                  when Text_Syntax.Ada_Source =>
                     for Word of Ada_Words loop
                        if Starts (T, Word.all) then
                           Add (L, Word.all & Identifier_At (T, T'First + Word'Length));
                           exit;
                        end if;
                     end loop;
                  when Text_Syntax.Rust_Source =>
                     for Word of Rust_Words loop
                        if Starts (T, Word.all) then
                           Add (L, Word.all & Identifier_At (T, T'First + Word'Length));
                           exit;
                        end if;
                     end loop;
                  when Text_Syntax.C_Source =>
                     if Indent = 0 and then T'Length > 0 and then T (T'First) in 'a' .. 'z' | 'A' .. 'Z' | '_'
                       and then T (T'Last) /= ';'
                     then
                        for I in T'Range loop
                           if T (I) = '(' then
                              declare
                                 Start : Natural := I;
                              begin
                                 while Start > T'First and then T (Start - 1) in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' loop
                                    Start := Start - 1;
                                 end loop;
                                 if Start < I then
                                    Add (L, T (Start .. I - 1));
                                 end if;
                              end;
                              exit;
                           end if;
                        end loop;
                     end if;
                  when Text_Syntax.Markdown =>
                     if T'Length > 1 and then T (T'First) = '#' then
                        Add (L, T);
                     end if;
                  when Text_Syntax.CCL_Source =>
                     if Starts (T, "(define ") then
                        Add (L, Identifier_At (T, T'First + 8));
                     elsif Starts (T, "(setting """) then
                        declare
                           Stop : Natural := T'First + 10;
                        begin
                           while Stop < T'Last and then T (Stop + 1) /= '"' loop
                              Stop := Stop + 1;
                           end loop;
                           Add (L, T (T'First + 10 .. Stop));
                        end;
                     end if;
                  when Text_Syntax.Plain_Text => null;
               end case;
            end;
         end;
      end loop;
   end Collect_Symbols;

   procedure Filter_Palette (S : in out Shell_State) is
      Query : constant String := Field_Text (S.Palette_Field);
      procedure Add (Item : Palette_Entry) is
      begin
         if S.Row_Count < MAXIMUM_PALETTE_ROWS then
            S.Row_Count := S.Row_Count + 1;
            S.Rows (S.Row_Count) := Item;
         end if;
      end Add;
      Is_Call : constant Boolean :=
        Query'Length > 0 and then (Query (Query'First) = '(' or else
                                   (Query'Length > 5 and then Query (Query'First .. Query'First + 4) = "text."));
   begin
      S.Row_Count := 0;
      case S.Mode is
         when Commands_Mode =>
            if Is_Call then
               Add ((Kind => Call_Row, others => <>));
            end if;
            for C in Real_Command loop
               declare
                  --  The name, or the description when the query is mostly runs
                  --  and word starts there (not letters scattered across it).
                  By_Name : constant Integer :=
                    (if Query'Length = 0 then 0 else Score (Query, Name (C)) + Name_Bonus (Query, Name (C)));
                  By_Text : constant Integer := Score (Query, Description (C));
                  Value : constant Integer :=
                    (if By_Name /= Integer'First then 2 * By_Name
                     elsif By_Text /= Integer'First and then By_Text >= 3 * Query'Length then By_Text
                     else Integer'First);
               begin
                  if Value /= Integer'First then
                     Add ((Kind => Command_Row, Item => C, Index => 0, Score => Value));
                  end if;
               end;
            end loop;
         when Line_Mode | Argument_Mode =>
            null;
         when Anything_Mode =>
            if Query'Length > 0 and then Query (Query'First) = ':' then
               Add ((Kind => Line_Row, others => <>));
            elsif Query'Length > 0 and then Query (Query'First) = '@' then
               for K in 1 .. S.Symbol_Count loop
                  declare
                     Value : constant Integer := Score (Query (Query'First + 1 .. Query'Last), Symbol_Text (S, K));
                  begin
                     if Value /= Integer'First then
                        Add ((Kind => Symbol_Row, Item => No_Command, Index => K, Score => Value));
                     end if;
                  end;
               end loop;
            else
               for K in 1 .. Granted_Count (S.Host.all) loop
                  declare
                     Value : constant Integer := Score (Query, Granted_Name (S.Host.all, K));
                  begin
                     if Value /= Integer'First then
                        Add ((Kind => Name_Row, Item => No_Command, Index => K, Score => Value));
                     end if;
                  end;
               end loop;
            end if;
         when Open_Mode =>
            for K in 1 .. Granted_Count (S.Host.all) loop
               declare
                  Value : constant Integer := Score (Query, Granted_Name (S.Host.all, K));
               begin
                  if Value /= Integer'First then
                     Add ((Kind => Name_Row, Item => No_Command, Index => K, Score => Value));
                  end if;
               end;
            end loop;
      end case;
      --  Best first (stable insertion sort; a few hundred rows).
      if Query'Length > 0 then
         for I in 2 .. S.Row_Count loop
            declare
               Item : constant Palette_Entry := S.Rows (I);
               J : Natural := I - 1;
            begin
               while J >= 1 and then S.Rows (J).Score < Item.Score and then S.Rows (J).Kind /= Call_Row loop
                  S.Rows (J + 1) := S.Rows (J);
                  J := J - 1;
               end loop;
               S.Rows (J + 1) := Item;
            end;
         end loop;
      end if;
      S.Selected := (if S.Row_Count > 0 then 1 else 0);
   end Filter_Palette;

   procedure Open_Palette (S : in out Shell_State; Mode : Palette_Mode; Initial : String := "") is
      Accepted : Boolean;
   begin
      S.Palette_On := True;
      S.Mode := Mode;
      CuBit.UI.Editor.Initialize (S.Palette_Field, Initial, Accepted);
      if Mode = Anything_Mode then
         Collect_Symbols (S);
      end if;
      Filter_Palette (S);
   end Open_Palette;

   procedure Close_Palette (S : in out Shell_State) is
   begin
      S.Palette_On := False;
      S.Pending := No_Command;
      Text_Editors.Invalidate (S.Editors (1).all);
      Text_Editors.Invalidate (S.Editors (2).all);
   end Close_Palette;

   function Row_Text (S : Shell_State; Row : Positive) return String is
      E : constant Palette_Entry := S.Rows (Row);
   begin
      case E.Kind is
         when Command_Row =>
            return Description (E.Item) & "|" & Qualified_Name (E.Item) & "|" & Chord_Text (Binding (E.Item));
         when Name_Row => return Display_Name (S.Host.all, Granted_Name (S.Host.all, E.Index)) & "||";
         when Symbol_Row => return Symbol_Text (S, E.Index) & "|line" & Line_Number'Image (S.Symbols (E.Index).Line) & "|";
         when Line_Row => return "Go to line " & Field_Text (S.Palette_Field) & "||";
         when Call_Row => return "Run " & Field_Text (S.Palette_Field) & "|CCL call|";
      end case;
   end Row_Text;

   ---------------------------------------------------------------------------
   --  Copy and paste.

   function Whole_Lines (D : Text_Documents.Document) return String is
      --  With no selection, copy and cut take the cursors' lines.
      Result_Length : Natural := 0;
   begin
      for K in 1 .. Selection_Count (D) loop
         declare
            L : constant Line_Number := Line_Of (D, Selection (D, K).Head);
         begin
            Result_Length := Result_Length + Natural (Line_Start (D, L + 1) - Line_Start (D, L))
              + (if L = Line_Total (D) then 1 else 0);
         end;
      end loop;
      declare
         Result : String (1 .. Result_Length);
         At_Index : Natural := 0;
      begin
         for K in 1 .. Selection_Count (D) loop
            declare
               L : constant Line_Number := Line_Of (D, Selection (D, K).Head);
               Part : constant String := Slice (D, Line_Start (D, L), Line_Start (D, L + 1));
            begin
               Result (At_Index + 1 .. At_Index + Part'Length) := Part;
               At_Index := At_Index + Part'Length;
               if L = Line_Total (D) then
                  At_Index := At_Index + 1;
                  Result (At_Index) := ASCII.LF;
               end if;
            end;
         end loop;
         return Result (1 .. At_Index);
      end;
   end Whole_Lines;

   function All_Empty (D : Text_Documents.Document) return Boolean is
     (for all K in 1 .. Selection_Count (D) => Text_Cursors.Is_Empty (Selection (D, K)));

   procedure Poll_Paste (S : in out Shell_State) is
      Length : Natural;
      Ready, Truncated : Boolean;
      D : constant Document_Access := Doc (S);
   begin
      if not S.Paste_Pending then
         return;
      end if;
      Poll_Paste (S.Host.all, S.Paste_Buffer.all, Length, Ready, Truncated);
      if Ready then
         S.Paste_Pending := False;
         if Truncated then
            Say (S, "Paste too large: only the first" & Natural'Image (PASTE_CAPACITY / 1_048_576) & " MiB");
         end if;
         if D /= null and then Length > 0 then
            if S.Focus = Find_Focus or else S.Focus = Replace_Focus or else S.Palette_On then
               declare
                  Changed : Boolean;
                  Line : String := S.Paste_Buffer (1 .. Natural'Min (Length, CuBit.UI.Editor.MAX_TEXT_LENGTH));
               begin
                  for C of Line loop
                     if C in ASCII.LF | ASCII.CR then
                        C := ' ';
                     end if;
                  end loop;
                  if S.Palette_On then
                     CuBit.UI.Editor.Insert (S.Palette_Field, Line, Changed);
                     Filter_Palette (S);
                  elsif S.Focus = Find_Focus then
                     CuBit.UI.Editor.Insert (S.Find_Field, Line, Changed);
                     Apply_Search (S);
                  else
                     CuBit.UI.Editor.Insert (S.Replace_Field, Line, Changed);
                  end if;
               end;
            else
               Paste (D.all, S.Paste_Buffer (1 .. Length), S.Now);
               Text_Editors.Reveal_Primary (Editor (S).all);
            end if;
         end if;
      end if;
   end Poll_Paste;

   ---------------------------------------------------------------------------
   --  Saving.

   procedure Start_Save (S : in out Shell_State) is
      D : constant Document_Access := Doc (S);
      Status : Text_Hosts.File_Status;
   begin
      if D = null then
         return;
      elsif S.Phase /= Idle then
         Say (S, "Still saving");
         return;
      elsif Loading (D.all) then
         Say (S, "Still opening: save when it is in");
         return;
      end if;
      Start_Save (S.Host.all, Name (D.all), Status);
      case Status is
         when Text_Hosts.Done | Text_Hosts.Pending =>
            S.Frozen := Take_Snapshot (D.all);
            S.Save_Offset := 0;
            S.Save_Tab := S.Current;
            S.Phase := Writing;
         when Text_Hosts.Not_Granted => Say (S, Display_Name (S.Host.all, Name (D.all)) & ": not granted for writing");
         when Text_Hosts.Read_Only_Place => Say (S, Display_Name (S.Host.all, Name (D.all)) & ": read-only place");
         when others => Say (S, Display_Name (S.Host.all, Name (D.all)) & ": cannot save");
      end case;
   end Start_Save;

   procedure Pump_Save (S : in out Shell_State) is
      Status : Text_Hosts.File_Status := Text_Hosts.Done;
   begin
      case S.Phase is
         when Idle => null;
         when Writing =>
            declare
               D : constant Document_Access := S.Tab_List (S.Save_Tab).Doc;
               Total : constant Byte_Count := Snapshot_Length (S.Frozen.all);
            begin
               for Step in 1 .. CHUNKS_PER_PUMP loop
                  exit when S.Save_Offset >= Total;
                  declare
                     Take : constant Natural :=
                       Natural (Byte_Count'Min (Byte_Count'Min (CHUNK_BYTES, Byte_Count (Chunk_Limit (S.Host.all))),
                                                Total - S.Save_Offset));
                  begin
                     Read_Snapshot (D.all, S.Frozen.all, S.Save_Offset, S.Chunk (1 .. Take));
                     Save_Chunk (S.Host.all, S.Chunk (1 .. Take), Status);
                     --  Busy: the host took nothing; offer it again next pump.
                     exit when Status /= Text_Hosts.Done and then Status /= Text_Hosts.Pending;
                     S.Save_Offset := S.Save_Offset + Byte_Count (Take);
                  end;
               end loop;
               if Status = Text_Hosts.Busy then
                  Status := Text_Hosts.Pending;
               end if;
               if Status in Text_Hosts.Done | Text_Hosts.Pending and then S.Save_Offset >= Total then
                  Finish_Save (S.Host.all, Status);
                  S.Phase := Committing;
               end if;
               if Status not in Text_Hosts.Done | Text_Hosts.Pending then
                  Say (S, Display_Name (S.Host.all, Name (D.all)) & ": save failed; the file is unchanged");
                  Free_Snapshot (S.Frozen);
                  S.Phase := Idle;
                  S.Save_Tab := 0;
               end if;
            end;
         when Committing =>
            Poll_Save (S.Host.all, Status);
            if Status = Text_Hosts.Done then
               declare
                  D : constant Document_Access := S.Tab_List (S.Save_Tab).Doc;
               begin
                  Mark_Snapshot_Saved (D.all, S.Frozen.all);
                  Say (S, "Saved " & Display_Name (S.Host.all, Name (D.all)) & " (" & Image (Long_Long_Integer (Snapshot_Length (S.Frozen.all)))
                       & " bytes)");
               end;
               Free_Snapshot (S.Frozen);
               S.Phase := Idle;
               S.Save_Tab := 0;
            elsif Status /= Text_Hosts.Pending then
               Say (S, "Save failed; the file is unchanged");
               Free_Snapshot (S.Frozen);
               S.Phase := Idle;
               S.Save_Tab := 0;
            end if;
      end case;
   end Pump_Save;

   ---------------------------------------------------------------------------
   --  Commands.

   function Motion_Of (Item : Command) return Motion is
     (case Item is
         when Move_Left | Select_Left => Left, when Move_Right | Select_Right => Right,
         when Move_Up | Select_Up => Up, when Move_Down | Select_Down => Down,
         when Move_Word_Left | Select_Word_Left => Word_Left, when Move_Word_Right | Select_Word_Right => Word_Right,
         when Move_Line_Home | Select_Line_Home => Line_Home, when Move_Line_End | Select_Line_End => Line_End_Motion,
         when Move_Page_Up | Select_Page_Up => Page_Up, when Move_Page_Down | Select_Page_Down => Page_Down,
         when Move_Document_Start | Select_Document_Start => Document_Start,
         when others => Document_End);

   procedure Do_Execute
     (S : in out Shell_State; Item : Command; Number : Integer; Text : String; Ok : out Boolean)
   is
      D : constant Document_Access := Doc (S);
      E : constant Text_Editors.Editor_Access := Editor (S);
      Settings : constant Layout_Settings :=
        (if D = null then (others => <>) else Text_Editors.Settings (E.all));
      Reveal : Boolean := True;
      Needs_Document : constant Boolean :=
        Item not in New_Document | Open | Command_Palette | Toggle_Modal | Toggle_Overlay | Toggle_Theme
                  | Describe_Key | Quit | No_Command | Go_To_Anything;
   begin
      Ok := True;
      if Needs_Document and then D = null then
         Ok := False;
         Say (S, "No document: Ctrl+N for a new one, Ctrl+O to open");
         return;
      end if;
      case Item is
         when No_Command =>
            Ok := False;
            Reveal := False;
         when Move_Left .. Move_Document_End | Select_Left .. Select_Document_End =>
            Move (D.all, Motion_Of (Item), Extends (Item), Settings, Text_Editors.Page_Rows (E.all));
         when Scroll_Up =>
            Text_Editors.Scroll_Rows (E.all, -1);
            Reveal := False;
         when Scroll_Down =>
            Text_Editors.Scroll_Rows (E.all, 1);
            Reveal := False;
         when Select_All => Text_Documents.Select_All (D.all);
         when Select_Line => Text_Documents.Select_Line (D.all);
         when Select_Word_Or_Next => Text_Documents.Select_Word_Or_Next (D.all);
         when Split_Into_Lines => Text_Documents.Split_Into_Lines (D.all);
         when Add_Cursor_Above => Add_Cursor_Vertically (D.all, False, Settings);
         when Add_Cursor_Below => Add_Cursor_Vertically (D.all, True, Settings);
         when Collapse_Selection => Collapse (D.all);
         when Single_Selection =>
            if Selection_Count (D.all) > 1 then
               Keep_Primary (D.all);
            elsif not Text_Cursors.Is_Empty (Primary (D.all)) then
               Set_Caret (D.all, Primary (D.all).Head);
            elsif S.Find_Visible then
               S.Find_Visible := False;
               S.Replace_Visible := False;
               S.Everything := True;
            end if;
         when Text_Commands.New_Line => Text_Documents.New_Line (D.all, S.Now);
         when Text_Commands.Delete_Backward => Text_Documents.Delete_Backward (D.all, S.Now);
         when Text_Commands.Delete_Forward => Text_Documents.Delete_Forward (D.all, S.Now);
         when Delete_Word_Backward => Text_Documents.Delete_Backward (D.all, S.Now, Whole_Word => True);
         when Delete_Word_Forward => Text_Documents.Delete_Forward (D.all, S.Now, Whole_Word => True);
         when Delete_Line => Delete_Lines (D.all);
         when Duplicate_Line => Duplicate_Lines (D.all);
         when Text_Commands.Indent =>
            if All_Empty (D.all) and then Selection_Count (D.all) = 1 then
               --  A tab with no selection types spaces to the next stop.
               declare
                  Head : constant Byte_Offset := Primary (D.all).Head;
                  L : constant Line_Number := Line_Of (D.all, Head);
                  Before : constant String := Slice (D.all, Line_Start (D.all, L), Head);
                  Column : constant Natural :=
                    (if Before'Length <= SEGMENT_BYTES then Text_Layout.Column_Of (Before, Before'Last + 1, Settings.Tab)
                     else 0);
                  Pad : constant String (1 .. Settings.Tab - Column mod Settings.Tab) := [others => ' '];
               begin
                  Insert_Text (D.all, Pad, S.Now);
               end;
            else
               Text_Documents.Indent (D.all, Settings);
            end if;
         when Text_Commands.Outdent => Text_Documents.Outdent (D.all, Settings);
         when Text_Commands.Toggle_Comment => Text_Documents.Toggle_Comment (D.all);
         when Text_Commands.Undo =>
            Ok := Can_Undo (D.all);
            Text_Documents.Undo (D.all);
         when Text_Commands.Redo =>
            Ok := Can_Redo (D.all);
            Text_Documents.Redo (D.all);
         when Copy | Cut =>
            if All_Empty (D.all) then
               Copy (S.Host.all, Whole_Lines (D.all));
               if Item = Cut then
                  Delete_Lines (D.all);
               end if;
            else
               Copy (S.Host.all, Selected_Text (D.all));
               if Item = Cut then
                  Insert_Text (D.all, "", S.Now);
               end if;
            end if;
         when Text_Commands.Paste =>
            Request_Paste (S.Host.all);
            S.Paste_Pending := True;
            Poll_Paste (S);
         when Insert_Text =>
            Text_Documents.Insert_Text (D.all, Text, S.Now, Typed => True);
         when Text_Commands.Find =>
            S.Find_Visible := True;
            S.Focus := Find_Focus;
            if Text'Length > 0 then
               declare
                  Accepted : Boolean;
               begin
                  CuBit.UI.Editor.Initialize (S.Find_Field, Text, Accepted);
               end;
               Apply_Search (S);
               Run_Find (S, True);
            else
               declare
                  P : constant Text_Cursors.Selection := Primary (D.all);
                  Accepted : Boolean;
               begin
                  if not Text_Cursors.Is_Empty (P)
                    and then Text_Cursors.Last_Of (P) - Text_Cursors.First_Of (P) < 200
                    and then Line_Of (D.all, P.Anchor) = Line_Of (D.all, P.Head)
                  then
                     CuBit.UI.Editor.Initialize
                       (S.Find_Field, Slice (D.all, Text_Cursors.First_Of (P), Text_Cursors.Last_Of (P)), Accepted);
                     Apply_Search (S);
                  end if;
                  CuBit.UI.Editor.Select_All (S.Find_Field);
               end;
            end if;
            S.Everything := True;
            Reveal := Text'Length > 0;
         when Find_Next | Find_Previous =>
            if Field_Text (S.Find_Field)'Length = 0 then
               S.Find_Visible := True;
               S.Focus := Find_Focus;
               S.Everything := True;
            else
               Run_Find (S, Item = Find_Next);
            end if;
         when Replace =>
            S.Find_Visible := True;
            S.Replace_Visible := True;
            S.Everything := True;
            if Text'Length > 0 or else S.Focus = Replace_Focus then
               declare
                  Status : Search_Status;
               begin
                  if Search_Pattern (D.all) /= Field_Text (S.Find_Field) then
                     Apply_Search (S);
                  end if;
                  Replace_One (D.all, (if Text'Length > 0 then Text else Field_Text (S.Replace_Field)), Status);
               end;
            else
               S.Focus := (if Field_Text (S.Find_Field)'Length = 0 then Find_Focus else Replace_Focus);
               Reveal := False;
            end if;
         when Replace_All =>
            declare
               Count : Natural;
            begin
               if Search_Pattern (D.all) /= Field_Text (S.Find_Field) then
                  Apply_Search (S);
               end if;
               Text_Documents.Replace_All
                 (D.all, (if Text'Length > 0 then Text else Field_Text (S.Replace_Field)), Count);
               Say (S, "Replaced" & Natural'Image (Count) & " (one undo step)");
               Ok := Count > 0;
            end;
         when Select_All_Matches =>
            declare
               Count : Natural;
            begin
               if Search_Pattern (D.all) /= Field_Text (S.Find_Field) then
                  Apply_Search (S);
               end if;
               Text_Documents.Select_All_Matches (D.all, Count);
               Say (S, Natural'Image (Count) & " cursors");
            end;
         when Go_To_Line =>
            if Number > 0 then
               declare
                  L : constant Line_Number := Line_Number'Min (Line_Number (Number), Line_Total (D.all));
               begin
                  Set_Caret (D.all, Line_Start (D.all, L));
                  Text_Editors.Scroll_To_Line (E.all, L);
               end;
            else
               Open_Palette (S, Line_Mode);
               Reveal := False;
            end if;
         when Go_To_Anything =>
            Open_Palette (S, Anything_Mode, Text);
            Reveal := False;
         when Text_Commands.New_Document =>
            New_State_Document (S, (if Text'Length > 0 then Text else "untitled.txt"));
         when Text_Commands.Open =>
            if Text'Length > 0 then
               Open_State (S, Text);
            else
               Open_Palette (S, Open_Mode);
            end if;
            Reveal := False;
         when Save =>
            Start_Save (S);
            Reveal := False;
         when Save_As =>
            if Text'Length > 0 then
               Set_Name (D.all, Text);
               Set_Language (D.all, Text_Syntax.Language_Of (Text));
               Start_Save (S);
            else
               S.Pending := Save_As;
               Open_Palette (S, Argument_Mode, Name (D.all));
            end if;
            Reveal := False;
         when Close_Document =>
            if Modified (D.all) and then not S.Tab_List (S.Current).Close_Armed then
               S.Tab_List (S.Current).Close_Armed := True;
               Say (S, Display_Name (S.Host.all, Name (D.all)) & " has unsaved changes: Ctrl+W again closes it without saving");
            else
               Close_Tab (S, S.Current);
            end if;
            Reveal := False;
         when Next_Tab | Previous_Tab =>
            if S.Count > 1 then
               Show_Tab (S, (if Item = Next_Tab then S.Current mod S.Count + 1
                             else (S.Current + S.Count - 2) mod S.Count + 1));
            end if;
            Reveal := False;
         when Other_View =>
            if S.Split_On then
               S.Active := (if S.Active = 1 then 2 else 1);
               S.Focus := Editor_Focus;
               Text_Editors.Invalidate (S.Editors (1).all);
               Text_Editors.Invalidate (S.Editors (2).all);
            end if;
            Reveal := False;
         when Split_View =>
            S.Split_On := not S.Split_On;
            if S.Split_On then
               S.Active := 2;
            else
               S.Active := 1;
            end if;
            S.Everything := True;
         when Toggle_Wrap =>
            Text_Editors.Set_Wrap (E.all, not Text_Editors.Wrap_On (E.all));
            Say (S, (if Text_Editors.Wrap_On (E.all) then "Soft wrap on" else "Soft wrap off"));
         when Toggle_Minimap =>
            Text_Editors.Set_Minimap (S.Editors (1).all, not Text_Editors.Minimap_On (S.Editors (1).all));
            Text_Editors.Set_Minimap (S.Editors (2).all, Text_Editors.Minimap_On (S.Editors (1).all));
            Reveal := False;
         when Toggle_Modal =>
            S.Modal := not S.Modal;
            S.Modal_State := (others => <>);
            Say (S, (if S.Modal then "Modal keys: Esc for Normal, i to insert, : for the palette"
                     else "Modal keys off"));
            Reveal := False;
         when Toggle_Overlay =>
            S.Overlay := not S.Overlay;
            Reveal := False;
         when Toggle_Theme =>
            CuBit.UI.Set_Color_Scheme
              ((if S.Dark then CuBit.Appearance.Alloy_Light else CuBit.Appearance.Alloy_Dark));
            S.Dark := not S.Dark;
            S.Colors := Text_Editors.Palette_For (Current_Theme, S.Dark);
            S.Everything := True;
            Reveal := False;
         when Set_Tab_Width =>
            if Number in 1 .. MAXIMUM_TAB_WIDTH then
               Text_Editors.Set_Tab (S.Editors (1).all, Number);
               Text_Editors.Set_Tab (S.Editors (2).all, Number);
            else
               Ok := False;
               Say (S, "A tab width is 1 to" & Natural'Image (MAXIMUM_TAB_WIDTH));
            end if;
         when Command_Palette =>
            Open_Palette (S, Commands_Mode, Text);
            Reveal := False;
         when Describe_Key =>
            S.Describe := True;
            Say (S, "Press a key to see what it does");
            Reveal := False;
         when Quit =>
            S.Quit := True;
            Reveal := False;
      end case;
      --  The command may have closed D (Close_Document): only the document
      --  still shown is asked.
      if D /= null and then Doc (S) = D then
         case Last_Refusal (D.all) is
            when None => null;
            when Read_Only_Document => Say (S, "Still opening: read-only until it is in");
            when Protected_Region => Say (S, "That text is protected (read-only or generated)");
            when Document_Full => Say (S, "The document is at its capacity");
            when History_Dropped => Say (S, "That edit was too large to undo; earlier history dropped");
         end case;
      end if;
      if Reveal and then Doc (S) /= null and then Item not in Scroll_Up | Scroll_Down then
         Text_Editors.Reveal_Primary (Editor (S).all);
      end if;
   end Do_Execute;

   procedure Execute
     (S : in out Shell; Item : Text_Commands.Command; Number : Integer := 0; Text : String := ""; Ok : out Boolean)
   is
   begin
      Do_Execute (S.State.all, Item, Number, Text, Ok);
   end Execute;

   --  "(text.goto-line 120)", "text.find ""x""", "save".
   procedure Call_State (St : in out Shell_State; Call : String; Ok : out Boolean);

   procedure Execute_Call (S : in out Shell; Call : String; Ok : out Boolean) is
   begin
      Call_State (S.State.all, Call, Ok);
   end Execute_Call;

   procedure Call_State (St : in out Shell_State; Call : String; Ok : out Boolean) is
      First : Natural := Call'First;
      Last : Natural := Call'Last;
   begin
      Ok := False;
      while First <= Last and then Call (First) in ' ' | '(' loop
         First := First + 1;
      end loop;
      while Last >= First and then Call (Last) in ' ' | ')' loop
         Last := Last - 1;
      end loop;
      declare
         Space : Natural := First;
      begin
         while Space <= Last and then Call (Space) /= ' ' loop
            Space := Space + 1;
         end loop;
         declare
            Item : constant Command := Named (Call (First .. Space - 1));
            Argument : constant String := (if Space < Last then Call (Space + 1 .. Last) else "");
         begin
            if Item = No_Command then
               Say (St, "No command " & Call (First .. Space - 1));
               return;
            end if;
            case Argument_Of (Item) is
               when No_Argument =>
                  Do_Execute (St, Item, 0, "", Ok);
               when Integer_Argument =>
                  declare
                     Value : Integer := 0;
                  begin
                     for C of Argument loop
                        if C not in '0' .. '9' or else Value > Integer'Last / 10 - 9 then
                           Say (St, Qualified_Name (Item) & " takes an Integer");
                           return;
                        end if;
                        Value := Value * 10 + (Character'Pos (C) - Character'Pos ('0'));
                     end loop;
                     Do_Execute (St, Item, Value, "", Ok);
                  end;
               when String_Argument =>
                  if Argument'Length >= 2 and then Argument (Argument'First) = '"'
                    and then Argument (Argument'Last) = '"'
                  then
                     --  A CCL string: \" and \\ escapes.
                     declare
                        Result : String (1 .. Argument'Length);
                        Length : Natural := 0;
                        I : Natural := Argument'First + 1;
                     begin
                        while I < Argument'Last loop
                           if Argument (I) = '\' and then I + 1 < Argument'Last then
                              I := I + 1;
                              Length := Length + 1;
                              Result (Length) := (if Argument (I) = 'n' then ASCII.LF
                                                  elsif Argument (I) = 't' then ASCII.HT else Argument (I));
                           else
                              Length := Length + 1;
                              Result (Length) := Argument (I);
                           end if;
                           I := I + 1;
                        end loop;
                        Do_Execute (St, Item, 0, Result (1 .. Length), Ok);
                     end;
                  elsif Argument'Length = 0 then
                     Do_Execute (St, Item, 0, "", Ok);
                  else
                     Say (St, Qualified_Name (Item) & " takes a String (in quotes)");
                  end if;
            end case;
         end;
      end;
   end Call_State;

   ---------------------------------------------------------------------------
   --  Input.

   --  Editing keys for a one-line field; False when the key is not one.
   function Field_Key (F : in out CuBit.UI.Editor.Edit_State; Pressed : Chord) return Boolean is
      Changed : Boolean;
      use CuBit.UI.Editor;
   begin
      if Pressed.Alt then
         return False;
      end if;
      case Pressed.Item is
         when Text_Commands.Left =>
            Move (F, (if Pressed.Control then Move_Word_Left else Move_Left), Pressed.Shift);
         when Text_Commands.Right =>
            Move (F, (if Pressed.Control then Move_Word_Right else Move_Right), Pressed.Shift);
         when Home => Move (F, Move_Start, Pressed.Shift);
         when End_Key => Move (F, Move_End, Pressed.Shift);
         when Text_Commands.Backspace => Backspace (F, Changed);
         when Text_Commands.Delete => Delete_Forward (F, Changed);
         when Key_A =>
            if not Pressed.Control then
               return False;
            end if;
            Select_All (F);
         when others => return False;
      end case;
      return True;
   end Field_Key;

   procedure Run_Palette_Row (S : in out Shell_State) is
      Query : constant String := Field_Text (S.Palette_Field);
      Ok : Boolean;
   begin
      case S.Mode is
         when Line_Mode =>
            Close_Palette (S);
            Call_State (S, "goto-line " & Query, Ok);
            return;
         when Argument_Mode =>
            declare
               Item : constant Command := S.Pending;
            begin
               Close_Palette (S);
               case Argument_Of (Item) is
                  when Integer_Argument => Call_State (S, Name (Item) & " " & Query, Ok);
                  when others => Do_Execute (S, Item, 0, Query, Ok);
               end case;
            end;
            return;
         when others => null;
      end case;
      if S.Selected = 0 then
         return;
      end if;
      declare
         Row : constant Palette_Entry := S.Rows (S.Selected);
      begin
         Close_Palette (S);
         case Row.Kind is
            when Command_Row =>
               if Argument_Of (Row.Item) /= No_Argument
                 and then Row.Item not in Find | Replace | Go_To_Anything | Open | Go_To_Line | Save_As
               then
                  S.Pending := Row.Item;
                  Open_Palette (S, Argument_Mode);
               else
                  Do_Execute (S, Row.Item, 0, "", Ok);
               end if;
            when Name_Row =>
               Open_State (S, Granted_Name (S.Host.all, Row.Index));
            when Symbol_Row =>
               Do_Execute (S, Go_To_Line, Integer (S.Symbols (Row.Index).Line), "", Ok);
            when Line_Row =>
               Call_State (S, "goto-line " & Query (Query'First + 1 .. Query'Last), Ok);
            when Call_Row =>
               Call_State (S, Query, Ok);
         end case;
      end;
   end Run_Palette_Row;

   procedure Palette_Key (S : in out Shell_State; Pressed : Chord) is
   begin
      case Pressed.Item is
         when Escape => Close_Palette (S);
         when Enter => Run_Palette_Row (S);
         when Text_Commands.Up =>
            if S.Selected > 1 then
               S.Selected := S.Selected - 1;
            end if;
         when Text_Commands.Down =>
            if S.Selected < S.Row_Count then
               S.Selected := S.Selected + 1;
            end if;
         when Text_Commands.Page_Down =>
            S.Selected := Natural'Min (S.Row_Count, S.Selected + PALETTE_SHOWN);
         when Text_Commands.Page_Up =>
            S.Selected := Natural'Max (Natural'Min (1, S.Row_Count), S.Selected - Natural'Min (S.Selected, PALETTE_SHOWN));
         when others =>
            if Field_Key (S.Palette_Field, Pressed) then
               Filter_Palette (S);
            elsif Pressed.Control and then Pressed.Item = Key_V then
               Request_Paste (S.Host.all);
               S.Paste_Pending := True;
               Poll_Paste (S);
            end if;
      end case;
   end Palette_Key;

   procedure Run_Modal_Actions
     (S : in out Shell_State; Actions : Text_Modal.Action_Array; Count : Text_Modal.Action_Count)
   is
      Ok : Boolean;
   begin
      for K in 1 .. Count loop
         for Times in 1 .. Actions (K).Repeat loop
            Do_Execute (S, Actions (K).Item, Actions (K).Line, "", Ok);
            exit when Actions (K).Item = Go_To_Line;
         end loop;
      end loop;
   end Run_Modal_Actions;

   procedure Handle_Key (S : in out Shell_State; Pressed : Chord) is
      Item : Command;
      Ok : Boolean;
   begin
      if S.Describe then
         S.Describe := False;
         Item := Lookup (Pressed);
         Say (S, Chord_Text (Pressed) & ": "
                 & (if Item = No_Command then "nothing" else Qualified_Name (Item) & " - " & Description (Item)));
         return;
      end if;
      if S.Palette_On then
         Palette_Key (S, Pressed);
         return;
      end if;
      if S.Focus in Find_Focus | Replace_Focus and then S.Find_Visible then
         if Pressed.Item = Escape then
            S.Focus := Editor_Focus;
            S.Find_Visible := False;
            S.Replace_Visible := False;
            S.Everything := True;
            return;
         elsif Pressed.Item = Enter and then not Pressed.Control then
            if S.Focus = Replace_Focus then
               Do_Execute (S, Replace, 0, "", Ok);
            elsif Pressed.Alt then
               Do_Execute (S, Select_All_Matches, 0, "", Ok);
               S.Focus := Editor_Focus;
            else
               Run_Find (S, not Pressed.Shift);
            end if;
            return;
         elsif Pressed.Item = Text_Commands.Tab and then not Pressed.Control then
            S.Focus := (if S.Focus = Find_Focus and then S.Replace_Visible then Replace_Focus else Find_Focus);
            return;
         elsif Pressed.Alt and then Pressed.Item in Key_C | Key_R | Key_W then
            case Pressed.Item is
               when Key_C => S.Options.Case_Sensitive := not S.Options.Case_Sensitive;
               when Key_R => S.Options.Regular := not S.Options.Regular;
               when others => S.Options.Whole_Word := not S.Options.Whole_Word;
            end case;
            Apply_Search (S);
            return;
         elsif S.Focus = Find_Focus and then Field_Key (S.Find_Field, Pressed) then
            Apply_Search (S);
            return;
         elsif S.Focus = Replace_Focus and then Field_Key (S.Replace_Field, Pressed) then
            return;
         end if;
      end if;
      if S.Modal and then S.Focus = Editor_Focus and then Doc (S) /= null then
         declare
            Actions : Text_Modal.Action_Array;
            Count : Text_Modal.Action_Count;
            Handled : Boolean;
         begin
            Text_Modal.Feed_Key (S.Modal_State, Pressed, Actions, Count, Handled);
            if Handled then
               Run_Modal_Actions (S, Actions, Count);
               return;
            end if;
         end;
      end if;
      Item := Lookup (Pressed);
      if Item = Single_Selection and then S.Find_Visible and then Doc (S) /= null
        and then Selection_Count (Doc (S).all) = 1 and then Text_Cursors.Is_Empty (Primary (Doc (S).all))
      then
         S.Find_Visible := False;
         S.Replace_Visible := False;
         S.Everything := True;
         return;
      end if;
      if Item /= No_Command then
         if S.Focus /= Editor_Focus and then Item not in Find | Replace | Find_Next | Find_Previous then
            S.Focus := Editor_Focus;
         end if;
         Do_Execute (S, Item, 0, "", Ok);
      end if;
   end Handle_Key;

   procedure Handle_Text (S : in out Shell_State; Text : String) is
      Changed : Boolean;
   begin
      if S.Describe then
         S.Describe := False;
         Say (S, "'" & Text & "' types itself");
         return;
      end if;
      if S.Palette_On then
         CuBit.UI.Editor.Insert (S.Palette_Field, Text, Changed);
         Filter_Palette (S);
      elsif S.Find_Visible and then S.Focus = Find_Focus then
         CuBit.UI.Editor.Insert (S.Find_Field, Text, Changed);
         Apply_Search (S);
         --  Incremental: jump to the first match as you type.
         if Doc (S) /= null then
            declare
               D : constant Document_Access := Doc (S);
               P : constant Text_Cursors.Selection := Primary (D.all);
            begin
               Set_Caret (D.all, Text_Cursors.First_Of (P));
               Run_Find (S, True);
            end;
         end if;
      elsif S.Find_Visible and then S.Focus = Replace_Focus then
         CuBit.UI.Editor.Insert (S.Replace_Field, Text, Changed);
      elsif Doc (S) /= null then
         if S.Modal then
            for C of Text loop
               declare
                  Actions : Text_Modal.Action_Array;
                  Count : Text_Modal.Action_Count;
                  Insert_It : Boolean;
                  Ok : Boolean;
               begin
                  Text_Modal.Feed_Text (S.Modal_State, C, Actions, Count, Insert_It);
                  if Insert_It then
                     Do_Execute (S, Insert_Text, 0, [1 => C], Ok);
                  else
                     Run_Modal_Actions (S, Actions, Count);
                  end if;
               end;
            end loop;
         else
            declare
               Ok : Boolean;
            begin
               Do_Execute (S, Insert_Text, 0, Text, Ok);
            end;
         end if;
      end if;
   end Handle_Text;

   function Tool_Box (S : Shell_State; T : Tool) return Rect is
      X : Natural := S.Toolbar_Area.x + MARGIN;
   begin
      for Each in Tool'First .. T loop
         if Each = T then
            return (X, S.Toolbar_Area.y + (TOOLBAR_HEIGHT - TOOL_SIZE) / 2, TOOL_SIZE, TOOL_SIZE);
         end if;
         X := X + TOOL_SIZE + TOOL_GAP + (if Each in Tool_Save | Tool_Redo | Tool_Palette then TOOL_GROUP_GAP else 0);
      end loop;
      return (others => 0);
   end Tool_Box;

   function Tab_Box (S : Shell_State; T : Tab_Index) return Rect is
      Width : constant Natural :=
        (if S.Count = 0 then 0 else Natural'Min (MAXIMUM_TAB_WIDTH, S.Tabs_Area.w / Natural'Max (1, S.Count)));
   begin
      return (S.Tabs_Area.x + (T - 1) * Width, S.Tabs_Area.y, Width, S.Tabs_Area.h);
   end Tab_Box;

   function Option_Box (S : Shell_State; K : Positive) return Rect is
     (S.Find_Box.x + S.Find_Box.w + MARGIN + (K - 1) * (OPTION_WIDTH + 2), S.Find_Box.y, OPTION_WIDTH, FIELD_HEIGHT);

   procedure Handle_Pointer (S : in out Shell_State; Item : Event) is
      Ok : Boolean;
   begin
      case Item.Action is
         when Press =>
            if S.Palette_On then
               if Point_In_Rect (Item.X, Item.Y, S.Palette_Area) then
                  declare
                     List_Top : constant Natural := S.Palette_Area.y + FIELD_HEIGHT + 2 * MARGIN;
                  begin
                     if Item.Y >= List_Top then
                        declare
                           Row : constant Natural := (Item.Y - List_Top) / PALETTE_ROW_HEIGHT + 1;
                           First_Shown : constant Natural :=
                             (if S.Selected > PALETTE_SHOWN then S.Selected - PALETTE_SHOWN + 1 else 1);
                        begin
                           if First_Shown + Row - 1 <= S.Row_Count then
                              S.Selected := First_Shown + Row - 1;
                              Run_Palette_Row (S);
                           end if;
                        end;
                     end if;
                  end;
               else
                  Close_Palette (S);
               end if;
               return;
            end if;
            --  Multi-clicks.
            if Item.Time_Ms - S.Last_Press_Ms <= DOUBLE_CLICK_MS and then Item.Time_Ms >= S.Last_Press_Ms
              and then abs (Item.X - S.Last_Press_X) <= DOUBLE_CLICK_SLOP
              and then abs (Item.Y - S.Last_Press_Y) <= DOUBLE_CLICK_SLOP
            then
               S.Clicks := (if S.Clicks >= 3 then 1 else S.Clicks + 1);
            else
               S.Clicks := 1;
            end if;
            S.Last_Press_Ms := Item.Time_Ms;
            S.Last_Press_X := Item.X;
            S.Last_Press_Y := Item.Y;
            for T in Tool loop
               if Point_In_Rect (Item.X, Item.Y, Tool_Box (S, T)) then
                  S.Hot_Tool := Tool'Pos (T) + 1;
                  Do_Execute (S, Tool_Command (T), 0, "", Ok);
                  return;
               end if;
            end loop;
            for T in 1 .. S.Count loop
               if Point_In_Rect (Item.X, Item.Y, Tab_Box (S, T)) then
                  Show_Tab (S, T);
                  return;
               end if;
            end loop;
            if S.Find_Visible then
               if Point_In_Rect (Item.X, Item.Y, S.Find_Box) then
                  S.Focus := Find_Focus;
                  return;
               elsif S.Replace_Visible and then Point_In_Rect (Item.X, Item.Y, S.Replace_Box) then
                  S.Focus := Replace_Focus;
                  return;
               end if;
               for K in 1 .. 3 loop
                  if Point_In_Rect (Item.X, Item.Y, Option_Box (S, K)) then
                     case K is
                        when 1 => S.Options.Case_Sensitive := not S.Options.Case_Sensitive;
                        when 2 => S.Options.Whole_Word := not S.Options.Whole_Word;
                        when others => S.Options.Regular := not S.Options.Regular;
                     end case;
                     Apply_Search (S);
                     return;
                  end if;
               end loop;
            end if;
            for K in 1 .. (if S.Split_On then 2 else 1) loop
               if Doc (S) /= null and then Point_In_Rect (Item.X, Item.Y, S.Editor_Areas (K)) then
                  if S.Active /= K then
                     S.Active := K;
                     Text_Editors.Invalidate (S.Editors (1).all);
                     Text_Editors.Invalidate (S.Editors (2).all);
                  end if;
                  S.Focus := Editor_Focus;
                  S.Pressed_Editor := K;
                  Text_Editors.Pointer_Press (S.Editors (K).all, Item.X, Item.Y, S.Clicks, Item.Control, Item.Shift,
                                              Item.Alt);
                  return;
               end if;
            end loop;
         when Move =>
            declare
               Hot : Natural := 0;
            begin
               for T in Tool loop
                  if Point_In_Rect (Item.X, Item.Y, Tool_Box (S, T)) then
                     Hot := Tool'Pos (T) + 1;
                  end if;
               end loop;
               S.Hot_Tool := Hot;
            end;
            if S.Pressed_Editor > 0 then
               Text_Editors.Pointer_Drag (S.Editors (S.Pressed_Editor).all, Item.X, Item.Y);
            end if;
         when Release =>
            if S.Pressed_Editor > 0 then
               Text_Editors.Pointer_Release (S.Editors (S.Pressed_Editor).all);
               S.Pressed_Editor := 0;
            end if;
      end case;
   end Handle_Pointer;

   procedure Handle (S : in out Shell; Item : Event) is
      St : Shell_State renames S.State.all;
   begin
      St.Prepared := False;
      if Item.Kind in Key_Event | Text_Event | Pointer_Event then
         St.Occurrence_Due := St.Now + OCCURRENCE_REST_US;
      end if;
      --  Occurrences of the word under the cursor show only once it rests:
      --  a key or a press takes them away at once, so none are ever left
      --  around a word that is no longer there (or in another tab).
      if Item.Kind in Key_Event | Text_Event or else (Item.Kind = Pointer_Event and then Item.Action = Press) then
         Text_Editors.Clear_Occurrence (St.Editors (1).all);
         Text_Editors.Clear_Occurrence (St.Editors (2).all);
      end if;
      case Item.Kind is
         when Key_Event => Handle_Key (St, Item.Pressed);
         when Text_Event => Handle_Text (St, Item.Text (1 .. Item.Text_Length));
         when Pointer_Event => Handle_Pointer (St, Item);
         when Wheel_Event =>
            if St.Palette_On then
               null;
            else
               for K in 1 .. (if St.Split_On then 2 else 1) loop
                  if Doc (St) /= null and then Point_In_Rect (Item.X, Item.Y, St.Editor_Areas (K)) then
                     Text_Editors.Wheel (St.Editors (K).all, Item.Steps);
                  end if;
               end loop;
            end if;
         when Resize_Event =>
            St.Everything := True;
      end case;
   end Handle;

   ---------------------------------------------------------------------------
   --  Background work.

   procedure Pump_Load (S : in out Shell_State; Changed : in out Boolean) is
      T : constant Tab_Count := S.Load_Tab;
   begin
      if T = 0 then
         return;
      end if;
      declare
         D : constant Document_Access := S.Tab_List (T).Doc;
         Got : Natural;
         Status : Text_Hosts.File_Status := Text_Hosts.Pending;
         Ok : Boolean := True;
      begin
         for Step in 1 .. CHUNKS_PER_PUMP loop
            Poll_Load (S.Host.all, S.Chunk (1 .. Natural'Min (CHUNK_BYTES, Chunk_Limit (S.Host.all))), Got, Status);
            if Status = Text_Hosts.Busy then
               Status := Text_Hosts.Pending;
            end if;
            if Got > 0 then
               Load_Chunk (D.all, S.Chunk (1 .. Got), Ok);
               S.Tab_List (T).Loaded := S.Tab_List (T).Loaded + Byte_Count (Got);
               Changed := True;
            end if;
            exit when Status /= Text_Hosts.Pending or else not Ok or else Got = 0;
         end loop;
         if not Ok then
            Say (S, Display_Name (S.Host.all, Name (D.all)) & ": larger than its capacity; stopped");
            Status := Text_Hosts.Done;
         end if;
         if Status = Text_Hosts.Done then
            End_Load (D.all);
            S.Tab_List (T).Loading := False;
            S.Load_Tab := 0;
            Changed := True;
            S.Everything := True;
            Say (S, "Opened " & Display_Name (S.Host.all, Name (D.all)) & ":" & Line_Number'Image (Line_Total (D.all)) & " lines, "
                    & Image (Long_Long_Integer (Length (D.all))) & " bytes in"
                    & Unsigned_64'Image ((S.Now - S.Tab_List (T).Started_Us) / MICROSECONDS_PER_MILLISECOND) & " ms");
         elsif Status /= Text_Hosts.Pending then
            End_Load (D.all);
            S.Tab_List (T).Loading := False;
            S.Load_Tab := 0;
            Say (S, Display_Name (S.Host.all, Name (D.all)) & ": read failed");
         end if;
      end;
   end Pump_Load;

   procedure Pump (S : in out Shell; Budget : Work_Budget; Busy, Changed : out Boolean) is
      St : Shell_State renames S.State.all;
      D : constant Document_Access := Doc (St);
   begin
      Busy := False;
      Changed := False;
      Pump_Load (St, Changed);
      if St.Load_Tab /= 0 then
         Busy := True;
      end if;
      if St.Phase /= Idle then
         Pump_Save (St);
         Busy := True;
         Changed := True;
      end if;
      if St.Paste_Pending then
         Poll_Paste (St);
         Changed := True;
      end if;
      if D /= null then
         declare
            Highlight_Busy, Highlight_Changed : Boolean;
         begin
            Highlight_Pump (D.all, Natural'Min (Budget, HIGHLIGHT_BUDGET), Highlight_Busy, Highlight_Changed);
            if Highlight_Changed or else Text_Editors.Guessed_Highlight (Editor (St).all) then
               Changed := True;
            end if;
            Busy := Busy or else Highlight_Busy;
         end;
         if St.Finding then
            declare
               Status : Search_Status;
            begin
               Find (D.all, St.Find_Forward, FIND_BUDGET, Status);
               St.Finding := Status = Searching;
               if Status = Found then
                  Text_Editors.Reveal_Primary (Editor (St).all);
               elsif Status = Not_Found then
                  Say (St, "Not found: " & Field_Text (St.Find_Field));
               end if;
               Busy := Busy or else St.Finding;
               Changed := True;
            end;
         end if;
         if St.Find_Visible and then not St.Count_Done and then Search_Pattern (D.all)'Length > 0 then
            Count_Matches (D.all, COUNT_BUDGET, St.Match_Total, St.Count_Done);
            Busy := Busy or else not St.Count_Done;
            Changed := True;
         end if;
      end if;
      if St.Occurrence_Due /= Unsigned_64'Last and then St.Now >= St.Occurrence_Due then
         St.Occurrence_Due := Unsigned_64'Last;
         if D /= null then
            Text_Editors.Refresh_Occurrence (St.Editors (1).all);
            Text_Editors.Refresh_Occurrence (St.Editors (2).all);
            Changed := True;
         end if;
      end if;
      if St.Message_Length > 0 and then St.Now >= St.Message_Until then
         St.Message_Length := 0;
         Changed := True;
      end if;
      if Changed then
         St.Prepared := False;
      end if;
   end Pump;

   ---------------------------------------------------------------------------
   --  Drawing.

   function Tab_Label (S : Shell_State; T : Tab_Index) return String is
      D : constant Document_Access := S.Tab_List (T).Doc;
      Full : constant String := Name (D.all);
      Slash : Natural := 0;
   begin
      for I in Full'Range loop
         if Full (I) = '/' then
            Slash := I;
         end if;
      end loop;
      return Full (Natural'Max (Full'First, Slash + 1) .. Full'Last) & (if Modified (D.all) then " *" else "")
        & (if S.Tab_List (T).Loading and then Expected_Bytes (D.all) > 0 then
              " (" & Image (Long_Long_Integer (S.Tab_List (T).Loaded * 100 / Expected_Bytes (D.all))) & "%)"
           else "");
   end Tab_Label;

   function Status_Text (S : Shell_State) return String is
      D : constant Document_Access := Doc (S);
   begin
      if D = null then
         return (if S.Message_Length > 0 then S.Message (1 .. S.Message_Length) else "Ctrl+N new   Ctrl+O open   Ctrl+Shift+P commands")
           & "|";
      end if;
      declare
         P : constant Text_Cursors.Selection := Primary (D.all);
         L : constant Line_Number := Line_Of (D.all, P.Head);
         Start : constant Byte_Offset := Line_Start (D.all, L);
         Column : constant Natural :=
           (if P.Head - Start <= SEGMENT_BYTES
            then Text_Layout.Column_Of (Slice (D.all, Start, P.Head), Natural (P.Head - Start) + 1,
                                        Text_Editors.Settings (Editor (S).all).Tab) + 1
            else Natural (P.Head - Start) + 1);
         Selected : constant Byte_Count := Text_Cursors.Last_Of (P) - Text_Cursors.First_Of (P);
         Left : constant String :=
           "Ln" & Line_Number'Image (L) & ", Col" & Natural'Image (Column)
           & (if Selection_Count (D.all) > 1 then "   " & Image (Long_Long_Integer (Selection_Count (D.all))) & " cursors"
              elsif Selected > 0 then "   " & Image (Long_Long_Integer (Selected)) & " selected" else "")
           & (if S.Message_Length > 0 then "   " & S.Message (1 .. S.Message_Length) else "");
         Right : constant String :=
           (if S.Overlay then "frame" & Unsigned_64'Image (S.Last_Render_Us) & " us, worst"
                & Unsigned_64'Image (S.Worst_Render_Us) & " us, pump" & Unsigned_64'Image (S.Last_Pump_Us)
                & " us, rows" & Natural'Image (S.Painted) & ", pieces" & Natural'Image (Pieces_Used (D.all)) & "   "
            else "")
           & (if S.Modal then Text_Modal.Mode_Name (S.Modal_State) & "   " else "")
           & (if Text_Editors.Wrap_On (Editor (S).all) then "Wrap   " else "")
           & Text_Syntax.Name_Of (Language (D.all)) & "   UTF-8   " & (if Uses_CRLF (D.all) then "CRLF" else "LF")
           & "   " & (if S.Phase /= Idle then "Saving..." elsif Loading (D.all) then "Opening..."
                     elsif Modified (D.all) then "Modified" else "Saved");
      begin
         return Left & "|" & Right;
      end;
   end Status_Text;

   function Find_Text (S : Shell_State) return String is
     (Field_Text (S.Find_Field) & "|" & Field_Text (S.Replace_Field) & "|" & Boolean'Image (S.Replace_Visible)
      & Boolean'Image (S.Options.Case_Sensitive) & Boolean'Image (S.Options.Whole_Word)
      & Boolean'Image (S.Options.Regular) & Natural'Image (Focus_Kind'Pos (S.Focus)) & Natural'Image (S.Match_Total)
      & Boolean'Image (S.Count_Done) & Boolean'Image (S.Search_Bad)
      & Natural'Image (CuBit.UI.Editor.Cursor (S.Find_Field)) & Natural'Image (CuBit.UI.Editor.Cursor (S.Replace_Field)));

   function Palette_Text (S : Shell_State) return String is
      H : Unsigned_64 := FNV_OFFSET;
   begin
      if not S.Palette_On then
         return "off";
      end if;
      H := Mix (H, Field_Text (S.Palette_Field) & Natural'Image (S.Selected) & Natural'Image (S.Row_Count)
                   & Natural'Image (Palette_Mode'Pos (S.Mode)) & Natural'Image (CuBit.UI.Editor.Cursor (S.Palette_Field)));
      return Unsigned_64'Image (H);
   end Palette_Text;

   procedure Layout (S : in out Shell_State; Bounds : Rect) is
      Changed_Bounds : constant Boolean := Bounds /= S.Bounds;
      Find_H : constant Natural := (if S.Find_Visible then FIND_HEIGHT else 0);
      Top : constant Natural := Bounds.y + TOOLBAR_HEIGHT + TAB_HEIGHT;
      Bottom : constant Natural := Natural'Max (Top, Integer (Bounds.y + Bounds.h) - STATUS_HEIGHT - Find_H);
      Middle : constant Rect := (Bounds.x, Top, Bounds.w, Bottom - Top);
   begin
      if Changed_Bounds then
         S.Everything := True;
      end if;
      S.Bounds := Bounds;
      S.Toolbar_Area := (Bounds.x, Bounds.y, Bounds.w, TOOLBAR_HEIGHT);
      S.Tabs_Area := (Bounds.x, Bounds.y + TOOLBAR_HEIGHT, Bounds.w, TAB_HEIGHT);
      S.Find_Area := (Bounds.x, Bottom, Bounds.w, Find_H);
      S.Status_Area := (Bounds.x, Natural'Max (Bounds.y, Integer (Bounds.y + Bounds.h) - STATUS_HEIGHT), Bounds.w,
                        STATUS_HEIGHT);
      S.Find_Box := (Bounds.x + MARGIN + 60, Bottom + (FIND_HEIGHT - FIELD_HEIGHT) / 2, FIELD_WIDTH, FIELD_HEIGHT);
      S.Replace_Box := (S.Find_Box.x + FIELD_WIDTH + 3 * (OPTION_WIDTH + 2) + 3 * MARGIN + 70, S.Find_Box.y,
                        FIELD_WIDTH, FIELD_HEIGHT);
      if S.Split_On then
         declare
            Half : constant Natural := (if Middle.w > SPLIT_GAP then (Middle.w - SPLIT_GAP) / 2 else 0);
            Body_H : constant Natural := Natural'Max (0, Middle.h - PANE_HEADER_HEIGHT);
         begin
            S.Header_Areas (1) := (Middle.x, Middle.y, Half, PANE_HEADER_HEIGHT);
            S.Header_Areas (2) := (Middle.x + Half + SPLIT_GAP, Middle.y, Natural'Max (0, Middle.w - Half - SPLIT_GAP),
                                   PANE_HEADER_HEIGHT);
            S.Editor_Areas (1) := (Middle.x, Middle.y + PANE_HEADER_HEIGHT, Half, Body_H);
            S.Editor_Areas (2) := (Middle.x + Half + SPLIT_GAP, Middle.y + PANE_HEADER_HEIGHT,
                                   Natural'Max (0, Middle.w - Half - SPLIT_GAP), Body_H);
         end;
      else
         S.Editor_Areas (1) := Middle;
         S.Editor_Areas (2) := (others => 0);
         S.Header_Areas := [others => (others => 0)];
      end if;
      S.Palette_Area :=
        (Bounds.x + (if Bounds.w > PALETTE_WIDTH then (Bounds.w - PALETTE_WIDTH) / 2 else 0), Bounds.y + PALETTE_TOP,
         Natural'Min (PALETTE_WIDTH, Bounds.w),
         FIELD_HEIGHT + 3 * MARGIN + Natural'Min (PALETTE_SHOWN, Natural'Max (S.Row_Count, 1)) * PALETTE_ROW_HEIGHT);
      for K in 1 .. 2 loop
         Text_Editors.Set_Area (S.Editors (K).all, S.Editor_Areas (K));
      end loop;
   end Layout;

   procedure Set_Bounds (S : in out Shell; Bounds : CuBit.UI.Rect) is
   begin
      if Bounds /= S.State.Bounds then
         S.State.Bounds := Bounds;
         S.State.Everything := True;
         S.State.Prepared := False;
      end if;
   end Set_Bounds;

   procedure Take_Damage (S : in out Shell; Area : out CuBit.UI.Rect) is
      St : Shell_State renames S.State.all;
      Result : Rect := St.Dirty;
      Old_Palette : constant Rect := St.Palette_Area;
   begin
      Layout (St, St.Bounds);
      if Doc (St) /= null then
         for K in 1 .. (if St.Split_On then 2 else 1) loop
            declare
               Damage : Rect;
            begin
               Text_Editors.Set_Show_Matches (St.Editors (K).all, St.Find_Visible);
               Text_Editors.Prepare (St.Editors (K).all, St.Colors, St.Focus = Editor_Focus and then K = St.Active
                                                                    and then not St.Palette_On, Damage);
               Result := Union_Rect (Result, Damage);
            end;
         end loop;
      end if;
      St.Toolbar_Hash := Mix (FNV_OFFSET, Natural'Image (St.Hot_Tool) & Boolean'Image (St.Split_On)
                                          & Boolean'Image (Doc (St) /= null and then Can_Undo (Doc (St).all))
                                          & Boolean'Image (Doc (St) /= null and then Can_Redo (Doc (St).all))
                                          & Boolean'Image (St.Find_Visible)
                                          & Boolean'Image (Text_Editors.Wrap_On (Editor (St).all))
                                          & Boolean'Image (Text_Editors.Minimap_On (St.Editors (1).all)));
      declare
         Tabs : Unsigned_64 := Mix (FNV_OFFSET, Natural'Image (St.Current) & Natural'Image (St.Active)
                                               & Boolean'Image (St.Split_On) & Natural'Image (Focus_Kind'Pos (St.Focus)));
      begin
         for T in 1 .. St.Count loop
            Tabs := Mix (Tabs, Tab_Label (St, T));
         end loop;
         St.Tabs_Hash := Tabs;
      end;
      St.Status_Hash := Mix (FNV_OFFSET, Status_Text (St));
      St.Find_Hash := Mix (FNV_OFFSET, (if St.Find_Visible then Find_Text (St) else "closed"));
      St.Palette_Hash := Mix (FNV_OFFSET, Palette_Text (St));
      if St.Everything then
         Result := St.Bounds;
      else
         if St.Toolbar_Hash /= St.Drawn_Toolbar then
            Result := Union_Rect (Result, St.Toolbar_Area);
         end if;
         if St.Tabs_Hash /= St.Drawn_Tabs then
            Result := Union_Rect (Union_Rect (Union_Rect (Result, St.Tabs_Area), St.Header_Areas (1)),
                                  St.Header_Areas (2));
         end if;
         if St.Status_Hash /= St.Drawn_Status then
            Result := Union_Rect (Result, St.Status_Area);
         end if;
         if St.Find_Hash /= St.Drawn_Find then
            Result := Union_Rect (Result, St.Find_Area);
         end if;
         if St.Palette_Hash /= St.Drawn_Palette then
            Result := Union_Rect (Union_Rect (Result, St.Palette_Area), Old_Palette);
            --  What the palette covered shows again.
            Text_Editors.Invalidate (St.Editors (1).all);
            Text_Editors.Invalidate (St.Editors (2).all);
            if not St.Palette_On then
               Result := Union_Rect (Result, Old_Palette);
            end if;
         end if;
         if St.Palette_On and then Intersects (Result, St.Palette_Area) then
            Result := Union_Rect (Result, St.Palette_Area);
         end if;
      end if;
      St.Dirty := (others => 0);
      St.Prepared := True;
      Area := Result;
   end Take_Damage;

   procedure Draw_Toolbar (S : Shell_State; C : Canvas) is
      Colors : constant Theme := Current_Theme;
      D : constant Document_Access := Doc (S);
   begin
      CuBit.UI.Widgets.Toolbar (C, S.Toolbar_Area, Colors);
      for T in Tool loop
         declare
            Box : constant Rect := Tool_Box (S, T);
            Enabled : constant Boolean :=
              (case T is
                  when Tool_New | Tool_Open | Tool_Palette => True,
                  when Tool_Undo => D /= null and then Can_Undo (D.all),
                  when Tool_Redo => D /= null and then Can_Redo (D.all),
                  when others => D /= null);
         begin
            CuBit.UI.Icons.Draw_Tool_Button
              (C, Box, Colors, Tool_Icon (T), Enabled, Hot => S.Hot_Tool = Tool'Pos (T) + 1, Pressed => False,
               Checked => (T = Tool_Split and then S.Split_On) or else (T = Tool_Find and then S.Find_Visible)
                          or else (T = Tool_Wrap and then Text_Editors.Wrap_On (Editor (S).all))
                          or else (T = Tool_Minimap and then Text_Editors.Minimap_On (S.Editors (1).all)));
            if T in Tool_Save | Tool_Redo | Tool_Palette then
               CuBit.UI.Widgets.Toolbar_Separator
                 (C, (Box.x + TOOL_SIZE + TOOL_GAP, S.Toolbar_Area.y + 4, TOOL_GROUP_GAP, TOOLBAR_HEIGHT - 8), Colors);
            end if;
         end;
      end loop;
      --  The app's name, right-aligned, quiet.
      declare
         Title : constant String := "CuBit Text";
         Width : constant Natural := UI_Text_Width (Title);
      begin
         if S.Toolbar_Area.w > Width + 2 * MARGIN + 400 then
            Draw_UI_Text_Transparent (C, S.Toolbar_Area.x + S.Toolbar_Area.w - Width - 3 * MARGIN,
                                      S.Toolbar_Area.y + (TOOLBAR_HEIGHT - UI_Text_Height) / 2, Title, Colors.muted);
         end if;
      end;
   end Draw_Toolbar;

   procedure Draw_Tabs (S : Shell_State; C : Canvas) is
      Colors : constant Theme := Current_Theme;
   begin
      Draw_Tab_Strip (C, S.Tabs_Area, Colors);
      for T in 1 .. S.Count loop
         Draw_Tab (C, Tab_Box (S, T), Colors, Selected => T = S.Current, Hot => False, Active => T = S.Current,
                   label => Tab_Label (S, T));
      end loop;
   end Draw_Tabs;

   procedure Draw_Find (S : Shell_State; C : Canvas) is
      Colors : constant Theme := Current_Theme;
      Text_Y : constant Natural := S.Find_Area.y + (FIND_HEIGHT - UI_Text_Height) / 2;
      procedure Field (F : CuBit.UI.Editor.Edit_State; Box : Rect; Focused : Boolean) is
      begin
         Draw_Text_Edit_Field (C, Box, Colors, CuBit.UI.Editor.Content (F), CuBit.UI.Editor.Cursor (F) - 1,
                               CuBit.UI.Editor.Selection_First (F) - 1, CuBit.UI.Editor.Selection_Last (F) - 1,
                               Focused, False);
      end Field;
      Labels : constant array (1 .. 3) of access constant String :=
        [new String'("Aa"), new String'("W"), new String'(".*")];
      States : constant array (1 .. 3) of Boolean :=
        [S.Options.Case_Sensitive, S.Options.Whole_Word, S.Options.Regular];
   begin
      Fill_Rect (C, S.Find_Area, Colors.panel);
      Fill_Rect (C, (S.Find_Area.x, S.Find_Area.y, S.Find_Area.w, 1), Colors.edge);
      Draw_UI_Text_Transparent (C, S.Find_Area.x + MARGIN + 6, Text_Y, "Find", Colors.text);
      Field (S.Find_Field, S.Find_Box, S.Focus = Find_Focus);
      for K in 1 .. 3 loop
         Draw_Button (C, Option_Box (S, K), Colors, (if States (K) then Button_Active else Button_Normal), Labels (K).all);
      end loop;
      declare
         X : Natural := Option_Box (S, 3).x + OPTION_WIDTH + 3 * MARGIN;
      begin
         if S.Replace_Visible then
            Draw_UI_Text_Transparent (C, X, Text_Y, "Replace", Colors.text);
            Field (S.Replace_Field, S.Replace_Box, S.Focus = Replace_Focus);
            X := S.Replace_Box.x + S.Replace_Box.w + 2 * MARGIN;
         end if;
         Draw_UI_Text_Transparent
           (C, X, Text_Y,
            (if S.Search_Bad then "not a valid pattern"
             elsif Field_Text (S.Find_Field)'Length = 0 then "Enter: next   Shift+Enter: previous   Alt+Enter: all"
             else Natural'Image (S.Match_Total) & (if S.Match_Total = 1 then " match" else " matches")
                  & (if S.Count_Done then "" else "...")),
            (if S.Search_Bad then Colors.danger else Colors.muted));
      end;
   end Draw_Find;

   procedure Draw_Palette (S : Shell_State; C : Canvas) is
      Colors : constant Theme := Current_Theme;
      Box : constant Rect := S.Palette_Area;
      Field_Box : constant Rect := (Box.x + MARGIN, Box.y + MARGIN, Natural'Max (0, Box.w - 2 * MARGIN), FIELD_HEIGHT);
      First_Shown : constant Natural := (if S.Selected > PALETTE_SHOWN then S.Selected - PALETTE_SHOWN + 1 else 1);
      Prompt : constant String :=
        (case S.Mode is
            when Commands_Mode => "",
            when Line_Mode => "Line number, then Enter",
            when Anything_Mode => "File name, :line or @symbol",
            when Open_Mode => "Open a granted file",
            when Argument_Mode => Qualified_Name (S.Pending) & ": the argument, then Enter");
   begin
      Fill_Rect (C, (Box.x + 3, Box.y + 3, Box.w, Box.h), Colors.shadow);
      Fill_Rect (C, Box, Colors.panel);
      Stroke_Rect (C, Box, Colors.edge, Colors.darkShadow);
      Draw_Text_Edit_Field (C, Field_Box, Colors, Field_Text (S.Palette_Field), CuBit.UI.Editor.Cursor (S.Palette_Field) - 1,
                            CuBit.UI.Editor.Selection_First (S.Palette_Field) - 1,
                            CuBit.UI.Editor.Selection_Last (S.Palette_Field) - 1, True, False,
                            suggestion => (if Field_Text (S.Palette_Field)'Length = 0 then Prompt else ""));
      for K in 0 .. PALETTE_SHOWN - 1 loop
         exit when First_Shown + K > S.Row_Count;
         declare
            Row : constant Positive := First_Shown + K;
            Line_Box : constant Rect :=
              (Box.x + MARGIN, Field_Box.y + FIELD_HEIGHT + MARGIN + K * PALETTE_ROW_HEIGHT, Field_Box.w,
               PALETTE_ROW_HEIGHT);
            Text : constant String := Row_Text (S, Row);
            Bar1 : Natural := Text'First;
            Text_Y : constant Natural := Line_Box.y + (PALETTE_ROW_HEIGHT - UI_Text_Height) / 2;
            Selected : constant Boolean := Row = S.Selected;
            Ink : constant Color := (if Selected then Colors.selectionText else Colors.text);
            Quiet : constant Color := (if Selected then Colors.selectionText else Colors.muted);
         begin
            if Selected then
               Fill_Rect (C, Line_Box, Colors.selection);
            end if;
            while Bar1 <= Text'Last and then Text (Bar1) /= '|' loop
               Bar1 := Bar1 + 1;
            end loop;
            declare
               Bar2 : Natural := Bar1 + 1;
            begin
               while Bar2 <= Text'Last and then Text (Bar2) /= '|' loop
                  Bar2 := Bar2 + 1;
               end loop;
               Draw_UI_Text_Transparent (With_Clip (C, Line_Box), Line_Box.x + 6, Text_Y, Text (Text'First .. Bar1 - 1),
                                         Ink);
               if Bar2 - 1 >= Bar1 + 1 then
                  Draw_Code_Text_Transparent (With_Clip (C, Line_Box), Line_Box.x + Line_Box.w / 2 + 20, Text_Y,
                                              Text (Bar1 + 1 .. Bar2 - 1), Quiet);
               end if;
               --  The characters the query matched, underlined: in the CCL
               --  name when it matched there, else in the title.
               declare
                  Query : constant String := Field_Text (S.Palette_Field);
                  Title : constant String := Text (Text'First .. Bar1 - 1);
                  Code : constant String := (if Bar2 - 1 >= Bar1 + 1 then Text (Bar1 + 1 .. Bar2 - 1) else "");
                  Short : constant String :=
                    (if Code'Length > 5 and then Code (Code'First .. Code'First + 4) = "text." then Code (Code'First + 5 .. Code'Last)
                     else Code);
                  In_Code : constant Match_Marks := Matched (Query, Short);
                  In_Title : constant Match_Marks := Matched (Query, Title);
                  Under_Y : constant Natural := Text_Y + UI_Text_Height;
                  Mark : constant Color := (if Selected then Colors.selectionText else Colors.accent);
               begin
                  if Query'Length > 0 and then S.Mode in Commands_Mode | Anything_Mode | Open_Mode then
                     if (for some M of In_Code => M) then
                        for I in In_Code'Range loop
                           if In_Code (I) then
                              Fill_Rect (With_Clip (C, Line_Box),
                                         (Line_Box.x + Line_Box.w / 2 + 20
                                          + (Code'Length - Short'Length + I - Short'First) * Code_Text_Width ("x"),
                                          Under_Y, Code_Text_Width ("x"), 2), Mark);
                           end if;
                        end loop;
                     else
                        for I in In_Title'Range loop
                           if In_Title (I) then
                              Fill_Rect (With_Clip (C, Line_Box),
                                         (Line_Box.x + 6 + UI_Text_Width (Title (Title'First .. I - 1)), Under_Y,
                                          Natural'Max (2, UI_Text_Width (Title (I .. I))), 2), Mark);
                           end if;
                        end loop;
                     end if;
                  end if;
               end;
               if Bar2 + 1 <= Text'Last then
                  declare
                     Keys : constant String := Text (Bar2 + 1 .. Text'Last);
                  begin
                     Draw_UI_Text_Transparent (With_Clip (C, Line_Box), Line_Box.x + Line_Box.w - UI_Text_Width (Keys) - 8,
                                               Text_Y, Keys, Quiet);
                  end;
               end if;
            end;
         end;
      end loop;
      if S.Row_Count = 0 and then S.Mode in Commands_Mode | Anything_Mode | Open_Mode then
         Draw_UI_Text_Transparent (C, Box.x + 10, Field_Box.y + FIELD_HEIGHT + MARGIN + 3, "No match", Colors.muted);
      end if;
   end Draw_Palette;

   procedure Render (S : in out Shell; C : CuBit.UI.Canvas; Bounds : CuBit.UI.Rect) is
      St : Shell_State renames S.State.all;
      Clip : Rect;
      Colors : constant Theme := Current_Theme;
   begin
      if Bounds /= St.Bounds or else not St.Prepared then
         St.Bounds := Bounds;
         declare
            Ignored : Rect;
         begin
            Take_Damage (S, Ignored);
         end;
      end if;
      Clip := (if C.clipEnabled then C.clip else Bounds);
      if St.Everything then
         Text_Editors.Invalidate (St.Editors (1).all);
         Text_Editors.Invalidate (St.Editors (2).all);
      end if;
      if Intersects (Clip, St.Toolbar_Area) then
         Draw_Toolbar (St, C);
      end if;
      if Intersects (Clip, St.Tabs_Area) then
         Draw_Tabs (St, C);
      end if;
      St.Painted := 0;
      if Doc (St) = null then
         declare
            Area : constant Rect := St.Editor_Areas (1);
            Hint : constant String := "No document.  Ctrl+N: new   Ctrl+O: open   Ctrl+P: go to anything   Ctrl+Shift+P: every command";
         begin
            if Intersects (Clip, Area) then
               Fill_Rect (C, Area, Colors.face);
               Draw_UI_Text_Transparent (C, Area.x + (if Area.w > UI_Text_Width (Hint) then (Area.w - UI_Text_Width (Hint)) / 2 else 0),
                                         Area.y + Area.h / 3, Hint, Colors.muted);
            end if;
         end;
      else
         for K in 1 .. (if St.Split_On then 2 else 1) loop
            if Intersects (Clip, St.Editor_Areas (K)) then
               Text_Editors.Draw (St.Editors (K).all, C);
               St.Painted := St.Painted + Text_Editors.Rows_Painted (St.Editors (K).all);
            end if;
         end loop;
         if St.Split_On then
            Fill_Rect (C, (St.Header_Areas (1).x + St.Header_Areas (1).w, St.Header_Areas (1).y, SPLIT_GAP,
                           St.Editor_Areas (1).h + PANE_HEADER_HEIGHT), Colors.edge);
            for K in 1 .. 2 loop
               if Intersects (Clip, St.Header_Areas (K)) then
                  declare
                     Box : constant Rect := St.Header_Areas (K);
                     Focused : constant Boolean := K = St.Active and then St.Focus = Editor_Focus;
                     Label : constant String :=
                       Display_Name (St.Host.all, Name (Doc (St).all)) & (if Modified (Doc (St).all) then " *" else "")
                       & (if K = St.Active then "" else "   (F6: focus this view)");
                  begin
                     Fill_Vertical_Gradient
                       (C, Box, (if Focused then Colors.activeTitleTop else Colors.inactiveTitleTop),
                        (if Focused then Colors.activeTitleBottom else Colors.inactiveTitleBottom));
                     Draw_UI_Text_Transparent
                       (With_Clip (C, Box), Box.x + 8, Box.y + (PANE_HEADER_HEIGHT - UI_Text_Height) / 2, Label,
                        (if Focused then Colors.selectionText else Colors.text));
                  end;
               end if;
            end loop;
         end if;
      end if;
      if St.Find_Visible and then Intersects (Clip, St.Find_Area) then
         Draw_Find (St, C);
      end if;
      if Intersects (Clip, St.Status_Area) then
         declare
            Text : constant String := Status_Text (St);
            Bar : Natural := Text'First;
         begin
            while Bar <= Text'Last and then Text (Bar) /= '|' loop
               Bar := Bar + 1;
            end loop;
            Draw_Status_Bar (C, St.Status_Area, Colors, Text (Text'First .. Bar - 1),
                             (if Bar < Text'Last then Text (Bar + 1 .. Text'Last) else ""));
         end;
      end if;
      if St.Palette_On and then Intersects (Clip, St.Palette_Area) then
         Draw_Palette (St, C);
      end if;
      St.Drawn_Toolbar := St.Toolbar_Hash;
      St.Drawn_Tabs := St.Tabs_Hash;
      St.Drawn_Status := St.Status_Hash;
      St.Drawn_Find := St.Find_Hash;
      St.Drawn_Palette := St.Palette_Hash;
      St.Everything := False;
      St.Prepared := False;
   end Render;

   procedure Note_Frame (S : in out Shell; Render_Us, Pump_Us : Unsigned_64) is
   begin
      S.State.Last_Render_Us := Render_Us;
      S.State.Last_Pump_Us := Pump_Us;
      S.State.Worst_Render_Us := Unsigned_64'Max (S.State.Worst_Render_Us, Render_Us);
   end Note_Frame;

   function Quit_Requested (S : Shell) return Boolean is (S.State.Quit);
   function Next_Deadline_Us (S : Shell) return Unsigned_64 is
     (Unsigned_64'Min (S.State.Occurrence_Due,
                       (if S.State.Message_Length > 0 then S.State.Message_Until else Unsigned_64'Last)));

   function Tabs (S : Shell) return Tab_Count is (S.State.Count);
   function Current_Tab (S : Shell) return Tab_Count is (S.State.Current);
   function Document (S : Shell; Tab : Tab_Index) return Text_Documents.Document_Access is
     (S.State.Tab_List (Tab).Doc);
   function Active_Document (S : Shell) return Text_Documents.Document_Access is (Doc (S.State.all));
   function Active_Editor (S : Shell) return Text_Editors.Editor_Access is (Editor (S.State.all));
   function Status_Message (S : Shell) return String is (S.State.Message (1 .. S.State.Message_Length));
   function Status_Line (S : Shell) return String is (Status_Text (S.State.all));
   function Palette_Open (S : Shell) return Boolean is (S.State.Palette_On);
   function Palette_Query (S : Shell) return String is (Field_Text (S.State.Palette_Field));
   function Palette_Rows (S : Shell) return Natural is (S.State.Row_Count);
   function Palette_Row (S : Shell; Row : Positive) return String is (Row_Text (S.State.all, Row));
   function Find_Open (S : Shell) return Boolean is (S.State.Find_Visible);
   function Split (S : Shell) return Boolean is (S.State.Split_On);
   function Modal_On (S : Shell) return Boolean is (S.State.Modal);
   function Loading (S : Shell) return Boolean is (S.State.Load_Tab /= 0);
   function Saving (S : Shell) return Boolean is (S.State.Phase /= Idle);
   function Rows_Painted (S : Shell) return Natural is (S.State.Painted);
end Text_Shells;
