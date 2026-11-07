------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Bible app's view (APP-005): a sidebar tree of the canon (testament,
--  section, book, chapter, verse), a reading pane that sets a chapter in
--  the reading face (Literata) with headings, paragraphs, poetry and
--  superscript verse numbers at a comfortable measure, a search field that
--  takes a reference ("John 3:16", through the proved Bible_Parser) or
--  words (Bible_Search over the image's index), the translations as tabs,
--  and Back and Forward through the passages read.
--
--  Platform-free: events in, frames out. Translations come from Load
--  (natively the asset package through the filesystem; hosted, files).
------------------------------------------------------------------------------
with CuBit.UI;
with CuBit.UI.Controls;
with CuBit.UI.Editor;
with CuBit.UI.State;
with Bible_Canon;
with Bible_Images;
with Bible_References;
with Bible_Search;
with Bible_Translations;

generic
   --  Read one translation's image (Data null unless Status is Loaded;
   --  the view opens and indexes it).
   with procedure Load (Which : Bible_Translations.Translation; Data : out Bible_Translations.Image_Access;
                        Status : out Bible_Translations.Load_Status);
   --  The reader's remembered choices ("" when none), and saving one. The
   --  faces themselves are CuBit.Reading_Faces' (whatever the program offered).
   with function Saved (Item : Bible_Translations.Reader_Preference) return String;
   with procedure Save (Item : Bible_Translations.Reader_Preference; Value : String);
package Bible_Views is
   package Controls renames CuBit.UI.Controls;
   subtype Translation is Bible_Translations.Translation;

   type Key_Name is
     (No_Key, Up, Down, Left, Right, Page_Up, Page_Down, Home, End_Key, Enter, Escape, Backspace, Delete, Tab,
      Next_Face_Key,    --  F8: the next reading face
      Red_Letters_Key); --  F9: the words of Christ in red, or not
   type Event_Kind is (Key_Event, Text_Event, Pointer_Event, Wheel, Resize);
   type Event is record
      Kind : Event_Kind := Key_Event;
      Key : Key_Name := No_Key;
      Shift, Control, Alt : Boolean := False;
      Character_Value : Character := ' ';
      Action : Controls.Pointer_Action := Controls.Pointer_Move;
      X, Y : Natural := 0;
      Steps : Integer := 0;
   end record;

   BACK_ID       : constant Controls.Control_ID := 1;
   FORWARD_ID    : constant Controls.Control_ID := 2;
   SEARCH_ID     : constant Controls.Control_ID := 3;
   PREVIOUS_ID   : constant Controls.Control_ID := 4;
   NEXT_ID       : constant Controls.Control_ID := 5;
   PAGE_SCROLL_ID : constant Controls.Control_ID := 6;
   TREE_SCROLL_ID : constant Controls.Control_ID := 7;
   SPLIT_ID      : constant Controls.Control_ID := 8;
   FACE_ID       : constant Controls.Control_ID := 9;
   RED_ID        : constant Controls.Control_ID := 13;
   --  One tab per translation, from here.
   FIRST_TAB_ID  : constant Controls.Control_ID := 10;
   --  The tree's visible rows, from here.
   FIRST_ROW_ID  : constant Controls.Control_ID := 100;

   type Page_Mode is
     (Reading,        --  a chapter
      Results,        --  verses holding the words in the field
      Unavailable);   --  the translation could not be loaded

   type View_State is limited private;

   procedure Initialize (State : out View_State);
   --  Show a passage of the current translation (recorded for Back).
   procedure Open (State : in out View_State; Item : Bible_References.Passage);
   procedure Set_Translation (State : in out View_State; Which : Translation);
   --  The reading face: the next one offered (the toolbar's face button),
   --  or one by name. Saved for the next start.
   procedure Next_Face (State : in out View_State);
   procedure Set_Face (State : in out View_State; Name : String);
   function Face_Name return String;
   --  The words of Christ in red (on unless the reader turned it off).
   procedure Set_Red_Letters (State : in out View_State; On : Boolean);
   function Red_Letters (State : View_State) return Boolean;
   --  The red for a page of this background: deep on light pages, light on
   --  dark ones, at least 4.5:1 against it (tests/bible-app checks).
   function Red_Letter_Color (Page : CuBit.UI.Color) return CuBit.UI.Color;
   --  How many runs the last layout set in red.
   function Red_Runs (State : View_State) return Natural;
   --  Every run of the verse on the page is red (False: some is not, or
   --  none is laid out); and some run of it is.
   function Verse_All_Red (State : View_State; Verse : Bible_References.Verse_Number) return Boolean;
   function Verse_Any_Red (State : View_State; Verse : Bible_References.Verse_Number) return Boolean;
   --  As if typed in the field and entered: a reference opens it, words
   --  search.
   procedure Enter_Text (State : in out View_State; Text : String);

   procedure Handle
     (State : in out View_State; Item : Event; Map : in out Controls.Control_Map; Redraw : out Boolean);
   procedure Render
     (State : in out View_State; C : CuBit.UI.Canvas; Bounds : CuBit.UI.Rect;
      UI : in out CuBit.UI.State.UI_State; Map : in out Controls.Control_Map);

   --  What the last frame showed, for tests and automation.
   function Mode (State : View_State) return Page_Mode;
   function Current_Translation (State : View_State) return Translation;
   function Shown (State : View_State) return Bible_References.Passage;
   function Heading (State : View_State) return String;
   function Status_Text (State : View_State) return String;
   function Result_Count (State : View_State) return Natural;
   function Result_Reference (State : View_State; Index : Positive) return Bible_References.Reference;
   function Query (State : View_State) return String;
   --  The verse's number's box on screen (w = 0: not visible).
   function Verse_Area (State : View_State; Verse : Bible_References.Verse_Number) return CuBit.UI.Rect;
   function Result_Area (State : View_State; Index : Positive) return CuBit.UI.Rect;
   function Tree_Rows (State : View_State) return Natural;
   function Tree_Row_Label (State : View_State; Row : Positive) return String;
   --  Milliseconds-free work counters: the last layout's lines and ops.
   function Laid_Out_Lines (State : View_State) return Natural;

private
   use Bible_Canon;
   use Bible_References;
   subtype Field_Length is Natural range 0 .. CuBit.UI.Editor.MAX_TEXT_LENGTH;

   --  Back and Forward: a bounded navigation list, as a browser keeps; the
   --  oldest drop off.
   HISTORY_DEPTH : constant := 64;
   type Place is record
      Which : Translation := Bible_Translations.DEFAULT;
      Item : Passage;
   end record;
   subtype History_Count is Natural range 0 .. HISTORY_DEPTH;
   type History_Array is array (1 .. HISTORY_DEPTH) of Place;

   type Edition_Array is array (Translation) of Bible_Translations.Edition;

   --  The laid-out page: operations in content coordinates, rebuilt when
   --  the chapter, the results or the width change. Text ops hold face text
   --  (Bible_Text) in one growable store.
   type Op_Kind is (Text_Op, Rule_Op, Tint_Op, Link_Op);
   type Op_Style is
     (Body_Style, Red_Style, Verse_Number_Style, Heading_Style, Caption_Style, Section_Style, Title_Style,
      Muted_Style, Reference_Style, Note_Style);
   type Op is record
      Kind   : Op_Kind := Text_Op;
      Style  : Op_Style := Body_Style;
      X, Y, W, H : Natural := 0;
      First  : Natural := 0;    --  the text: Length bytes of the store from First
      Length : Natural := 0;
      Verse  : Verse_Count := 0;   --  the verse it belongs to (0: none)
      Target : Natural := 0;    --  a link's result number
   end record;
   type Op_Array is array (Positive range <>) of Op;
   type Ops_Access is access Op_Array;
   type Store_Access is access String;

   type Hits_Access is access Bible_Search.Hit_Array;
   type Work_Access is access Bible_Search.Work_Area;

   --  The sidebar's visible rows.
   type Node_Kind is (Testament_Node, Section_Node, Book_Node, Chapter_Node, Verse_Node);
   type Node is record
      Kind : Node_Kind := Testament_Node;
      Part : Testament := Old_Testament;
      Group : Section := Law;
      Item : Book := Genesis;
      Chapter : Chapter_Count := 0;
      Verse : Verse_Count := 0;
      Depth : Natural := 0;
      Has_Children, Expanded, Last_Sibling : Boolean := False;
   end record;
   type Node_Array is array (Positive range <>) of Node;
   type Nodes_Access is access Node_Array;
   type Testament_Flags is array (Testament) of Boolean;
   type Section_Flags is array (Section) of Boolean;
   type Book_Flags is array (Book) of Boolean;

   type View_State is limited record
      Which : Translation := Bible_Translations.DEFAULT;
      Editions : Edition_Array;
      Current : Page_Mode := Reading;
      Item : Passage;                          --  the passage shown
      Highlight : Boolean := False;            --  tint Item's verses
      Search : CuBit.UI.Editor.Edit_State;
      --  Results.
      Hits : Hits_Access;
      Work : Work_Access;
      Found, Total : Natural := 0;
      Searched : Boolean := False;             --  Results holds a search
      Whole_Words : Boolean := False;          --  ... of whole words (else the last is a prefix)
      Query_Text : String (1 .. CuBit.UI.Editor.MAX_TEXT_LENGTH) := [others => ' '];
      Query_Length : Field_Length := 0;
      Reference_Hint : Boolean := False;       --  the field parses as a reference
      Hint : Passage;
      --  History.
      History : History_Array;
      History_Used : History_Count := 0;
      History_At : History_Count := 0;
      --  The page.
      Ops : Ops_Access;
      Op_Count : Natural := 0;
      Store : Store_Access;
      Store_Used : Natural := 0;
      Lines : Natural := 0;
      Laid_Out_Width : Natural := 0;           --  0: the layout is stale
      Content_Height : Natural := 0;
      Scroll : Natural := 0;
      Scroll_To_Verse : Verse_Count := 0;      --  after the next layout
      Content : CuBit.UI.Rect := (others => 0);
      --  The sidebar.
      Testament_Open : Testament_Flags := [others => True];
      Section_Open : Section_Flags := [others => False];
      Book_Open : Book_Flags := [others => False];
      Chapter_Open : Chapter_Count := 0;       --  of Item.From.Book (0: none)
      Nodes : Nodes_Access;
      Node_Count : Natural := 0;
      Tree_Scroll : Natural := 0;
      Tree_Box : CuBit.UI.Rect := (others => 0);
      Tree_Stale : Boolean := True;
      Tree_Follow : Boolean := True;           --  scroll the passage's row into view
      Red : Boolean := True;                   --  the words of Christ in red
      Red_Count : Natural := 0;
      Split : Natural := 0;                    --  the sidebar's width (0: default)
      Width, Height : Natural := 0;
      Hover_X, Hover_Y : Natural := 0;
      Pressed_Link : Natural := 0;
   end record;
end Bible_Views;
