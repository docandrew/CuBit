------------------------------------------------------------------------------
--  CuBit Dictionary and Thesaurus (docs/lexicon.md, "The apps"): one view,
--  two first-class modes. A search field, an entry laid out as a dictionary
--  page (senses numbered under their parts of speech, examples, word lists)
--  whose words are links, back and forward through the words looked up, and
--  near spellings for a word the lexicon does not have.
--
--  Platform-free: events in, frames out. The answers come from Ask, the
--  same Lexicon_Answers pages every face of the lexicon uses: natively the
--  lexicon service's channel (Lexicon_Client), hosted the image file.
------------------------------------------------------------------------------
with Interfaces;
with CuBit.UI;
with CuBit.UI.Controls;
with CuBit.UI.Editor;
with CuBit.UI.State;
with Lexicon_Answers;
with Lexicon_Wire;

generic
   --  One answer page (Lexicon_Answers.Answer), or Available False when
   --  there is no lexicon to ask.
   with procedure Ask
     (Op : Lexicon_Answers.Operation; Text : String; Resume : Interfaces.Unsigned_32;
      Suggestions : Lexicon_Answers.Suggestion_Count; Into : out Lexicon_Wire.Answer_Bytes;
      Used : out Natural; Available : out Boolean);
   --  The size of a page Ask fills.
   PAGE_BYTES : Positive;
package Dictionary_Views is
   package Controls renames CuBit.UI.Controls;

   type Mode is (Dictionary_Mode, Thesaurus_Mode);
   function Title (Value : Mode) return String is
     (case Value is when Dictionary_Mode => "Dictionary", when Thesaurus_Mode => "Thesaurus");

   type Key_Name is
     (No_Key, Up, Down, Left, Right, Page_Up, Page_Down, Home, End_Key, Enter, Escape, Backspace, Delete, Tab);
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
   DICTIONARY_ID : constant Controls.Control_ID := 4;
   THESAURUS_ID  : constant Controls.Control_ID := 5;
   SCROLL_ID     : constant Controls.Control_ID := 6;

   type Lookup_Status is
     (Idle,          --  nothing asked yet
      Found,         --  an entry
      Not_Found,     --  not in the lexicon: near spellings
      Unavailable,   --  no lexicon service or image
      Not_A_Word);   --  empty, or longer than any word

   type View_State is limited private;

   procedure Initialize (State : out View_State; Start : Mode);
   --  Look Word up as if typed and entered (recorded for Back).
   procedure Look_Up (State : in out View_State; Word : String);
   procedure Set_Mode (State : in out View_State; Value : Mode);

   procedure Handle
     (State : in out View_State; Item : Event; Map : in out Controls.Control_Map; Redraw : out Boolean);
   procedure Render
     (State : in out View_State; C : CuBit.UI.Canvas; Bounds : CuBit.UI.Rect;
      UI : in out CuBit.UI.State.UI_State; Map : in out Controls.Control_Map);

   --  What the last frame showed, for tests and automation.
   function Status (State : View_State) return Lookup_Status;
   function Query (State : View_State) return String;
   function Current_Mode (State : View_State) return Mode;
   function Headword (State : View_State) return String;
   function Senses (State : View_State) return Natural;
   function Links (State : View_State) return Natural;
   function Link_Text (State : View_State; Index : Positive) return String;
   function Link_Area (State : View_State; Index : Positive) return CuBit.UI.Rect;
   function Can_Go_Back (State : View_State) return Boolean;

private
   use Interfaces;
   subtype Word_Length is Natural range 0 .. CuBit.UI.Editor.MAX_TEXT_LENGTH;

   --  The words looked up, for Back and Forward: a bounded navigation
   --  list, as a browser keeps; the oldest drop off.
   HISTORY_DEPTH : constant := 64;
   type Word_Text is record
      Length : Word_Length := 0;
      Text : String (1 .. CuBit.UI.Editor.MAX_TEXT_LENGTH) := [others => ' '];
   end record;
   subtype History_Count is Natural range 0 .. HISTORY_DEPTH;
   type History_Array is array (1 .. HISTORY_DEPTH) of Word_Text;

   --  The answer: every item of every page, in order (Continue_At items
   --  dropped), in one heap buffer that grows as entries need.
   type Store_Access is access Lexicon_Wire.Answer_Bytes;

   --  The laid-out page: one operation per run of text or link, in content
   --  coordinates; rebuilt when the entry or the width changes.
   type Op_Kind is (Text_Op, Link_Op, Rule_Op);
   type Op_Style is
     (Headword_Style, Count_Style, Part_Style, Number_Style, Body_Style, Muted_Body_Style,
      Example_Style, Label_Style, Link_Style, Missing_Style);
   type Label_Name is
     (No_Label, Senses_Label, Synonyms_Label, Antonyms_Label, Related_Label, Broader_Label,
      Narrower_Label, Also_Label, Suggestions_Label, Missing_Label, Welcome_Label, Welcome_Thesaurus_Label,
      Try_Label,
      Unavailable_Label, Unavailable_Hint_Label, Not_A_Word_Label, Separator_Label,
      Noun_Label, Verb_Label, Adjective_Label, Adverb_Label);
   type Op is record
      Kind   : Op_Kind := Text_Op;
      Style  : Op_Style := Body_Style;
      X, Y, W : Natural := 0;
      --  The text: Length bytes of the store from First, else Label (with
      --  Number before it when Number > 0).
      First  : Natural := 0;
      Length : Natural := 0;
      Label  : Label_Name := No_Label;
      Number : Natural := 0;
      Open_Quote, Close_Quote : Boolean := False;
   end record;
   type Op_Array is array (Positive range <>) of Op;
   type Ops_Access is access Op_Array;

   type View_State is limited record
      Current : Mode := Dictionary_Mode;
      Search : CuBit.UI.Editor.Edit_State;
      Status : Lookup_Status := Idle;
      Asked : Word_Text;                 --  what the shown answer is for
      Asked_Mode : Mode := Dictionary_Mode;
      Store : Store_Access;
      Store_Used : Natural := 0;
      Sense_Total : Natural := 0;
      History : History_Array;
      History_Used : History_Count := 0;
      History_At : History_Count := 0;   --  the entry shown (0: none)
      Ops : Ops_Access;
      Op_Count : Natural := 0;
      Laid_Out_Width : Natural := 0;     --  0: the layout is stale
      Content_Height : Natural := 0;
      Scroll : Natural := 0;
      Content : CuBit.UI.Rect := (others => 0);   --  the page's area, last frame
      Width, Height : Natural := 0;
      Hover_X, Hover_Y : Natural := 0;
      Pressed_Link : Natural := 0;
   end record;
end Dictionary_Views;
