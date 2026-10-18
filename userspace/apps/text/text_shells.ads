with Interfaces;
with CuBit.UI;
with Text_Limits; use Text_Limits;
with Text_Commands;
with Text_Documents;
with Text_Editors;
with Text_Hosts;

--  CuBit Text's window (docs/text-editor.md, "Layout"): one host of the
--  editor core. Tabs of documents, one or two editors (a split), the
--  toolbar, the find bar, the palette and the status line, drawn with the
--  shared toolkit. Platform-free: the native main and the hosted harness
--  translate input into Events, call Pump between frames and Render into
--  their canvas. Nothing here blocks: files load and save through the
--  host a chunk at a time.
--
--  Every command, from a key, the palette, a toolbar button or a CCL call,
--  goes through Execute.
generic
   type Host_Type (<>) is limited private;
   with function Chunk_Limit (H : Host_Type) return Positive is <>;
   with procedure Start_Load
     (H : in out Host_Type; Name : String; Size : out Byte_Count; Status : out Text_Hosts.File_Status) is <>;
   with procedure Poll_Load
     (H : in out Host_Type; Into : out String; Got : out Natural; Status : out Text_Hosts.File_Status) is <>;
   with procedure Start_Save (H : in out Host_Type; Name : String; Status : out Text_Hosts.File_Status) is <>;
   with procedure Save_Chunk (H : in out Host_Type; Bytes : String; Status : out Text_Hosts.File_Status) is <>;
   with procedure Finish_Save (H : in out Host_Type; Status : out Text_Hosts.File_Status) is <>;
   with procedure Poll_Save (H : in out Host_Type; Status : out Text_Hosts.File_Status) is <>;
   with procedure Copy (H : in out Host_Type; Text : String) is <>;
   with procedure Request_Paste (H : in out Host_Type) is <>;
   with procedure Poll_Paste
     (H : in out Host_Type; Into : out String; Length : out Natural; Ready, Truncated : out Boolean) is <>;
   with function Granted_Count (H : Host_Type) return Natural is <>;
   with function Granted_Name (H : Host_Type; Index : Positive) return String is <>;
   with function Display_Name (H : Host_Type; Name : String) return String is <>;
package Text_Shells is
   use Interfaces;

   type Pointer_Action is (Press, Release, Move);
   type Event_Kind is (Key_Event, Text_Event, Pointer_Event, Wheel_Event, Resize_Event);
   MAXIMUM_EVENT_TEXT : constant := 32;
   type Event is record
      Kind : Event_Kind := Key_Event;
      Pressed : Text_Commands.Chord := Text_Commands.NO_CHORD;
      --  Text: UTF-8, as typed.
      Text : String (1 .. MAXIMUM_EVENT_TEXT) := [others => ' '];
      Text_Length : Natural := 0;
      Action : Pointer_Action := Move;
      X, Y : Natural := 0;
      Control, Shift, Alt : Boolean := False;
      --  Wheel: positive scrolls toward the top.
      Steps : Integer := 0;
      --  Monotonic milliseconds (multi-clicks).
      Time_Ms : Unsigned_64 := 0;
   end record;
   function Key (Item : Text_Commands.Key; Control, Shift, Alt : Boolean := False) return Event is
     ((Kind => Key_Event, Pressed => (Item => Item, Control => Control, Shift => Shift, Alt => Alt), others => <>));
   function Typed (Text : String) return Event
     with Pre => Text'Length <= MAXIMUM_EVENT_TEXT;

   MAXIMUM_TABS : constant := 32;
   subtype Tab_Count is Natural range 0 .. MAXIMUM_TABS;
   subtype Tab_Index is Tab_Count range 1 .. MAXIMUM_TABS;

   type Shell is limited private;
   type Shell_Access is access Shell;

   --  Host: file access and the clipboard. Edit_Bytes: room for typing and
   --  pasting per document, beyond its file (explicit capacity).
   procedure Initialize
     (S : in out Shell; Host : not null access Host_Type; Dark : Boolean; Edit_Bytes : Byte_Count);
   procedure Set_Dark (S : in out Shell; Dark : Boolean);

   --  A new tab loading Name, or an empty one.
   procedure Open (S : in out Shell; Name : String);
   procedure New_Document (S : in out Shell; Name : String := "untitled.txt");

   procedure Set_Clock (S : in out Shell; Now_Us : Unsigned_64);
   procedure Handle (S : in out Shell; Item : Event);

   --  The one entry for every command (keys, palette, CCL). Ok False:
   --  the command did not apply (Status_Message says why).
   procedure Execute
     (S : in out Shell; Item : Text_Commands.Command; Number : Integer := 0; Text : String := ""; Ok : out Boolean);
   --  A typed CCL call's text form: "(text.goto-line 120)",
   --  "text.find ""needle""", "save". What the palette accepts.
   procedure Execute_Call (S : in out Shell; Call : String; Ok : out Boolean);

   --  Between frames: load and save chunks, the highlighting pass, the
   --  search jobs, a paste arriving. Busy: call again without waiting.
   procedure Pump (S : in out Shell; Budget : Work_Budget; Busy, Changed : out Boolean);
   --  The window's area (at start and after every resize).
   procedure Set_Bounds (S : in out Shell; Bounds : CuBit.UI.Rect);
   procedure Take_Damage (S : in out Shell; Area : out CuBit.UI.Rect);
   procedure Render (S : in out Shell; C : CuBit.UI.Canvas; Bounds : CuBit.UI.Rect);
   procedure Note_Frame (S : in out Shell; Render_Us, Pump_Us : Unsigned_64);
   function Quit_Requested (S : Shell) return Boolean;
   --  When Pump is next needed without input (a message expires).
   function Next_Deadline_Us (S : Shell) return Unsigned_64;

   --  What the window shows, for tests and the overlay.
   function Tabs (S : Shell) return Tab_Count;
   function Current_Tab (S : Shell) return Tab_Count;
   function Document (S : Shell; Tab : Tab_Index) return Text_Documents.Document_Access;
   function Active_Document (S : Shell) return Text_Documents.Document_Access;
   function Active_Editor (S : Shell) return Text_Editors.Editor_Access;
   function Status_Message (S : Shell) return String;
   function Status_Line (S : Shell) return String;
   function Palette_Open (S : Shell) return Boolean;
   function Palette_Query (S : Shell) return String;
   function Palette_Rows (S : Shell) return Natural;
   function Palette_Row (S : Shell; Row : Positive) return String;
   function Find_Open (S : Shell) return Boolean;
   function Split (S : Shell) return Boolean;
   function Modal_On (S : Shell) return Boolean;
   function Loading (S : Shell) return Boolean;
   function Saving (S : Shell) return Boolean;
   --  Rows the editors repainted in the last frame.
   function Rows_Painted (S : Shell) return Natural;
private
   type Shell_State;
   type Shell is limited record
      State : access Shell_State;
   end record;
end Text_Shells;
