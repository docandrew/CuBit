--  The Dictionary and Thesaurus apps' view, hosted (docs/lexicon.md): the
--  image answered in process by Lexicon_Answers, keys and clicks in, state
--  and frames out. Clicks go through the toolkit's retained dispatch first,
--  as CuBit.UI.App.Run does on CuBit. Linux only: natively the view asks
--  lexicon.svc through Lexicon_Client.
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.UI;
with CuBit.UI.State;
with Dictionary_Views;
with Lexicon_Answers;
with Lexicon_Images;
with Lexicon_Support;
with Lexicon_Wire;

procedure Dictionary_App_Tests is
   use type Ada.Streams.Stream_Element_Offset;
   use type Lexicon_Images.Open_Status;
   Path : constant String :=
     (if Ada.Environment_Variables.Exists ("LEXICON_IMAGE")
      then Ada.Environment_Variables.Value ("LEXICON_IMAGE") else "../lexicon/build/english.lexicon");
   Image_Data : constant Lexicon_Support.Image_Access := Lexicon_Support.Load (Path);
   Layout : Lexicon_Images.Layout;
   Opened : Lexicon_Images.Open_Status;
   Asks : Natural := 0;

   PAGE : constant := 4_084;
   procedure Ask
     (Op : Lexicon_Answers.Operation; Text : String; Resume : Unsigned_32;
      Suggestions : Lexicon_Answers.Suggestion_Count; Into : out Lexicon_Wire.Answer_Bytes;
      Used : out Natural; Available : out Boolean) is
   begin
      Asks := Asks + 1;
      Available := Opened = Lexicon_Images.Opened;
      if Available then
         Lexicon_Answers.Answer (Image_Data.all, Layout, Op, Text, Resume, Suggestions, Into, Used);
      else
         Into := [others => 0];
         Used := 0;
      end if;
   end Ask;
   package Views is new Dictionary_Views (Ask, PAGE);
   use Views;
   use type Controls.Pointer_Action;
   use type Controls.Control_ID;

   WIDTH : constant := 980;
   HEIGHT : constant := 680;
   type Pixels is array (0 .. WIDTH * HEIGHT - 1) of Unsigned_32;
   type Pixels_Access is access Pixels;
   Image : constant Pixels_Access := new Pixels'(others => 0);
   Canvas : constant CuBit.UI.Canvas :=
     (addr => Image.all'Address, width => WIDTH, height => HEIGHT, pitch => WIDTH * 4,
      clipEnabled => False, clip => (others => 0), others => <>);
   Bounds : constant CuBit.UI.Rect := (0, 0, WIDTH, HEIGHT);
   type View_Access is access View_State;
   State : constant View_Access := new View_State;
   type UI_Access is access CuBit.UI.State.UI_State;
   UI : constant UI_Access := new CuBit.UI.State.UI_State;
   type Map_Access is access Controls.Control_Map;
   Map : constant Map_Access := new Controls.Control_Map;
   Checks, Failures : Natural := 0;
   Redraw : Boolean;

   procedure Check (Condition : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Label);
      end if;
   end Check;

   procedure Frame is
   begin
      Render (State.all, Canvas, Bounds, UI.all, Map.all);
   end Frame;
   procedure Key (Name : Key_Name; Alt : Boolean := False) is
   begin
      Handle (State.all, (Kind => Key_Event, Key => Name, Alt => Alt, others => <>), Map.all, Redraw);
      Frame;
   end Key;
   procedure Type_Text (Text : String) is
   begin
      for C of Text loop
         Handle (State.all, (Kind => Text_Event, Character_Value => C, others => <>), Map.all, Redraw);
         Frame;
      end loop;
   end Type_Text;
   procedure Pointer_At (X, Y : Natural; Action : Controls.Pointer_Action) is
      Target : constant Controls.Control_ID := Controls.Hit (Map.all, X, Y);
      Visual, Dispatched : Boolean;
   begin
      CuBit.UI.State.Set_Pointer (UI.all, X, Y, down => Action = Controls.Pointer_Press);
      Controls.Dispatch_Pointer (Map.all, Target, Action, X, Y, Visual, Dispatched);
      Handle (State.all, (Kind => Pointer_Event, Action => Action, X => X, Y => Y, others => <>), Map.all, Redraw);
   end Pointer_At;
   procedure Click_At (X, Y : Natural) is
   begin
      Frame;
      Pointer_At (X, Y, Controls.Pointer_Move);
      Pointer_At (X, Y, Controls.Pointer_Press);
      Frame;
      Pointer_At (X, Y, Controls.Pointer_Release);
      Frame;
   end Click_At;
   procedure Click (Target : Controls.Control_ID) is
      Box : CuBit.UI.Rect;
   begin
      Frame;
      Box := Controls.Bounds (Map.all, Target);
      Check (Box.w > 0, "control" & Target'Image & " is on screen");
      Click_At (Box.x + Box.w / 2, Box.y + Box.h / 2);
   end Click;
   procedure Click_Link (Index : Positive) is
      Box : constant CuBit.UI.Rect := Link_Area (State.all, Index);
   begin
      Check (Box.w > 0, "link" & Index'Image & " is on screen");
      Click_At (Box.x + 2, Box.y + Box.h / 2);
   end Click_Link;
   function Link_Index (Text : String) return Natural is
   begin
      for I in 1 .. Links (State.all) loop
         if Link_Text (State.all, I) = Text and then Link_Area (State.all, I).w > 0 then
            return I;
         end if;
      end loop;
      return 0;
   end Link_Index;

   procedure Save (Name : String) is
      use Ada.Streams.Stream_IO;
      File : File_Type;
      Header : constant String := "P6" & ASCII.LF & "980 680" & ASCII.LF & "255" & ASCII.LF;
      Row : Ada.Streams.Stream_Element_Array (1 .. WIDTH * 3);
   begin
      Frame;
      Create (File, Out_File, "build/" & Name & ".ppm");
      for C of Header loop
         Ada.Streams.Stream_Element_Array'Write (Stream (File), [1 => Ada.Streams.Stream_Element (Character'Pos (C))]);
      end loop;
      for Y in 0 .. HEIGHT - 1 loop
         for X in 0 .. WIDTH - 1 loop
            declare
               P : constant Unsigned_32 := Image (Y * WIDTH + X);
               Base : constant Ada.Streams.Stream_Element_Offset := Ada.Streams.Stream_Element_Offset (X * 3);
            begin
               Row (Base + 1) := Ada.Streams.Stream_Element (Shift_Right (P, 16) and 16#FF#);
               Row (Base + 2) := Ada.Streams.Stream_Element (Shift_Right (P, 8) and 16#FF#);
               Row (Base + 3) := Ada.Streams.Stream_Element (P and 16#FF#);
            end;
         end loop;
         Write (File, Row);
      end loop;
      Close (File);
   end Save;

   procedure Clear_Field is
   begin
      Key (Escape);
   end Clear_Field;
begin
   Lexicon_Images.Open (Image_Data.all, Layout, Opened);
   Check (Opened = Lexicon_Images.Opened, "image opens");
   Initialize (State.all, Dictionary_Mode);
   Handle (State.all, (Kind => Resize, X => WIDTH, Y => HEIGHT, others => <>), Map.all, Redraw);
   Frame;
   Check (Status (State.all) = Idle and then Links (State.all) = 4, "an empty page offers sample words");
   Save ("dictionary-welcome");

   --  Typing looks up as you type; Enter records it for Back.
   Type_Text ("cataract");
   Check (Status (State.all) = Found and then Headword (State.all) = "cataract" and then Senses (State.all) = 2,
          "cataract: an entry with 2 senses");
   Key (Enter);
   Save ("dictionary-cataract");

   Clear_Field;
   Type_Text ("wharf");
   Key (Enter);
   Check (Senses (State.all) = 6, "wharf: 6 senses, got" & Senses (State.all)'Image);
   Check (Link_Index ("pier") > 0, "wharf: pier is a link");
   Save ("dictionary-wharf");

   --  A synonym is a link: it looks the word up, and Back returns.
   Click_Link (Link_Index ("pier"));
   Check (Query (State.all) = "pier" and then Status (State.all) = Found, "clicking pier looks it up");
   Check (Can_Go_Back (State.all), "Back is possible after a link");
   Click (BACK_ID);
   Check (Query (State.all) = "wharf", "Back returns to wharf: " & Query (State.all));
   Click (FORWARD_ID);
   Check (Query (State.all) = "pier", "Forward returns to pier: " & Query (State.all));
   Key (Left, Alt => True);
   Check (Query (State.all) = "wharf", "Alt+Left is Back");

   --  The thesaurus mode: the same word, its synonym groups.
   Click (THESAURUS_ID);
   Check (Current_Mode (State.all) = Thesaurus_Mode and then Status (State.all) = Found, "thesaurus mode");
   Check (Link_Index ("platform") > 0, "thesaurus wharf: broader platform");
   Save ("thesaurus-wharf");
   Clear_Field;
   Type_Text ("accipitrine");
   Save ("thesaurus-accipitrine");
   Check (Status (State.all) = Found, "thesaurus accipitrine");
   Key (Tab);
   Check (Current_Mode (State.all) = Dictionary_Mode, "Tab switches back to the dictionary");

   --  An unknown word: near spellings, each a link.
   Clear_Field;
   Type_Text ("recieve");
   Check (Status (State.all) = Not_Found and then Link_Index ("receive") > 0, "recieve: did you mean receive");
   Save ("dictionary-not-found");
   Click_Link (Link_Index ("receive"));
   Check (Query (State.all) = "receive" and then Status (State.all) = Found, "the suggestion looks receive up");

   --  A long entry scrolls.
   Clear_Field;
   Type_Text ("break");
   Check (Senses (State.all) = 75, "break: 75 senses");
   Handle (State.all, (Kind => Wheel, Steps => -10, others => <>), Map.all, Redraw);
   Save ("dictionary-break-scrolled");

   --  Inflections: the base forms' entries.
   Clear_Field;
   Type_Text ("geese");
   Check (Headword (State.all) = "goose", "geese shows goose");

   Ada.Text_IO.Put_Line ("dictionary app tests:" & Checks'Image & " checks," & Failures'Image & " failures," &
                         Asks'Image & " pages asked");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Dictionary_App_Tests;
