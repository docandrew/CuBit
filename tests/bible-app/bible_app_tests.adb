--  The Bible app's view, hosted (APP-005): translations read from the image
--  files, the reading face registered from a font file, keys and clicks
--  in, state and frames out. Clicks go through the toolkit's retained
--  dispatch first, as CuBit.UI.App.Run does on CuBit. Linux only: natively
--  the app reads the cubit-bible asset package through the filesystem.
--
--  BIBLE_DIR: the images (tests/bible/build/bible); BIBLE_FACES: the
--  reading faces to offer, "Name=path" separated by "|" (run.sh passes the
--  pinned Literata, Source Serif 4 and EB Garamond); BIBLE_SHOT_PREFIX:
--  names the screenshots.
with Ada.Calendar; use Ada.Calendar;
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Ada.Strings.Unbounded;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with CuBit.Reading_Faces;
with CuBit.UI;
with CuBit.UI.Combo_Boxes;
with CuBit.UI.State;
with Bible_Canon; use Bible_Canon;
with Bible_References; use Bible_References;
with Bible_Support;
with Bible_Translations; use Bible_Translations;
with Bible_Views;

procedure Bible_App_Tests is
   use type Ada.Streams.Stream_Element_Offset;
   function Setting (Name, Default : String) return String is
     (if Ada.Environment_Variables.Exists (Name) then Ada.Environment_Variables.Value (Name) else Default);
   Directory : constant String := Setting ("BIBLE_DIR", "../bible/build/bible");
   Faces : constant String := Setting ("BIBLE_FACES", "");
   Prefix : constant String := Setting ("BIBLE_SHOT_PREFIX", "bible");

   Loads : Natural := 0;
   Missing_WEB : Boolean := False;

   procedure Load (Which : Bible_Translations.Translation; Data : out Image_Access; Status : out Load_Status) is
   begin
      Loads := Loads + 1;
      if Missing_WEB and then Which = WEB then
         Data := null;
         Status := Open_Failed;
         return;
      end if;
      Data := Bible_Support.Load_Bytes (Directory & "/" & File_Name (Which));
      Status := Loaded;
   end Load;
   --  The remembered choices, in memory (natively: Config settings).
   Saved_Face, Saved_Translation, Saved_Red : Ada.Strings.Unbounded.Unbounded_String;
   Saves : Natural := 0;
   function Saved (Item : Reader_Preference) return String is
     (Ada.Strings.Unbounded.To_String
        (case Item is
            when Reading_Face_Preference => Saved_Face, when Translation_Preference => Saved_Translation,
            when Red_Letters_Preference => Saved_Red));
   procedure Save (Item : Reader_Preference; Value : String) is
   begin
      Saves := Saves + 1;
      case Item is
         when Reading_Face_Preference => Saved_Face := Ada.Strings.Unbounded.To_Unbounded_String (Value);
         when Translation_Preference => Saved_Translation := Ada.Strings.Unbounded.To_Unbounded_String (Value);
         when Red_Letters_Preference => Saved_Red := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      end case;
   end Save;
   package Views is new Bible_Views (Load, Saved, Save);
   use Views;

   WIDTH : constant := 1_180;
   HEIGHT : constant := 780;
   type Pixels is array (0 .. WIDTH * HEIGHT - 1) of Unsigned_32;
   type Pixels_Access is access Pixels;
   Image : constant Pixels_Access := new Pixels'(others => 0);
   Canvas : constant CuBit.UI.Canvas :=
     (addr => Image.all'Address, width => WIDTH, height => HEIGHT, pitch => WIDTH * 4,
      clipEnabled => False, clip => (others => 0), others => <>);
   Bounds : constant CuBit.UI.Rect := (0, 0, WIDTH, HEIGHT);
   type View_Access is access View_State;
   State : View_Access := new View_State;
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
   procedure Key (Name : Key_Name; Alt, Control, Shift : Boolean := False) is
   begin
      Handle (State.all, (Kind => Key_Event, Key => Name, Alt => Alt, Control => Control, Shift => Shift,
                          others => <>), Map.all, Redraw);
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
      use type Controls.Pointer_Action;
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

   --  A translation from the drop-down list: a click opens it, a click on
   --  the translation's row chooses it and closes it.
   procedure Choose (Which : Bible_Translations.Translation) is
   begin
      Click (TRANSLATION_BASE);
      Check (Translations_Open (State.all), "a click opens the translation list");
      Click (CuBit.UI.Combo_Boxes.Choice_ID (TRANSLATION_BASE, Bible_Translations.Translation'Pos (Which) + 1));
      Check (not Translations_Open (State.all) and then Current_Translation (State.all) = Which,
             "the list chooses " & Bible_Translations.Abbreviation (Which));
   end Choose;

   procedure Save (Name : String) is
      use Ada.Streams.Stream_IO;
      File : File_Type;
      Header : constant String := "P6" & ASCII.LF & "1180 780" & ASCII.LF & "255" & ASCII.LF;
      Row : Ada.Streams.Stream_Element_Array (1 .. WIDTH * 3);
   begin
      Frame;
      Create (File, Out_File, "build/" & Prefix & "-" & Name & ".ppm");
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

   --  The reading faces, kept for the process (as embedded blobs are).
   procedure Offer_Faces is
      First : Positive := Faces'First;
      use type CuBit.Reading_Faces.Offer_Status;
   begin
      if Faces'Length = 0 then
         Ada.Text_IO.Put_Line ("BIBLE_FACES is not set: no reading face is offered");
         return;
      end if;
      for I in Faces'First .. Faces'Last + 1 loop
         if I > Faces'Last or else Faces (I) = '|' then
            declare
               Item : constant String := Faces (First .. I - 1);
               Equals : Natural := 0;
               Index : CuBit.Reading_Faces.Face_Index;
               Status : CuBit.Reading_Faces.Offer_Status;
               Font : Bible_Support.Image_Access;
            begin
               for J in Item'Range loop
                  if Item (J) = '=' and then Equals = 0 then Equals := J; end if;
               end loop;
               Font := Bible_Support.Load_Bytes (Item (Equals + 1 .. Item'Last));
               CuBit.Reading_Faces.Offer (Item (Item'First .. Equals - 1), Font.all'Address, Font'Length, Index,
                                          Status);
               Check (Status = CuBit.Reading_Faces.Offered, "offer " & Item (Item'First .. Equals - 1) & ": " &
                      Status'Image);
            end;
            First := I + 1;
         end if;
      end loop;
   end Offer_Faces;

   function Milliseconds (Since : Time) return String is (Duration'Image ((Clock - Since) * 1_000) & " ms");

   Started : Time;
   --  Pages: the light themes' fields (white, warm paper) and dark ones.
   type Page_List is array (Positive range <>) of Unsigned_32;
   Page_Colors : constant Page_List := [16#FFFFFF#, 16#F4F1EA#, 16#232C33#, 16#1E1E1E#, 16#2D3343#];
begin
   Offer_Faces;
   Check (CuBit.Reading_Faces.Count = 3 and then Face_Name = "Literata", "three faces, Literata first");
   Initialize (State.all);
   Handle (State.all, (Kind => Resize, X => WIDTH, Y => HEIGHT, others => <>), Map.all, Redraw);
   Frame;
   Check (Mode (State.all) = Reading and then Heading (State.all) = "John 1", "opens at John 1: " & Heading (State.all));
   Save ("john-1");

   --  A reference typed and entered opens its chapter.
   Type_Text ("John 3");
   Check (Mode (State.all) = Results and then Status_Text (State.all) = "John 3", "John 3 is offered");
   Started := Clock;
   Key (Enter);
   Ada.Text_IO.Put_Line ("  open John 3 (layout and first frame):" & Milliseconds (Started));
   Check (Mode (State.all) = Reading and then Heading (State.all) = "John 3", "Enter opens John 3");
   Check (Shown (State.all) = ((John, 3, 1), (John, 3, 36)), "John 3 is the whole chapter");
   Save ("john-3");

   --  The words of Christ in red (KJV \wj), on by default; F9 turns it off
   --  and on, remembered; the BSB marks none.
   Check (Red_Letters (State.all) and then Red_Runs (State.all) > 0, "John 3 KJV: red letters");
   Check (Verse_All_Red (State.all, 16), "John 3:16 KJV is all red");
   Check (not Verse_Any_Red (State.all, 1), "John 3:1 KJV is not red");
   Check (not Verse_All_Red (State.all, 10) and then Verse_Any_Red (State.all, 10),
          "John 3:10 KJV: red from Jesus' words on");
   Key (Next_Face_Key);
   Key (Red_Letters_Key);
   Check (not Red_Letters (State.all) and then Red_Runs (State.all) = 0 and then Saved (Red_Letters_Preference) = "off",
          "F9: red letters off, remembered");
   Save ("john-3-red-off");
   Key (Red_Letters_Key);
   Check (Red_Letters (State.all) and then Verse_All_Red (State.all, 16), "F9 again: red letters on");
   Key (Next_Face_Key);
   Key (Next_Face_Key);
   Choose (BSB);
   Check (Red_Runs (State.all) = 0, "the BSB marks no words of Christ");
   Choose (WEB);
   Check (Verse_All_Red (State.all, 16), "John 3:16 WEB is all red");
   Choose (VUL);
   Check (Heading (State.all) = "John 3" and then Red_Runs (State.all) = 0,
          "John 3 in the Vulgate: its source marks no words of Christ, none red");
   Save ("john-3-vulgate");
   Choose (KJV);
   --  The red reads on light and dark pages (WCAG contrast 4.5:1 or more).
   declare
      function L (C : Unsigned_32) return Float is
         function Ch (Shift : Natural) return Float is
            V : constant Float := Float (Shift_Right (C, Shift) and 16#FF#) / 255.0;
         begin
            return (if V <= 0.03928 then V / 12.92 else ((V + 0.055) / 1.055) ** 2.4);
         end Ch;
      begin
         return 0.2126 * Ch (16) + 0.7152 * Ch (8) + 0.0722 * Ch (0);
      end L;
      function Contrast (A, B : Unsigned_32) return Float is
        ((Float'Max (L (A), L (B)) + 0.05) / (Float'Min (L (A), L (B)) + 0.05));
   begin
      for Page of Page_Colors loop
         Check (Contrast (Page, Red_Letter_Color (Page)) >= 4.5,
                "red letters on" & Page'Image & ":" & Contrast (Page, Red_Letter_Color (Page))'Image);
      end loop;
   end;
   Save ("john-3-red");

   --  The reader's face, live: each face the toolbar's button offers, the
   --  choice saved for the next start.
   Click (FACE_ID);
   Check (Face_Name = "Source Serif 4" and then Saved (Reading_Face_Preference) = "Source Serif 4",
          "the face button: Source Serif 4, remembered");
   Save ("john-3-source-serif-4");
   Click (FACE_ID);
   Check (Face_Name = "EB Garamond", "the face button: EB Garamond");
   Save ("john-3-eb-garamond");
   Click (FACE_ID);
   Check (Face_Name = "Literata", "the face button wraps to Literata");
   Save ("john-3-literata");
   Clear_Field;
   Type_Text ("Psalm 23");
   Key (Enter);
   declare
      procedure Shoot (Name, Shot : String) is
      begin
         Set_Face (State.all, Name);
         Save ("psalm-23-" & Shot);
      end Shoot;
   begin
      Shoot ("Literata", "literata");
      Shoot ("Source Serif 4", "source-serif-4");
      Shoot ("EB Garamond", "eb-garamond");
   end;
   Set_Face (State.all, "Literata");

   --  A verse: highlighted and scrolled to.
   Type_Text ("Jn 3:16");
   Check (Mode (State.all) = Results and then Status_Text (State.all) = "John 3:16", "Jn 3:16 is offered");
   Save ("john-3-16-offered");
   Key (Enter);
   Check (Shown (State.all) = ((John, 3, 16), (John, 3, 16)), "Enter opens John 3:16");
   Check (Verse_Area (State.all, 16).w > 0, "verse 16 is on screen");
   Save ("john-3-16");

   --  A wrong reference says what is wrong.
   Type_Text ("John 22");
   Check (Mode (State.all) = Results and then Status_Text (State.all) = "Not a reference", "John 22 is refused");
   Save ("john-22");
   Clear_Field;

   --  Words search the index as typed; a result opens its verse.
   Started := Clock;
   Type_Text ("faith");
   Ada.Text_IO.Put_Line ("  search ""faith"" as typed (5 keystrokes, each searched and laid out):" &
                         Milliseconds (Started));
   Check (Mode (State.all) = Results and then Result_Count (State.all) > 100, "faith: verses found" &
          Result_Count (State.all)'Image);
   Save ("search-faith");
   declare
      First : constant Reference := Result_Reference (State.all, 1);
      Box : constant CuBit.UI.Rect := Result_Area (State.all, 1);
   begin
      Check (Box.w > 0, "the first result is on screen");
      Click_At (Box.x + 2, Box.y + Box.h / 2);
      Check (Mode (State.all) = Reading and then Shown (State.all) = (First, First),
             "clicking a result opens its verse: " & Bible_References.Image (Shown (State.all).From));
   end;
   Clear_Field;
   Type_Text ("in the beginning was the word");
   Check (Result_Count (State.all) = 1 and then Result_Reference (State.all, 1) = (John, 1, 1),
          "in the beginning was the word: John 1:1");
   Clear_Field;

   --  Back and Forward through the passages read.
   Click (BACK_ID);
   Check (Shown (State.all) = ((John, 3, 16), (John, 3, 16)), "Back returns to John 3:16");
   Click (FORWARD_ID);
   Key (Left, Alt => True);
   Check (Shown (State.all) = ((John, 3, 16), (John, 3, 16)), "Alt+Left is Back");

   --  Next and previous chapter, by key and button, across books.
   Enter_Text (State.all, "Malachi 4");
   Key (Page_Down, Control => True);
   Check (Heading (State.all) = "Tobit 1", "after Malachi comes the Apocrypha in the KJV: " & Heading (State.all));
   Save ("tobit-1");
   Click (PREVIOUS_ID);
   Check (Heading (State.all) = "Malachi 4", "Previous returns to Malachi 4");
   Enter_Text (State.all, "Revelation 22");
   Click (NEXT_ID);
   Check (Heading (State.all) = "Revelation 22", "nothing after Revelation 22");

   --  The translations: the drop-down list and Tab; the place is kept,
   --  carried across numberings.
   Check (Translation_Caption (KJV) = "King James Version (English)"
          and then Translation_Caption (VUL) = "Biblia Sacra Vulgata Clementina (Latin)", "the list's captions");
   Enter_Text (State.all, "Psalm 23");
   Click (TRANSLATION_BASE);
   Check (Translations_Open (State.all), "the translation list opens");
   Save ("translations-open");
   Key (Escape);
   Check (not Translations_Open (State.all) and then Current_Translation (State.all) = KJV,
          "Escape closes the list, nothing chosen");
   Choose (BSB);
   Check (Heading (State.all) = "Psalm 23", "BSB Psalm 23");
   Save ("psalm-23-bsb");
   Key (Tab);
   Check (Current_Translation (State.all) = WEB, "Tab: WEB");
   Save ("psalm-23-web");
   Key (Tab);
   Check (Current_Translation (State.all) = VUL and then Heading (State.all) = "Psalm 22 (23)"
          and then Shown (State.all) = ((Psalms, 22, 1), (Psalms, 22, 6)),
          "Tab: the Vulgate, the same psalm by its own number: " & Heading (State.all));
   Save ("psalm-22-vulgate");
   Key (Tab);
   Check (Current_Translation (State.all) = KJV and then Heading (State.all) = "Psalm 23", "Tab: back to the KJV");
   Save ("psalm-23-kjv");
   --  F4 opens the list; arrows move, Enter chooses.
   Key (Translations_Key);
   Check (Translations_Open (State.all), "F4 opens the list");
   Key (Down);
   Key (Enter);
   Check (not Translations_Open (State.all) and then Current_Translation (State.all) = BSB, "F4, Down, Enter: BSB");
   Key (Translations_Key);
   Key (Escape);
   Choose (KJV);
   --  A verse carried into the Vulgate's numbering: Psalm 51:1 is its 50:3.
   Enter_Text (State.all, "Psalm 51:1");
   Choose (VUL);
   Check (Shown (State.all) = ((Psalms, 50, 3), (Psalms, 50, 3)) and then Heading (State.all) = "Psalm 50 (51)",
          "Psalm 51:1 is the Vulgate's 50:3");
   Enter_Text (State.all, "Ps 9");
   Check (Heading (State.all) = "Psalm 9 (9-10)", "the Vulgate's Psalm 9 holds Hebrew 9 and 10");
   Choose (KJV);
   Check (Heading (State.all) = "Psalm 9", "back: Psalm 9");
   Enter_Text (State.all, "Bel and the Dragon 1:3");
   Choose (VUL);
   Check (Shown (State.all) = ((Daniel, 14, 3), (Daniel, 14, 3)), "Bel and the Dragon is the Vulgate's Daniel 14");
   Choose (BSB);
   Check (Heading (State.all) = "Daniel 1", "the BSB has no Daniel 14: the book's start");
   Choose (KJV);
   Enter_Text (State.all, "Sirach 1");
   Save ("sirach-1-kjv");
   Choose (BSB);
   Check (Heading (State.all) = "Genesis 1", "BSB has no Sirach: Genesis 1");
   Choose (KJV);

   --  The longest chapter, scrolled.
   Started := Clock;
   Enter_Text (State.all, "Psalm 119");
   Frame;
   Ada.Text_IO.Put_Line ("  open Psalm 119 (176 verses, layout and first frame):" & Milliseconds (Started) &
                         "," & Laid_Out_Lines (State.all)'Image & " lines");
   Started := Clock;
   for K in 1 .. 20 loop
      Handle (State.all, (Kind => Wheel, Steps => -1, X => 700, Y => 400, others => <>), Map.all, Redraw);
      Frame;
   end loop;
   Ada.Text_IO.Put_Line ("  20 scrolled frames of Psalm 119:" & Milliseconds (Started));
   Save ("psalm-119-scrolled");

   --  The sidebar: open to the passage; a row opens its chapter.
   Enter_Text (State.all, "Romans 8");
   Frame;
   Check (Tree_Rows (State.all) > 20, "the sidebar lists the canon");
   declare
      Row_Found : Natural := 0;
   begin
      for R in 1 .. Tree_Rows (State.all) loop
         if Tree_Row_Label (State.all, R) = "Chapter 12" then
            Row_Found := R;
            exit;
         end if;
      end loop;
      Check (Row_Found > 0, "Romans' chapters are listed");
      Save ("romans-8");
   end;

   --  At density 2 (a 4K output at 200%) the reading face comes from the
   --  density masks: the same page, twice the pixels.
   declare
      type Dense_Pixels is array (0 .. 4 * WIDTH * HEIGHT - 1) of Unsigned_32;
      type Dense_Access is access Dense_Pixels;
      Dense : constant Dense_Access := new Dense_Pixels'(others => 16#00FF_00FF#);
      Dense_Canvas : constant CuBit.UI.Canvas :=
        (addr => Dense.all'Address, width => WIDTH, height => HEIGHT, pitch => 2 * WIDTH * 4,
         densityNumerator => 2, densityDenominator => 1, clipEnabled => False, clip => (others => 0),
         others => <>);
      Inked : Natural := 0;
      use Ada.Streams.Stream_IO;
      File : File_Type;
      Header : constant String := "P6" & ASCII.LF & "2360 1560" & ASCII.LF & "255" & ASCII.LF;
      Row : Ada.Streams.Stream_Element_Array (1 .. 2 * WIDTH * 3);
   begin
      Enter_Text (State.all, "John 3");
      Render (State.all, Dense_Canvas, Bounds, UI.all, Map.all);
      --  Dark ink in the reading column (the page is light): text drawn.
      for Y in 400 .. 700 loop
         for X in 900 .. 1_900 loop
            if (Dense (Y * 2 * WIDTH + X) and 16#FF#) < 16#60# then
               Inked := Inked + 1;
            end if;
         end loop;
      end loop;
      Check (Inked > 5_000, "density 2: the chapter is drawn in the reading face:" & Inked'Image & " ink pixels");
      Create (File, Out_File, "build/" & Prefix & "-john-3-density-2.ppm");
      for C of Header loop
         Ada.Streams.Stream_Element_Array'Write (Stream (File), [1 => Ada.Streams.Stream_Element (Character'Pos (C))]);
      end loop;
      for Y in 0 .. 2 * HEIGHT - 1 loop
         for X in 0 .. 2 * WIDTH - 1 loop
            declare
               P : constant Unsigned_32 := Dense (Y * 2 * WIDTH + X);
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
   end;

   --  A new start remembers the reader's face and translation.
   Set_Face (State.all, "EB Garamond");
   Choose (WEB);
   declare
      Again : constant View_Access := new View_State;
   begin
      CuBit.Reading_Faces.Choose (1);
      Initialize (Again.all);
      Check (Face_Name = "EB Garamond" and then Current_Translation (Again.all) = WEB,
             "a new start: EB Garamond and the WEB, remembered");
   end;
   Saved_Face := Ada.Strings.Unbounded.Null_Unbounded_String;
   Saved_Translation := Ada.Strings.Unbounded.Null_Unbounded_String;
   Set_Face (State.all, "Literata");
   Saved_Face := Ada.Strings.Unbounded.Null_Unbounded_String;
   Saved_Translation := Ada.Strings.Unbounded.Null_Unbounded_String;

   --  A translation that is not installed says so.
   Missing_WEB := True;
   State := new View_State;
   Initialize (State.all);
   Handle (State.all, (Kind => Resize, X => WIDTH, Y => HEIGHT, others => <>), Map.all, Redraw);
   Key (Tab);
   Key (Tab);
   Check (Current_Translation (State.all) = WEB and then Mode (State.all) = Unavailable, "WEB missing: unavailable");
   Save ("web-missing");
   Key (Tab);
   Check (Current_Translation (State.all) = VUL and then Mode (State.all) = Reading, "on to the Vulgate");

   Ada.Text_IO.Put_Line ("bible app tests:" & Checks'Image & " checks," & Failures'Image & " failures," &
                         Loads'Image & " translations loaded");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Bible_App_Tests;
