--  Hosted (Linux) tests of the CCL Bible bindings (APP-005): bible.* through
--  the CCL console view and session engine, the images read by the hosted
--  CCL_Bible_Source from CCL_BIBLE_DIR. Frames go to build/*.ppm for
--  review. Natively the console reads the cubit-bible asset package.
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
with CCL.Language;
with CCL.Sessions;
with CCL_Console_View; use CCL_Console_View;
with CCL_Bible_Bindings;
with CuBit.UI;

procedure Main is
   use type Ada.Streams.Stream_Element_Offset;
   Failures : Natural := 0;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Label);
      end if;
   end Check;

   Clock : Unsigned_64 := 1_000;
   function Now return Unsigned_64 is
   begin
      Clock := Clock + 3;
      return Clock;
   end Now;

   Catalog : CCL.Catalog.Interface_Catalog;
   Grants : CCL.Catalog.Granted_Bindings;
   type No_Context is null record;
   Host : No_Context;
   procedure Invoke
     (Context : in out No_Context; Binding : Interfaces.Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (Context);
   begin
      CCL_Bible_Bindings.Invoke (Binding, Argument, Reply);
   end Invoke;
   procedure Submit_Bible is new CCL.Sessions.Submit_With_Values (No_Context, Invoke);
   procedure Submit_Granted
     (Item : in out CCL.Sessions.Session; Source : String;
      Fuel : CCL.Sessions.Fuel_Budget; Outcome : out CCL.Language.Interpretation_Result) is
   begin
      Submit_Bible (Item, Source, Fuel, Grants, Host, Outcome);
   end Submit_Granted;
   procedure Bible_Handle is new Handle (Submit_Granted, Now);

   WIDTH  : constant := 1200;
   HEIGHT : constant := 760;
   type Pixels is array (0 .. WIDTH * HEIGHT - 1) of Unsigned_32;
   type Pixels_Access is access Pixels;
   Image : constant Pixels_Access := new Pixels'(others => 0);
   Canvas : constant CuBit.UI.Canvas :=
     (addr => Image.all'Address, width => WIDTH, height => HEIGHT, pitch => WIDTH * 4,
      clipEnabled => False, clip => (others => 0), others => <>);
   Bounds : constant CuBit.UI.Rect := (0, 0, WIDTH, HEIGHT);
   State : View_State;

   procedure Run_Entry (Source : String) is
      Submitted, Redraw : Boolean;
   begin
      for C of Source loop
         Bible_Handle (State, (Kind => Text_Input, Character_Value => C, others => <>), Submitted, Redraw);
      end loop;
      Bible_Handle (State, (Kind => Run, others => <>), Submitted, Redraw);
      Draw (State, Canvas, Bounds);
   end Run_Entry;

   function Starts (Text, Prefix : String) return Boolean is
     (Text'Length >= Prefix'Length and then Text (Text'First .. Text'First + Prefix'Length - 1) = Prefix);
   function Contains (Text, Part : String) return Boolean is
     (for some I in Text'First .. Text'Last - Part'Length + 1 => Text (I .. I + Part'Length - 1) = Part);

   procedure Save (Name : String) is
      use Ada.Streams.Stream_IO;
      File : File_Type;
      Header : constant String := "P6" & ASCII.LF & "1200 760" & ASCII.LF & "255" & ASCII.LF;
      Row : Ada.Streams.Stream_Element_Array (1 .. WIDTH * 3);
   begin
      Create (File, Out_File, "build/" & Name & ".ppm");
      for C of Header loop
         Ada.Streams.Stream_Element_Array'Write
           (Stream (File), [1 => Ada.Streams.Stream_Element (Character'Pos (C))]);
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

   procedure Expect (Source, Prefix : String; Label : String) is
   begin
      Run_Entry (Source);
      Check (Starts (Latest_Result (State), Prefix), Label & ": " & Latest_Result (State));
   end Expect;

   Installed : Boolean;
begin
   CCL.Catalog.Initialize (Catalog);
   CCL.Catalog.Initialize (Grants);
   CCL_Bible_Bindings.Install (Catalog, Grants, Installed);
   Check (Installed, "the bible interface installs");
   Initialize (State, Catalog);

   --  The user's examples.
   Expect ("(bible.parse ""John 3:16"")",
           "Parse_Result: (Parse_Result.Parsed (Passage (Reference ""John"" 3 16) (Reference ""John"" 3 16)))",
           "parse John 3:16");
   Expect ("(bible.parse ""Jn 3:16-18"")",
           "Parse_Result: (Parse_Result.Parsed (Passage (Reference ""John"" 3 16) (Reference ""John"" 3 18)))",
           "parse Jn 3:16-18");
   Expect ("(bible.parse ""1 Cor 13"")",
           "Parse_Result: (Parse_Result.Parsed (Passage (Reference ""1 Corinthians"" 13 1) " &
           "(Reference ""1 Corinthians"" 13 13)))", "parse 1 Cor 13");
   Expect ("(bible.parse ""Tobit 1:1"")", "Parse_Result: (Parse_Result.Parsed (Passage (Reference ""Tobit"" 1 1)",
           "parse Tobit 1:1");
   --  Typed refusals name what is wrong.
   Expect ("(bible.parse ""Jhon 3:16"")",
           "Parse_Result: (Parse_Result.Refused (Refusal Parse_Error.Unknown_Book ""No book is called \""Jhon\"""" ""Jhon"" 0 0))",
           "parse Jhon");
   Expect ("(bible.parse ""John 22"")",
           "Parse_Result: (Parse_Result.Refused (Refusal Parse_Error.No_Such_Chapter ""John has 21 chapters"" ""John"" 22 21))",
           "parse John 22");
   Save ("bible-parse");
   --  A verse by its typed reference, in the KJV and in another translation.
   Expect ("(bible.verse (Reference ""John"" 3 16))",
           "Verse_Result: (Verse_Result.Found (Verse (Reference ""John"" 3 16) ""For God so loved the world",
           "verse John 3:16");
   Expect ("(bible.verse-in (Verse_Query (Reference ""Jn"" 11 35) Translation.WEB))",
           "Verse_Result: (Verse_Result.Found (Verse (Reference ""John"" 11 35) ""Jesus wept.""))", "verse-in WEB");
   --  The Vulgate, in its own numbering and in ASCII ("aeternam"), by a Latin name.
   Expect ("(bible.verse-in (Verse_Query (Reference ""Ioannes"" 3 16) Translation.VUL))",
           "Verse_Result: (Verse_Result.Found (Verse (Reference ""John"" 3 16) ""Sic enim Deus dilexit mundum",
           "verse-in VUL");
   Check (Contains (Latest_Result (State), "habeat vitam aeternam."), "VUL John 3:16 in ASCII");
   Expect ("(bible.verse-in (Verse_Query (Reference ""Psalmi"" 22 1) Translation.VUL))",
           "Verse_Result: (Verse_Result.Found (Verse (Reference ""Psalms"" 22 1) ""Psalmus David. [Dominus regit me",
           "VUL Psalm 22 is the shepherd psalm");
   Expect ("(bible.verse (Reference ""John"" 3 40))",
           "Verse_Result: (Verse_Result.Refused (Refusal Parse_Error.No_Such_Verse", "verse John 3:40");
   Check (Contains (Latest_Result (State), " 40 36))"), "John 3:40: value 40, limit 36");
   Expect ("(bible.verse-in (Verse_Query (Reference ""Tobit"" 1 1) Translation.BSB))",
           "Verse_Result: (Verse_Result.Refused (Refusal Parse_Error.Not_In_Translation", "Tobit not in the BSB");
   Save ("bible-verse");
   --  Passages and the text form.
   Expect ("(bible.read ""Ps 23"")",
           "Passage_Result: (Passage_Result.Found (Passage_Text (Passage (Reference ""Psalms"" 23 1) " &
           "(Reference ""Psalms"" 23 6)) Translation.KJV [(Verse (Reference ""Psalms"" 23 1) ""The LORD is my shepherd",
           "read Ps 23");
   Check (Contains (Latest_Result (State), "] 6))"), "Psalm 23: all 6 verses");
   Save ("bible-psalm-23");
   Expect ("(bible.passage (Passage (Reference ""Genesis"" 1 1) (Reference ""Genesis"" 1 3)))",
           "Passage_Result: (Passage_Result.Found (Passage_Text", "passage Genesis 1:1-3");
   Check (Contains (Latest_Result (State), "Let there be light"), "Genesis 1:3 is there");
   Expect ("(bible.passage-in (Passage_Query (Passage (Reference ""Psalm"" 119 1) (Reference ""Psalm"" 119 176)) " &
           "Translation.BSB))", "Passage_Result: (Passage_Result.Found (Passage_Text", "Psalm 119 in the BSB");
   Check (Contains (Latest_Result (State), "] 176))"), "Psalm 119 says it has 176 verses (shows what fits)");
   Expect ("(bible.read ""John 3:18-16"")", "Passage_Result: (Passage_Result.Refused (Refusal Parse_Error.Reversed",
           "a range backwards");
   --  Search.
   Expect ("(bible.search ""Jesus wept"")", "Search_Result: (Search_Result ""Jesus wept"" Translation.KJV [",
           "search Jesus wept");
   Check (Contains (Latest_Result (State), "(Verse (Reference ""John"" 11 35) ""Jesus wept."")] 3)"),
          "Jesus wept: John 11:35, the last of three");
   Expect ("(bible.search-in (Search_Query ""faith hope love"" Translation.WEB))",
           "Search_Result: (Search_Result ""faith hope love"" Translation.WEB [", "search-in WEB");
   Check (Contains (Latest_Result (State), "1 Corinthians"" 13 13"), "faith hope love: 1 Corinthians 13:13");
   Save ("bible-search");
   --  Scripts take results apart.
   Expect ("(match (bible.parse ""Jude 5"") ((Parse_Result.Parsed p) (field (field p from) verse)) " &
           "((Parse_Result.Refused r) 0))", "Integer: 5", "Jude 5 is verse 5");

   Ada.Text_IO.Put_Line ("ccl bible tests:" & Natural'Image (Checks) & " checks," & Natural'Image (Failures)
                         & " failures");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
