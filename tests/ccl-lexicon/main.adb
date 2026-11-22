--  Hosted (Linux) tests of the CCL lexicon bindings (docs/lexicon.md):
--  (dict ..), (synonym ..), (thesaurus ..), (spell-check ..) and the
--  lexicon.* operations through the CCL console view and session engine,
--  answered from the image file by the hosted CCL_Lexicon_Source. Frames go
--  to build/*.ppm for review. Not CuBit: the native host asks lexicon.svc.
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
with CCL.Language;
with CCL.Sessions;
with CCL_Console_View; use CCL_Console_View;
with CCL_Lexicon_Bindings;
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
      CCL_Lexicon_Bindings.Invoke (Binding, Argument, Reply);
   end Invoke;
   procedure Submit_Lexicon is new CCL.Sessions.Submit_With_Values (No_Context, Invoke);
   procedure Submit_Granted
     (Item : in out CCL.Sessions.Session; Source : String;
      Fuel : CCL.Sessions.Fuel_Budget; Outcome : out CCL.Language.Interpretation_Result) is
   begin
      Submit_Lexicon (Item, Source, Fuel, Grants, Host, Outcome);
   end Submit_Granted;
   procedure Lexicon_Handle is new Handle (Submit_Granted, Now);

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
         Lexicon_Handle (State, (Kind => Text_Input, Character_Value => C, others => <>), Submitted, Redraw);
      end loop;
      Lexicon_Handle (State, (Kind => Run, others => <>), Submitted, Redraw);
      Draw (State, Canvas, Bounds);
   end Run_Entry;

   function Starts (Text, Prefix : String) return Boolean is
     (Text'Length >= Prefix'Length and then Text (Text'First .. Text'First + Prefix'Length - 1) = Prefix);
   function Contains (Text, Part : String) return Boolean is
     (for some I in Text'First .. Text'Last - Part'Length + 1 => Text (I .. I + Part'Length - 1) = Part);

   --  build/NAME.ppm: the current frame.
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
   CCL_Lexicon_Bindings.Install (Catalog, Grants, Installed);
   Check (Installed, "the lexicon interface installs");
   Initialize (State, Catalog);
   --  A bare command names one operation: a second interface may not take
   --  the same command (CCL.Catalog.Mark_Command).
   declare
      Copy : CCL.Catalog.Interface_Catalog := Catalog;
      Other : CCL.Catalog.Interface_Descriptor;
      Op : CCL.Catalog.Operation_Descriptor;
      Error : CCL.Catalog.Catalog_Error;
      use type CCL.Catalog.Catalog_Error;
   begin
      CCL.Catalog.Define_Interface ("other", 1, 0, [1, 2, 3, 4], Other, Error);
      CCL.Catalog.Define_Host_Operation
        ("dict", 1, (Argument => CCL.Host_Values.Text_Value, Argument_Text_Limit => 8,
                     Result => CCL.Host_Values.Boolean_Value, others => <>), Op, Error);
      CCL.Catalog.Mark_Command (Op);
      CCL.Catalog.Add_Operation (Other, Op, Error);
      CCL.Catalog.Publish (Copy, Other, Error);
      Check (Error = CCL.Catalog.Ambiguous_Name, "a second dict command is refused");
   end;

   --  The user's examples: bare commands, typed results.
   Expect ("(dict ""cataract"")", "Definition: (Definition.Found (Word_Entry ""cataract"" [(Word_Sense Part_Of_Speech.Noun ""an eye disease",
           "dict cataract");
   Check (Contains (Latest_Result (State), "a large waterfall"), "dict: every sense");
   Check (Contains (Latest_Result (State), " 1 2))"), "dict: first sense 1 of 2");
   Save ("lexicon-dict");
   Expect ("(synonym ""wharf"")", "Synonyms: (Synonyms.Found (Synonym_List ""wharf"" [""pier"" ""wharfage"" ""dock""",
           "synonym wharf");
   Expect ("(thesaurus ""accipitrine"")",
           "Thesaurus_Result: (Thesaurus_Result.Found (Thesaurus_Entry ""accipitrine"" [(Synonym_Group Part_Of_Speech.Adjective",
           "thesaurus accipitrine");
   Save ("lexicon-thesaurus");
   --  The qualified names are the same operations.
   Expect ("(lexicon.dict ""cataract"")", "Definition: (Definition.Found", "lexicon.dict");
   --  A typed not-found, with near spellings.
   Expect ("(dict ""recieve"")", "Definition: (Definition.Not_Found (Near_Spellings ""recieve"" [""receive""",
           "dict not found");
   --  Scripts take fields out of results.
   Expect ("(match (dict ""cataract"") ((Definition.Found e) (length (field e senses))) ((Definition.Not_Found n) 0))",
           "Integer: 2", "count senses in a script");
   Expect ("(match (synonym ""wharf"") ((Synonyms.Found s) (field s synonyms)) ((Synonyms.Not_Found n) (field n suggestions)))",
           "List<String>: [""pier""", "synonyms as a list");
   --  Spelling: misspellings and their offsets, and a yes/no.
   Expect ("(spell-check ""Teh quick brown fox jumpd over the lazy dog."")",
           "List<Misspelling>: [(Misspelling ""Teh"" 0 [""the""", "spell-check");
   Check (Contains (Latest_Result (State), "(Misspelling ""jumpd"" 20 ["), "spell-check: jumpd at 20");
   Save ("lexicon-spell-check");
   Expect ("(lexicon.known ""receive"")", "Boolean: true", "known receive");
   Expect ("(lexicon.known ""recieve"")", "Boolean: false", "known recieve");
   Expect ("(lexicon.suggest ""thesaurs"")", "Near_Spellings: (Near_Spellings ""thesaurs"" [", "suggest");
   Check (Contains (Latest_Result (State), """thesaurus"""), "suggest: thesaurus");
   --  A big entry is shown a page at a time, and says how many there are.
   Expect ("(dict ""break"")", "Definition: (Definition.Found (Word_Entry ""break"" [", "dict break");
   Check (Contains (Latest_Result (State), " 1 75))"), "break: total 75 senses: " & Latest_Result (State));
   Expect ("(lexicon.dict-page (Page_Query ""break"" 60))", "Definition: (Definition.Found (Word_Entry ""break"" [",
           "dict-page");
   Check (Contains (Latest_Result (State), " 60 75))"), "dict-page from sense 60");
   --  The console shows an entry as an article: its words are links that
   --  look the word up with the same command.
   Initialize (State, Catalog);
   Expect ("(thesaurus ""wharf"")", "Thesaurus_Result: (Thesaurus_Result.Found", "thesaurus wharf");
   Check (Region (State, Article_Link, 1).w > 0, "an entry's words are links");
   Save ("lexicon-article");
   declare
      Link : constant CuBit.UI.Rect := Region (State, Article_Link, 1);
      Submitted, Redraw : Boolean;
   begin
      Lexicon_Handle (State, (Kind => Pointer_Down, X => Link.x + 2, Y => Link.y + 2, others => <>), Submitted, Redraw);
      Lexicon_Handle (State, (Kind => Pointer_Up, X => Link.x + 2, Y => Link.y + 2, others => <>), Submitted, Redraw);
      Draw (State, Canvas, Bounds);
      Check (Starts (Latest_Result (State), "Thesaurus_Result: (Thesaurus_Result.Found (Thesaurus_Entry ""pier"""),
             "clicking a synonym looks it up: " & Latest_Result (State));
   end;
   Initialize (State, Catalog);
   Run_Entry ("(dict ""break"")");
   Check (Region (State, Article_Page, 1).w > 0, "a long entry links its next page");
   Save ("lexicon-break");
   declare
      Link : constant CuBit.UI.Rect := Region (State, Article_Page, 1);
      Submitted, Redraw : Boolean;
   begin
      Lexicon_Handle (State, (Kind => Pointer_Down, X => Link.x + 2, Y => Link.y + 2, others => <>), Submitted, Redraw);
      Lexicon_Handle (State, (Kind => Pointer_Up, X => Link.x + 2, Y => Link.y + 2, others => <>), Submitted, Redraw);
      Draw (State, Canvas, Bounds);
      Check (Contains (Latest_Result (State), " 75))") and then not Contains (Latest_Result (State), " 1 75))"),
             "next page follows: " & Latest_Result (State));
   end;
   Initialize (State, Catalog);
   Run_Entry ("(dict ""recieve"")");
   Run_Entry ("(dict ""cataract"")");
   Save ("lexicon-not-found");
   Expect ("(dict """")", "", "empty word");
   Check (not Starts (Latest_Result (State), "Definition:"), "an empty word is refused");
   Save ("lexicon-pages");

   Ada.Text_IO.Put_Line ("ccl lexicon tests:" & Natural'Image (Checks) & " checks," & Natural'Image (Failures)
                         & " failures");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
