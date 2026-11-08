--  Hosted tests of the Bible core (APP-005): the parser against every
--  form, every alias-like name (English and Latin) and every verse of four
--  translations (round trips), with fuzzing; the image reader's bounds on
--  damaged and truncated images; search against a brute-force scan; the
--  face mapping; carrying a place between the English and Vulgate
--  numberings; no pilcrow in any image.
--  Run by tests/bible/run.sh with checks and contracts on.
with Ada.Calendar; use Ada.Calendar;
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Bible_Canon; use Bible_Canon;
with Bible_Images; use Bible_Images;
with Bible_Parser; use Bible_Parser;
with Bible_References; use Bible_References;
with Bible_Search;
with Bible_Support; use Bible_Support;
with Bible_Text;
with Bible_Translations;
with Bible_Versification;

procedure Bible_Tests is
   Directory : constant String :=
     (if Ada.Environment_Variables.Exists ("BIBLE_DIR") then Ada.Environment_Variables.Value ("BIBLE_DIR")
      else "build/bible");
   KJV : constant Translation_Access := Load (Directory & "/kjv.bible");
   BSB : constant Translation_Access := Load (Directory & "/bsb.bible");
   WEB : constant Translation_Access := Load (Directory & "/web.bible");
   VUL : constant Translation_Access := Load (Directory & "/vul.bible");
   type Translation_List is array (Positive range <>) of Translation_Access;
   ALL_TRANSLATIONS : constant Translation_List := [KJV, BSB, WEB, VUL];
   Checks, Failures : Natural := 0;
   Seed : constant Integer :=
     (if Ada.Command_Line.Argument_Count >= 1 then Integer'Value (Ada.Command_Line.Argument (1)) else 20261106);

   procedure Check (Condition : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & Label);
      end if;
   end Check;

   function Show (R : Reference) return String is (Image (R));

   ---------------------------------------------------------------------------
   --  Images and versification.
   ---------------------------------------------------------------------------
   procedure Test_Images is
      Abbreviation : Text_Slice;
      Ok : Boolean;
   begin
      Read_About (KJV.Data.all, KJV.L, Bible_Images.Abbreviation, Abbreviation, Ok);
      Check (Ok and then Slice (KJV.all, Abbreviation) = "KJV", "KJV about");
      Check (KJV.Index.Table.Chapters (Psalms) = 150, "KJV Psalms has 150 chapters");
      Check (Verses_In (KJV.Index.Table, Psalms, 119) = 176, "KJV Psalm 119 has 176 verses");
      Check (KJV.Index.Table.Chapters (Malachi) = 4, "KJV Malachi has 4 chapters");
      Check (Verses_In (KJV.Index.Table, John, 3) = 36, "KJV John 3 has 36 verses");
      Check (Verses_In (KJV.Index.Table, Jude, 1) = 25, "KJV Jude has 25 verses");
      Check (KJV.Index.Table.Chapters (Revelation) = 22, "Revelation has 22 chapters");
      Check (KJV.Index.Table.Chapters (Tobit) = 14, "KJV has Tobit's 14 chapters");
      Check (KJV.Index.Table.Chapters (Third_Maccabees) = 0, "KJV has no 3 Maccabees");
      Check (WEB.Index.Table.Chapters (Third_Maccabees) = 7, "WEB has 3 Maccabees");
      Check (WEB.Index.Table.Chapters (Psalm_151) = 1, "WEB has Psalm 151");
      Check (BSB.Index.Table.Chapters (Tobit) = 0, "BSB has no deuterocanon");
      Check (KJV.Index.Table.Chapters (Esther_Greek) = 16
             and then Verses_In (KJV.Index.Table, Esther_Greek, 9) = 0
             and then Verses_In (KJV.Index.Table, Esther_Greek, 10) > 0,
             "KJV Additions to Esther start at chapter 10");
      Check (Verse_Text (KJV.all, (John, 3, 16)) =
               "For God so loved the world, that he gave his only begotten Son, that whosoever believeth "
               & "in him should not perish, but have everlasting life.", "KJV John 3:16, no pilcrow");
      --  The King James Version's pilcrows are paragraph breaks, not text:
      --  John 3:16 starts a paragraph (its source has "\v 16 \wj ¶ For God").
      declare
         VR : constant Verse_Record := Read_Verse (KJV.Data.all, KJV.L, Verse_Row (KJV.L, KJV.Index, (John, 3, 16)));
      begin
         Check (VR.Segments > 0 and then Read_Segment (KJV.Data.all, KJV.L, VR.First_Segment).Kind = Paragraph,
                "KJV John 3:16 starts a paragraph");
      end;
      --  No image holds a pilcrow anywhere in its text (U+00B6, C2 B6).
      for T of ALL_TRANSLATIONS loop
         declare
            Text : constant Extent := T.L.Sections (Bible_Images.Text);
            Pilcrows : Natural := 0;
         begin
            for I in Text.Offset .. Text.Offset + Text.Count - 2 loop
               if T.Data (I) = 16#C2# and then T.Data (I + 1) = 16#B6# then
                  Pilcrows := Pilcrows + 1;
               end if;
            end loop;
            Check (Pilcrows = 0, "no pilcrow in an image's text:" & Pilcrows'Image);
         end;
      end loop;
      --  The Vulgate: its own numbering, its language.
      declare
         About : Text_Slice;
         Valid : Boolean;
         VUL_Red : Natural := 0;
      begin
         Read_About (VUL.Data.all, VUL.L, Bible_Images.Abbreviation, About, Valid);
         Check (Valid and then Slice (VUL.all, About) = "VUL", "VUL about");
         Read_About (VUL.Data.all, VUL.L, Bible_Images.Language_Code, About, Valid);
         Check (Valid and then Slice (VUL.all, About) = "la", "VUL is in Latin (la)");
         Read_About (VUL.Data.all, VUL.L, Bible_Images.Language_Name, About, Valid);
         Check (Valid and then Slice (VUL.all, About) = "Latin", "VUL language name");
         Read_About (KJV.Data.all, KJV.L, Bible_Images.Language_Code, About, Valid);
         Check (Valid and then Slice (KJV.all, About) = "en", "KJV is in English (en)");
         Check (VUL.Index.Table.Chapters (Psalms) = 150, "VUL Psalms has 150 chapters");
         Check (Verses_In (VUL.Index.Table, Psalms, 118) = 176, "VUL Psalm 118 (Hebrew 119) has 176 verses");
         Check (Verses_In (VUL.Index.Table, Psalms, 9) = 39, "VUL Psalm 9 holds Hebrew 9 and 10: 39 verses");
         Check (VUL.Index.Table.Chapters (Daniel) = 14, "VUL Daniel has 14 chapters (Susanna, Bel)");
         Check (Verses_In (VUL.Index.Table, Daniel, 3) = 100, "VUL Daniel 3 holds the Song of the Three");
         Check (VUL.Index.Table.Chapters (Esther) = 16, "VUL Esther runs to chapter 16");
         Check (VUL.Index.Table.Chapters (Song_Of_The_Three) = 0 and then VUL.Index.Table.Chapters (Tobit) = 14,
                "VUL: no separate Song of the Three; Tobit");
         Check (Verse_Text (VUL.all, (John, 3, 16)) =
                  "Sic enim Deus dilexit mundum, ut Filium suum unigenitum daret: ut omnis qui credit in eum, "
                  & "non pereat, sed habeat vitam æternam.", "VUL John 3:16");
         Check (Verse_Text (VUL.all, (Genesis, 1, 1)) = "In principio creavit Deus cælum et terram.",
                "VUL Genesis 1:1");
         Check (Ada.Strings.Fixed.Index (Verse_Text (VUL.all, (Psalms, 22, 1)), "Dominus regit me") > 0, "VUL Psalm 22:1");
         for S in 0 .. Bible_Images.Count (VUL.L, Segments) - 1 loop
            if Read_Segment (VUL.Data.all, VUL.L, S).Words_Of_Christ then VUL_Red := VUL_Red + 1; end if;
         end loop;
         Check (VUL_Red = 0, "the Vulgate source marks no words of Christ:" & VUL_Red'Image);
      end;
      Check (Verse_Text (WEB.all, (John, 11, 35)) = "Jesus wept.", "WEB John 11:35");
      Check (Verse_Text (BSB.all, (Genesis, 1, 1)) =
               "In the beginning God created the heavens and the earth.", "BSB Genesis 1:1");
      --  The words of Christ (\wj) through the reader: all of John 3:16 in
      --  the KJV, none of John 3:1, nothing in the BSB.
      declare
         function Red (T : Translation_Access; Item : Reference; Every : Boolean) return Boolean is
            VR : constant Verse_Record := Read_Verse (T.Data.all, T.L, Verse_Row (T.L, T.Index, Item));
            Any, All_Red : Boolean := False;
         begin
            All_Red := VR.Segments > 0;
            for S in VR.First_Segment .. VR.First_Segment + VR.Segments - 1 loop
               declare
                  Seg : constant Segment_Record := Read_Segment (T.Data.all, T.L, S);
               begin
                  if Is_Verse_Text (Seg.Kind) then
                     Any := Any or else Seg.Words_Of_Christ;
                     All_Red := All_Red and then Seg.Words_Of_Christ;
                  end if;
               end;
            end loop;
            return (if Every then All_Red else Any);
         end Red;
         BSB_Red : Natural := 0;
      begin
         Check (Red (KJV, (John, 3, 16), True), "KJV John 3:16 is all words of Christ");
         Check (not Red (KJV, (John, 3, 1), False), "KJV John 3:1 is not");
         Check (Red (WEB, (John, 3, 16), True), "WEB John 3:16 is all words of Christ");
         for S in 0 .. Bible_Images.Count (BSB.L, Segments) - 1 loop
            if Read_Segment (BSB.Data.all, BSB.L, S).Words_Of_Christ then BSB_Red := BSB_Red + 1; end if;
         end loop;
         Check (BSB_Red = 0, "the BSB marks no words of Christ:" & BSB_Red'Image);
      end;
      --  Every book's verse total in the Old and New Testaments.
      declare
         Total : Natural := 0;
      begin
         for B in Book loop
            if Testament_Of (B) /= Deuterocanon then
               for C in 1 .. KJV.Index.Table.Chapters (B) loop
                  Total := Total + Verses_In (KJV.Index.Table, B, C);
               end loop;
            end if;
         end loop;
         Check (Total = 31_102, "KJV Old and New Testaments number 31,102 verses:" & Total'Image);
      end;
      --  Locate inverts Verse_Row for every verse of every translation.
      for T of ALL_TRANSLATIONS loop
         declare
            Mismatch : Natural := 0;
            Found : Boolean;
            Back : Reference;
         begin
            for B in Book loop
               for C in 1 .. T.Index.Table.Chapters (B) loop
                  for V in 1 .. Verses_In (T.Index.Table, B, C) loop
                     Locate (T.L, T.Index, Verse_Row (T.L, T.Index, (B, C, V)), Back, Found);
                     if not Found or else Back /= (B, C, V) then
                        Mismatch := Mismatch + 1;
                     end if;
                  end loop;
               end loop;
            end loop;
            Check (Mismatch = 0, "Locate inverts Verse_Row");
         end;
      end loop;
   end Test_Images;

   ---------------------------------------------------------------------------
   --  The parser.
   ---------------------------------------------------------------------------
   procedure Expect (T : Translation_Access; Text : String; From, To : Reference) is
      R : Result;
   begin
      Parse (Text, T.Index.Table, R);
      Check (R.Outcome = Parsed and then R.Passage = (From, To),
             """" & Text & """ -> " & R.Outcome'Image &
             (if R.Outcome = Parsed then " " & Show (R.Passage.From) & " .. " & Show (R.Passage.To)
              else " (" & Describe (Text, R) & ")"));
   end Expect;

   procedure Expect_Error (T : Translation_Access; Text : String; Outcome : Status;
                           Value : Number := 0; Limit : Number := 0; Named : String := "") is
      R : Result;
   begin
      Parse (Text, T.Index.Table, R);
      Check (R.Outcome = Outcome
             and then (Value = 0 or else R.Value = Value)
             and then (Limit = 0 or else R.Limit = Limit)
             and then (Named = "" or else (R.Book_First in Text'Range and then R.Book_Last in Text'Range
                                           and then Text (R.Book_First .. R.Book_Last) = Named)),
             """" & Text & """ -> " & R.Outcome'Image & R.Value'Image & R.Limit'Image &
             (if R.Outcome /= Parsed then " (" & Describe (Text, R) & ")" else ""));
   end Expect_Error;

   procedure Test_Parser is
      R : Result;
      Item : Book;
      Found, Ambiguous : Boolean;
   begin
      Expect (KJV, "John 3:16", (John, 3, 16), (John, 3, 16));
      Expect (KJV, "Jn 3:16-18", (John, 3, 16), (John, 3, 18));
      Expect (KJV, "1 Cor 13", (First_Corinthians, 13, 1), (First_Corinthians, 13, 13));
      Expect (KJV, "Ps 23", (Psalms, 23, 1), (Psalms, 23, 6));
      Expect (KJV, "Tobit 1:1", (Tobit, 1, 1), (Tobit, 1, 1));
      Expect (KJV, "john 3:16", (John, 3, 16), (John, 3, 16));
      Expect (KJV, "JOHN 3.16", (John, 3, 16), (John, 3, 16));
      Expect (KJV, "  John   3 : 16  ", (John, 3, 16), (John, 3, 16));
      Expect (KJV, "John 3:16" & Character'Val (16#E2#) & Character'Val (16#80#) & Character'Val (16#93#) & "18",
              (John, 3, 16), (John, 3, 18));
      Expect (KJV, "John 3:16 - 4:2", (John, 3, 16), (John, 4, 2));
      Expect (KJV, "John 3-4", (John, 3, 1), (John, 4, 54));
      Expect (KJV, "John 3-4:2", (John, 3, 1), (John, 4, 2));
      Expect (KJV, "Jude 5", (Jude, 1, 5), (Jude, 1, 5));
      Expect (KJV, "Jude 3-5", (Jude, 1, 3), (Jude, 1, 5));
      Expect (KJV, "Jude 1", (Jude, 1, 1), (Jude, 1, 25));
      Expect (KJV, "Jude 1:5", (Jude, 1, 5), (Jude, 1, 5));
      Expect (KJV, "Obadiah", (Obadiah, 1, 1), (Obadiah, 1, 21));
      Expect (KJV, "Genesis", (Genesis, 1, 1), (Genesis, 50, 26));
      Expect (KJV, "I John 1:9", (First_John, 1, 9), (First_John, 1, 9));
      Expect (KJV, "First John 1:9", (First_John, 1, 9), (First_John, 1, 9));
      Expect (KJV, "1st John 1:9", (First_John, 1, 9), (First_John, 1, 9));
      Expect (KJV, "1Jn 1:9", (First_John, 1, 9), (First_John, 1, 9));
      Expect (KJV, "III John 2", (Third_John, 1, 2), (Third_John, 1, 2));
      Expect (KJV, "2 Tim. 3:16", (Second_Timothy, 3, 16), (Second_Timothy, 3, 16));
      Expect (KJV, "Song of Songs 2:1", (Song_Of_Solomon, 2, 1), (Song_Of_Solomon, 2, 1));
      Expect (KJV, "Song of Solomon 2:1", (Song_Of_Solomon, 2, 1), (Song_Of_Solomon, 2, 1));
      Expect (KJV, "Canticles 2:1", (Song_Of_Solomon, 2, 1), (Song_Of_Solomon, 2, 1));
      Expect (KJV, "Rev 22:21", (Revelation, 22, 21), (Revelation, 22, 21));
      Expect (KJV, "Isa 53:5", (Isaiah, 53, 5), (Isaiah, 53, 5));
      Expect (KJV, "Ecclus 1:1", (Sirach, 1, 1), (Sirach, 1, 1));
      Expect (KJV, "Wisdom of Solomon 3:1", (Wisdom, 3, 1), (Wisdom, 3, 1));
      Expect (KJV, "Bel and the Dragon 1", (Bel_And_The_Dragon, 1, 1),
              (Bel_And_The_Dragon, 1, Verses_In (KJV.Index.Table, Bel_And_The_Dragon, 1)));
      Expect (KJV, "Gene 1:1", (Genesis, 1, 1), (Genesis, 1, 1));       --  a prefix
      Expect (KJV, "Philem 4", (Philemon, 1, 4), (Philemon, 1, 4));
      Expect (KJV, "Phil 4:13", (Philippians, 4, 13), (Philippians, 4, 13));
      Expect (KJV, "Esther (Greek)", (Esther_Greek, 10, 1),
              (Esther_Greek, 16, Verses_In (KJV.Index.Table, Esther_Greek, 16)));
      Expect (WEB, "Psalm 151", (Psalm_151, 1, 1), (Psalm_151, 1, Verses_In (WEB.Index.Table, Psalm_151, 1)));
      Expect (WEB, "Psalm 151 1:3", (Psalm_151, 1, 3), (Psalm_151, 1, 3));
      Expect (WEB, "Ps 151:3", (Psalm_151, 1, 3), (Psalm_151, 1, 3));
      Expect (WEB, "3 Maccabees 1:1", (Third_Maccabees, 1, 1), (Third_Maccabees, 1, 1));
      --  Latin names and abbreviations, as the Vulgate has them.
      Expect (VUL, "Ioannes 3:16", (John, 3, 16), (John, 3, 16));
      Expect (VUL, "Io 3:16", (John, 3, 16), (John, 3, 16));
      Expect (VUL, "Ioh 3:16", (John, 3, 16), (John, 3, 16));
      Expect (KJV, "Iohannes 3:16", (John, 3, 16), (John, 3, 16));
      Expect (VUL, "Ps 22", (Psalms, 22, 1), (Psalms, 22, 6));
      Expect (VUL, "Psalmi 22:1", (Psalms, 22, 1), (Psalms, 22, 1));
      Expect (VUL, "Psalmus 118:105", (Psalms, 118, 105), (Psalms, 118, 105));
      Expect (VUL, "Matthaeus 5:3", (Matthew, 5, 3), (Matthew, 5, 3));
      Expect (VUL, "Lucas 2:1", (Luke, 2, 1), (Luke, 2, 1));
      Expect (VUL, "Marcus 1:1", (Mark, 1, 1), (Mark, 1, 1));
      Expect (VUL, "1 Regum 1:1", (First_Samuel, 1, 1), (First_Samuel, 1, 1));
      Expect (VUL, "III Regum 1:1", (First_Kings, 1, 1), (First_Kings, 1, 1));
      Expect (VUL, "4 Reg 1:1", (Second_Kings, 1, 1), (Second_Kings, 1, 1));
      Expect (VUL, "1 Paralipomenon 1:1", (First_Chronicles, 1, 1), (First_Chronicles, 1, 1));
      Expect (VUL, "Isaias 53:5", (Isaiah, 53, 5), (Isaiah, 53, 5));
      Expect (VUL, "Canticum Canticorum 2:1", (Song_Of_Solomon, 2, 1), (Song_Of_Solomon, 2, 1));
      Expect (VUL, "Sapientia 3:1", (Wisdom, 3, 1), (Wisdom, 3, 1));
      Expect (VUL, "Ecclesiasticus 1:1", (Sirach, 1, 1), (Sirach, 1, 1));
      Expect (VUL, "1 Machabaeorum 1:1", (First_Maccabees, 1, 1), (First_Maccabees, 1, 1));
      Expect (VUL, "Iacobi 1:1", (James, 1, 1), (James, 1, 1));
      Expect (VUL, "1 Ioannis 1:9", (First_John, 1, 9), (First_John, 1, 9));
      Expect (VUL, "Apocalypsis 22:21", (Revelation, 22, 21), (Revelation, 22, 21));
      Expect (VUL, "Daniel 13:1", (Daniel, 13, 1), (Daniel, 13, 1));
      Expect (VUL, "Esther 16:24", (Esther, 16, 24), (Esther, 16, 24));
      Expect_Error (VUL, "Susanna 1:1", Book_Not_In_Translation);
      Expect_Error (KJV, "Daniel 13:1", Chapter_Out_Of_Range, Value => 13, Limit => 12);

      Expect_Error (KJV, "", Empty_Text);
      Expect_Error (KJV, "    ", Empty_Text);
      Expect_Error (KJV, [1 .. MAXIMUM_TEXT + 1 => 'a'], Text_Too_Long);
      Expect_Error (KJV, "Jhon 3:16", Unknown_Book, Named => "Jhon");
      Expect_Error (KJV, "Hezekiah 3:16", Unknown_Book, Named => "Hezekiah");
      Expect_Error (KJV, "Jo 3:16", Ambiguous_Book, Named => "Jo");
      Expect_Error (KJV, "Ph 1:1", Ambiguous_Book, Named => "Ph");
      Expect_Error (KJV, "John 22", Chapter_Out_Of_Range, Value => 22, Limit => 21);
      Expect_Error (KJV, "John 3:40", Verse_Out_Of_Range, Value => 40, Limit => 36);
      Expect_Error (KJV, "Revelation 22:22", Verse_Out_Of_Range, Value => 22, Limit => 21);
      Expect_Error (KJV, "John 0:1", Chapter_Out_Of_Range);
      Expect_Error (KJV, "John 3:0", Verse_Out_Of_Range);
      Expect_Error (KJV, "Jude 26", Verse_Out_Of_Range, Value => 26, Limit => 25);
      Expect_Error (KJV, "John 3:18-16", Reversed_Range);
      Expect_Error (KJV, "John 4-3", Reversed_Range);
      Expect_Error (KJV, "3:16", Malformed);
      Expect_Error (KJV, "John 3:", Malformed);
      Expect_Error (KJV, "John 3:16-", Malformed);
      Expect_Error (KJV, "John 3:16a", Malformed);
      Expect_Error (KJV, "John 3:16, 18", Malformed);
      Expect_Error (KJV, "John :16", Malformed);
      Expect_Error (KJV, "John 3:9999", Number_Too_Large);
      Expect_Error (KJV, "John 99999999999999999999", Number_Too_Large);
      Expect_Error (KJV, "Psalm 151", Chapter_Out_Of_Range, Value => 151, Limit => 150);
      Expect_Error (KJV, "3 Maccabees 1:1", Book_Not_In_Translation);
      Expect_Error (BSB, "Tobit 1:1", Book_Not_In_Translation);
      Expect_Error (KJV, "Esther (Greek) 3", Verse_Out_Of_Range);

      --  The message names what was wrong.
      Parse ("Jhon 3:16", KJV.Index.Table, R);
      Check (Describe ("Jhon 3:16", R) = "No book is called ""Jhon""", "unknown book message");
      Parse ("John 22", KJV.Index.Table, R);
      Check (Describe ("John 22", R) = "John has 21 chapters", "chapter message");
      Parse ("John 3:40", KJV.Index.Table, R);
      Check (Describe ("John 3:40", R) = "John 3 has 36 verses", "verse message");

      --  Every book by its full name, alone (the whole book, where present).
      for B in Book loop
         if B /= Psalm_151 then
            Find_Book (Name (B), Item, Found, Ambiguous);
            Check (Found and then Item = B, "Find_Book (" & Name (B) & ")");
         end if;
         for T of ALL_TRANSLATIONS loop
            Parse (Name (B), T.Index.Table, R);
            --  "Psalm 151" without the book is Psalms 151, past its last chapter.
            Check ((R.Outcome = Parsed) = Has_Book (T.Index.Table, B)
                   and then (R.Outcome in Parsed | Book_Not_In_Translation
                             or else (B = Psalm_151 and then R.Outcome = Chapter_Out_Of_Range)),
                   "whole book " & Name (B) & " " & R.Outcome'Image);
         end loop;
      end loop;

      --  Every verse of every translation: its image parses back to itself.
      for T of ALL_TRANSLATIONS loop
         declare
            Wrong : Natural := 0;
            Total : Natural := 0;
         begin
            for B in Book loop
               for C in 1 .. T.Index.Table.Chapters (B) loop
                  for V in 1 .. Verses_In (T.Index.Table, B, C) loop
                     Total := Total + 1;
                     Parse (Image ((B, C, V)), T.Index.Table, R);
                     if R.Outcome /= Parsed or else R.Passage /= ((B, C, V), (B, C, V)) then
                        if Wrong < 5 then
                           Put_Line ("  round trip: " & Image ((B, C, V)) & " -> " & R.Outcome'Image);
                        end if;
                        Wrong := Wrong + 1;
                     end if;
                  end loop;
                  if Verses_In (T.Index.Table, B, C) > 0 then
                     declare
                        Whole : constant Passage :=
                          ((B, C, 1), (B, C, Verses_In (T.Index.Table, B, C)));
                     begin
                        Parse (Image (T.Index.Table, Whole), T.Index.Table, R);
                        if R.Outcome /= Parsed or else R.Passage /= Whole then
                           Wrong := Wrong + 1;
                        end if;
                     end;
                  end if;
               end loop;
            end loop;
            Check (Wrong = 0 and then Total > 31_000, "every verse round-trips:" & Total'Image & " verses," &
                   Wrong'Image & " wrong");
         end;
      end loop;
   end Test_Parser;

   --  Random text into the parser: every result is typed, every contract
   --  holds (-gnata), nothing raises.
   procedure Fuzz_Parser (Rounds : Positive) is
      ALPHABET : constant String :=
        "0123456789:.- ()abcdefghijklmnopqrstuvwxyzJPSIV" & Character'Val (16#E2#) & Character'Val (16#80#) &
        Character'Val (16#93#) & Character'Val (16#FF#) & ASCII.HT & ASCII.NUL;
      WORDS : constant array (1 .. 10) of Unbounded_String :=
        [To_Unbounded_String ("John "), To_Unbounded_String ("1 Cor "), To_Unbounded_String ("Psalm "),
         To_Unbounded_String ("151"), To_Unbounded_String (":"), To_Unbounded_String ("-"),
         To_Unbounded_String ("Jude "), To_Unbounded_String ("III "), To_Unbounded_String ("Song of "),
         To_Unbounded_String ("999")];
      subtype Pick is Positive range 1 .. ALPHABET'Length + WORDS'Length;
      package Random_Pick is new Ada.Numerics.Discrete_Random (Pick);
      subtype Size is Natural range 0 .. 40;
      package Random_Size is new Ada.Numerics.Discrete_Random (Size);
      G : Random_Pick.Generator;
      S : Random_Size.Generator;
      R : Result;
      Outcomes : array (Status) of Natural := [others => 0];
      Text : Unbounded_String;
   begin
      Random_Pick.Reset (G, Seed);
      Random_Size.Reset (S, Seed + 1);
      for Round in 1 .. Rounds loop
         Text := Null_Unbounded_String;
         for K in 1 .. Random_Size.Random (S) loop
            declare
               P : constant Pick := Random_Pick.Random (G);
            begin
               if P <= ALPHABET'Length then
                  Append (Text, ALPHABET (P));
               else
                  Append (Text, WORDS (P - ALPHABET'Length));
               end if;
            end;
         end loop;
         declare
            T : constant Translation_Access := (case Round mod 3 is when 0 => KJV, when 1 => BSB, when others => WEB);
            Input : constant String := To_String (Text);
         begin
            Parse (Input, T.Index.Table, R);
            Outcomes (R.Outcome) := Outcomes (R.Outcome) + 1;
            if R.Outcome /= Parsed then
               Check (Describe (Input, R)'Length > 0, "a message for every failure");
            end if;
         end;
      end loop;
      Put ("  fuzz:");
      for O in Status loop
         Put (" " & O'Image & Outcomes (O)'Image);
      end loop;
      New_Line;
      Check (Outcomes (Parsed) > 0 and then Outcomes (Malformed) > 0 and then Outcomes (Unknown_Book) > 0,
             "the fuzzer reaches parsed and failing inputs");
   end Fuzz_Parser;

   ---------------------------------------------------------------------------
   --  Search against a brute-force scan.
   ---------------------------------------------------------------------------
   type Word_List is array (Positive range <>) of Unbounded_String;

   --  The search keys of a verse text (tools/build_bible.py words_of).
   function Keys_Of (Text : String) return Word_List is
      Result : Word_List (1 .. Text'Length + 1);
      Count : Natural := 0;
      Current : Unbounded_String;
      I : Natural := Text'First;
      procedure Close is
      begin
         if Length (Current) > 0 then
            Count := Count + 1;
            Result (Count) := Current;
            Current := Null_Unbounded_String;
         end if;
      end Close;
   begin
      while I <= Text'Last loop
         declare
            C : constant Character := Text (I);
            Two : constant String := (if I < Text'Last then Text (I .. I + 1) else "");
            Three : constant String := (if I + 1 < Text'Last then Text (I .. I + 2) else "");
         begin
            if C in 'a' .. 'z' | '0' .. '9' then
               Append (Current, C);
               I := I + 1;
            elsif C in 'A' .. 'Z' then
               Append (Current, Character'Val (Character'Pos (C) + 32));
               I := I + 1;
            elsif C = ''' then
               if Length (Current) = 0 then Close; end if;
               I := I + 1;
            elsif Three = "’" then
               if Length (Current) = 0 then Close; end if;
               I := I + 3;
            elsif Two = "æ" or else Two = "Æ" then
               Append (Current, "ae");
               I := I + 2;
            elsif Two = "à" then
               Append (Current, "a");
               I := I + 2;
            elsif Two = "œ" or else Two = "Œ" then
               Append (Current, "oe");
               I := I + 2;
            elsif Two = "é" or else Two = "ē" or else Two = "ë" or else Two = "Ë" then
               Append (Current, "e");
               I := I + 2;
            elsif Two = "ō" then
               Append (Current, "o");
               I := I + 2;
            else
               Close;
               I := I + 1;
            end if;
         end;
      end loop;
      Close;
      return Result (1 .. Count);
   end Keys_Of;

   type Hits_Access is access Bible_Search.Hit_Array;
   Hits : constant Hits_Access := new Bible_Search.Hit_Array (1 .. 50_000);
   type Work_Access is access Bible_Search.Work_Area;
   Work : constant Work_Access := new Bible_Search.Work_Area;

   procedure Expect_Search (T : Translation_Access; Text : String; Prefix : Boolean; Minimum : Natural := 1) is
      Q : Bible_Search.Query;
      Normalized : Bible_Search.Normalize_Status;
      Found, Total : Natural;
      Status : Bible_Search.Search_Status;
      Expected : Natural := 0;
      Mismatch : Natural := 0;
      Started : Time;
      Spent : Duration;
      use type Bible_Search.Normalize_Status, Bible_Search.Search_Status;
   begin
      Bible_Search.Normalize (Text, Q, Normalized);
      if Normalized /= Bible_Search.Normalized then
         Check (False, "normalize """ & Text & """: " & Normalized'Image);
         return;
      end if;
      Started := Clock;
      Bible_Search.Search (T.Data.all, T.L, Q, Prefix, Work.all, Hits.all, Found, Total, Status);
      Spent := Clock - Started;
      Check (Status = Bible_Search.Searched and then Found = Total, "search """ & Text & """ " & Status'Image);
      --  Brute force: every verse whose keys hold every term.
      for Row in 0 .. Bible_Images.Count (T.L, Verses) - 1 loop
         declare
            Keys : constant Word_List := Keys_Of (Verse_Text (T.all, Row));
            All_Found : Boolean := True;
         begin
            for I in 1 .. Q.Count loop
               declare
                  Term : constant String := Q.Terms (I).Text (1 .. Q.Terms (I).Length);
                  Any : Boolean := False;
               begin
                  for K of Keys loop
                     if To_String (K) = Term
                       or else (Prefix and then I = Q.Count and then Length (K) >= Term'Length
                                and then Slice (K, 1, Term'Length) = Term)
                     then
                        Any := True;
                        exit;
                     end if;
                  end loop;
                  All_Found := All_Found and then Any;
               end;
            end loop;
            if All_Found then
               Expected := Expected + 1;
               if Expected > Found or else Hits (Expected) /= Row then
                  Mismatch := Mismatch + 1;
               end if;
            end if;
         end;
      end loop;
      Check (Expected = Total and then Mismatch = 0 and then Total >= Minimum,
             "search """ & Text & """ (prefix " & Prefix'Image & "):" & Total'Image & " hits, brute force" &
             Expected'Image & "," & Mismatch'Image & " differ");
      Put_Line ("  search """ & Text & """" & (if Prefix then " (prefix)" else "") & ":" & Total'Image &
                " verses in" & Duration'Image (Spent * 1000) & " ms (checked build)");
   end Expect_Search;

   procedure Test_Search is
      Q : Bible_Search.Query;
      Normalized : Bible_Search.Normalize_Status;
      use type Bible_Search.Normalize_Status;
   begin
      Bible_Search.Normalize ("Lord’s  SUPPER", Q, Normalized);
      Check (Normalized = Bible_Search.Normalized and then Q.Count = 2
             and then Q.Terms (1).Text (1 .. Q.Terms (1).Length) = "lords"
             and then Q.Terms (2).Text (1 .. Q.Terms (2).Length) = "supper", "normalize Lord's supper");
      Bible_Search.Normalize ("  ,;  ", Q, Normalized);
      Check (Normalized = Bible_Search.Empty_Query, "normalize punctuation only");
      Bible_Search.Normalize ("a b c d e f g h i", Q, Normalized);
      Check (Normalized = Bible_Search.Too_Many_Terms, "normalize nine words");
      Bible_Search.Normalize ([1 .. 40 => 'x'], Q, Normalized);
      Check (Normalized = Bible_Search.Term_Too_Long, "normalize a long word");
      Bible_Search.Normalize ("Cæsar", Q, Normalized);
      Check (Normalized = Bible_Search.Normalized and then Q.Terms (1).Text (1 .. Q.Terms (1).Length) = "caesar",
             "normalize Cæsar");

      Expect_Search (KJV, "faith", False, 200);
      Expect_Search (KJV, "God so loved", False, 1);
      Expect_Search (KJV, "Jesus wept", False, 1);
      Expect_Search (KJV, "Lord's", False, 100);
      Expect_Search (KJV, "belie", True, 100);
      Expect_Search (KJV, "alpha omega", False, 1);
      Expect_Search (KJV, "Melchisedec", False, 1);
      Expect_Search (KJV, "caesar", False, 10);
      Expect_Search (KJV, "the", False, 20_000);
      Expect_Search (KJV, "a", True, 30_000);
      Expect_Search (KJV, "zzzzqx", False, 0);
      Expect_Search (WEB, "Yahweh shepherd", False, 1);
      Expect_Search (WEB, "faith hope love", False, 1);
      Expect_Search (BSB, "in the beginning", False, 3);
      Expect_Search (BSB, "grace", True, 100);
      --  Latin: the ligatures and diaeresis fold (cælum is caelum, Israël
      --  Israel, cœlum coelum).
      Bible_Search.Normalize ("cœlum Israël", Q, Normalized);
      Check (Normalized = Bible_Search.Normalized and then Q.Count = 2
             and then Q.Terms (1).Text (1 .. Q.Terms (1).Length) = "coelum"
             and then Q.Terms (2).Text (1 .. Q.Terms (2).Length) = "israel", "normalize cœlum Israël");
      Expect_Search (VUL, "caelum terram", False, 20);
      Expect_Search (VUL, "dilexit mundum", False, 1);
      Expect_Search (VUL, "Israel", False, 1_000);
      Expect_Search (VUL, "Dominus regit", False, 1);
      Expect_Search (VUL, "coel", True, 1);
   end Test_Search;

   ---------------------------------------------------------------------------
   --  The face mapping.
   ---------------------------------------------------------------------------
   procedure Test_Text is
      use Bible_Text;
   begin
      Check (Face_Text ("God’s “word” — … æ") =
               "God" & RIGHT_SINGLE & "s " & LEFT_DOUBLE & "word" & RIGHT_DOUBLE & " " & EM_DASH & " " &
               ELLIPSIS & " " & Character'Val (16#E6#), "face text of typography");
      Check (Face_Text ("cœlum Œ Israël Ë Æ") =
               "c" & OE_LOWER & "lum " & OE_UPPER & " Isra" & Character'Val (16#EB#) & "l " &
               Character'Val (16#CB#) & " " & Character'Val (16#C6#), "face text of Latin");
      Check (To_ASCII ("cœlum Œ Israël Ë Æ æ") = "coelum OE Israel E AE ae", "ASCII of Latin");
      Check (Face_Text ("Mahēr ō") = "Maher o", "macrons fold");
      Check (Face_Text ("א") = "?", "Hebrew is not in the face");
      Check (Face_Text ("") = "", "empty");
      Check (Face_Text ([1 => Character'Val (16#E2#)]) = "?", "a cut sequence");
      --  Every verse of every translation maps with no unknown character.
      for T of ALL_TRANSLATIONS loop
         declare
            --  Question marks in the face text that were not in the source.
            Unknowns : Integer := 0;
         begin
            for Row in 0 .. Bible_Images.Count (T.L, Verses) - 1 loop
               declare
                  Source : constant String := Verse_Text (T.all, Row);
               begin
                  for C of Face_Text (Source) loop
                     if C = UNKNOWN then Unknowns := Unknowns + 1; end if;
                  end loop;
                  for C of Source loop
                     if C = UNKNOWN then Unknowns := Unknowns - 1; end if;
                  end loop;
               end;
            end loop;
            Check (Unknowns = 0, "every verse is in the reading face:" & Unknowns'Image & " unknown");
         end;
      end loop;
   end Test_Text;

   ---------------------------------------------------------------------------
   --  Carrying a place between numberings; the translations' languages.
   ---------------------------------------------------------------------------
   procedure Test_Versification is
      use Bible_Versification;
      procedure Pair (English_Item, Vulgate_Item : Reference) is
         To_Latin : constant Reference := Map (English, Vulgate, English_Item);
         To_English : constant Reference := Map (Vulgate, English, Vulgate_Item);
      begin
         Check (To_Latin = Vulgate_Item, Show (English_Item) & " is Vulgate " & Show (Vulgate_Item) & ", not " &
                Show (To_Latin));
         Check (To_English = English_Item, "Vulgate " & Show (Vulgate_Item) & " is " & Show (English_Item) &
                ", not " & Show (To_English));
      end Pair;
      Wrong, Missing, Total : Natural := 0;
   begin
      Pair ((John, 3, 16), (John, 3, 16));
      Pair ((Psalms, 1, 1), (Psalms, 1, 1));
      Pair ((Psalms, 23, 1), (Psalms, 22, 1));
      Pair ((Psalms, 51, 1), (Psalms, 50, 3));
      Pair ((Psalms, 3, 1), (Psalms, 3, 2));
      Pair ((Psalms, 9, 1), (Psalms, 9, 2));
      Pair ((Psalms, 10, 1), (Psalms, 9, 22));
      Pair ((Psalms, 10, 18), (Psalms, 9, 39));
      Pair ((Psalms, 114, 1), (Psalms, 113, 1));
      Pair ((Psalms, 115, 1), (Psalms, 113, 9));
      Pair ((Psalms, 116, 1), (Psalms, 114, 1));
      Pair ((Psalms, 116, 10), (Psalms, 115, 1));
      Pair ((Psalms, 119, 105), (Psalms, 118, 105));
      Pair ((Psalms, 147, 1), (Psalms, 146, 1));
      Pair ((Psalms, 147, 12), (Psalms, 147, 1));
      Pair ((Psalms, 150, 6), (Psalms, 150, 6));
      Pair ((Daniel, 3, 23), (Daniel, 3, 23));
      Pair ((Song_Of_The_Three, 1, 1), (Daniel, 3, 24));
      Pair ((Song_Of_The_Three, 1, 67), (Daniel, 3, 90));
      Check (Map (English, Vulgate, (Song_Of_The_Three, 1, 68)) = (Daniel, 3, 90),
             "the Song of the Three's last verse is the Vulgate's Daniel 3:90");
      Pair ((Daniel, 3, 24), (Daniel, 3, 91));
      Pair ((Daniel, 4, 1), (Daniel, 3, 98));
      Pair ((Daniel, 4, 4), (Daniel, 4, 1));
      Pair ((Susanna, 1, 1), (Daniel, 13, 1));
      Pair ((Bel_And_The_Dragon, 1, 1), (Daniel, 14, 1));
      Pair ((Esther_Greek, 11, 2), (Esther, 11, 2));
      Pair ((Esther, 10, 3), (Esther, 10, 3));
      --  A title verse is the psalm's first English verse.
      Check (Map (Vulgate, English, (Psalms, 50, 1)) = (Psalms, 51, 1), "Vulgate 50:1 (title) is English 51:1");
      Check (Vulgate_Psalm (23) = 22 and then Vulgate_Psalm (10) = 9 and then Vulgate_Psalm (148) = 148,
             "Vulgate psalm numbers");
      --  Every KJV verse: carried to the Vulgate and back, the same verse
      --  (the Additions to Esther's empty 10:1-3 aside, and the Song of the
      --  Three's 68th verse, which the Vulgate's 67 hold in its last); and
      --  nearly every one lands on a verse the Vulgate has.
      for B in Book loop
         for C in 1 .. KJV.Index.Table.Chapters (B) loop
            for V in 1 .. Verses_In (KJV.Index.Table, B, C) loop
               if not (B = Esther_Greek and then C = 10 and then V <= 3)
                 and then (B, C, V) /= Reference'(Song_Of_The_Three, 1, 68)
               then
                  Total := Total + 1;
                  declare
                     Latin : constant Reference := Map (English, Vulgate, (B, C, V));
                  begin
                     if Map (Vulgate, English, Latin) /= (B, C, V) then
                        Wrong := Wrong + 1;
                        if Wrong <= 5 then
                           Put_Line ("  round trip: " & Show ((B, C, V)) & " -> " & Show (Latin));
                        end if;
                     end if;
                     if Has_Book (VUL.Index.Table, Latin.Book) and then not Exists (VUL.Index.Table, Latin) then
                        Missing := Missing + 1;
                     end if;
                  end;
               end if;
            end loop;
         end loop;
      end loop;
      Check (Wrong = 0, "every KJV verse round-trips through the Vulgate's numbering:" & Wrong'Image & " wrong");
      Check (Missing * 200 < Total, "nearly every KJV verse is a Vulgate verse:" & Missing'Image & " of" &
             Total'Image & " are not (the Vulgate divides some verses differently)");
      Put_Line ("  versification: " & Total'Image & " KJV verses carried," & Missing'Image &
                " past the Vulgate's verses in their chapter");

      --  Languages and the default translation (the system language seam).
      declare
         package BT renames Bible_Translations;
         use type BT.Translation;
      begin
         Check (BT.Display_Name (BT.KJV) = "King James Version (English)", "KJV display name");
         Check (BT.Display_Name (BT.VUL) = "Biblia Sacra Vulgata Clementina (Latin)", "VUL display name");
         Check (BT.ISO_639 (BT.Language_Of (BT.VUL)) = "la" and then BT.ISO_639 (BT.Language_Of (BT.WEB)) = "en", "ISO 639 codes");
         Check (BT.Default_Translation (BT.NO_PREFERENCE, "") = BT.KJV, "default: nothing remembered, the KJV");
         Check (BT.Default_Translation (BT.NO_PREFERENCE, "WEB") = BT.WEB, "default: the remembered WEB");
         Check (BT.Default_Translation (BT.NO_PREFERENCE, "XYZ") = BT.KJV, "default: an unknown name, the KJV");
         Check (BT.Default_Translation ((Known => True, Value => BT.Latin), "") = BT.VUL, "default: Latin preferred, VUL");
         Check (BT.Default_Translation ((Known => True, Value => BT.Latin), "BSB") = BT.VUL,
                "default: Latin preferred over a remembered English one");
         Check (BT.Default_Translation ((Known => True, Value => BT.English), "BSB") = BT.BSB,
                "default: the remembered one in the preferred language");
         Check (BT.Default_Translation ((Known => True, Value => BT.English), "VUL") = BT.KJV,
                "default: English preferred, the first English one");
      end;
   end Test_Versification;

   ---------------------------------------------------------------------------
   --  Damaged and truncated images: every read is checked, nothing raises.
   ---------------------------------------------------------------------------
   procedure Exercise (Data : Byte_Array; Opened_Count, Indexed_Count : in out Natural) is
      L : Layout;
      O : Open_Status;
      I : Book_Index;
      S : Index_Status;
      Back : Reference;
      Found_Ref : Boolean;
      Verse_Ok : Boolean;
      V : Image_Size;
      Q : Bible_Search.Query;
      N : Bible_Search.Normalize_Status;
      Found, Total : Natural;
      Status : Bible_Search.Search_Status;
      Content : Text_Slice;
   begin
      Open (Data, L, O);
      if O /= Bible_Images.Opened then
         return;
      end if;
      Opened_Count := Opened_Count + 1;
      Index (Data, L, I, S);
      if S = Index_Read then
         Indexed_Count := Indexed_Count + 1;
      end if;
      for Row in 0 .. Bible_Images.Count (L, Verses) - 1 loop
         declare
            VR : constant Verse_Record := Read_Verse (Data, L, Row);
         begin
            if VR.Valid then
               for Seg in VR.First_Segment .. VR.First_Segment + VR.Segments - 1 loop
                  declare
                     SR : constant Segment_Record := Read_Segment (Data, L, Seg);
                  begin
                     if SR.Valid then
                        Check (Inside (Data, SR.Content), "segment inside");
                     end if;
                  end;
               end loop;
            end if;
         end;
         if Row mod 997 = 0 then
            Locate (L, I, Row, Back, Found_Ref);
         end if;
      end loop;
      for W in 0 .. Bible_Images.Count (L, Words) - 1 loop
         declare
            WR : constant Word_Record := Read_Word (Data, L, W);
         begin
            if WR.Valid and then WR.Postings > 0 then
               Read_Posting (Data, L, WR.First_Posting, V, Verse_Ok);
            end if;
         end;
      end loop;
      for K in About_Kind loop
         Read_About (Data, L, K, Content, Verse_Ok);
      end loop;
      Bible_Search.Normalize ("lo", Q, N);
      Bible_Search.Search (Data, L, Q, True, Work.all, Hits.all, Found, Total, Status);
      Bible_Search.Normalize ("faith", Q, N);
      Bible_Search.Search (Data, L, Q, False, Work.all, Hits.all, Found, Total, Status);
   end Exercise;

   procedure Test_Damage (Rounds : Positive) is
      Original : constant Image_Access := Load_Bytes (Directory & "/bsb.bible");
      Copy : constant Image_Access := new Byte_Array (Original'Range);
      subtype Position is Natural range 0 .. Original'Length - 1;
      package Random_Position is new Ada.Numerics.Discrete_Random (Position);
      package Random_Byte is new Ada.Numerics.Discrete_Random (Unsigned_8);
      subtype Header_Position is Natural range 0 .. HEADER_BYTES + 2_000;
      package Random_Header is new Ada.Numerics.Discrete_Random (Header_Position);
      G : Random_Position.Generator;
      B : Random_Byte.Generator;
      H : Random_Header.Generator;
      Opened_Count, Indexed_Count : Natural := 0;
      Truncated_Opened : Natural := 0;
      Ignore_Opened, Ignore_Indexed : Natural := 0;
   begin
      Random_Position.Reset (G, Seed);
      Random_Byte.Reset (B, Seed + 2);
      Random_Header.Reset (H, Seed + 3);
      for Round in 1 .. Rounds loop
         Copy.all := Original.all;
         for Flip in 1 .. 64 loop
            --  In even rounds half the flips land in the header and the first
            --  rows, where they matter most; odd rounds flip anywhere (most
            --  then still index, and every read goes deep).
            if Round mod 2 = 0 and then Flip mod 2 = 0 then
               Copy (Random_Header.Random (H)) := Random_Byte.Random (B);
            else
               Copy (Random_Position.Random (G)) := Random_Byte.Random (B);
            end if;
         end loop;
         Exercise (Copy.all, Opened_Count, Indexed_Count);
      end loop;
      Put_Line ("  damage:" & Rounds'Image & " rounds," & Opened_Count'Image & " opened," & Indexed_Count'Image &
                " indexed");
      --  Every truncation of the header and the first tables.
      for Length in 0 .. HEADER_BYTES + 64 loop
         declare
            Short : constant Byte_Array (0 .. Length - 1) := Original (0 .. Length - 1);
            L : Layout;
            O : Open_Status;
         begin
            Open (Short, L, O);
            if O = Bible_Images.Opened then
               Truncated_Opened := Truncated_Opened + 1;
            end if;
         end;
      end loop;
      Check (Truncated_Opened = 0, "no truncated image opens");
      declare
         Short : constant Byte_Array := Original (0 .. Original'Length - 2);
         L : Layout;
         O : Open_Status;
      begin
         Open (Short, L, O);
         Check (O = Bad_Table, "an image one byte short is refused: " & O'Image);
         Exercise (Short, Ignore_Opened, Ignore_Indexed);
      end;
      Check (Opened_Count > 0, "some damaged images still open (and are read safely)");
   end Test_Damage;

begin
   Test_Images;
   Test_Parser;
   Fuzz_Parser (300_000);
   Test_Search;
   Test_Text;
   Test_Versification;
   Test_Damage (150);
   Put_Line ("bible tests:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Bible_Tests;
