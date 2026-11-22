--  Hosted tests of the lexicon core (docs/lexicon.md): lookups against the
--  real WordNet/SCOWL image, paging equivalence, spelling checks, and
--  damaged images and answers (which must never fault). Linux only.
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Lexicon_Answers; use Lexicon_Answers;
with Lexicon_Images; use Lexicon_Images;
with Lexicon_Lookup; use Lexicon_Lookup;
with Lexicon_Support; use Lexicon_Support;
with Lexicon_Wire; use Lexicon_Wire;

procedure Lexicon_Tests is
   Failures : Natural := 0;
   Checks : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   Path : constant String :=
     (if Ada.Environment_Variables.Exists ("LEXICON_IMAGE")
      then Ada.Environment_Variables.Value ("LEXICON_IMAGE") else "build/english.lexicon");
   Image : constant Image_Access := Load (Path);
   L : Layout;
   Status : Open_Status;

   function K (S : String) return Key is
      Result : Key;
      N : Normal_Status;
   begin
      Normalize (S, Result, N);
      return Result;
   end K;

   function Text_Of (Slice : Text_Slice) return String is
      Result : String (1 .. Slice.Length);
   begin
      for I in Result'Range loop
         Result (I) := Character'Val (Image (Slice.First + I - 1));
      end loop;
      return Result;
   end Text_Of;

   Big_Page : constant := 2 ** 20;

   type Key_Array is array (Positive range <>) of Key;
   type String_Access is access constant String;
   type String_Array is array (Positive range <>) of String_Access;

   function Ask (Op : Operation; Text : String; Suggestions : Suggestion_Count := 8) return Item_Lists.Vector is
      Items : Item_Lists.Vector;
      Pages : Natural;
   begin
      Ask_All (Image.all, L, Op, Text, Big_Page, Suggestions, Items, Pages);
      return Items;
   end Ask;

   procedure Test_Normalize is
      R : Key;
      N : Normal_Status;
   begin
      Normalize ("  Ice_Cream  ", R, N);
      Check (N = Normalized and then R.Text (1 .. R.Length) = "ice cream", "normalize multiword");
      Normalize ("dog" & Character'Val (16#E2#) & Character'Val (16#80#) & Character'Val (16#99#) & "s",
                 R, N);
      Check (N = Normalized and then R.Text (1 .. R.Length) = "dog's", "normalize typographic apostrophe");
      Normalize ("   ", R, N);
      Check (N = Empty, "normalize empty");
      Normalize ([1 .. 300 => 'a'], R, N);
      Check (N = Too_Long, "normalize too long");
   end Test_Normalize;

   procedure Test_Find is
      Found : Boolean;
      W : Word_Id;
   begin
      for Word of Key_Array'[K ("cataract"), K ("wharf"), K ("accipitrine"), K ("the"), K ("ice cream")] loop
         Find (Image.all, L, Word, Found, W);
         Check (Found, "find " & Word.Text (1 .. Word.Length));
      end loop;
      Find (Image.all, L, K ("cataract"), Found, W);
      Check (Found and then Sense_Count (Image.all, L, W) = 2, "cataract has 2 senses");
      Find (Image.all, L, K ("the"), Found, W);
      Check (Found and then Sense_Count (Image.all, L, W) = 0, "'the' is a spelling without senses");
      Find (Image.all, L, K ("zzzzqx"), Found, W);
      Check (not Found, "unknown word not found");
      Find (Image.all, L, K ("aa"), Found, W);
      Check (Found, "first words found");
   end Test_Find;

   procedure Test_Forms is
      Forms : Word_List (1 .. MAXIMUM_FORMS);
      N : Natural;
      function Has_Form (Word : String) return Boolean is
      begin
         for I in 1 .. N loop
            if Text_Of (Key_Of (Image.all, L, Forms (I))) = Word then
               return True;
            end if;
         end loop;
         return False;
      end Has_Form;
   begin
      Base_Forms (Image.all, L, K ("geese"), Forms, N);
      Check (Has_Form ("goose"), "geese -> goose");
      Base_Forms (Image.all, L, K ("running"), Forms, N);
      Check (N >= 2 and then Text_Of (Key_Of (Image.all, L, Forms (1))) = "running" and then Has_Form ("run"),
             "running -> running, run");
      Base_Forms (Image.all, L, K ("better"), Forms, N);
      Check (Has_Form ("good") and then Has_Form ("well"), "better -> good, well");
      Base_Forms (Image.all, L, K ("churches"), Forms, N);
      Check (Has_Form ("church"), "churches -> church");
      Base_Forms (Image.all, L, K ("zzzzqx"), Forms, N);
      Check (N = 0, "no forms for nonsense");
   end Test_Forms;

   procedure Test_Known is
   begin
      for Word of Key_Array'[K ("the"), K ("and"), K ("running"), K ("dog's"), K ("dogs'"), K ("colour"), K ("color"),
                   K ("receive"), K ("accipitrine"), K ("ice cream"), K ("geese")] loop
         Check (Known (Image.all, L, Word), "known " & Word.Text (1 .. Word.Length));
      end loop;
      for Word of Key_Array'[K ("teh"), K ("recieve"), K ("jumpd"), K ("zzzzqx")] loop
         Check (not Known (Image.all, L, Word), "unknown " & Word.Text (1 .. Word.Length));
      end loop;
   end Test_Known;

   procedure Test_Suggest is
      Near : Near_Spelling_List (1 .. 8);
      N : Natural;
   begin
      Suggest (Image.all, L, K ("recieve"), 2, Near, N);
      Check (N > 0 and then Text_Of (Key_Of (Image.all, L, Near (1).Word)) = "receive",
             "recieve -> receive first");
      Suggest (Image.all, L, K ("thesaurs"), 2, Near, N);
      Check ((for some I in 1 .. Natural'Min (N, 3) => Text_Of (Key_Of (Image.all, L, Near (I).Word)) = "thesaurus"),
             "thesaurs -> thesaurus in the first three");
      for I in 2 .. N loop
         Check (Near (I - 1).Distance <= Near (I).Distance, "suggestions nearest first");
      end loop;
      Suggest (Image.all, L, K ("x"), 1, Near (1 .. 0), N);
      Check (N = 0, "no room, no suggestions");
   end Test_Suggest;

   procedure Test_Define is
      Items : Item_Lists.Vector := Ask (Define, "cataract");
   begin
      Check (Has (Items, Query, "cataract"), "define: query echoed");
      Check (Has (Items, Headword, "cataract"), "define: headword");
      Check (Count_Items (Items, Sense_Start) = 2, "define cataract: 2 senses, got"
             & Natural'Image (Count_Items (Items, Sense_Start)));
      Check (Count_Items (Items, Definition) = 2, "define cataract: 2 definitions");
      Check (not Has (Items, Narrower, "cortical cataract"), "define cataract: no narrower terms in Define");
      Put_Line ("  cataract: " & Texts (Items, Definition));

      Items := Ask (Define, "accipitrine");
      Check (Count_Items (Items, Sense_Start) = 1 and then Items (3).Number = 1
             and then Items (4).Number = Part_Of_Speech'Pos (Adjective),
             "accipitrine: one adjective sense");
      Put_Line ("  accipitrine: " & Texts (Items, Definition));

      Items := Ask (Thesaurus, "cataract");
      Check (Has (Items, Narrower, "cortical cataract") and then Has (Items, Broader, "waterfall"),
             "thesaurus cataract: narrower cortical cataract, broader waterfall");

      Items := Ask (Define, "hot");
      Check (Has (Items, Antonym, "cold"), "hot: antonym cold");

      Items := Ask (Define, "geese");
      Check (Has (Items, Headword, "goose"), "geese: entry for goose");

      Items := Ask (Define, "recieve");
      Check (Count_Items (Items, Headword) = 0 and then Has (Items, Suggestion, "receive"),
             "recieve: not found, suggests receive");

      Items := Ask (Define, "Paris");
      Check (Has (Items, Headword, "Paris"), "Paris: headword keeps its capital");
   end Test_Define;

   procedure Test_Thesaurus is
      Items : constant Item_Lists.Vector := Ask (Thesaurus, "wharf");
   begin
      Check (Has (Items, Synonym, "pier") and then Has (Items, Synonym, "dock")
             and then Has (Items, Synonym, "wharfage"), "wharf: pier, dock, wharfage");
      Check (Has (Items, Broader, "platform"), "wharf: broader platform");
      Check (Count_Items (Items, Narrower) > 0, "wharf: narrower terms");
      Put_Line ("  wharf synonyms: " & Texts (Items, Synonym));
      declare
         Accipitrine : constant Item_Lists.Vector := Ask (Thesaurus, "accipitrine");
      begin
         Check (Count_Items (Accipitrine, Sense_Start) = 1, "thesaurus accipitrine answers");
      end;
   end Test_Thesaurus;

   --  Small pages give the same items as one large page, for every
   --  operation and many page sizes.
   procedure Test_Paging is
      Words : constant String_Array :=
        [new String'("break"), new String'("set"), new String'("person"), new String'("good"),
         new String'("run")];
      Whole, Paged : Item_Lists.Vector;
      Pages : Natural;
      use type Item_Lists.Vector;
      type Size_Array is array (Positive range <>) of Positive;
   begin
      for Op in Define .. Thesaurus loop
         for W of Words loop
            Ask_All (Image.all, L, Op, W.all, Big_Page, 8, Whole, Pages);
            for Size of Size_Array'[MINIMUM_PAGE_BYTES, MINIMUM_PAGE_BYTES + 37, 4_000] loop
               Ask_All (Image.all, L, Op, W.all, Size, 8, Paged, Pages);
               Check (Paged = Whole, "paging " & Operation'Image (Op) & " " & W.all & " size"
                      & Natural'Image (Size));
            end loop;
         end loop;
      end loop;
      Ask_All (Image.all, L, Thesaurus, "person", MINIMUM_PAGE_BYTES, 8, Paged, Pages);
      Put_Line ("  thesaurus person: " & Natural'Image (Natural (Paged.Length)) & " items in"
                & Natural'Image (Pages) & " minimum pages");
   end Test_Paging;

   procedure Test_Check is
      Apostrophe : constant String := Character'Val (16#E2#) & Character'Val (16#80#) & Character'Val (16#99#);
      Text : constant String :=
        "Teh quick brown fox jumpd over the lazy dog's back; it" & Apostrophe & "s well-known, "
        & "isn't it? Recieve 42 items from the U.S. and colour them.";
      Items : Item_Lists.Vector;
      Pages : Natural;
      Small : Item_Lists.Vector;
      Spans : Span_List (1 .. 8);
      N : Natural;
      Next : Positive;
      use type Item_Lists.Vector;
   begin
      Ask_All (Image.all, L, Check, Text, Big_Page, 4, Items, Pages);
      Check (Count_Items (Items, Misspelling) = 3, "check: 3 misspellings, got"
             & Natural'Image (Count_Items (Items, Misspelling)));
      for D of Items loop
         if D.Kind = Misspelling then
            Put_Line ("  misspelt: " & Text (Text'First + Natural (D.Number) ..
                                             Text'First + Natural (D.Number) + Natural (D.Span_Length) - 1));
         end if;
      end loop;
      Check (Has (Items, Suggestion, "the") and then Has (Items, Suggestion, "jumped")
             and then Has (Items, Suggestion, "receive"), "check: suggestions the, jumped, receive");
      Ask_All (Image.all, L, Check, Text, MINIMUM_PAGE_BYTES, 4, Small, Pages);
      Check (Small = Items, "check: minimum pages give the same answer");
      --  A paragraph per call: from a later offset, only the later words.
      Check_Text (Image.all, L, Text, Text'First + 10, Spans, N, Next);
      Check (N = 2 and then Next = Text'Last + 1, "check from an offset");
      Check_Text (Image.all, L, Text, Text'First, Spans (1 .. 1), N, Next);
      Check (N = 1 and then Next > Text'First and then Next < Text'Last, "check stops when full");
      Check_Text (Image.all, L, "", 1, Spans, N, Next);
      Check (N = 0 and then Next = 1, "check empty text");
   end Test_Check;

   --  Damaged images: header corruption is refused; random byte damage in
   --  the tables never faults (contracts and checks are on in this build).
   procedure Test_Damage is
      subtype Small_Seed is Integer;
      package Random_Positions is new Ada.Numerics.Discrete_Random (Natural);
      G : Random_Positions.Generator;
      Copy : constant Image_Access := new Byte_Array'(Image.all);
      Damaged_Layout : Layout;
      Damaged_Status : Open_Status;
      Page : Answer_Bytes (0 .. 8191);
      Used : Natural;
      Seed : Small_Seed := 1;
   begin
      if Ada.Command_Line.Argument_Count > 0 then
         Seed := Small_Seed'Value (Ada.Command_Line.Argument (1));
      end if;
      Random_Positions.Reset (G, Seed);
      Open (Copy (0 .. 10), Damaged_Layout, Damaged_Status);
      Check (Damaged_Status = Too_Short, "short image refused");
      Copy (0) := Character'Pos ('X');
      Open (Copy.all, Damaged_Layout, Damaged_Status);
      Check (Damaged_Status = Bad_Magic, "bad magic refused");
      Copy (0) := Image (0);
      Copy (8) := 99;
      Open (Copy.all, Damaged_Layout, Damaged_Status);
      Check (Damaged_Status = Wrong_Version, "wrong version refused");
      Copy (8) := Image (8);
      Copy (12 + 3) := 16#7F#;   --  the word table's offset past the end
      Open (Copy.all, Damaged_Layout, Damaged_Status);
      Check (Damaged_Status = Bad_Table, "table outside the image refused");
      Copy (12 + 3) := Image (12 + 3);
      for Round in 1 .. 200 loop
         for Flip in 1 .. 2_000 loop
            declare
               P : constant Natural := HEADER_BYTES + Random_Positions.Random (G) mod (Copy'Length - HEADER_BYTES);
            begin
               Copy (P) := Unsigned_8 (Random_Positions.Random (G) mod 256);
            end;
         end loop;
         Open (Copy.all, Damaged_Layout, Damaged_Status);
         Check (Damaged_Status = Opened, "damaged tables still open");
         for Word of String_Array'[new String'("cataract"), new String'("break"), new String'("person"),
                                   new String'("wharf"), new String'("recieve"), new String'("a")] loop
            for Op in Operation loop
               Answer (Copy.all, Damaged_Layout, Op, Word.all, 0, 8, Page, Used);
            end loop;
         end loop;
         Answer (Copy.all, Damaged_Layout, Check, "Teh quick brown fox jumpd", 0, 8, Page, Used);
      end loop;
      Check (True, "damaged images never faulted");
   end Test_Damage;

   --  Random answers decode or report damage, never fault.
   procedure Test_Wire is
      package Random_Bytes is new Ada.Numerics.Discrete_Random (Unsigned_8);
      G : Random_Bytes.Generator;
      Page : Answer_Bytes (0 .. 255);
      Items : Item_Lists.Vector;
      Damaged : Boolean;
   begin
      Random_Bytes.Reset (G, 7);
      for Round in 1 .. 20_000 loop
         for B of Page loop
            B := Random_Bytes.Random (G);
         end loop;
         Items.Clear;
         Decode (Page, Items, Damaged);
      end loop;
      Check (True, "random answers never faulted");
   end Test_Wire;

begin
   Open (Image.all, L, Status);
   Check (Status = Opened, "image opens: " & Open_Status'Image (Status));
   if Status /= Opened then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   Put_Line ("lexicon image:" & Natural'Image (Image'Length) & " bytes," & Natural'Image (Word_Count (L))
             & " words");
   Test_Normalize;
   Test_Find;
   Test_Forms;
   Test_Known;
   Test_Suggest;
   Test_Define;
   Test_Thesaurus;
   Test_Paging;
   Test_Check;
   Test_Wire;
   Test_Damage;
   Put_Line ("lexicon tests:" & Natural'Image (Checks) & " checks," & Natural'Image (Failures) & " failures");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Lexicon_Tests;
