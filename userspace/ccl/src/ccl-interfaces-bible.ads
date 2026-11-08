with Interfaces;
with CCL.Binding_Blocks;
with CCL.Catalog;
with CCL.Objects;
with CCL.Types;

--  The Bible (APP-005): typed references, verses, passages and search over
--  the translations CuBit ships (the King James Version with the
--  Apocrypha, the Berean Standard Bible, the World English Bible with the
--  deuterocanon, the Clementine Vulgate in Latin; its verse text comes back
--  in ASCII, the ligature ae as "ae").
--
--    (bible.parse "John 3:16")                 Parse_Result: the passage, or why not
--    (bible.verse (Reference "John" 3 16))     Verse_Result, in the KJV
--    (bible.verse-in (Verse_Query (Reference "John" 3 16) Translation.WEB))
--    (bible.passage (Passage (Reference "John" 3 16) (Reference "John" 3 18)))
--    (bible.passage-in (Passage_Query ... Translation.BSB))
--    (bible.read "1 Cor 13")                   Passage_Result: parse, then the passage
--    (bible.search "faith hope love")          Search_Result: verses with every word
--    (bible.search-in (Search_Query "faith" Translation.BSB))
--
--  A Reference's book is a name the proved parser accepts ("John", "Jn",
--  "1 Cor"); the reference is checked against the translation's own
--  versification. A wrong one is a typed Refusal naming the unknown book or
--  the out-of-range chapter or verse, never an error.
--
--  CCL has no overloading yet (CCL-009): verse and verse-in, passage,
--  passage-in and read, search and search-in are one operation each once it
--  has, (bible.verse "John 3:16") among them.
package CCL.Interfaces.Bible with SPARK_Mode is

   --  What Publish adds to a catalog (CCL_Host_Environment sums these).
   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 9;
   PUBLISHED_TYPES : constant := 15;
   use Standard.Interfaces;

   --  The types, as CCL source; the digest and keys are SHA-256 of it (then
   --  "#" and the type's name); tests/ccl-console checks them.
   TYPE_SOURCE : constant String :=
     "(type Translation (enum KJV BSB WEB VUL)) (type Reference (record (book String) (chapter Integer) " &
     "(verse Integer))) (type Passage (record (from Reference) (to Reference))) (type Parse_Error (enum " &
     "Empty Too_Long Unknown_Book Ambiguous_Book Malformed Too_Large Not_In_Translation No_Such_Chapter " &
     "No_Such_Verse Reversed Omitted)) (type Refusal (record (error Parse_Error) (message String) (book " &
     "String) (value Integer) (limit Integer))) (type Parse_Result (variant (Parsed Passage) (Refused " &
     "Refusal))) (type Verse (record (reference Reference) (text String))) (type Verse_Result (variant " &
     "(Found Verse) (Refused Refusal))) (type Verse_Query (record (reference Reference) (translation " &
     "Translation))) (type Passage_Query (record (passage Passage) (translation Translation))) (type " &
     "Passage_Text (record (passage Passage) (translation Translation) (verses (List Verse)) (total " &
     "Integer))) (type Passage_Result (variant (Found Passage_Text) (Refused Refusal))) (type " &
     "Search_Query (record (words String) (translation Translation))) (type Search_Result (record (words " &
     "String) (translation Translation) (hits (List Verse)) (total Integer)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#A3F1_0B96_FAC1_5827#,
      16#3A07_8F69_CA04_29BD#,
      16#0DDD_F0CA_ABB7_D432#,
      16#1FD1_B78E_E016_C86C#];
   PARSE_KEY : constant CCL.Objects.Schema_Key :=
     [16#4BA2_3F82_8178_ED16#,
      16#6B17_117E_D44D_A6CC#,
      16#4566_30B0_18EF_F3A1#,
      16#44C0_B826_EDE4_A70D#];
   VERSE_KEY : constant CCL.Objects.Schema_Key :=
     [16#EE77_37A9_A047_7D7D#,
      16#665E_6B8C_3EEA_4A22#,
      16#8204_7CB6_E0B4_4D2D#,
      16#0339_4313_D63D_6096#];
   PASSAGE_RESULT_KEY : constant CCL.Objects.Schema_Key :=
     [16#1AE9_FAC8_326A_D52F#,
      16#A6D1_7BA4_715E_2462#,
      16#82E6_E65F_5F03_2018#,
      16#2E7F_8CD0_E0F8_C3BA#];
   SEARCH_KEY : constant CCL.Objects.Schema_Key :=
     [16#457B_1E13_F9A3_0F3B#,
      16#A278_82FE_E79A_EA1E#,
      16#5B56_D931_13D3_5D45#,
      16#43C1_39E5_918A_D086#];
   REFERENCE_KEY : constant CCL.Objects.Schema_Key :=
     [16#8CA8_1D11_E38E_BDF9#,
      16#39B7_3EE9_F6C6_BFA2#,
      16#AA93_6D44_BC5A_D5F4#,
      16#DCDF_BB9E_A358_E578#];
   PASSAGE_KEY : constant CCL.Objects.Schema_Key :=
     [16#5D25_81E3_46E1_B49F#,
      16#D217_9211_428D_4439#,
      16#5D27_E466_0DEA_EAA8#,
      16#C541_8B48_8E3E_A875#];
   VERSE_QUERY_KEY : constant CCL.Objects.Schema_Key :=
     [16#6065_964D_B8AE_8D0D#,
      16#0389_6E55_1371_FA4D#,
      16#EE0A_A573_8207_CF77#,
      16#6AA5_90B9_008A_E986#];
   PASSAGE_QUERY_KEY : constant CCL.Objects.Schema_Key :=
     [16#1F57_6A97_AB8E_87BE#,
      16#3036_0BE2_0204_F90C#,
      16#13DD_8249_35C7_071C#,
      16#E95C_3CEA_BB7A_2639#];
   SEARCH_QUERY_KEY : constant CCL.Objects.Schema_Key :=
     [16#6A7F_1479_C004_5187#,
      16#B82D_2201_0D6A_F650#,
      16#25D0_CA5D_0920_F66E#,
      16#84BF_B051_592A_5D01#];

   --  The members, in declaration order (Variant_Cell is 1-based).
   type Translation_Member is (KJV, BSB, WEB, VUL);
   type Parse_Error is
     (Empty, Too_Long, Unknown_Book, Ambiguous_Book, Malformed, Too_Large, Not_In_Translation,
      No_Such_Chapter, No_Such_Verse, Reversed, Omitted);
   type Outcome_Choice is (Found, Refused);
   REFERENCE_FIELDS : constant := 3;
   PASSAGE_FIELDS : constant := 2;
   REFUSAL_FIELDS : constant := 5;
   VERSE_FIELDS : constant := 2;
   QUERY_FIELDS : constant := 2;
   PASSAGE_TEXT_FIELDS : constant := 4;
   SEARCH_RESULT_FIELDS : constant := 4;

   --  A text argument: a reference or a query.
   MAXIMUM_TEXT : constant := 256;

   type Operation is (Parse, Verse, Verse_In, Passage, Passage_In, Read, Search, Search_In);
   function Name (Item : Operation) return String is
     (case Item is
         when Parse => "parse", when Verse => "verse", when Verse_In => "verse-in",
         when Passage => "passage", when Passage_In => "passage-in", when Read => "read",
         when Search => "search", when Search_In => "search-in");
   FIRST_BINDING : constant Unsigned_32 := CCL.Binding_Blocks.First (CCL.Binding_Blocks.Bible);
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));

   type Contracts is record
      Parse, Verse, Passage_Result, Search, Reference, Passage, Verse_Query, Passage_Query,
      Search_Query : CCL.Objects.Binding;
   end record;
   procedure Define_Types
     (Types : in out CCL.Types.Registry; Bound : out Contracts; Accepted : out Boolean);
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Bible;
