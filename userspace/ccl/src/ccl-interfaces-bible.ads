with Interfaces;
with CCL.Binding_Blocks;
with CCL.Catalog;
with CCL.Objects;
with CCL.Types;

--  The Bible (APP-005): typed references, verses, passages and search over
--  the translations CuBit ships (the King James Version with the
--  Apocrypha, the Berean Standard Bible, the World English Bible with the
--  deuterocanon).
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
     "(type Translation (enum KJV BSB WEB)) (type Reference (record (book String) (chapter Integer) " &
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
     [16#9436_17D6_3BA2_2C67#,
      16#396F_2DCA_01DB_9879#,
      16#A8C5_1888_F95B_4AA7#,
      16#EBE9_AEE2_F169_137B#];
   PARSE_KEY : constant CCL.Objects.Schema_Key :=
     [16#34DD_4FF7_2474_0D7B#,
      16#E2B6_6029_7C4A_CAAB#,
      16#EAD9_F767_E7D5_FFBB#,
      16#413C_629D_B129_DF58#];
   VERSE_KEY : constant CCL.Objects.Schema_Key :=
     [16#E387_4B30_213E_CAD1#,
      16#C03C_EB31_70EA_7720#,
      16#CCFC_D2DC_BF2F_A73B#,
      16#B087_B2A2_C537_BD78#];
   PASSAGE_RESULT_KEY : constant CCL.Objects.Schema_Key :=
     [16#E356_41B8_429A_49C0#,
      16#588F_EA15_E5E0_6782#,
      16#FBCC_1E40_8E90_21E7#,
      16#8B62_C915_B90B_FDF4#];
   SEARCH_KEY : constant CCL.Objects.Schema_Key :=
     [16#6E03_A328_1C8C_67E7#,
      16#BDCB_408E_4C85_F106#,
      16#3D48_39C9_AAA2_C93B#,
      16#081C_E121_84BD_3C29#];
   REFERENCE_KEY : constant CCL.Objects.Schema_Key :=
     [16#3E41_DDBA_C5C4_26C3#,
      16#D80B_AB35_7437_96D4#,
      16#3967_9630_B62C_F97F#,
      16#B236_77AE_1003_5870#];
   PASSAGE_KEY : constant CCL.Objects.Schema_Key :=
     [16#8DC7_BEE3_85CB_1C76#,
      16#5B2C_3ECE_52D6_0BC9#,
      16#A8E5_226D_09A4_48D4#,
      16#3858_A088_2F88_404B#];
   VERSE_QUERY_KEY : constant CCL.Objects.Schema_Key :=
     [16#5ED5_04C4_0760_1A3A#,
      16#659F_9CB0_69FF_3B52#,
      16#FAC0_7DB4_0DFD_E7F3#,
      16#1291_FD62_E502_A969#];
   PASSAGE_QUERY_KEY : constant CCL.Objects.Schema_Key :=
     [16#7474_9149_0BD5_6FF8#,
      16#D80F_BDAE_DDF2_333E#,
      16#30B6_EA31_BEDE_964A#,
      16#4FFB_17C5_3685_76F6#];
   SEARCH_QUERY_KEY : constant CCL.Objects.Schema_Key :=
     [16#AD6B_EF70_AC7D_BB2E#,
      16#B095_C0AF_7E29_CF49#,
      16#9808_FAFD_A006_4CEF#,
      16#3DB4_0668_D63A_C4AE#];

   --  The members, in declaration order (Variant_Cell is 1-based).
   type Translation_Member is (KJV, BSB, WEB);
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
