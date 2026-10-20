with Interfaces;
with CCL.Catalog;
with CCL.Objects;
with CCL.Types;

--  The lexicon: dictionary, thesaurus and spelling (docs/lexicon.md).
--  (dict "cataract") is the word's entry with its senses, (thesaurus
--  "wharf") its synonym groups, (synonym "wharf") just the words, and
--  (spell-check "Teh text") the misspelt words with near spellings. A word
--  not in the lexicon is a typed Not_Found with near spellings, never an
--  error. lexicon.suggest, lexicon.known, lexicon.dict-page and
--  lexicon.thesaurus-page complete the interface.
--
--  The four common operations are commands: callable by their bare names
--  as well as lexicon.dict and so on (CCL.Catalog.Mark_Command).
--
--  A host answers from the lexicon service (lexicon.svc) natively, or from
--  the image file in hosted tests; this package holds only the types, the
--  catalog publication and the sizes of results.
package CCL.Interfaces.Lexicon with SPARK_Mode is
   use Standard.Interfaces;

   --  The types, as CCL source; the digest and keys are SHA-256 of it (then
   --  "#" and the type's name); tests/ccl-console checks them.
   TYPE_SOURCE : constant String :=
     "(type Part_Of_Speech (enum Noun Verb Adjective Adverb)) (type Word_Sense (record (part " &
     "Part_Of_Speech) (definition String) (examples (List String)) (synonyms (List String)) " &
     "(antonyms (List String)))) (type Word_Entry (record (word String) (senses (List " &
     "Word_Sense)) (also (List String)) (first_sense Integer) (total_senses Integer))) (type " &
     "Near_Spellings (record (word String) (suggestions (List String)))) (type Definition " &
     "(variant (Found Word_Entry) (Not_Found Near_Spellings))) (type Synonym_Group (record " &
     "(part Part_Of_Speech) (meaning String) (synonyms (List String)) (antonyms (List String)) " &
     "(related (List String)) (broader (List String)))) (type Thesaurus_Entry (record (word " &
     "String) (groups (List Synonym_Group)) (also (List String)) (first_group Integer) " &
     "(total_groups Integer))) (type Thesaurus_Result (variant (Found Thesaurus_Entry) " &
     "(Not_Found Near_Spellings))) (type Synonym_List (record (word String) (synonyms (List " &
     "String)) (antonyms (List String)))) (type Synonyms (variant (Found Synonym_List) " &
     "(Not_Found Near_Spellings))) (type Misspelling (record (word String) (offset Integer) " &
     "(suggestions (List String)))) (type Page_Query (record (word String) (first Integer)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#D404_0E91_EB2E_1064#,
      16#4E08_4AB5_7E56_3702#,
      16#30A9_8295_953E_F55D#,
      16#0A94_F770_1E19_70DF#];
   DEFINITION_KEY : constant CCL.Objects.Schema_Key :=
     [16#E09A_1C3A_5A7E_DA3A#,
      16#A5B6_4FD0_F208_C23A#,
      16#A9DE_9A6D_0FD4_C629#,
      16#DBAD_9DEE_106D_475D#];
   THESAURUS_KEY : constant CCL.Objects.Schema_Key :=
     [16#B8DC_0CE3_ED32_ED9C#,
      16#691E_3E61_4FF2_968D#,
      16#DBD8_513A_41B3_C132#,
      16#A6B2_1D6F_1CFD_A2F1#];
   SYNONYMS_KEY : constant CCL.Objects.Schema_Key :=
     [16#FB55_0FF6_F768_9A58#,
      16#CCD4_58AC_DA47_00C1#,
      16#695C_546A_A5FC_A32B#,
      16#07D5_CF8E_ED90_3581#];
   SPELLINGS_KEY : constant CCL.Objects.Schema_Key :=
     [16#9B6A_5298_4975_7F9E#,
      16#8953_2767_D07A_971D#,
      16#600B_9589_CB13_46D9#,
      16#D421_DC40_A5A9_AFBE#];
   MISSPELLINGS_KEY : constant CCL.Objects.Schema_Key :=
     [16#7083_977D_4447_F340#,
      16#80F7_15E6_F79E_8B5A#,
      16#F31B_9E5F_240C_7064#,
      16#63DB_9ABC_5373_61A0#];
   PAGE_KEY : constant CCL.Objects.Schema_Key :=
     [16#C59C_5092_6B86_63B8#,
      16#7B6C_0E22_7F56_7D66#,
      16#9328_14A7_9C4F_B966#,
      16#4EEC_694C_1BEE_415A#];

   --  The members, in declaration order (Variant_Cell is 1-based).
   type Part_Of_Speech is (Noun, Verb, Adjective, Adverb);
   type Found_Choice is (Found, Not_Found);
   WORD_SENSE_FIELDS : constant := 5;
   WORD_ENTRY_FIELDS : constant := 5;
   NEAR_SPELLINGS_FIELDS : constant := 2;
   SYNONYM_GROUP_FIELDS : constant := 6;
   THESAURUS_ENTRY_FIELDS : constant := 5;
   SYNONYM_LIST_FIELDS : constant := 3;
   MISSPELLING_FIELDS : constant := 3;
   PAGE_FIELDS : constant := 2;

   --  A word argument: the lexicon's longest key (Lexicon_Images).
   MAXIMUM_WORD : constant := 255;
   --  A text to check: what one host text value holds.
   MAXIMUM_TEXT : constant := 1_024;
   --  Near spellings shown for one word.
   SUGGESTIONS : constant := 8;

   type Operation is (Dict, Synonym, Thesaurus, Spell_Check, Suggest, Known, Dict_Page, Thesaurus_Page);
   function Name (Item : Operation) return String is
     (case Item is
         when Dict => "dict", when Synonym => "synonym", when Thesaurus => "thesaurus",
         when Spell_Check => "spell-check", when Suggest => "suggest", when Known => "known",
         when Dict_Page => "dict-page", when Thesaurus_Page => "thesaurus-page");
   --  The operations callable by their bare names.
   function Is_Command (Item : Operation) return Boolean is
     (Item in Dict | Synonym | Thesaurus | Spell_Check);
   FIRST_BINDING : constant Unsigned_32 := 16#000B_0001#;
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));

   type Contracts is record
      Definition, Thesaurus, Synonyms, Spellings, Misspellings, Page : CCL.Objects.Binding;
   end record;
   procedure Define_Types
     (Types : in out CCL.Types.Registry; Bound : out Contracts; Accepted : out Boolean);
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Lexicon;
