------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Where a place in one translation is in another (APP-005). Each
--  translation numbers its own chapters and verses, and every lookup is in
--  the translation's own numbering (Bible_References.Versification, read
--  from its image): "Psalm 22" in the Vulgate is its Psalm 22, "Dominus
--  regit me". This package carries a reader across when the translation
--  changes.
--
--  Two numberings:
--  - English: the King James Version's (the Berean Standard Bible and the
--    World English Bible share it for every book they have in common with
--    it), Hebrew Psalm numbers, psalm titles not numbered as verses, the
--    Greek additions to Esther and Daniel as books of their own.
--  - Vulgate: the Clementine Vulgate's. The Psalms are numbered as in the
--    Septuagint (Hebrew 9-10 are its 9, 114-115 its 113, 116 its 114-115,
--    147 its 146-147), and a psalm's title is its first verse or two, as
--    in Hebrew. Esther runs on to chapter 16 with the Greek additions
--    (10:4-16:24, the numbering the King James Version's Apocrypha also
--    uses). Daniel holds the Song of the Three (3:24-90), Susanna (13) and
--    Bel and the Dragon (14), so the King James Version's Daniel 3:24-4:3
--    is the Vulgate's 3:91-100 and its 4:4-37 the Vulgate's 4:1-34.
--
--  The table is those rules: the Septuagint psalm numbers, the Hebrew
--  psalm titles' verse counts, and the Greek additions' places. Elsewhere
--  a chapter and verse carry over unchanged; where the Vulgate divides
--  verses differently inside a chapter (Genesis 50 has 25 verses in the
--  Vulgate, 26 in English) the verse can be one off, never the chapter.
--  Results are always numbers a translation could have; whether the target
--  translation has them is Bible_References.Exists' question.
--
--  Proved (SPARK, level 2): no run-time errors for any reference.
------------------------------------------------------------------------------
with Bible_References; use Bible_References;

package Bible_Versification with SPARK_Mode, Pure is

   type Numbering is (English, Vulgate);

   --  The place Item (numbered in From) has in To's numbering.
   function Map (From, To : Numbering; Item : Reference) return Reference;

   --  The Vulgate psalm that holds Hebrew (English) Psalm Hebrew's first
   --  verse, and the Hebrew psalms a Vulgate psalm holds (a heading's
   --  "Psalm 22 (23)", "Psalm 9 (9-10)").
   function Vulgate_Psalm (Hebrew : Chapter_Number) return Chapter_Number
     with Post => Vulgate_Psalm'Result <= Hebrew;
   procedure Hebrew_Psalms (Vulgate : Chapter_Number; First, Last : out Chapter_Number)
     with Post => First <= Last and then Last <= First + 1;

   --  Verses a Hebrew psalm's title takes (0, 1 or 2): Hebrew and the
   --  Vulgate number them, English translations do not.
   subtype Title_Verses is Natural range 0 .. 2;
   function Title_Verse_Count (Hebrew : Chapter_Number) return Title_Verses;

end Bible_Versification;
