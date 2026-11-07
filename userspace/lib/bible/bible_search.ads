------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Full-text search of a Bible image (APP-005) over its prebuilt index:
--  every verse holding all of the query's words, in reading order.
--
--  Normalize makes a query's words the image's search keys, exactly as
--  tools/build_bible.py makes them from the verses: ASCII letters and
--  digits in lower case, apostrophes inside a word dropped ("Lord's" is
--  "lords"), the few accented letters folded (ae, a, e, o), anything else a
--  separator. With Prefix_Last the last word matches every key it starts
--  ("belie" finds believe, believed, belief): search as the user types.
--
--  Each word's verses are marked in a verse set (one bit per verse row)
--  and the sets intersected, so the cost is the postings read, bounded by
--  the index, never the text. Proved free of run-time errors on any image
--  and any query (SPARK, level 2).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Bible_Images; use Bible_Images;

package Bible_Search with SPARK_Mode, Pure is

   MAXIMUM_KEY : constant := Bible_Images.MAXIMUM_KEY_BYTES;
   --  Words in one query, and its length; more is a typed refusal.
   MAXIMUM_TERMS : constant := 8;
   MAXIMUM_QUERY : constant := 256;

   subtype Term_Length is Natural range 0 .. MAXIMUM_KEY;
   subtype Term_Count is Natural range 0 .. MAXIMUM_TERMS;
   subtype Term_Index is Positive range 1 .. MAXIMUM_TERMS;

   type Term is record
      Length : Term_Length := 0;
      Text   : String (1 .. MAXIMUM_KEY) := [others => ' '];
   end record;
   type Term_Array is array (Term_Index) of Term;

   type Query is record
      Terms : Term_Array;
      Count : Term_Count := 0;
   end record;

   type Normalize_Status is (Normalized, Empty_Query, Query_Too_Long, Too_Many_Terms, Term_Too_Long);

   procedure Normalize (Text : String; Item : out Query; Status : out Normalize_Status)
     with Post => (if Status = Normalized then Item.Count >= 1
                   and then (for all I in 1 .. Item.Count => Item.Terms (I).Length >= 1));

   --  The verse rows one search covers (all of every supported translation:
   --  the World English Bible with the deuterocanon has 38,074).
   MAXIMUM_VERSES : constant := 65_536;
   SET_WORDS : constant := MAXIMUM_VERSES / 64;
   subtype Set_Index is Natural range 0 .. SET_WORDS - 1;
   type Verse_Set is array (Set_Index) of Unsigned_64;

   --  A caller's scratch (16 KiB): kept between searches, never allocated
   --  by one.
   type Work_Area is record
      Matched, Current : Verse_Set := [others => 0];
   end record;

   type Hit_Array is array (Positive range <>) of Image_Size;

   type Search_Status is
     (Searched,          --  Hits (1 .. Found) of Total, in reading order
      Too_Many_Verses,   --  the image has more verse rows than MAXIMUM_VERSES
      Damaged_Index);    --  a word or posting record failed its checks

   procedure Search
     (Data : Byte_Array; L : Layout; Item : Query; Prefix_Last : Boolean; Work : in out Work_Area;
      Hits : out Hit_Array; Found, Total : out Natural; Status : out Search_Status)
     with Pre  => Usable (Data, L) and then Hits'First = 1 and then Item.Count >= 1,
          Post => Found <= Hits'Length and then Found <= Total
                  and then (for all I in 1 .. Found => Hits (I) < Count (L, Verses));

end Bible_Search;
