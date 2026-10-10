with Interfaces;
-- Identity and change extent of a client surface's pixels, independent of
-- the CPU mapping that currently carries them. A persistent GPU copy is
-- keyed by Source_Key; Content_Version names the pixels a draw expects; a
-- Row_Band names the source rows that changed since an earlier version.
package Compositor_Source_Content with SPARK_Mode, Pure is
   type Source_Key is new Interfaces.Unsigned_64;
   No_Key : constant Source_Key := 0;
   type Content_Version is new Interfaces.Unsigned_64;
   No_Version : constant Content_Version := 0;
   Maximum_Rows : constant := 65_535;
   subtype Row is Natural range 0 .. Maximum_Rows;
   -- Source rows First .. Last - 1. Every empty band is normalized to
   -- Empty_Band so equality means equal row sets.
   type Row_Band is record
      First, Last : Row := 0;
   end record;
   Empty_Band : constant Row_Band := (0, 0);
   function Is_Empty (B : Row_Band) return Boolean is (B.First >= B.Last);
   function Normal (B : Row_Band) return Boolean is
     (if Is_Empty (B) then B = Empty_Band);
   function Contains (B : Row_Band; R : Row) return Boolean is
     (B.First <= R and R < B.Last);
   -- Every row of Inner is a row of Outer.
   function Covers (Outer, Inner : Row_Band) return Boolean is
     (Is_Empty (Inner) or else (Outer.First <= Inner.First and Inner.Last <= Outer.Last));
   function Band (First, Last : Row) return Row_Band is
     (if First < Last then (First, Last) else Empty_Band)
     with Post => Normal (Band'Result) and
       (for all R in Row => Contains (Band'Result, R) = (First <= R and R < Last));
   -- Smallest single band covering both. Bands are conservative: rows
   -- between two disjoint bands are uploaded again, never skipped.
   function Union (A, B : Row_Band) return Row_Band is
     (if Is_Empty (A) then (if Is_Empty (B) then Empty_Band else B)
      elsif Is_Empty (B) then A
      else (Natural'Min (A.First, B.First), Natural'Max (A.Last, B.Last)))
     with Post => Normal (Union'Result) and
       (if Normal (A) and Normal (B) then Covers (Union'Result, A) and Covers (Union'Result, B)) and
       (if not Is_Empty (A) or not Is_Empty (B) then not Is_Empty (Union'Result));
   -- Rows of B inside an image of Height rows.
   function Clip (B : Row_Band; Height : Row) return Row_Band is
     (Band (B.First, Natural'Min (B.Last, Height)))
     with Post => Normal (Clip'Result) and Clip'Result.Last <= Height and
       (for all R in Row => Contains (Clip'Result, R) = (Contains (B, R) and R < Height));
   function Whole (Height : Row) return Row_Band is (Band (0, Height))
     with Post => Normal (Whole'Result) and
       (for all R in Row => Contains (Whole'Result, R) = (R < Height));
end Compositor_Source_Content;
