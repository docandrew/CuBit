------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Exact quantiles of one pixel column's raw samples: nearest rank,
--  selected in place (expected linear time), never sorted.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Charts_Quantiles with SPARK_Mode, Pure is
   type Sample_Array is array (Natural range <>) of Unsigned_64;

   --  Quantiles in thousandths: 500 is the median, 990 the 99th percentile.
   Permille_Scale : constant := 1_000;
   subtype Permille is Natural range 1 .. Permille_Scale;
   Median : constant Permille := 500;
   P90 : constant Permille := 900;
   P99 : constant Permille := 990;

   --  The nearest rank of quantile Q among Count samples, from 1:
   --  ceil (Q * Count / 1000).
   function Rank (Q : Permille; Count : Positive) return Positive is
     (Positive'Max (1, Positive'Min (Count, Natural ((Long_Long_Integer (Q) * Long_Long_Integer (Count)
       + Permille_Scale - 1) / Permille_Scale))))
     with Pre => Count <= Natural'Last / Permille_Scale, Post => Rank'Result <= Count;

   --  Reorder Items so that Items (Items'First + K - 1) holds the K-th
   --  smallest value (K from 1), and return it.
   procedure Select_Kth (Items : in out Sample_Array; K : Positive; Result : out Unsigned_64)
     with Pre => Items'Length >= 1 and then K <= Items'Length and then Items'Last < Natural'Last;
end Charts_Quantiles;
