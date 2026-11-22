------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Side-by-side parts and the dividers between them (CuBit.UI.Splits):
--  sharing a length out by weight, and a dragged divider moving length
--  between its two neighbours only. Pure policy, proved (tests/ui-popups).
------------------------------------------------------------------------------
package Client_Split_Layout with SPARK_Mode, Pure is
   MAXIMUM_PARTS : constant := 8;
   MAXIMUM_LENGTH : constant := 1_000_000;
   subtype Part_Count is Natural range 0 .. MAXIMUM_PARTS;
   subtype Part_Index is Part_Count range 1 .. MAXIMUM_PARTS;
   subtype Length is Natural range 0 .. MAXIMUM_LENGTH;
   type Lengths is array (Part_Index) of Length;

   --  Total shared among Count parts in proportion to Weights (a zero
   --  weight counts as one), after Count - 1 gaps of Gap; the last part
   --  takes what rounding leaves. Parts past Count are zero.
   procedure Distribute (Total : Length; Count : Part_Count; Gap : Length; Weights : Lengths; Parts : out Lengths)
     with Post => (for all I in Part_Index => (if I > Count then Parts (I) = 0 else Parts (I) <= Total));

   --  The divider after part K dragged so part K is New_Length long, kept
   --  so both neighbours stay at least Minimum (where they can); their sum
   --  and every other part stay the same.
   procedure Drag (Parts : in out Lengths; K : Part_Index; New_Length : Length; Minimum : Length)
     with Pre => K < MAXIMUM_PARTS and then Parts (K) + Parts (K + 1) <= MAXIMUM_LENGTH,
          Post => Parts (K) + Parts (K + 1) = Parts'Old (K) + Parts'Old (K + 1)
                  and then (for all I in Part_Index => (if I /= K and then I /= K + 1 then Parts (I) = Parts'Old (I)));
end Client_Split_Layout;
