------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Sharing a length among the children of a box (CuBit.UI.Boxes, UI-033):
--  each child's minimum first, then toward its preferred length, then what
--  is left to the expanding children by stretch factor. Pure policy, proved
--  at level 2 (tests/ui-layout).
--
--  @description
--  Proved for every input: no two children overlap (each starts Spacing
--  after the previous one ends), every child gets at least its minimum, a
--  child that does not expand gets at most its preferred length (so a
--  Fixed child gets exactly its length), and when the minimums and the
--  spacing fit (Fits) the last child ends inside the parent. When they do
--  not fit the result says so (Too_Small) and every child is at its
--  minimum, laid out past the parent's end: the caller's window keeps its
--  minimum size so this does not happen on screen.
--  Tested, not proved: the expanding children fill the parent exactly.
--  Prior art: Haiku's Layout Kit (BGroupLayout), Qt's box layouts, CSS
--  flexbox (flex-basis, flex-grow).
------------------------------------------------------------------------------
package Box_Distribution with SPARK_Mode, Pure is
   --  Named ceilings: logical pixels in one axis, children in one box, and
   --  a stretch factor.
   MAXIMUM_EXTENT  : constant := 1_000_000;
   MAXIMUM_ITEMS   : constant := 256;
   MAXIMUM_STRETCH : constant := 100;
   --  The farthest a child can end: the parent's start, then every child
   --  at its largest minimum with the largest spacing after it.
   MAXIMUM_POSITION : constant := MAXIMUM_EXTENT + MAXIMUM_ITEMS * 2 * MAXIMUM_EXTENT;

   subtype Extent is Natural range 0 .. MAXIMUM_EXTENT;
   subtype Position is Natural range 0 .. MAXIMUM_POSITION;
   subtype Item_Count is Natural range 0 .. MAXIMUM_ITEMS;
   subtype Item_Index is Item_Count range 1 .. MAXIMUM_ITEMS;
   subtype Stretch_Factor is Positive range 1 .. MAXIMUM_STRETCH;

   --  Fixed: always its preferred length. Preferred: shrinks to its
   --  minimum, never grows past its preferred length. Expanding: also takes
   --  a share of the space left over, by Stretch.
   type Policy is (Fixed, Preferred, Expanding);

   type Size_Hint is record
      Minimum   : Extent := 0;
      Preferred : Extent := 0;
      Kind      : Policy := Box_Distribution.Preferred;
      Stretch   : Stretch_Factor := 1;
   end record;

   --  A hint is well formed: its minimum is at most its preferred length,
   --  and a Fixed one has one length.
   function Valid (Hint : Size_Hint) return Boolean is
     (Hint.Minimum <= Hint.Preferred and then
      (if Hint.Kind = Fixed then Hint.Minimum = Hint.Preferred));

   --  Hint for a child of Length that never changes.
   function Fixed_Hint (Length : Extent) return Size_Hint is
     ((Minimum => Length, Preferred => Length, Kind => Fixed, Stretch => 1))
     with Post => Valid (Fixed_Hint'Result);

   --  The hint of two children sharing one column or row of a grid (or a
   --  box's cross axis): the larger minimum and preferred length; it
   --  expands when either does, with the larger stretch.
   function Merge (Left, Right : Size_Hint) return Size_Hint
     with Pre  => Valid (Left) and then Valid (Right),
          Post => Valid (Merge'Result)
                  and then Merge'Result.Minimum = Natural'Max (Left.Minimum, Right.Minimum)
                  and then Merge'Result.Preferred >= Merge'Result.Minimum
                  and then Merge'Result.Preferred >= Natural'Max (Left.Preferred, Right.Preferred);

   type Hint_Array is array (Item_Index range <>) of Size_Hint;

   type Span is record
      Offset : Position := 0;
      Length : Extent := 0;
   end record;
   type Span_Array is array (Item_Index range <>) of Span;

   type Fit is (Fits, Too_Small);

   --  The length the children need along the axis from child I on: its
   --  minimum, and Spacing and the rest after it.
   function Need (Hints : Hint_Array; I : Item_Index; Spacing : Extent) return Natural is
     (if I = Hints'Last then Hints (I).Minimum
      else Hints (I).Minimum + Spacing + Need (Hints, I + 1, Spacing))
     with Pre => Hints'Length > 0 and then I in Hints'Range,
          Post => Need'Result <= (Hints'Last - I + 1) * 2 * MAXIMUM_EXTENT,
          Subprogram_Variant => (Increases => I);

   --  The minimum length of a box of these children (no margins).
   function Required (Hints : Hint_Array; Spacing : Extent) return Natural is
     (if Hints'Length = 0 then 0 else Need (Hints, Hints'First, Spacing))
     with Post => Required'Result <= MAXIMUM_ITEMS * 2 * MAXIMUM_EXTENT;

   --  Share Available, starting at Start, among the children: see the
   --  package description.
   procedure Distribute
     (Hints     : Hint_Array;
      Start     : Extent;
      Available : Extent;
      Spacing   : Extent;
      Spans     : out Span_Array;
      Status    : out Fit)
     with Pre  => Spans'First = Hints'First and then Spans'Last = Hints'Last
                  and then (for all H of Hints => Valid (H)),
          Post =>
            (Status = Fits) = (Required (Hints, Spacing) <= Available)
            and then (for all I in Spans'Range =>
                        Spans (I).Length >= Hints (I).Minimum
                        and then (if Hints (I).Kind /= Expanding then Spans (I).Length <= Hints (I).Preferred)
                        and then (if Status = Too_Small then Spans (I).Length = Hints (I).Minimum))
            and then (if Spans'Length > 0 then Spans (Spans'First).Offset = Start)
            and then (for all I in Spans'Range =>
                        (if I < Spans'Last then
                           Spans (I + 1).Offset = Spans (I).Offset + Spans (I).Length + Spacing))
            and then (if Status = Fits and then Spans'Length > 0 then
                        Spans (Spans'Last).Offset + Spans (Spans'Last).Length <= Start + Available);

   --  A child's length across the box: all of Available when it expands,
   --  else its preferred length or Available if less, but never less than
   --  its minimum (Too_Small when Available is).
   procedure Cross
     (Hint      : Size_Hint;
      Available : Extent;
      Length    : out Extent;
      Status    : out Fit)
     with Pre  => Valid (Hint),
          Post => Length >= Hint.Minimum
                  and then (Status = Fits) = (Hint.Minimum <= Available)
                  and then (if Status = Fits then Length <= Available)
                  and then (if Hint.Kind /= Expanding then Length <= Hint.Preferred);
end Box_Distribution;
