pragma Ada_2022;

-- Numeric firmware admission policy, independent of address overlays. Byte
-- ranges are inclusive; normalized frame spans are half-open. Reserved bytes
-- exclude every page they touch; usable bytes admit only complete pages.
package Firmware_Frames with SPARK_Mode, Pure is
   type Count is range 0 .. 2 ** 53;
   Page_Bytes : constant Count := 4096;
   subtype Byte_Address is Count range 0 .. 2 ** 52 - 1;
   subtype Frame is Count range 0 .. 2 ** 40 - 1;
   subtype Boundary is Count range 0 .. 2 ** 40;
   type Span is private;
   function First (Area : Span) return Boundary with Inline_Always;
   function Limit (Area : Span) return Boundary with Inline_Always;
   function Empty (Area : Span) return Boolean with Inline_Always;

   function Whole_Pages (Low, High : Byte_Address) return Span with
     Pre => Low <= High,
     Post =>
       (if not Empty (Whole_Pages'Result) then
          First (Whole_Pages'Result) * Page_Bytes >= Low and then
          Limit (Whole_Pages'Result) * Page_Bytes - 1 <= High);

   function Touched_Pages (Low, High : Byte_Address) return Span with
     Pre => Low <= High,
     Post => not Empty (Touched_Pages'Result) and then
       First (Touched_Pages'Result) * Page_Bytes <= Low and then
       Limit (Touched_Pages'Result) * Page_Bytes - 1 >= High;

   type Region_Kind is (Usable, Reserved);
   type Region is record
      Kind : Region_Kind := Reserved;
      Pages : Span;
   end record;
   type Region_Array is array (Natural range <>) of Region;
   type Decision is (Admit, Reject, Split);
   type Block_Order is range 0 .. 39;
   function Block_Pages (Order : Block_Order) return Count is (2 ** Natural (Order))
     with Inline_Always;
   function Largest_Block (Low, High : Frame; Maximum : Block_Order)
     return Block_Order with
     Pre => Low <= High,
     Post => Largest_Block'Result <= Maximum and then
       Block_Pages (Largest_Block'Result) <= High - Low + 1 and then
       Low mod Block_Pages (Largest_Block'Result) = 0;

   function Contains (Area : Span; Low, High : Frame) return Boolean is
     (Low <= High and then First (Area) <= Low and then High < Limit (Area));
   function Overlaps (Area : Span; Low, High : Frame) return Boolean is
     (not Empty (Area) and then Low <= High and then
      First (Area) <= High and then Low < Limit (Area));

   -- Earlier usable entries own duplicate page coverage. Any reserved entry
   -- takes precedence regardless of its position in the firmware map.
   function Conflicts (Map : Region_Array; Owner, Index : Natural;
                       Low, High : Frame) return Boolean is
     ((Map (Index).Kind = Reserved or else Index < Owner) and then
      Overlaps (Map (Index).Pages, Low, High))
     with Pre => Owner in Map'Range and then Index in Map'Range;

   function Classify (Map : Region_Array; Owner : Natural;
                      Low, High : Frame) return Decision with
     Pre => Owner in Map'Range and then Map (Owner).Kind = Usable and then
       Contains (Map (Owner).Pages, Low, High),
     Post =>
       (if Classify'Result = Admit then
          (for all I in Map'Range => not Conflicts (Map, Owner, I, Low, High)))
       and then (if Classify'Result = Reject then
          (for some I in Map'Range =>
             (Map (I).Kind = Reserved or else I < Owner) and then
             Contains (Map (I).Pages, Low, High)))
       and then (if Classify'Result = Split then Low < High);

   procedure Prove_Unique_Owner
     (Map : Region_Array; Left, Right : Natural; Item : Frame) with Ghost,
     Pre => Left in Map'Range and then Right in Map'Range and then Left /= Right
       and then Map (Left).Kind = Usable and then Map (Right).Kind = Usable
       and then Contains (Map (Left).Pages, Item, Item)
       and then Contains (Map (Right).Pages, Item, Item),
     Post => not (Classify (Map, Left, Item, Item) = Admit and then
                  Classify (Map, Right, Item, Item) = Admit);

private
   type Span is record
      Start : Boundary := 0;
      Finish : Boundary := 0;
   end record;
   function First (Area : Span) return Boundary is (Area.Start);
   function Limit (Area : Span) return Boundary is (Area.Finish);
   function Empty (Area : Span) return Boolean is (Area.Start >= Area.Finish);
end Firmware_Frames;
