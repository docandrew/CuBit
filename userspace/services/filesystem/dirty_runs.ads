------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Planning the write-back of a client's dirty pages (a write delegation's
--  harvest, docs/filesystem-data-plane.md): which entries of the client's
--  dirty table to take, in file order, grouped into runs that are
--  contiguous in the file and fit the service's buffer. Values only; the
--  service copies the bytes, checks the entries' sequence words and writes.
--
--  Proved (tests/filesystem-truncate, level 1): an item is used only if its
--  byte range is non-empty and inside its page; sorting leaves the items in
--  file order; a run's items are contiguous and its bytes fit the buffer;
--  every index stays in range, and an empty table yields no run.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Dirty_Runs with SPARK_Mode, Pure is

   Page_Bytes    : constant := 4_096;
   Maximum_Items : constant := 2_048;
   Buffer_Bytes  : constant := 524_288;

   subtype Page_Offset is Natural range 0 .. Page_Bytes;
   subtype Item_Count is Natural range 0 .. Maximum_Items;
   subtype Item_Index is Natural range 0 .. Maximum_Items - 1;

   --  A snapshot of one dirty entry, taken by the service.
   type Item is record
      Entry_Index : Item_Index := 0;
      Sequence    : Unsigned_32 := 0;
      Page        : Unsigned_32 := 0;
      Start       : Page_Offset := 0;
      Stop        : Page_Offset := 0;
   end record;

   type Item_Array is array (Item_Index) of Item;

   --  A byte range the service may use: non-empty and inside the page.
   function Valid (I : Item) return Boolean is (I.Start < I.Stop);

   function Offset (I : Item) return Unsigned_64 is
     (Unsigned_64 (I.Page) * Page_Bytes + Unsigned_64 (I.Start));

   function Length (I : Item) return Page_Offset is (I.Stop - I.Start)
   with Pre => Valid (I);

   --  Where the item's bytes end in the file.
   function End_Offset (I : Item) return Unsigned_64 is
     (Unsigned_64 (I.Page) * Page_Bytes + Unsigned_64 (I.Stop));

   function Sorted (Items : Item_Array; Count : Item_Count) return Boolean is
     (for all K in 1 .. Count - 1 => Items (K - 1).Page <= Items (K).Page);

   --  Put Items (0 .. Count - 1) in page order.
   procedure Sort (Items : in out Item_Array; Count : Item_Count)
   with Post => Sorted (Items, Count);

   --  B's bytes start where A's end in the file: in the same page, or at
   --  the start of the next page after a full A (so Offset (B) =
   --  End_Offset (A), stated without the multiplication).
   function Adjacent (A, B : Item) return Boolean is
     ((B.Page = A.Page and then B.Start = A.Stop) or else
      (A.Page < Unsigned_32'Last and then B.Page = A.Page + 1 and then
       A.Stop = Page_Bytes and then B.Start = 0));

   --  Item K continues the run that ends with item K - 1.
   function Continues (Items : Item_Array; K : Item_Index) return Boolean is
     (K > 0 and then Valid (Items (K - 1)) and then Valid (Items (K)) and then
      Adjacent (Items (K - 1), Items (K)));

   --  The run starting at item First (valid, First < Count): its last item
   --  and its bytes. Every item of the run continues the previous one, and
   --  the bytes fit the buffer.
   procedure Next_Run
     (Items : Item_Array; Count : Item_Count; First : Item_Index;
      Last : out Item_Index; Bytes : out Natural)
   with
     Pre  => First < Count and then Valid (Items (First)),
     Post => Last in First .. Count - 1 and then
             (for all K in First + 1 .. Last => Continues (Items, K)) and then
             Bytes in 1 .. Buffer_Bytes;

end Dirty_Runs;
