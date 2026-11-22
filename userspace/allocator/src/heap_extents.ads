-- Single-owner page runs for the bounded runtime's large/over-aligned path.
-- Offsets and ownership only: mapping and payload accesses belong to adapters.
package Heap_Extents with SPARK_Mode, Pure is
   Page_Bytes : constant := 4_096;
   Page_Count : constant := 4_096;
   Arena_Bytes : constant := Page_Count * Page_Bytes;
   subtype Page_Id is Positive range 1 .. Page_Count;
   subtype Page_Reference is Natural range 0 .. Page_Count;
   subtype Run_Length is Positive range 1 .. Page_Count;
   No_Page : constant Page_Reference := 0;
   type State is private;
   function Valid (Heap : State) return Boolean with Ghost;
   function Owner (Heap : State; Page : Page_Id) return Page_Reference;
   function Length (Heap : State; Page : Page_Id) return Page_Reference;
   function Fits (First : Page_Id; Count : Run_Length) return Boolean is
     (Count <= Page_Count - First + 1);
   function Free_Run (Heap : State; First : Page_Id; Count : Run_Length) return Boolean is
     (Fits (First, Count) and then
       (for all P in First .. First + Count - 1 => Owner (Heap, P) = No_Page));
   function Can_Allocate (Heap : State; Count, Alignment : Run_Length) return Boolean is
     (for some P in Page_Id => (P - 1) mod Alignment = 0 and then Free_Run (Heap, P, Count))
     with Ghost;
   -- The page map alone (owners and lengths): what an allocation that
   -- fails leaves unchanged (the search hints may change).
   function Same_Map (A, B : State) return Boolean with Ghost;
   procedure Initialize (Heap : out State) with
     Post => Valid (Heap) and then (for all P in Page_Id => Owner (Heap, P) = No_Page);
   procedure Allocate (Heap : in out State; Count, Alignment : Run_Length;
                       First : out Page_Reference) with
     Pre => Valid (Heap),
     Post => Valid (Heap) and then
       (First /= No_Page) = Can_Allocate (Heap'Old, Count, Alignment) and then
       (if First /= No_Page then Fits (First, Count) and then
          (First - 1) mod Alignment = 0 and then Length (Heap, First) = Count and then
          Free_Run (Heap'Old, First, Count) and then
          (for all P in Page_Id =>
             (if P in First .. First + Count - 1 then Owner (Heap, P) = First
              else Owner (Heap, P) = Owner (Heap'Old, P))) and then
          (for all P in Page_Id => (if P /= First then Length (Heap, P) = Length (Heap'Old, P)))
        else Same_Map (Heap, Heap'Old));
   -- Allocate from a remembered hole only (no search): may fail although
   -- the arena has room; the caller then tries elsewhere, or Allocate.
   procedure Allocate_Hinted (Heap : in out State; Count : Run_Length; First : out Page_Reference) with
     Pre => Valid (Heap),
     Post => Valid (Heap) and then
       (if First /= No_Page then Fits (First, Count) and then Length (Heap, First) = Count and then
          Free_Run (Heap'Old, First, Count) and then
          (for all P in Page_Id =>
             (if P in First .. First + Count - 1 then Owner (Heap, P) = First
              else Owner (Heap, P) = Owner (Heap'Old, P))) and then
          (for all P in Page_Id => (if P /= First then Length (Heap, P) = Length (Heap'Old, P)))
        else Same_Map (Heap, Heap'Old));
   -- Grow the live, unparked run at First to Count pages where it is: the
   -- pages after it up to Count must be free (realloc growing a block in
   -- place). Otherwise nothing changes.
   procedure Grow (Heap : in out State; First : Page_Id; Count : Run_Length; Success : out Boolean) with
     Pre => Valid (Heap),
     Post => Valid (Heap) and then
       Success = (Length (Heap'Old, First) > 0 and then not Parked (Heap'Old, First)
                  and then Count > Length (Heap'Old, First) and then Fits (First, Count)
                  and then Free_Run (Heap'Old, First + Length (Heap'Old, First), Count - Length (Heap'Old, First)))
       and then
       (if Success then Length (Heap, First) = Count and then not Parked (Heap, First) and then
          (for all P in Page_Id =>
             (if P in First .. First + Count - 1 then Owner (Heap, P) = First
              else Owner (Heap, P) = Owner (Heap'Old, P))) and then
          (for all P in Page_Id => (if P /= First then Length (Heap, P) = Length (Heap'Old, P)))
        else Heap = Heap'Old);
   -- A parked run is allocated but held for reuse by its owner (a cache
   -- of freed blocks above this core): it is not released, and parking it
   -- again fails, so a block freed twice is caught.
   function Parked (Heap : State; First : Page_Id) return Boolean;
   procedure Release (Heap : in out State; First : Page_Id; Success : out Boolean) with
     Pre => Valid (Heap),
     Post => Valid (Heap) and then Success = (Length (Heap'Old, First) > 0 and then not Parked (Heap'Old, First))
       and then
       (if Success then Length (Heap, First) = 0 and then
          (for all P in Page_Id =>
             (if Owner (Heap'Old, P) = First then Owner (Heap, P) = No_Page
              else Owner (Heap, P) = Owner (Heap'Old, P))) and then
          (for all P in Page_Id => (if P /= First then Length (Heap, P) = Length (Heap'Old, P)))
        else Heap = Heap'Old);
   -- Park the allocated, unparked run at First (Pages: its length).
   procedure Park (Heap : in out State; First : Page_Id; Pages : out Page_Reference; Success : out Boolean) with
     Pre => Valid (Heap),
     Post => Valid (Heap) and then Same_Map (Heap, Heap'Old)
       and then Success = (Length (Heap'Old, First) > 0 and then not Parked (Heap'Old, First))
       and then (if Success then Parked (Heap, First) and then Pages = Length (Heap, First)
                 else Heap = Heap'Old);
   -- Take the parked run at First back (it stays allocated).
   procedure Unpark (Heap : in out State; First : Page_Id; Success : out Boolean) with
     Pre => Valid (Heap),
     Post => Valid (Heap) and then Same_Map (Heap, Heap'Old)
       and then Success = (Length (Heap'Old, First) > 0 and then Parked (Heap'Old, First))
       and then (if Success then not Parked (Heap, First) else Heap = Heap'Old);
private
   -- Two bytes a page: the whole map fits in 16 KiB of cache.
   type Page_Map is array (Page_Id) of Page_Reference with Component_Size => 16;
   -- Rover: where the next search starts (the last release's run, or just
   -- past the last allocation): recently freed, still cached memory is
   -- reused first, and a search does not walk the arena's packed prefix
   -- again on every request. Any page: a search that finds nothing from
   -- it searches the whole arena.
   -- Holes: recently released (merged with free neighbours) or split-off runs, by length (exact up to
   -- 64 pages, then one class per power of two), a short stack each. Only
   -- hints: an allocation takes one after checking the page map says its
   -- run is free (a stale one is dropped), and falls back to the search,
   -- so nothing about them needs to hold for the map's correctness.
   Exact_Holes : constant := 64;
   subtype Hole_Class is Natural range 0 .. Exact_Holes + 6;
   Hole_Depth : constant := 16;
   subtype Hole_Count is Natural range 0 .. Hole_Depth;
   subtype Hole_Slot is Positive range 1 .. Hole_Depth;
   type Hole is record
      Start : Page_Id;
      Pages : Run_Length;
   end record with Size => 32;
   for Hole use record
      Start at 0 range 0 .. 15;
      Pages at 2 range 0 .. 15;
   end record;
   type Hole_Stack is array (Hole_Slot) of Hole;
   type Hole_Stacks is array (Hole_Class) of Hole_Stack;
   type Hole_Depths is array (Hole_Class) of Hole_Count;
   -- Bit K of word K / 64: class K has a hole (a summary of Depths).
   type Hole_Word is mod 2 ** 64;
   type Hole_Words is array (0 .. 1) of Hole_Word;
   -- Spans: at a remembered hole's first and last page, its length (a
   -- hint like the holes; lets a release merge with free neighbours).
   type Flags is array (Page_Id) of Boolean with Pack;
   type State is record
      Owners, Lengths, Spans : Page_Map;
      Parks : Flags;
      Rover : Page_Id;
      Depths : Hole_Depths;
      Present : Hole_Words;
      Holes : Hole_Stacks;
   end record;
   function Parked (Heap : State; First : Page_Id) return Boolean is (Heap.Parks (First));
   function Same_Map (A, B : State) return Boolean is
     (A.Owners = B.Owners and then A.Lengths = B.Lengths);
   function Owner (Heap : State; Page : Page_Id) return Page_Reference is (Heap.Owners (Page));
   function Length (Heap : State; Page : Page_Id) return Page_Reference is (Heap.Lengths (Page));
   function Valid (Heap : State) return Boolean is
     ((for all P in Page_Id =>
        (if Heap.Owners (P) = No_Page then Heap.Lengths (P) = 0
         else Heap.Owners (P) <= P and then
           Heap.Lengths (Heap.Owners (P)) > P - Heap.Owners (P))) and then
      (for all P in Page_Id =>
         (if Heap.Lengths (P) > 0 then Fits (P, Heap.Lengths (P)) and then
            (for all Q in P .. P + Heap.Lengths (P) - 1 => Heap.Owners (Q) = P))));
end Heap_Extents;
