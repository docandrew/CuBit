pragma Ada_2022;
private with System;

--  An array of Element on the process heap (CuAlloc, userspace/allocator:
--  cualloc_allocate and cualloc_free; the C heap in the hosted tests), for
--  logstore's growable stores (LOG-005: the per-source histories).
--
--  The spec is SPARK, so proved callers index the array through Capacity's
--  bounds; the body is the trusted glue that overlays the block. A Block is
--  a handle: the caller keeps exactly one copy of each (a history owns its
--  block), never reads an index it has not set, and copies elements over
--  itself when it grows (Allocate a larger block, Set from Get, Release the
--  old one). Allocate never zeroes: Element's defaults are not applied.
generic
   type Element is private;
package Heap_Arrays with SPARK_Mode is
   type Block is private;
   --  The block's elements, Get and Set at 0 .. Capacity - 1.
   function Capacity (B : Block) return Natural with Global => null;
   --  No block (Capacity 0).
   function Empty return Block with Global => null, Post => Capacity (Empty'Result) = 0;

   function Get (B : Block; Index : Natural) return Element
   with Global => null, Pre => Index < Capacity (B);

   procedure Set (B : in out Block; Index : Natural; Item : Element)
   with Global => null, Pre => Index < Capacity (B), Post => Capacity (B) = Capacity (B'Old);

   --  A new block of Size elements; Capacity 0 (Empty) when the heap
   --  refuses or Size elements cannot be represented in bytes.
   procedure Allocate (B : out Block; Size : Positive)
   with Global => null, Post => Capacity (B) in 0 | Size;

   --  Give the block back.
   procedure Release (B : in out Block)
   with Global => null, Post => Capacity (B) = 0;
private
   pragma SPARK_Mode (Off);
   type Block is record
      Base : System.Address := System.Null_Address;
      Size : Natural := 0;
   end record;
end Heap_Arrays;
