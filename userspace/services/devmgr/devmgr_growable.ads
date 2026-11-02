------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Heap-backed growable storage for devmgr's records (LIM-001b): claim
--  ledgers, firmware windows, roots. Sized by what it holds, never by a
--  fixed count of hardware things.
--
--  @description
--  Elements live in one block from the process allocator (Allocate and
--  Release: CuAlloc in devmgr, the C heap in the hosted tests). Growing
--  doubles the block (at least Initial_Capacity) and copies the elements
--  over; a refused block leaves the store as it was and says why with a
--  named Capacity_Report (the demand, the capacity it stays at, the bytes
--  asked for). Elements are plain records: no controlled parts, no
--  pointers into the block survive a growth (callers index, they never
--  keep an element's address).
--
--  Invariant: Length <= Capacity, and Elements (1 .. Capacity) is the block
--  (Capacity = 0: no block).
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with Interfaces; use Interfaces;

generic
   type Element is private;
   --  A block of Bytes aligned to Alignment, or Null_Address (refused).
   with function Allocate (Bytes, Alignment : Unsigned_64) return System.Address;
   with procedure Release (Block : System.Address);
   --  The first block's elements: an allocation hint, never a limit.
   Initial_Capacity : Positive := 16;
package Devmgr_Growable is

   --  Why a store could not grow.
   type Growth_Status is
     (Grown,                 --  room for the demand
      Heap_Refused,          --  the allocator refused the block
      Representation_Limit); --  the demand is past what a count or size can say

   type Capacity_Report is record
      Status    : Growth_Status := Grown;
      Demand    : Unsigned_64 := 0;   --  elements wanted
      Limit     : Unsigned_64 := 0;   --  elements held (the capacity it stays at)
      Requested : Unsigned_64 := 0;   --  bytes asked of the allocator
   end record;

   type Store is limited private;

   function Length (S : Store) return Natural;
   function Capacity (S : Store) return Natural;

   --  The block's address (Elements (1 .. Capacity (S))), for a caller that
   --  overlays it to hand a proved core its array; Null_Address when empty.
   function Block (S : Store) return System.Address;

   function Element_At (S : Store; Index : Positive) return Element
   with Pre => Index <= Length (S);
   procedure Set_At (S : in out Store; Index : Positive; Item : Element)
   with Pre => Index <= Length (S);

   --  Room for Demand elements in all (Length and the elements unchanged).
   procedure Reserve (S : in out Store; Demand : Natural; Report : out Capacity_Report)
   with Post => (if Report.Status = Grown then Capacity (S) >= Demand);

   --  Item at the end, growing as needed (Length one more), or, when the
   --  store cannot grow, nothing changed.
   procedure Append (S : in out Store; Item : Element; Report : out Capacity_Report);

   --  The length a proved core settled on, within the block.
   procedure Set_Length (S : in out Store; New_Length : Natural)
   with Pre => New_Length <= Capacity (S);

   --  Give the block back (the store is empty after).
   procedure Clear (S : in out Store)
   with Post => Length (S) = 0 and then Capacity (S) = 0;

private
   type Store is limited record
      Base     : System.Address := System.Null_Address;
      Reserved : Natural := 0;
      Used     : Natural := 0;
   end record;
end Devmgr_Growable;
