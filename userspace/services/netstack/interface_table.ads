------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  netstack's interfaces (docs/net-link.md section 6): as many as its
--  memory holds. Each element lives in its own CuAlloc block, so a pointer
--  to it stays valid until it is removed; a directory of slots, grown and
--  shrunk with CuAlloc, maps slots to elements. Running out of memory
--  refuses the insertion (Inserted => False); nothing else fails.
--
--  Handles (CuBit.Net_Link.Interface_Handle) are a slot and the epoch of
--  its occupant. Each insertion takes the next epoch, so a handle to an
--  element that was removed never resolves to the slot's next occupant
--  (until 2 ** 32 - 1 insertions later).
--
--  Not proved: allocation glue. One thread uses it (netstack's loop).
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Net_Link;

generic
   type Element is private;
package Interface_Table is
   package NL renames CuBit.Net_Link;

   type Element_Access is access all Element;

   --  Slots 0 .. Slot_Count - 1 exist; some may be empty.
   function Slot_Count return Natural;
   function In_Use (Slot : Natural) return Boolean;
   --  The element in an occupied slot (Pre: In_Use (Slot)).
   function Get (Slot : Natural) return Element_Access;
   function Handle_Of (Slot : Natural) return NL.Interface_Handle;
   --  How many slots are occupied.
   function Count return Natural;

   --  Store Item in a free slot; Handle names it. Inserted is False when
   --  the heap refused the memory.
   procedure Insert
     (Item : Element; Handle : out NL.Interface_Handle; Inserted : out Boolean);

   --  The slot Handle names, if its occupant is still the one it named.
   procedure Find (Handle : NL.Interface_Handle; Slot : out Natural; Found : out Boolean);

   --  Free the slot's element (and shrink the directory past its last
   --  occupied slot).
   procedure Remove (Slot : Natural);

end Interface_Table;
