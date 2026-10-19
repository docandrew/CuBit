with System;
-- Audited pointer bridge: glyph mask storage from CuAlloc, the process's one
-- allocator (userspace/allocator). No fixed arena: an owner's cache budget
-- is its policy limit; the heap answers null when it is out and the glyph is
-- simply not cached. Every returned block stays at its address until
-- Release; the owner serializes all calls and retires every reader (Mesa
-- views included) before releasing.
package Compositor_Glyph_Memory with SPARK_Mode => Off is
   use type System.Address;
   -- Mesa imports masks as user pointers; keep them on a cache-line pair.
   Alignment : constant := 128;
   procedure Reserve (Size : Positive; Pixels : out System.Address; Capacity : out Natural);
   procedure Release (Pixels : in out System.Address)
     with Post => Pixels = System.Null_Address;
end Compositor_Glyph_Memory;
