with Interfaces; use Interfaces;
with System;
-- Stable, growable CPU storage for per-session tables (render sessions,
-- GuC contexts, queues and the driver's own per-session records).
--
-- A region reserves virtual address space for the most sessions the
-- hardware can name (Intel_GPU_Session_Limits) and commits pages only as
-- the table grows, so a table never moves (records hold intrusive links and
-- grant tokens that must not be copied) and costs memory in proportion to
-- the sessions admitted. Committed pages stay committed when sessions are
-- recycled, like a slab cache; the high-water mark is what the driver
-- needed at once.
generic
   -- Page-aligned virtual capacity, or zero when refused.
   with function Reserve (Bytes : Unsigned_64) return Unsigned_64;
   -- Append Bytes of zero-filled, writable pages at Base + Offset, where
   -- Offset is the committed prefix so far. At most Commit_Limit per call.
   with function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean;
   Commit_Limit : Unsigned_64;
package Intel_GPU_Slot_Storage is
   Page_Bytes : constant Unsigned_64 := 4096;
   type Region is record
      Base, Reserved, Committed : Unsigned_64 := 0;
   end record;
   -- Reserve address space for Bytes (rounded up to pages). Base = 0 when
   -- refused: such a region never commits, so its table never grows.
   function Open (Bytes : Unsigned_64) return Region;
   function Address (Object : Region) return System.Address;
   function Valid (Object : Region) return Boolean is
     (Object.Base /= 0 and then Object.Reserved /= 0);
   -- Make at least the first Bytes of the region usable. Never shrinks and
   -- never moves; OK = False (memory exhausted, or beyond the reservation)
   -- leaves the committed prefix as it was.
   procedure Commit_Through (Object : in out Region; Bytes : Unsigned_64; OK : out Boolean);
end Intel_GPU_Slot_Storage;
