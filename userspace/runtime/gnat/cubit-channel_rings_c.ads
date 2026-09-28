------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  C entry points for CuBit.Channel_Rings, so C, C++ and Rust programs
--  keep their network channel rings with the proved code rather than a C
--  copy. Part of this runtime (NetSurf's fetcher links it) and compiled
--  into the libc (userspace/libc/build.sh). The C declarations are in
--  userspace/libc/overlay/src/cubit/net_channel.h.
--
--  A C ring is {size, own, count}: for a producer, own is the produced
--  index and count the fill; for a consumer, own is the consumed index and
--  count the bytes available. These wrappers check the preconditions
--  themselves (the libc is built without run-time checks) and change
--  nothing when one does not hold.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Interfaces.C;

package CuBit.Channel_Rings_C with Preelaborate is

   type Ring is record
      Size  : Unsigned_32;
      Own   : Unsigned_32;
      Count : Unsigned_32;
   end record with Convention => C;

   --  1 if Value was accepted, 0 if it broke the ring rules.
   function Accept_Consumed (P : access Ring; Value : Unsigned_32)
     return Interfaces.C.int
   with Export, Convention => C,
        External_Name => "__cubit_ring_accept_consumed";

   function Accept_Produced (C : access Ring; Value : Unsigned_32)
     return Interfaces.C.int
   with Export, Convention => C,
        External_Name => "__cubit_ring_accept_produced";

   --  The free space (producer) or readable bytes (consumer) as two
   --  slices: Length_1 bytes at First, then Length_2 at 0.
   procedure Free_Slices
     (P : access constant Ring;
      First, Length_1, Length_2 : access Unsigned_32)
   with Export, Convention => C,
        External_Name => "__cubit_ring_free_slices";

   procedure Data_Slices
     (C : access constant Ring;
      First, Length_1, Length_2 : access Unsigned_32)
   with Export, Convention => C,
        External_Name => "__cubit_ring_data_slices";

   --  1 if committed (N fits the free space), 0 otherwise.
   function Commit
     (P : access Ring; N : Unsigned_32) return Interfaces.C.int
   with Export, Convention => C, External_Name => "__cubit_ring_commit";

   --  1 if released (N is at most what is available), 0 otherwise.
   function Consume
     (C : access Ring; N : Unsigned_32) return Interfaces.C.int
   with Export, Convention => C, External_Name => "__cubit_ring_consume";

   --  1 if Size is a ring size.
   function Valid_Size (Size : Unsigned_32) return Interfaces.C.int
   with Export, Convention => C, External_Name => "__cubit_ring_valid_size";

end CuBit.Channel_Rings_C;
