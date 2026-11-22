------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  C entry points for CuBit.Datagram_Rings, so C programs keep datagram
--  records (connected UDP, listener offers and arrivals) with the proved
--  code. Compiled into the libc (userspace/libc/build.sh); declared in
--  userspace/c/cubit_net_channel.h. Rings are CuBit.Channel_Rings_C's
--  {size, own, count}. The wrappers check the preconditions themselves
--  (the libc is built without run-time checks) and change nothing when
--  one does not hold.
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with Interfaces; use Interfaces;
with Interfaces.C;
with CuBit.Channel_Rings_C;

package CuBit.Datagram_Rings_C with Preelaborate is

   --  Put Length bytes at Data as one record in the producer P's ring at
   --  Ring: 1 if put, 0 if there is no room, -1 if too large or P is not a
   --  well-formed ring.
   function Put
     (P      : access Channel_Rings_C.Ring;
      Ring   : System.Address;
      Data   : System.Address;
      Length : Unsigned_32) return Interfaces.C.int
   with Export, Convention => C, External_Name => "__cubit_datagram_put";

   --  Take the oldest record from consumer C's ring into Into (at most
   --  Room bytes): 1 if taken (Length bytes copied, Truncated 1 if the
   --  record was longer), 0 if empty, -1 if malformed or C is not a
   --  well-formed ring.
   function Take
     (C         : access Channel_Rings_C.Ring;
      Ring      : System.Address;
      Into      : System.Address;
      Room      : Unsigned_32;
      Length    : access Unsigned_32;
      Truncated : access Interfaces.C.int) return Interfaces.C.int
   with Export, Convention => C, External_Name => "__cubit_datagram_take";

end CuBit.Datagram_Rings_C;
