------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Cycle accounting for netstack's per-packet stages, for performance
--  work. Off by default: with Enabled False every operation is an empty
--  inline call that the compiler removes, and nothing is printed.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Netstack_Profile is

   Enabled : constant Boolean := False;

   type Stage is
     (Checksum, Parse, Lookup, Arrive, Event, Batch_Done, Service,
      Timers, Deadline, Batch);

   function Now return Unsigned_64 with Inline;

   --  Charge the cycles since Since to S.
   procedure Charge (S : Stage; Since : Unsigned_64) with Inline;

   --  One received packet; every 2 ** 16 packets, print the average
   --  cycles per packet of each stage.
   procedure Packet with Inline;

end Netstack_Profile;
