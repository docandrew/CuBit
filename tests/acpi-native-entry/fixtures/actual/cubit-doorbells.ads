------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Channel wakes (CAP-002 step 3, docs/capability-handles.md section 4):
--  how a channel side wakes its peer, over the existing SEND_EVENT.
--
--  @description
--  A side wakes its peer only when the peer declared itself asleep (its
--  needs-wake word, CuBit.Request_Channels), by naming the grant of the
--  region it owns (Ring); the kernel keeps one pending wake per grant (a
--  repeat is absorbed), so wakes never take request credits, never overflow
--  and are never lost. The peer's ordinary receive returns them as one
--  event of up to four grants (Decode). Shared ABI: Channel_Wake_ABI.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Grant_References;
with Channel_Wake_ABI;

package CuBit.Doorbells is

   package GR renames CuBit.Grant_References;
   package ABI renames Channel_Wake_ABI;

   --  Wake the grantee of Grant, a grant this process owns. Sync: this
   --  thread is about to block (its end of turn), so the woken thread is
   --  placed on this CPU. False: refused (not the owner, or the grant has
   --  ended; the peer learns of an end from the kernel's grant notices).
   function Ring (Grant : GR.Reference; Sync : Boolean) return Boolean;

   --  A received event: Is_Wake when it is a channel wake; its grants in
   --  Into (1 .. Count).
   subtype Wake_Count is Natural range 0 .. ABI.Wakes_Per_Event;
   type Bell_Array is array (Positive range <>) of GR.Reference;
   type Event_Words is array (0 .. ABI.Wakes_Per_Event - 1) of Unsigned_64;
   procedure Decode
     (Label : Unsigned_32; Length : Unsigned_8; Words : Event_Words;
      Is_Wake : out Boolean; Into : out Bell_Array; Count : out Wake_Count)
   with Pre => Into'First = 1 and then Into'Length = ABI.Wakes_Per_Event;

end CuBit.Doorbells;
