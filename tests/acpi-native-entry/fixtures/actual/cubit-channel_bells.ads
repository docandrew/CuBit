------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  This process's woken channel grants (CAP-002 step 3): the one place
--  channel wake events land, so no reader drops another's wakes.
--
--  @description
--  A channel registers the peer grant it reads (Watch) and asks whether it
--  was woken (Take). Every receive loop that gets a channel wake event
--  passes it here (Note_Notice; CuBit.Process_Events does). A wake for a
--  grant nobody watches is dropped (its channel closed). A channel whose
--  bucket is full is Unrouted: it treats every wake as a possible wake of
--  its own (slower, never wrong).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Doorbells;
with CuBit.Grant_References;

package CuBit.Channel_Bells is

   package GR renames CuBit.Grant_References;

   type Watch_Result is (Routed, Unrouted);

   procedure Watch (Grant : GR.Reference; Result : out Watch_Result);
   procedure Unwatch (Grant : GR.Reference);

   --  Whether Grant was woken since the last Take; clears it. Unrouted
   --  grants always answer True (the caller looks at its rings).
   function Take (Grant : GR.Reference; Routed : Boolean) return Boolean;

   --  A received event: Is_Notice when it was a channel wake (its grants
   --  are noted). Wakes beyond what one event carries stay pending in the
   --  kernel and arrive with the next receive.
   subtype Words is CuBit.Doorbells.Event_Words;
   procedure Note_Notice (Label : Unsigned_32; Length : Unsigned_8; W : Words;
                          Is_Notice : out Boolean);

   --  Grants a wake event named, already decoded.
   procedure Note_Bells (Bells : CuBit.Doorbells.Bell_Array);

   --  Whether any wake was noted since Clear_Noted: a loop that looks only
   --  at the grants its own receive brought back must also look at these
   --  (a wake event another reader took, CuBit.Process_Events).
   function Noted return Boolean;
   procedure Clear_Noted;

end CuBit.Channel_Bells;
