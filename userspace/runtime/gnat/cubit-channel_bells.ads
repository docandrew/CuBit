------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  This process's channel wakes (IPC-010 option B): the one place they are
--  noted, so no reader drops another's.
--
--  @description
--  A channel peer wakes this process's wake word (CuBit.Wake_Words), not a
--  channel: a wake names no channel, so every channel looks at its ring
--  after one (the look is shared memory, no system call). An activity wait
--  that returns for the wake word notes it (Note_Wake, CuBit.Messages);
--  every receive loop that gets the wake notice instead (Channel_Wake_ABI's
--  label) passes it here (Note_Notice; CuBit.Process_Events does). A
--  channel registers (Watch) and asks whether to look (Take): always, so a
--  wake this process's own loop took without noting it is never missed.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Grant_References;

package CuBit.Channel_Bells is

   package GR renames CuBit.Grant_References;

   type Watch_Result is (Routed, Unrouted);

   --  Every channel is Unrouted: wakes name no channel.
   procedure Watch (Grant : GR.Reference; Result : out Watch_Result);
   procedure Unwatch (Grant : GR.Reference);

   --  Whether the channel of Grant should look at its ring: always.
   function Take (Grant : GR.Reference; Routed : Boolean) return Boolean;

   --  A received event: Is_Notice when it was the wake notice (noted).
   type Words is array (0 .. 3) of Unsigned_64;
   procedure Note_Notice (Label : Unsigned_32; Length : Unsigned_8; W : Words;
                          Is_Notice : out Boolean);

   --  An activity wait returned for the wake word.
   procedure Note_Wake;

   --  Whether a wake was noted since Clear_Noted.
   function Noted return Boolean;
   procedure Clear_Noted;

end CuBit.Channel_Bells;
