------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  DOOM's sound effect channels, played through one mixer stream
--  (CuBit.Audio) at 48 kHz stereo. Mixing is CuBit.Doom_Mixer (proved);
--  this keeps the channels and the stream's partial writes.
--
--  tests/doom-audio runs this unit unchanged on Linux against a
--  controllable sink.
------------------------------------------------------------------------------
pragma Ada_2022;
with System;

with CuBit.Doom_Mixer; use CuBit.Doom_Mixer;

package CuBit.Doom_Sound is

   --  Open and start the stream; False when the mixer refused it.
   function Init return Boolean;

   --  Close the stream; pending output and every channel are discarded.
   procedure Shutdown;

   --  Write what the stream did not accept last time, and only then mix
   --  the next batch. At most two writes; never waits for ring space.
   procedure Update;

   --  Play Length unsigned 8-bit samples at Data, which must stay readable
   --  until the channel ends or is stopped (DOOM keeps sound lumps).
   procedure Start (Channel : Channel_Index; Data : System.Address;
                    Length : Sample_Count; Rate : Sample_Rate;
                    Vol, Sep : Integer);

   --  Output already mixed is not recalled.
   procedure Stop (Channel : Channel_Index);

   function Is_Playing (Channel : Channel_Index) return Boolean;

   procedure Update_Parameters (Channel : Channel_Index; Vol, Sep : Integer);

end CuBit.Doom_Sound;
