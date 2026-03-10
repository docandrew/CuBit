------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  DOOM sound engine - channel mixer with resampling
--
--  @description
--  Manages 8 sound channels, resamples DOOM's 8-bit mono PCM (11025 Hz)
--  to 16-bit stereo at 48 kHz, mixes locally, and writes the result to
--  a CuBit.Audio ring buffer stream for output via the mixer service.
--
--  All public subprograms are exported with Convention => C so that the
--  thin C shim (doomgeneric_sound.c) can wire them into DOOM's
--  sound_module_t / music_module_t structs.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;

package CuBit.Doom_Sound is

   MAX_CHANNELS : constant := 8;

   ---------------------------------------------------------------------------
   --  sndInit - open a mixer stream, prepare channel table
   --  Returns 1 on success, 0 on failure.
   ---------------------------------------------------------------------------
   function sndInit return Integer
      with Export, Convention => C,
           External_Name => "cubit_snd_init";

   ---------------------------------------------------------------------------
   --  sndShutdown - close mixer stream
   ---------------------------------------------------------------------------
   procedure sndShutdown
      with Export, Convention => C,
           External_Name => "cubit_snd_shutdown";

   ---------------------------------------------------------------------------
   --  sndUpdate - mix all active channels, write to ring buffer.
   --  Called once per game tic (~35 Hz).
   ---------------------------------------------------------------------------
   procedure sndUpdate
      with Export, Convention => C,
           External_Name => "cubit_snd_update";

   ---------------------------------------------------------------------------
   --  sndStartChannel - begin playing a sound effect
   --
   --  data:       pointer to raw 8-bit unsigned PCM samples
   --  dataLen:    number of samples
   --  sampleRate: source sample rate (e.g. 11025)
   --  channel:    DOOM channel index (0..7)
   --  vol:        volume 0..127
   --  sep:        stereo separation 0..254 (128 = centre)
   --
   --  Returns channel index on success, -1 on failure.
   ---------------------------------------------------------------------------
   function sndStartChannel (data       : System.Address;
                              dataLen    : Unsigned_32;
                              sampleRate : Unsigned_32;
                              channel    : Integer;
                              vol        : Integer;
                              sep        : Integer) return Integer
      with Export, Convention => C,
           External_Name => "cubit_snd_start_channel";

   ---------------------------------------------------------------------------
   --  sndStopChannel
   ---------------------------------------------------------------------------
   procedure sndStopChannel (channel : Integer)
      with Export, Convention => C,
           External_Name => "cubit_snd_stop_channel";

   ---------------------------------------------------------------------------
   --  sndIsPlaying - returns 1 if channel is active, 0 otherwise
   ---------------------------------------------------------------------------
   function sndIsPlaying (channel : Integer) return Integer
      with Export, Convention => C,
           External_Name => "cubit_snd_is_playing";

   ---------------------------------------------------------------------------
   --  sndUpdateParams - update volume and stereo pan on a channel
   ---------------------------------------------------------------------------
   procedure sndUpdateParams (channel : Integer;
                               vol     : Integer;
                               sep     : Integer)
      with Export, Convention => C,
           External_Name => "cubit_snd_update_params";

end CuBit.Doom_Sound;
