------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Audio client API
--
--  @description
--  Provides audio stream management for userspace applications. Connects
--  to the mixer service via capCall on CAP_SLOT_MIXER. Audio data flows
--  through a shared ring buffer (grant) with zero syscall overhead per
--  sample.
--
--  Ring buffer layout (2 pages = 8KB, shared with mixer via grant):
--    Offset 0x000: Header (64 bytes)
--      +0x00  writePtr    : U32  (app advances after writing)
--      +0x04  readPtr     : U32  (mixer advances after reading)
--      +0x08  bufferSize  : U32  (ring data area bytes)
--      +0x0C  sampleRate  : U32  (48000)
--      +0x10  channels    : U16  (2)
--      +0x12  format      : U16  (0 = S16LE)
--      +0x14  underruns   : U32  (mixer increments when starved)
--      +0x18  overruns    : U32  (mixer increments when full)
--      +0x1C  state       : U32  (0=stopped, 1=running, 2=paused)
--    Offset 0x040: Ring data (bufferSize bytes)
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;

package CuBit.Audio is

   --  Audio format
   FORMAT_S16LE : constant Unsigned_16 := 0;

   --  Audio direction
   DIRECTION_OUTPUT : constant Unsigned_64 := 0;
   DIRECTION_INPUT  : constant Unsigned_64 := 1;

   --  Stream states
   STATE_STOPPED : constant Unsigned_32 := 0;
   STATE_RUNNING : constant Unsigned_32 := 1;
   STATE_PAUSED  : constant Unsigned_32 := 2;

   --  Volume: 16.16 fixed-point, 0.0 = silent, 1.0 = unity, 2.0 = max
   type Volume is delta 1.0 / 2**16 range 0.0 .. 2.0
      with Small => 1.0 / 2**16, Size => 32;

   --  Max streams a single app can open
   MAX_APP_STREAMS : constant := 4;

   type StreamHandle is private;
   NULL_STREAM : constant StreamHandle;

   ---------------------------------------------------------------------------
   --  open
   --  Open an audio stream to the mixer. Returns a handle for subsequent
   --  operations. The mixer creates a shared ring buffer grant.
   --
   --  sampleRate: samples per second (e.g. 48000)
   --  channels:   1 (mono) or 2 (stereo)
   --  direction:  DIRECTION_OUTPUT or DIRECTION_INPUT
   ---------------------------------------------------------------------------
   function open
     (sampleRate : Unsigned_32;
      channels   : Unsigned_16;
      direction  : Unsigned_64 := DIRECTION_OUTPUT)
      return StreamHandle;

   ---------------------------------------------------------------------------
   --  write
   --  Write PCM frames to the stream's ring buffer. Non-blocking: writes as
   --  many frames as fit, returns the number actually written.
   --
   --  buf:    pointer to interleaved S16LE samples
   --  frames: number of frames (each frame = channels * 2 bytes)
   ---------------------------------------------------------------------------
   function write (stream : StreamHandle;
                   buf    : System.Address;
                   frames : Natural) return Natural;

   ---------------------------------------------------------------------------
   --  read
   --  Read PCM frames from an input stream's ring buffer. Non-blocking.
   --
   --  buf:    destination for interleaved S16LE samples
   --  frames: max frames to read
   ---------------------------------------------------------------------------
   function read (stream : StreamHandle;
                  buf    : System.Address;
                  frames : Natural) return Natural;

   ---------------------------------------------------------------------------
   --  start
   --  Set the stream state to RUNNING. The mixer begins reading from it.
   ---------------------------------------------------------------------------
   procedure start (stream : StreamHandle);

   ---------------------------------------------------------------------------
   --  pause
   --  Set the stream state to PAUSED. The mixer stops reading.
   ---------------------------------------------------------------------------
   procedure pause (stream : StreamHandle);

   ---------------------------------------------------------------------------
   --  stop
   --  Set the stream state to STOPPED.
   ---------------------------------------------------------------------------
   procedure stop (stream : StreamHandle);

   ---------------------------------------------------------------------------
   --  setVolume
   --  Set per-stream volume. 0.0 = silent, 1.0 = unity gain, 2.0 = max.
   ---------------------------------------------------------------------------
   procedure setVolume (stream : StreamHandle; vol : Volume);

   ---------------------------------------------------------------------------
   --  close
   --  Close the stream. Sends OP_AUDIO_CLOSE to the mixer.
   ---------------------------------------------------------------------------
   procedure close (stream : in out StreamHandle);

   ---------------------------------------------------------------------------
   --  isValid
   --  Returns True if the stream handle refers to an open stream.
   ---------------------------------------------------------------------------
   function isValid (stream : StreamHandle) return Boolean;

   ---------------------------------------------------------------------------
   --  notify
   --  Send a notification to the mixer to wake it for mixing. Call after
   --  writing a batch of samples to reduce latency.
   ---------------------------------------------------------------------------
   procedure notify;

private

   type StreamRecord is record
      active     : Boolean := False;
      streamId   : Unsigned_64 := 0;
      ringAddr   : Unsigned_64 := 0;  --  Grant base in our address space
      bufferSize : Unsigned_32 := 0;  --  Ring data area size
      hdrSize    : Unsigned_32 := 0;  --  Header size (offset to data)
      channels   : Unsigned_16 := 2;
   end record;

   type StreamIndex is range 0 .. MAX_APP_STREAMS - 1;

   type StreamHandle is record
      idx   : StreamIndex := 0;
      valid : Boolean := False;
   end record;

   NULL_STREAM : constant StreamHandle := (idx => 0, valid => False);

end CuBit.Audio;
