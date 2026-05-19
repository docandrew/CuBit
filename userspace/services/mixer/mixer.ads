------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Audio mixer service definitions
--
--  @description
--  Central audio hub that manages multiple client streams, mixes PCM data,
--  and feeds the result to the HDA driver. Each client gets a shared ring
--  buffer (grant region) for zero-copy PCM transfer.
--
--  Ring buffer layout (per client, 2 pages = 8KB):
--    Offset 0x000: Header (64 bytes)
--      +0x00  writePtr    : U32  (producer advances after writing)
--      +0x04  readPtr     : U32  (consumer advances after reading)
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

package Mixer is

   ---------------------------------------------------------------------------
   --  Ring buffer header (matches shared memory layout)
   ---------------------------------------------------------------------------
   type RingHeader is record
      writePtr   : Unsigned_32;
      readPtr    : Unsigned_32;
      bufferSize : Unsigned_32;
      sampleRate : Unsigned_32;
      channels   : Unsigned_16;
      format     : Unsigned_16;
      underruns  : Unsigned_32;
      overruns   : Unsigned_32;
      state      : Unsigned_32;
   end record
      with Convention => C, Size => 256;  --  32 bytes used, padded to 32

   for RingHeader use record
      writePtr   at 16#00# range 0 .. 31;
      readPtr    at 16#04# range 0 .. 31;
      bufferSize at 16#08# range 0 .. 31;
      sampleRate at 16#0C# range 0 .. 31;
      channels   at 16#10# range 0 .. 15;
      format     at 16#12# range 0 .. 15;
      underruns  at 16#14# range 0 .. 31;
      overruns   at 16#18# range 0 .. 31;
      state      at 16#1C# range 0 .. 31;
   end record;

   --  Ring buffer states
   STATE_STOPPED : constant Unsigned_32 := 0;
   STATE_RUNNING : constant Unsigned_32 := 1;
   STATE_PAUSED  : constant Unsigned_32 := 2;

   --  Audio format IDs
   FORMAT_S16LE : constant Unsigned_16 := 0;

   --  Audio direction
   DIRECTION_OUTPUT : constant Unsigned_64 := 0;
   DIRECTION_INPUT  : constant Unsigned_64 := 1;

   ---------------------------------------------------------------------------
   --  Fixed-point types for audio math
   ---------------------------------------------------------------------------
   type Volume is delta 1.0 / 2**16 range 0.0 .. 2.0
      with Small => 1.0 / 2**16, Size => 32;

   type Sample is delta 1.0 range -32768.0 .. 32767.0
      with Small => 1.0, Size => 16;

   type MixSample is delta 1.0 range -2.0**31 .. 2.0**31 - 1.0
      with Small => 1.0, Size => 32;

   type Pan is delta 1.0 / 2**16 range 0.0 .. 1.0
      with Small => 1.0 / 2**16, Size => 32;

   --  Conversion from Volume to raw 16.16 representation for IPC replies
   function volToRaw (v : Volume) return Unsigned_32;

   ---------------------------------------------------------------------------
   --  Stream table
   ---------------------------------------------------------------------------
   MAX_STREAMS : constant := 8;

   type StreamInfo is record
      active    : Boolean := False;
      pid       : Unsigned_64 := 0;
      grantId   : Unsigned_64 := 0;
      ringAddr  : Unsigned_64 := 0;    --  Virtual address of grant in our space
      vol       : Volume := 1.0;
      panPos    : Pan := 0.5;
      direction : Unsigned_64 := 0;
   end record;

   type StreamTable is array (0 .. MAX_STREAMS - 1) of StreamInfo;

   streams : StreamTable;

   ---------------------------------------------------------------------------
   --  Grant / ring buffer constants
   ---------------------------------------------------------------------------
   RING_PAGES     : constant Unsigned_64 := 2;
   RING_TOTAL     : constant Unsigned_32 := 8192;  --  2 pages
   RING_HDR_SIZE  : constant Unsigned_32 := 64;     --  Header occupies 64 bytes
   RING_DATA_SIZE : constant Unsigned_32 := RING_TOTAL - RING_HDR_SIZE;

   GRANT_REGION_BASE : constant Unsigned_64 := 16#4000_0000_0000#;
   GRANT_SLOT_SIZE   : constant Unsigned_64 := 4096 * 4096;

   --  Pre-allocated ring buffer addresses (set at startup via sbrk)
   type RingBufAddrs is array (0 .. MAX_STREAMS - 1) of Unsigned_64;
   ringBufBase : RingBufAddrs := (others => 0);

   ---------------------------------------------------------------------------
   --  Mix buffer (32-bit intermediate for accumulation)
   ---------------------------------------------------------------------------
   --  256 stereo frames (= 1 period at 48kHz ~5.3ms)
   MIX_FRAMES   : constant := 256;
   MIX_SAMPLES  : constant := MIX_FRAMES * 2;  --  stereo

   type MixBuffer is array (0 .. MIX_SAMPLES - 1) of Integer_32;

   ---------------------------------------------------------------------------
   --  Procedures
   ---------------------------------------------------------------------------

   --  Allocate a stream slot, create grant to client, initialize ring header.
   --  Returns stream index (0..MAX_STREAMS-1) or -1 on failure.
   --  After success, streams(idx).grantId has the kernel grant ID.
   function openStream (clientPID  : Unsigned_64;
                        sampleRate : Unsigned_32;
                        channels   : Unsigned_16;
                        format     : Unsigned_16;
                        direction  : Unsigned_64) return Integer;

   --  Close a stream: revoke grant, free slot.
   procedure closeStream (streamIdx : Natural);

   --  Set volume on a stream. IPC sends 16.16 fixed-point as Unsigned_32.
   procedure setVolume (streamIdx : Natural; vol : Unsigned_32);

   --  Set pan on a stream. IPC sends 16.16 fixed-point as Unsigned_32.
   --  0.0 = left, 0.5 = center, 1.0 = right.
   procedure setPan (streamIdx : Natural; p : Unsigned_32);

   --  Read available samples from a stream's ring buffer and add to mixBuf.
   --  Returns number of frames actually read.
   function readAndMix (streamIdx : Natural;
                        mixBuf    : in out MixBuffer;
                        maxFrames : Natural) return Natural;

   --  Mix all active output streams into mixBuf, clamp to S16LE, write
   --  to the HDA staging buffer.  Returns number of frames mixed.
   function mixPeriod (mixBuf     : in out MixBuffer;
                       stagingOff : Unsigned_64;
                       maxFrames  : Natural) return Natural;

end Mixer;
