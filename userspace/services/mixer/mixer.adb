------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Audio mixer service implementation
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;

package body Mixer is

   function toVolume is new Ada.Unchecked_Conversion (Unsigned_32, Volume);
   function fromVolume is new Ada.Unchecked_Conversion (Volume, Unsigned_32);
   function toPan is new Ada.Unchecked_Conversion (Unsigned_32, Pan);

   function readAtomicU32 (addr : Unsigned_64) return Unsigned_32 is
      value : Unsigned_32
         with Import,
              Address => To_Address (Integer_Address (addr)),
              Atomic;
   begin
      return value;
   end readAtomicU32;

   procedure writeAtomicU32 (addr : Unsigned_64; value : Unsigned_32) is
      target : Unsigned_32
         with Import,
              Address => To_Address (Integer_Address (addr)),
              Atomic;
   begin
      target := value;
   end writeAtomicU32;

   function volToRaw (v : Volume) return Unsigned_32 is
   begin
      return fromVolume (v);
   end volToRaw;

   ---------------------------------------------------------------------------
   --  openStream
   ---------------------------------------------------------------------------
   function openStream (clientPID  : Unsigned_64;
                        sampleRate : Unsigned_32;
                        channels   : Unsigned_16;
                        format     : Unsigned_16;
                        direction  : Unsigned_64) return Integer
   is
      slotIdx   : Integer := -1;
      grantAddr : Unsigned_64;
      gid       : Unsigned_64;
      grantOK   : Boolean;
   begin
      --  Find free slot
      for i in streams'Range loop
         if not streams (i).active then
            slotIdx := i;
            exit;
         end if;
      end loop;

      if slotIdx < 0 then
         return -1;
      end if;

      --  Use pre-allocated ring buffer for this slot
      grantAddr := ringBufBase (slotIdx);
      if grantAddr = 0 then
         return -1;
      end if;

      --  Create grant to client (2 pages, read/write)
      createGrant
        (grantee   => ProcessID (clientPID),
         localAddr => To_Address (Integer_Address (grantAddr)),
         numPages  => Natural (RING_PAGES),
         readWrite => True,
         grantId   => gid,
         success   => grantOK);

      if not grantOK then
         return -1;
      end if;

      streams (slotIdx) := (active    => True,
                             pid       => clientPID,
                             grantId   => gid,
                             ringAddr  => grantAddr,
                             vol       => 1.0,
                             panPos    => 0.5,
                             direction => direction);

      --  Initialize ring header in the granted region
      initHeader : declare
         hdrMem : RingHeader
            with Import,
                 Address => To_Address (Integer_Address (grantAddr)),
                 Volatile;
      begin
         hdrMem.writePtr   := 0;
         hdrMem.readPtr    := 0;
         hdrMem.bufferSize := RING_DATA_SIZE;
         hdrMem.sampleRate := sampleRate;
         hdrMem.channels   := channels;
         hdrMem.format     := format;
         hdrMem.underruns  := 0;
         hdrMem.overruns   := 0;
         hdrMem.state      := STATE_STOPPED;
      end initHeader;

      return slotIdx;
   end openStream;

   ---------------------------------------------------------------------------
   --  closeStream
   ---------------------------------------------------------------------------
   procedure closeStream (streamIdx : Natural) is
   begin
      if streamIdx > streams'Last or else not streams (streamIdx).active then
         return;
      end if;

      --  Revoke the grant
      revokeGrant (streams (streamIdx).grantId);

      streams (streamIdx).active := False;
      streams (streamIdx).pid := 0;
   end closeStream;

   ---------------------------------------------------------------------------
   --  setVolume
   ---------------------------------------------------------------------------
   procedure setVolume (streamIdx : Natural; vol : Unsigned_32) is
   begin
      if streamIdx <= streams'Last and then streams (streamIdx).active then
         streams (streamIdx).vol := toVolume (vol);
      end if;
   end setVolume;

   ---------------------------------------------------------------------------
   --  setPan
   ---------------------------------------------------------------------------
   procedure setPan (streamIdx : Natural; p : Unsigned_32) is
   begin
      if streamIdx <= streams'Last and then streams (streamIdx).active then
         streams (streamIdx).panPos := toPan (p);
      end if;
   end setPan;

   ---------------------------------------------------------------------------
   --  hasRunningOutput
   ---------------------------------------------------------------------------
   function hasRunningOutput return Boolean is
   begin
      for i in streams'Range loop
         if streams (i).active and then streams (i).direction = 0 and then
            streams (i).ringAddr /= 0
         then
            if readAtomicU32
                 (streams (i).ringAddr + 16#1C#) = STATE_RUNNING
            then
               return True;
            end if;
         end if;
      end loop;

      return False;
   end hasRunningOutput;

   ---------------------------------------------------------------------------
   --  readAndMix - read samples from one stream and accumulate into mix buffer
   ---------------------------------------------------------------------------
   function readAndMix (streamIdx : Natural;
                        mixBuf    : in out MixBuffer;
                        maxFrames : Natural) return Natural
   is
      s         : StreamInfo renames streams (streamIdx);
      hdrAddr   : constant Unsigned_64 := s.ringAddr;
      dataAddr  : constant Unsigned_64 := s.ringAddr + Unsigned_64 (RING_HDR_SIZE);

      type SampleArray is array (Natural range <>) of Integer_16
         with Convention => C;

      ringData : SampleArray (0 .. Natural (RING_DATA_SIZE) / 2 - 1)
         with Import,
              Address => To_Address (Integer_Address (dataAddr)),
              Volatile;

      wp        : Unsigned_32;
      rp        : Unsigned_32;
      available : Unsigned_32;
      avFrames  : Natural;
      toRead    : Natural;
      ringIdx   : Natural;
      vol       : Volume;
      samp      : Sample;
      scaled    : MixSample;
   begin
      if readAtomicU32 (hdrAddr + 16#1C#) /= STATE_RUNNING then
         return 0;
      end if;

      wp := readAtomicU32 (hdrAddr);
      rp := readAtomicU32 (hdrAddr + 4);
      available := wp - rp;  --  Wrapping unsigned arithmetic

      --  Header memory is writable by the untrusted producer. Validate the
      --  complete S16LE ring invariant once before indexing shared samples.
      if available > RING_DATA_SIZE or else wp mod 4 /= 0 or else rp mod 4 /= 0
      then
         writeAtomicU32
           (hdrAddr + 16#18#, readAtomicU32 (hdrAddr + 16#18#) + 1);
         return 0;
      end if;

      --  Convert available bytes to frames (stereo 16-bit = 4 bytes/frame)
      avFrames := Natural (available) / 4;
      toRead := Natural'Min (avFrames, maxFrames);

      if toRead = 0 then
         --  Underrun
         writeAtomicU32
           (hdrAddr + 16#14#, readAtomicU32 (hdrAddr + 16#14#) + 1);
         return 0;
      end if;

      vol := s.vol;

      --  Read samples from ring and accumulate
      for f in 0 .. toRead - 1 loop
         --  Ring offset in sample indices (2 samples per frame)
         ringIdx := Natural ((rp + Unsigned_32 (f * 4)) mod RING_DATA_SIZE) / 2;

         --  Left channel: fixed-point multiply handles scaling
         samp := Sample (ringData (ringIdx));
         scaled := MixSample (samp * vol);
         mixBuf (f * 2) := mixBuf (f * 2) + Integer_32 (scaled);

         --  Right channel
         samp := Sample (ringData (ringIdx + 1));
         scaled := MixSample (samp * vol);
         mixBuf (f * 2 + 1) := mixBuf (f * 2 + 1) + Integer_32 (scaled);
      end loop;

      --  Advance read pointer
      writeAtomicU32 (hdrAddr + 4, rp + Unsigned_32 (toRead * 4));

      return toRead;
   end readAndMix;

   ---------------------------------------------------------------------------
   --  mixPeriod - mix all active output streams into one HDA DMA period
   ---------------------------------------------------------------------------
   function mixPeriod (mixBuf     : in out MixBuffer;
                       periodAddr : Unsigned_64;
                       maxFrames  : Natural) return Natural
   is
      type SampleArray is array (Natural range <>) of Integer_16
         with Convention => C;

      period : SampleArray (0 .. maxFrames * 2 - 1)
         with Import,
              Address => To_Address (Integer_Address (periodAddr)),
              Volatile;

      framesRead : Natural;
      maxRead    : Natural := 0;
      val        : Integer_32;
   begin
      --  Clear mix buffer
      for i in mixBuf'Range loop
         mixBuf (i) := 0;
      end loop;

      --  Accumulate from all active output streams
      for i in streams'Range loop
         if streams (i).active and streams (i).direction = 0 then
            framesRead := readAndMix (i, mixBuf, maxFrames);
            if framesRead > maxRead then
               maxRead := framesRead;
            end if;
         end if;
      end loop;

      --  Clamp 32-bit mix to 16-bit and always write the full DMA period
      --  so DMA never loops stale data (unread portion is silence from
      --  the cleared mixBuf).
      for i in 0 .. maxFrames * 2 - 1 loop
         val := mixBuf (i);
         if val > 32767 then
            val := 32767;
         elsif val < -32768 then
            val := -32768;
         end if;
         period (i) := Integer_16 (val);
      end loop;

      return maxRead;
   end mixPeriod;

end Mixer;
