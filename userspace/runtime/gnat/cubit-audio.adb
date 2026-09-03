------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Audio client API implementation
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.String;

package body CuBit.Audio is

   --  IPC labels (must match kernel/src/ipc_labels.ads)
   OP_AUDIO_OPEN    : constant Unsigned_32 := 16#0500#;
   OP_AUDIO_CLOSE   : constant Unsigned_32 := 16#0501#;
   OP_AUDIO_SET_VOL : constant Unsigned_32 := 16#0502#;
   OP_AUDIO_WAKE    : constant Unsigned_32 := 16#0506#;
   REPLY_OK         : constant Unsigned_32 := 16#F000#;

   function volToU32 is new Ada.Unchecked_Conversion (Volume, Unsigned_32);

   --  Ring buffer header offsets (bytes from grant base)
   HDR_WRITE_PTR    : constant := 16#00#;
   HDR_READ_PTR     : constant := 16#04#;
   HDR_STATE        : constant := 16#1C#;

   --  Grant region base (must match mixer)
   GRANT_REGION_BASE : constant Unsigned_64 := 16#4000_0000_0000#;
   GRANT_SLOT_SIZE   : constant Unsigned_64 := 4096 * 4096;

   --  Local stream table
   streamTable : array (StreamIndex) of StreamRecord;

   ---------------------------------------------------------------------------
   --  Atomic U32 read/write at an address. Publishing writePtr is the release
   --  edge for sample data; observing it is the consumer's acquire edge.
   ---------------------------------------------------------------------------
   procedure writeU32
     (addr : Unsigned_64; val : Unsigned_32);

   function readU32
     (addr : Unsigned_64) return Unsigned_32;

   procedure writeU32
     (addr : Unsigned_64; val : Unsigned_32) is
      mem : Unsigned_32
         with Import, Address => To_Address (Integer_Address (addr)), Atomic;
   begin
      mem := val;
   end writeU32;

   function readU32 (addr : Unsigned_64) return Unsigned_32 is
      mem : Unsigned_32
         with Import, Address => To_Address (Integer_Address (addr)), Atomic;
   begin
      return mem;
   end readU32;

   ---------------------------------------------------------------------------
   --  open
   ---------------------------------------------------------------------------
   function open
     (sampleRate : Unsigned_32;
      channels   : Unsigned_16;
      direction  : Unsigned_64 := DIRECTION_OUTPUT)
      return StreamHandle
   is
      msg      : Message;
      localIdx : StreamIndex;
      found    : Boolean := False;
   begin
      --  Find a free local slot
      for i in StreamIndex loop
         if not streamTable (i).active then
            localIdx := i;
            found := True;
            exit;
         end if;
      end loop;

      if not found then
         return NULL_STREAM;
      end if;

      --  Send OP_AUDIO_OPEN to mixer via capCall
      --  words(0) = sampleRate
      --  words(1) = channels(16) | format(16) | direction(32 high)
      msg := (tag => (label  => OP_AUDIO_OPEN,
                      length => 2,
                      flags  => 0,
                      badge  => 0),
              capBadge => 0,
              words => (0 => Unsigned_64 (sampleRate),
                        1 => Unsigned_64 (channels) or
                             Shift_Left (Unsigned_64 (FORMAT_S16LE), 16) or
                             Shift_Left (direction, 32),
                        others => 0));

      msg.tag := capCall (CAP_SLOT_MIXER, msg);

      if msg.tag.label /= REPLY_OK then
         return NULL_STREAM;
      end if;

      --  msg.words(0) = stream index (from mixer)
      --  msg.words(1) = grant ID (for ring addr computation)
      --  msg.words(2) = header size
      --  msg.words(3) = ring data size
      streamTable (localIdx) :=
        (active     => True,
         streamId   => msg.words (0),
         ringAddr   => GRANT_REGION_BASE +
                       msg.words (1) * GRANT_SLOT_SIZE,
         hdrSize    => Unsigned_32 (msg.words (2)),
         bufferSize => Unsigned_32 (msg.words (3)),
         channels   => channels);

      return (idx => localIdx, valid => True);
   end open;

   ---------------------------------------------------------------------------
   --  reserveWrite
   ---------------------------------------------------------------------------
   function reserveWrite
     (stream    : StreamHandle;
      maxFrames : Natural) return WriteReservation
   is
      s         : StreamRecord renames streamTable (stream.idx);
      hdrAddr   : constant Unsigned_64 := s.ringAddr;
      dataAddr  : constant Unsigned_64 := s.ringAddr + Unsigned_64 (s.hdrSize);
      wp        : Unsigned_32;
      rp        : Unsigned_32;
      used      : Unsigned_32;
      space     : Unsigned_32;
      frameSize : constant Unsigned_32 := Unsigned_32 (s.channels) * 2;
      toReserve : Natural;
      totalBytes : Unsigned_32;
      ringOff   : Unsigned_32;
      firstBytes : Unsigned_32;
      secondBytes : Unsigned_32;
   begin
      if not stream.valid or else not s.active or else maxFrames = 0 then
         return NULL_WRITE_RESERVATION;
      end if;

      wp := readU32 (hdrAddr + HDR_WRITE_PTR);
      rp := readU32 (hdrAddr + HDR_READ_PTR);

      --  The peer can modify shared header words, so validate the complete
      --  ring invariant once at this trust boundary before doing arithmetic.
      used := wp - rp;
      if s.bufferSize = 0 or else frameSize = 0 or else
         s.bufferSize mod frameSize /= 0 or else used > s.bufferSize or else
         wp mod frameSize /= 0 or else rp mod frameSize /= 0
      then
         return NULL_WRITE_RESERVATION;
      end if;

      space := s.bufferSize - used;
      toReserve := Natural'Min
        (maxFrames, Natural (space / frameSize));
      if toReserve = 0 then
         return NULL_WRITE_RESERVATION;
      end if;

      totalBytes := Unsigned_32 (toReserve) * frameSize;
      ringOff := wp mod s.bufferSize;
      firstBytes := Unsigned_32'Min
        (totalBytes, s.bufferSize - ringOff);
      secondBytes := totalBytes - firstBytes;

      return
        (streamIdx     => stream.idx,
         startWritePtr => wp,
         frameBytes    => frameSize,
         frameCount    => toReserve,
         first         =>
           (address => To_Address
              (Integer_Address (dataAddr + Unsigned_64 (ringOff))),
            frames  => Natural (firstBytes / frameSize)),
         second        =>
           (address => (if secondBytes = 0 then System.Null_Address
                        else To_Address (Integer_Address (dataAddr))),
            frames  => Natural (secondBytes / frameSize)),
         valid         => True);
   end reserveWrite;

   function isReservationValid
     (reservation : WriteReservation) return Boolean is
   begin
      return reservation.valid;
   end isReservationValid;

   function reservedFrames (reservation : WriteReservation) return Natural is
   begin
      if reservation.valid then
         return reservation.frameCount;
      else
         return 0;
      end if;
   end reservedFrames;

   function firstSpan (reservation : WriteReservation) return BufferSpan is
   begin
      if reservation.valid then
         return reservation.first;
      else
         return (address => System.Null_Address, frames => 0);
      end if;
   end firstSpan;

   function secondSpan (reservation : WriteReservation) return BufferSpan is
   begin
      if reservation.valid then
         return reservation.second;
      else
         return (address => System.Null_Address, frames => 0);
      end if;
   end secondSpan;

   procedure commitWrite
     (stream      : StreamHandle;
      reservation : in out WriteReservation;
      frames      : Natural;
      committed   : out Boolean)
   is
      s : StreamRecord renames streamTable (stream.idx);
      expectedFrameBytes : constant Unsigned_32 :=
        Unsigned_32 (s.channels) * 2;
   begin
      committed := False;
      if not stream.valid or else not s.active or else
         not reservation.valid or else
         reservation.streamIdx /= stream.idx or else
         frames = 0 or else frames > reservation.frameCount or else
         reservation.frameBytes /= expectedFrameBytes or else
         readU32 (s.ringAddr + HDR_WRITE_PTR) /= reservation.startWritePtr
      then
         return;
      end if;

      writeU32
        (s.ringAddr + HDR_WRITE_PTR,
         reservation.startWritePtr +
           Unsigned_32 (frames) * reservation.frameBytes);
      reservation := NULL_WRITE_RESERVATION;
      committed := True;
      notify;
   end commitWrite;

   procedure cancelWrite (reservation : in out WriteReservation) is
   begin
      reservation := NULL_WRITE_RESERVATION;
   end cancelWrite;

   ---------------------------------------------------------------------------
   --  write
   ---------------------------------------------------------------------------
   function write (stream : StreamHandle;
                   buf    : System.Address;
                   frames : Natural) return Natural
   is
      reservation : WriteReservation := reserveWrite (stream, frames);
      span1       : constant BufferSpan := firstSpan (reservation);
      span2       : constant BufferSpan := secondSpan (reservation);
      frameBytes  : constant Natural :=
        (if reservation.valid then Natural (reservation.frameBytes) else 0);
      copiedFrames : Natural := 0;
      committed    : Boolean;
   begin
      if not reservation.valid then
         return 0;
      end if;

      if span1.frames > 0 then
         copyFirst : declare
            result : constant System.Address := CuBit.String.memcpy
              (span1.address, buf,
               Storage_Count (span1.frames * frameBytes));
            pragma Unreferenced (result);
         begin
            null;
         end copyFirst;
         copiedFrames := span1.frames;
      end if;
      if span2.frames > 0 then
         copySecond : declare
            result : constant System.Address := CuBit.String.memcpy
              (span2.address,
               buf + Storage_Offset (copiedFrames * frameBytes),
               Storage_Count (span2.frames * frameBytes));
            pragma Unreferenced (result);
         begin
            null;
         end copySecond;
         copiedFrames := copiedFrames + span2.frames;
      end if;

      commitWrite (stream, reservation, copiedFrames, committed);
      if committed then
         return copiedFrames;
      else
         return 0;
      end if;
   end write;

   ---------------------------------------------------------------------------
   --  read
   ---------------------------------------------------------------------------
   function read (stream : StreamHandle;
                  buf    : System.Address;
                  frames : Natural) return Natural
   is
      s         : StreamRecord renames streamTable (stream.idx);
      hdrAddr   : constant Unsigned_64 := s.ringAddr;
      dataAddr  : constant Unsigned_64 := s.ringAddr + Unsigned_64 (s.hdrSize);

      wp        : Unsigned_32;
      rp        : Unsigned_32;
      available : Unsigned_32;
      frameSize : constant Unsigned_32 := Unsigned_32 (s.channels) * 2;
      toRead    : Natural;
      readBytes : Unsigned_32;
      ringOff   : Unsigned_32;

      type ByteArray is array (Natural range <>) of Unsigned_8
         with Convention => C;

      dst : ByteArray (0 .. frames * Natural (frameSize) - 1)
         with Import, Address => buf;
   begin
      if not stream.valid or not s.active then
         return 0;
      end if;

      wp := readU32 (hdrAddr + HDR_WRITE_PTR);
      rp := readU32 (hdrAddr + HDR_READ_PTR);
      available := wp - rp;

      toRead := Natural'Min (frames, Natural (available / frameSize));
      if toRead = 0 then
         return 0;
      end if;

      readBytes := Unsigned_32 (toRead) * frameSize;

      for i in 0 .. Natural (readBytes) - 1 loop
         ringOff := (rp + Unsigned_32 (i)) mod s.bufferSize;
         copyByte : declare
            srcByte : Unsigned_8
               with Import,
                    Address => To_Address (Integer_Address (
                       dataAddr + Unsigned_64 (ringOff))),
                    Volatile;
         begin
            dst (i) := srcByte;
         end copyByte;
      end loop;

      --  Advance read pointer
      writeU32 (hdrAddr + HDR_READ_PTR, rp + readBytes);

      return toRead;
   end read;

   ---------------------------------------------------------------------------
   --  start
   ---------------------------------------------------------------------------
   procedure start (stream : StreamHandle) is
      s : StreamRecord renames streamTable (stream.idx);
   begin
      if stream.valid and s.active then
         writeU32 (s.ringAddr + HDR_STATE, STATE_RUNNING);
         notify;
      end if;
   end start;

   ---------------------------------------------------------------------------
   --  pause
   ---------------------------------------------------------------------------
   procedure pause (stream : StreamHandle) is
      s : StreamRecord renames streamTable (stream.idx);
   begin
      if stream.valid and s.active then
         writeU32 (s.ringAddr + HDR_STATE, STATE_PAUSED);
         notify;
      end if;
   end pause;

   ---------------------------------------------------------------------------
   --  stop
   ---------------------------------------------------------------------------
   procedure stop (stream : StreamHandle) is
      s : StreamRecord renames streamTable (stream.idx);
   begin
      if stream.valid and s.active then
         writeU32 (s.ringAddr + HDR_STATE, STATE_STOPPED);
         notify;
      end if;
   end stop;

   ---------------------------------------------------------------------------
   --  setVolume
   ---------------------------------------------------------------------------
   procedure setVolume (stream : StreamHandle;
                        vol : Volume)
   is
      msg    : Message;
      ignore : MessageTag;
      pragma Warnings (Off, ignore);
      s   : StreamRecord renames streamTable (stream.idx);
   begin
      if not stream.valid or not s.active then
         return;
      end if;

      msg := (tag => (label  => OP_AUDIO_SET_VOL,
                      length => 2,
                      flags  => 0,
                      badge  => 0),
              capBadge => 0,
              words => (0 => s.streamId,
                        1 => Unsigned_64 (volToU32 (vol)),
                        others => 0));

      ignore := capCall (CAP_SLOT_MIXER, msg);
   end setVolume;

   ---------------------------------------------------------------------------
   --  close
   ---------------------------------------------------------------------------
   procedure close (stream : in out StreamHandle) is
      msg    : Message;
      ignore : MessageTag;
      pragma Warnings (Off, ignore);
      s   : StreamRecord renames streamTable (stream.idx);
   begin
      if not stream.valid or not s.active then
         return;
      end if;

      msg := (tag => (label  => OP_AUDIO_CLOSE,
                      length => 1,
                      flags  => 0,
                      badge  => 0),
              capBadge => 0,
              words => (0 => s.streamId, others => 0));

      ignore := capCall (CAP_SLOT_MIXER, msg);

      s.active := False;
      stream.valid := False;
   end close;

   ---------------------------------------------------------------------------
   --  isValid
   ---------------------------------------------------------------------------
   function isValid (stream : StreamHandle) return Boolean is
   begin
      return stream.valid;
   end isValid;

   ---------------------------------------------------------------------------
   --  notify
   ---------------------------------------------------------------------------
   procedure notify is
      msg : constant Message :=
        (tag => (label => OP_AUDIO_WAKE, length => 0,
                 flags => 0, badge => 0),
         capBadge => 0,
         words => (others => 0));
      submitted : Boolean;
   begin
      submitted := capSubmit
        (CAP_SLOT_MIXER, msg, NO_COMPLETION_TOKEN);
      if not submitted then
         return;
      end if;
   end notify;

end CuBit.Audio;
