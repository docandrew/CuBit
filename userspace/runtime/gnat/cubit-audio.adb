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

package body CuBit.Audio is

   --  IPC labels (must match kernel/src/ipc_labels.ads)
   OP_AUDIO_OPEN    : constant Unsigned_32 := 16#0500#;
   OP_AUDIO_CLOSE   : constant Unsigned_32 := 16#0501#;
   OP_AUDIO_SET_VOL : constant Unsigned_32 := 16#0502#;
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
   --  Volatile U32 read/write at an address
   ---------------------------------------------------------------------------
   procedure writeU32
     (addr : Unsigned_64; val : Unsigned_32);

   function readU32
     (addr : Unsigned_64) return Unsigned_32;

   procedure writeU32
     (addr : Unsigned_64; val : Unsigned_32) is
      mem : Unsigned_32
         with Import, Address => To_Address (Integer_Address (addr)), Volatile;
   begin
      mem := val;
   end writeU32;

   function readU32 (addr : Unsigned_64) return Unsigned_32 is
      mem : Unsigned_32
         with Import, Address => To_Address (Integer_Address (addr)), Volatile;
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
   --  write
   ---------------------------------------------------------------------------
   function write (stream : StreamHandle;
                   buf    : System.Address;
                   frames : Natural) return Natural
   is
      s         : StreamRecord renames streamTable (stream.idx);
      hdrAddr   : constant Unsigned_64 := s.ringAddr;
      dataAddr  : constant Unsigned_64 := s.ringAddr + Unsigned_64 (s.hdrSize);

      wp        : Unsigned_32;
      rp        : Unsigned_32;
      space     : Unsigned_32;
      frameSize : constant Unsigned_32 := Unsigned_32 (s.channels) * 2;
      toWrite   : Natural;
      writeBytes : Unsigned_32;
      ringOff   : Unsigned_32;

      type ByteArray is array (Natural range <>) of Unsigned_8
         with Convention => C;

      src : ByteArray (0 .. frames * Natural (frameSize) - 1)
         with Import, Address => buf;
   begin
      if not stream.valid or not s.active then
         return 0;
      end if;

      wp := readU32 (hdrAddr + HDR_WRITE_PTR);
      rp := readU32 (hdrAddr + HDR_READ_PTR);
      space := s.bufferSize - (wp - rp);

      toWrite := Natural'Min (frames, Natural (space / frameSize));
      if toWrite = 0 then
         return 0;
      end if;

      writeBytes := Unsigned_32 (toWrite) * frameSize;

      --  Copy samples to ring buffer with wrapping
      for i in 0 .. Natural (writeBytes) - 1 loop
         ringOff := (wp + Unsigned_32 (i)) mod s.bufferSize;
         copyByte : declare
            dst : Unsigned_8
               with Import,
                    Address => To_Address (Integer_Address (
                       dataAddr + Unsigned_64 (ringOff))),
                    Volatile;
         begin
            dst := src (i);
         end copyByte;
      end loop;

      --  Advance write pointer
      writeU32 (hdrAddr + HDR_WRITE_PTR, wp + writeBytes);

      return toWrite;
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
   begin
      capNotify (CAP_SLOT_MIXER_NTF);
   end notify;

end CuBit.Audio;
