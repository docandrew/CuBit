------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Audio mixer service (userspace)
--
--  @description
--  Central audio hub. Manages client stream grants, mixes PCM data from
--  multiple clients, and feeds the HDA driver.  Uses the ring buffer protocol
--  for zero-copy client communication and capCall for HDA driver interaction.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with Mixer;

procedure main is
   use ASCII;

   --  IPC labels
   OP_AUDIO_OPEN     : constant Unsigned_32 := 16#0500#;
   OP_AUDIO_CLOSE    : constant Unsigned_32 := 16#0501#;
   OP_AUDIO_SET_VOL  : constant Unsigned_32 := 16#0502#;
   OP_AUDIO_GET_VOL  : constant Unsigned_32 := 16#0503#;
   OP_AUDIO_SET_PAN  : constant Unsigned_32 := 16#0504#;
   OP_AUDIO_SET_FMT  : constant Unsigned_32 := 16#0505#;
   OP_AUDIO_HW_FILL  : constant Unsigned_32 := 16#0513#;
   REPLY_OK           : constant Unsigned_32 := 16#F000#;
   REPLY_ERR          : constant Unsigned_32 := 16#F001#;

   --  Capability slots (assigned by devmgr)
   CAP_SLOT_HDA     : constant Unsigned_64 := 4;
   CAP_SLOT_HDA_NTF : constant Unsigned_64 := 5;

   STAGING_PAGES : constant := 2;

   msg         : Message;
   from        : ProcessID;
   ret         : Unsigned_64;
   found       : Boolean;
   mixBuf      : Mixer.MixBuffer;
   mixFrames   : Natural;
   fillMsg     : Message;
   stagingAddr : Unsigned_64 := 0;

   --  Silence flush: after audio stops, send enough silent periods to
   --  overwrite all BDL slots, then stop capCalling to save CPU.
   silenceCount : Natural := 0;
   SILENCE_FLUSH : constant Natural := 4;  --  NUM_BDL_ENTRIES

   --  Mix timer: run at ~5ms intervals (~48kHz / 256 frames = 5.33ms)
   MIX_INTERVAL_MS : constant Unsigned_64 := 5;

   procedure sendReply (dest  : ProcessID;
                        label : Unsigned_32;
                        w0    : Unsigned_64 := 0;
                        w1    : Unsigned_64 := 0;
                        w2    : Unsigned_64 := 0;
                        w3    : Unsigned_64 := 0)
   is
      ignore : Unsigned_64;
   begin
      ignore := reply (dest,
         (tag => (label  => label,
                  length => 4,
                  flags  => 0,
                  badge  => 0),
          capBadge => 0,
          words => (0 => w0, 1 => w1, 2 => w2, 3 => w3)));
   end sendReply;

begin
   debugPrint ("mixer: starting" & ASCII.LF);

   --  Pre-allocate ring buffers for all stream slots via sbrk.
   --  Each slot gets RING_PAGES (2) pages of page-aligned memory.
   declare
      PAGE_SIZE : constant Unsigned_64 := 4096;
      totalBytes : constant Unsigned_64 :=
         Unsigned_64 (Mixer.MAX_STREAMS) *
         Unsigned_64 (Mixer.RING_PAGES) * PAGE_SIZE;
      raw     : Unsigned_64;
      aligned : Unsigned_64;
   begin
      --  Allocate with extra page for alignment
      raw := syscall (SYSCALL_SBRK, totalBytes + PAGE_SIZE);
      if raw /= Unsigned_64'Last then
         aligned := (raw + PAGE_SIZE - 1) and not (PAGE_SIZE - 1);
         for i in 0 .. Mixer.MAX_STREAMS - 1 loop
            Mixer.ringBufBase (i) := aligned +
               Unsigned_64 (i) * Unsigned_64 (Mixer.RING_PAGES) * PAGE_SIZE;
         end loop;
         debugPrint ("mixer: ring buffers allocated" & ASCII.LF);
      else
         debugPrint ("mixer: sbrk failed for ring buffers" & ASCII.LF);
      end if;
   end;

   --  Allocate staging buffer and grant to HDA driver
   declare
      PAGE_SIZE : constant Unsigned_64 := 4096;
      raw       : Unsigned_64;
      aligned   : Unsigned_64;
      gid       : Unsigned_64;
      grantOK   : Boolean;
      hdaPID    : Unsigned_64;
   begin
      raw := syscall (SYSCALL_SBRK,
         Unsigned_64 (STAGING_PAGES) * PAGE_SIZE + PAGE_SIZE);

      if raw /= Unsigned_64'Last then
         aligned := (raw + PAGE_SIZE - 1) and not (PAGE_SIZE - 1);
         stagingAddr := aligned;

         --  Wait for HDA driver to register
         hdaPID := 0;
         for attempt in 1 .. 40 loop
            hdaPID := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_HDA);
            exit when hdaPID /= 0;
            ret := syscall (SYSCALL_SLEEP, 50);
         end loop;

         if hdaPID /= 0 then
            createGrant (
               grantee   => ProcessID (hdaPID),
               localAddr => To_Address (Integer_Address (stagingAddr)),
               numPages  => STAGING_PAGES,
               readWrite => False,
               grantId   => gid,
               success   => grantOK);

            if grantOK then
               --  Tell HDA where the staging buffer is mapped
               declare
                  OP_AUDIO_HW_INIT : constant Unsigned_32 := 16#0510#;
                  initMsg : Message :=
                     (tag => (label  => OP_AUDIO_HW_INIT,
                              length => 1,
                              flags  => 0,
                              badge  => 0),
                      capBadge => 0,
                      words => (0 => gid, others => 0));
               begin
                  initMsg.tag := capCall (CAP_SLOT_HDA, initMsg);
               end;
               debugPrint ("mixer: staging granted to HDA" & ASCII.LF);
            else
               debugPrint ("mixer: staging grant failed" & ASCII.LF);
               stagingAddr := 0;
            end if;
         else
            debugPrint ("mixer: HDA driver not found" & ASCII.LF);
            stagingAddr := 0;
         end if;
      else
         debugPrint ("mixer: sbrk failed for staging" & ASCII.LF);
      end if;
   end;

   --  Register as mixer driver
   ret := registerDriver (DRIVER_MIXER);

   --  Signal devmgr that we are ready
   declare
      CAP_SLOT_READY : constant Unsigned_64 := 15;
      OP_READY       : constant Unsigned_32 := 16#FF00#;
      ignore : MessageTag;
   begin
      ignore := capSend (CAP_SLOT_READY,
         (tag      => (label => OP_READY, length => 0,
                       flags => 0, badge => 0),
          capBadge => 0,
          words    => (others => 0)));
   end;

   debugPrint ("mixer: registered, entering service loop" & ASCII.LF);

   --  Main loop: poll for IPC messages and periodically mix audio
   loop
      --  Non-blocking service-request receive. Audio control messages are
      --  client work; event/notification traffic must not be consumed here.
      Poll_Service_Request (from, msg, found);

      if found then
         from := ProcessID (msg.capBadge);

         case msg.tag.label is
            when OP_AUDIO_OPEN =>
               --  words(0) = sampleRate
               --  words(1) = channels(16) | format(16) | direction(32 high)
               openStream : declare
                  sampleRate : constant Unsigned_32 :=
                     Unsigned_32 (msg.words (0));
                  channels : constant Unsigned_16 :=
                     Unsigned_16 (msg.words (1) and 16#FFFF#);
                  format : constant Unsigned_16 :=
                     Unsigned_16 (Shift_Right (msg.words (1), 16) and 16#FFFF#);
                  direction : constant Unsigned_64 :=
                     Shift_Right (msg.words (1), 32);
                  idx : Integer;
               begin
                  idx := Mixer.openStream (from, sampleRate,
                                           channels, format, direction);
                  if idx >= 0 then
                     --  Reply: w0=streamIdx, w1=grantId, w2=hdrSz, w3=dataSz
                     sendReply (from, REPLY_OK,
                                w0 => Unsigned_64 (idx),
                                w1 => Mixer.streams (idx).grantId,
                                w2 => Unsigned_64 (Mixer.RING_HDR_SIZE),
                                w3 => Unsigned_64 (Mixer.RING_DATA_SIZE));
                  else
                     sendReply (from, REPLY_ERR);
                  end if;
               end openStream;

            when OP_AUDIO_CLOSE =>
               --  words(0) = streamId
               closeStream : declare
                  streamIdx : constant Natural :=
                     Natural (msg.words (0));
               begin
                  Mixer.closeStream (streamIdx);
                  sendReply (from, REPLY_OK);
               end closeStream;

            when OP_AUDIO_SET_VOL =>
               --  words(0) = streamId, words(1) = volume 16.16
               Mixer.setVolume (Natural (msg.words (0)),
                                Unsigned_32 (msg.words (1)));
               sendReply (from, REPLY_OK);

            when OP_AUDIO_GET_VOL =>
               --  words(0) = streamId
               getVol : declare
                  idx : constant Natural := Natural (msg.words (0));
               begin
                  if idx <= Mixer.streams'Last and then
                     Mixer.streams (idx).active
                  then
                     sendReply (from, REPLY_OK,
                                w0 => Unsigned_64 (Mixer.volToRaw (
                                   Mixer.streams (idx).vol)));
                  else
                     sendReply (from, REPLY_ERR);
                  end if;
               end getVol;

            when OP_AUDIO_SET_PAN =>
               --  words(0) = streamId, words(1) = pan 16.16
               Mixer.setPan (Natural (msg.words (0)),
                             Unsigned_32 (msg.words (1)));
               sendReply (from, REPLY_OK);

            when OP_AUDIO_SET_FMT =>
               --  Format change not yet supported at runtime
               sendReply (from, REPLY_ERR);

            when others =>
               sendReply (from, REPLY_ERR);
         end case;
      end if;

      --  Mix a period and send to HDA driver.
      --  When audio stops, flush all BDL slots with silence then stop
      --  capCalling to avoid starving other processes.
      if stagingAddr /= 0 then
         mixFrames := Mixer.mixPeriod (mixBuf, stagingAddr,
                                        Mixer.MIX_FRAMES);

         if mixFrames > 0 then
            --  Active audio: always send, reset flush counter
            silenceCount := 0;
            fillMsg := (tag => (label  => OP_AUDIO_HW_FILL,
                                length => 2,
                                flags  => 0,
                                badge  => 0),
                        capBadge => 0,
                        words => (0 => 0,
                                  1 => Unsigned_64 (Mixer.MIX_FRAMES * 4),
                                  others => 0));
            fillMsg.tag := capCall (CAP_SLOT_HDA, fillMsg);
         elsif silenceCount < SILENCE_FLUSH then
            --  Flush remaining BDL slots with silence
            silenceCount := silenceCount + 1;
            fillMsg := (tag => (label  => OP_AUDIO_HW_FILL,
                                length => 2,
                                flags  => 0,
                                badge  => 0),
                        capBadge => 0,
                        words => (0 => 0,
                                  1 => Unsigned_64 (Mixer.MIX_FRAMES * 4),
                                  others => 0));
            fillMsg.tag := capCall (CAP_SLOT_HDA, fillMsg);
         end if;
      end if;

      --  Sleep briefly to yield CPU when no audio is active
      ret := syscall (SYSCALL_SLEEP, MIX_INTERVAL_MS);
   end loop;
end main;
