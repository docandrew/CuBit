------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Audio mixer service (userspace)
--
--  @description
--  Central audio hub. Manages client stream grants, mixes PCM data from
--  multiple clients, and writes completed mixes directly into the isolated
--  HDA PCM DMA grant.  Device-period notifications and client wakeups arrive
--  through one-way capability IPC; control operations use capCall.
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
   OP_AUDIO_WAKE     : constant Unsigned_32 := 16#0506#;
   OP_AUDIO_HW_INIT  : constant Unsigned_32 := 16#0510#;
   OP_AUDIO_HW_START : constant Unsigned_32 := 16#0511#;
   OP_AUDIO_HW_STOP  : constant Unsigned_32 := 16#0512#;
   OP_AUDIO_HW_PERIOD : constant Unsigned_32 := 16#0516#;
   REPLY_OK           : constant Unsigned_32 := 16#F000#;
   REPLY_ERR          : constant Unsigned_32 := 16#F001#;

   --  Capability slots (assigned by devmgr)
   CAP_SLOT_HDA     : constant Unsigned_64 := 4;

   msg         : Message;
   from        : ProcessID;
   ret         : Unsigned_64;
   mixBuf      : Mixer.MixBuffer;
   mixFrames   : Natural;
   dmaRingAddr : Unsigned_64 := 0;
   periodBytes : Unsigned_32 := 0;
   periodCount : Natural := 0;
   hdaReady    : Boolean := False;
   hdaRunning  : Boolean := False;
   lastPeriodSequence : Unsigned_64 := 0;
   statsStartMs : Unsigned_64 := 0;
   statsPeriods : Unsigned_64 := 0;
   statsActivePeriods : Unsigned_64 := 0;
   statsSilentPeriods : Unsigned_64 := 0;
   statsMissedPeriods : Unsigned_64 := 0;
   statsUnderrunsLast : Unsigned_64 := 0;
   periodIRQObserved : Boolean := False;

   procedure printDec (val : Unsigned_64) is
      buf : String (1 .. 20);
      pos : Natural := buf'Last;
      v   : Unsigned_64 := val;
   begin
      if v = 0 then
         debugPrint ("0");
         return;
      end if;

      while v > 0 loop
         buf (pos) := Character'Val (Character'Pos ('0') +
                                      Natural (v mod 10));
         v := v / 10;
         pos := pos - 1;
      end loop;

      debugPrint (buf (pos + 1 .. buf'Last));
   end printDec;

   function totalUnderruns return Unsigned_64 is
      total : Unsigned_64 := 0;
   begin
      for i in Mixer.streams'Range loop
         if Mixer.streams (i).active and then Mixer.streams (i).ringAddr /= 0
         then
            declare
               hdr : Mixer.RingHeader
                  with Import,
                       Address => To_Address
                         (Integer_Address (Mixer.streams (i).ringAddr)),
                       Volatile;
            begin
               total := total + Unsigned_64 (hdr.underruns);
            end;
         end if;
      end loop;

      return total;
   end totalUnderruns;

   procedure maybePrintStats is
      now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      underruns : Unsigned_64;
   begin
      if now = Unsigned_64'Last then
         return;
      end if;

      if statsStartMs = 0 then
         statsStartMs := now;
         statsUnderrunsLast := totalUnderruns;
         return;
      end if;

      if now < statsStartMs or else now - statsStartMs < 1000 then
         return;
      end if;

      underruns := totalUnderruns;
      debugPrint ("mixer: stats periods=");
      printDec (statsPeriods);
      debugPrint (" active=");
      printDec (statsActivePeriods);
      debugPrint (" silent=");
      printDec (statsSilentPeriods);
      debugPrint (" missed_periods=");
      printDec (statsMissedPeriods);
      debugPrint (" underruns=");
      if underruns >= statsUnderrunsLast then
         printDec (underruns - statsUnderrunsLast);
      else
         printDec (underruns);
      end if;
      debugPrint ("" & LF);

      statsStartMs := now;
      statsPeriods := 0;
      statsActivePeriods := 0;
      statsSilentPeriods := 0;
      statsMissedPeriods := 0;
      statsUnderrunsLast := underruns;
   end maybePrintStats;

   procedure sendReply (label : Unsigned_32;
                        w0    : Unsigned_64 := 0;
                        w1    : Unsigned_64 := 0;
                        w2    : Unsigned_64 := 0;
                        w3    : Unsigned_64 := 0)
   is
      ignore : Unsigned_64;
   begin
      ignore := replyCap (CapabilitySlot'Last,
         (tag => (label  => label,
                  length => 4,
                  flags  => 0,
                  badge  => 0),
          capBadge => 0,
          words => (0 => w0, 1 => w1, 2 => w2, 3 => w3)));
   end sendReply;

   procedure mixIntoPeriod (slot : Natural) is
      target : Unsigned_64;
   begin
      if not hdaReady or else slot >= periodCount then
         return;
      end if;

      target := dmaRingAddr + Unsigned_64 (slot) * Unsigned_64 (periodBytes);
      mixFrames := Mixer.mixPeriod (mixBuf, target, Mixer.MIX_FRAMES);
      statsPeriods := statsPeriods + 1;
      if mixFrames > 0 then
         statsActivePeriods := statsActivePeriods + 1;
      else
         statsSilentPeriods := statsSilentPeriods + 1;
      end if;
   end mixIntoPeriod;

   procedure startHardware is
      ctlMsg : Message;
   begin
      if not hdaReady or else hdaRunning or else
         not Mixer.hasRunningOutput
      then
         return;
      end if;

      --  Before the device starts, every period belongs to the mixer.  Prime
      --  the complete cyclic buffer so the first sample can play immediately.
      for slot in 0 .. periodCount - 1 loop
         mixIntoPeriod (slot);
      end loop;

      ctlMsg :=
        (tag => (label => OP_AUDIO_HW_START, length => 0,
                 flags => 0, badge => 0),
         capBadge => 0,
         words => (others => 0));
      ctlMsg.tag := capCall (CAP_SLOT_HDA, ctlMsg);
      if ctlMsg.tag.label = REPLY_OK then
         hdaRunning := True;
         lastPeriodSequence := 0;
      else
         debugPrint ("mixer: HDA start failed" & LF);
      end if;
   end startHardware;

   procedure stopHardware is
      ctlMsg : Message;
   begin
      if not hdaReady or else not hdaRunning then
         return;
      end if;

      ctlMsg :=
        (tag => (label => OP_AUDIO_HW_STOP, length => 0,
                 flags => 0, badge => 0),
         capBadge => 0,
         words => (others => 0));
      ctlMsg.tag := capCall (CAP_SLOT_HDA, ctlMsg);
      if ctlMsg.tag.label = REPLY_OK then
         hdaRunning := False;
      end if;
   end stopHardware;

begin
   debugPrint ("mixer: starting" & ASCII.LF);

   ret := setLatencyContract
      (LATENCY_REALTIME,
       5_000,   --  One 256-frame period is about 5.3 ms at 48 kHz.
       1_500);  --  Budget hint: mix and submit one short audio period.
   if ret = Unsigned_64'Last then
      debugPrint ("mixer: latency contract rejected" & ASCII.LF);
   end if;

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

   --  Ask HDA to grant only its DMA-visible PCM period page to the mixer.
   --  Descriptor, CORB/RIRB, and MMIO pages remain inaccessible here.
   declare
      initMsg : Message;
      gid     : Unsigned_64;
      bytes   : Unsigned_64;
      count   : Unsigned_64;
   begin
      initMsg :=
        (tag => (label => OP_AUDIO_HW_INIT, length => 0,
                 flags => 0, badge => 0),
         capBadge => 0,
         words => (others => 0));
      initMsg.tag := capCall (CAP_SLOT_HDA, initMsg);

      gid := initMsg.words (0);
      bytes := initMsg.words (1);
      count := initMsg.words (2);
      if initMsg.tag.label = REPLY_OK and then
         bytes = Unsigned_64 (Mixer.MIX_FRAMES * 4) and then
         count > 0 and then count <= 32 and then
         bytes * count <= 4096
      then
         dmaRingAddr := 16#4000_0000_0000# + gid * (4096 * 4096);
         periodBytes := Unsigned_32 (bytes);
         periodCount := Natural (count);
         hdaReady := True;
         debugPrint ("mixer: direct HDA period grant ready" & LF);
      else
         debugPrint ("mixer: HDA period grant failed" & LF);
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

   --  Main loop: block for client control/wakeup messages or HDA completion
   --  messages.  There is no timer polling and no synchronous per-period IPC.
   loop
      receive (from, msg);

      if msg.tag.label = OP_AUDIO_HW_PERIOD then
         if hdaRunning and then msg.words (0) < Unsigned_64 (periodCount) then
            if not periodIRQObserved then
               debugPrint ("mixer: HDA period IRQ active" & LF);
               periodIRQObserved := True;
            end if;
            if lastPeriodSequence /= 0 and then
               msg.words (1) > lastPeriodSequence + 1
            then
               statsMissedPeriods := statsMissedPeriods +
                 (msg.words (1) - lastPeriodSequence - 1);
            end if;
            if msg.words (1) > lastPeriodSequence then
               lastPeriodSequence := msg.words (1);
               mixIntoPeriod (Natural (msg.words (0)));
            end if;
         end if;

      elsif msg.tag.label = OP_AUDIO_WAKE then
         if Mixer.hasRunningOutput then
            startHardware;
         else
            stopHardware;
         end if;

      else
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
                     sendReply (REPLY_OK,
                                w0 => Unsigned_64 (idx),
                                w1 => Mixer.streams (idx).grantId,
                                w2 => Unsigned_64 (Mixer.RING_HDR_SIZE),
                                w3 => Unsigned_64 (Mixer.RING_DATA_SIZE));
                  else
                     sendReply (REPLY_ERR);
                  end if;
               end openStream;

            when OP_AUDIO_CLOSE =>
               --  words(0) = streamId
               closeStream : declare
                  streamIdx : constant Natural :=
                     Natural (msg.words (0));
               begin
                  Mixer.closeStream (streamIdx);
                  sendReply (REPLY_OK);
                  if not Mixer.hasRunningOutput then
                     stopHardware;
                  end if;
               end closeStream;

            when OP_AUDIO_SET_VOL =>
               --  words(0) = streamId, words(1) = volume 16.16
               Mixer.setVolume (Natural (msg.words (0)),
                                Unsigned_32 (msg.words (1)));
               sendReply (REPLY_OK);

            when OP_AUDIO_GET_VOL =>
               --  words(0) = streamId
               getVol : declare
                  idx : constant Natural := Natural (msg.words (0));
               begin
                  if idx <= Mixer.streams'Last and then
                     Mixer.streams (idx).active
                  then
                     sendReply (REPLY_OK,
                                w0 => Unsigned_64 (Mixer.volToRaw (
                                   Mixer.streams (idx).vol)));
                  else
                     sendReply (REPLY_ERR);
                  end if;
               end getVol;

            when OP_AUDIO_SET_PAN =>
               --  words(0) = streamId, words(1) = pan 16.16
               Mixer.setPan (Natural (msg.words (0)),
                             Unsigned_32 (msg.words (1)));
               sendReply (REPLY_OK);

            when OP_AUDIO_SET_FMT =>
               --  Format change not yet supported at runtime
               sendReply (REPLY_ERR);

            when others =>
               sendReply (REPLY_ERR);
         end case;
      end if;

      maybePrintStats;
   end loop;
end main;
