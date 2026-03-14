------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Intel High Definition Audio controller driver implementation
------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;

package body HDA is

   --  MMIO base (mapped via SYSCALL_MAP_DEVICE at BAR_VIRT_BASE)
   barBase : Unsigned_64 := BAR_VIRT_BASE;

   procedure printHex64 (val : Unsigned_64);

   procedure printHex64 (val : Unsigned_64) is
      hex : constant String := "0123456789ABCDEF";
      s   : String (1 .. 18) := "0x0000000000000000";
      v   : Unsigned_64 := val;
   begin
      for i in reverse 3 .. 18 loop
         s (i) := hex (Natural (v and 16#0F#) + 1);
         v := Shift_Right (v, 4);
      end loop;
      debugPrint (s);
   end printHex64;

   --  CORB/RIRB state
   corbWp   : Unsigned_16 := 0;  --  Our write pointer into CORB
   rirbRp   : Unsigned_16 := 0;  --  Our read pointer from RIRB

   ---------------------------------------------------------------------------
   --  MMIO helpers
   ---------------------------------------------------------------------------

   procedure writeReg32 (offset : Unsigned_64; value : Unsigned_32) is
      reg : Unsigned_32
         with Import, Address => To_Address (Integer_Address (barBase + offset)),
              Volatile;
   begin
      reg := value;
   end writeReg32;

   function readReg32 (offset : Unsigned_64) return Unsigned_32 is
      reg : Unsigned_32
         with Import, Address => To_Address (Integer_Address (barBase + offset)),
              Volatile;
   begin
      return reg;
   end readReg32;

   procedure writeReg16 (offset : Unsigned_64; value : Unsigned_16) is
      reg : Unsigned_16
         with Import, Address => To_Address (Integer_Address (barBase + offset)),
              Volatile;
   begin
      reg := value;
   end writeReg16;

   function readReg16 (offset : Unsigned_64) return Unsigned_16 is
      reg : Unsigned_16
         with Import, Address => To_Address (Integer_Address (barBase + offset)),
              Volatile;
   begin
      return reg;
   end readReg16;

   procedure writeReg8 (offset : Unsigned_64; value : Unsigned_8) is
      reg : Unsigned_8
         with Import, Address => To_Address (Integer_Address (barBase + offset)),
              Volatile;
   begin
      reg := value;
   end writeReg8;

   function readReg8 (offset : Unsigned_64) return Unsigned_8 is
      reg : Unsigned_8
         with Import, Address => To_Address (Integer_Address (barBase + offset)),
              Volatile;
   begin
      return reg;
   end readReg8;

   ---------------------------------------------------------------------------
   --  DMA buffer overlays
   ---------------------------------------------------------------------------

   corb : CORBArray
      with Import,
           Address => To_Address (Integer_Address (DMA_VIRT_BASE + DMA_CORB_OFF)),
           Volatile;

   rirb : RIRBArray
      with Import,
           Address => To_Address (Integer_Address (DMA_VIRT_BASE + DMA_RIRB_OFF)),
           Volatile;

   bdl : BDLArray
      with Import,
           Address => To_Address (Integer_Address (DMA_VIRT_BASE + DMA_BDL_OFF)),
           Volatile;

   ---------------------------------------------------------------------------
   --  spinWait - simple delay loop
   ---------------------------------------------------------------------------
   procedure spinWait (ms : Unsigned_64) is
      ignore : Unsigned_64;
   begin
      ignore := syscall (SYSCALL_SLEEP, ms);
   end spinWait;

   ---------------------------------------------------------------------------
   --  initController
   ---------------------------------------------------------------------------
   procedure initController is
      gctl   : Unsigned_32;
      gcap   : Unsigned_16;
      numISS : Natural;
      numOSS : Natural;
      statests : Unsigned_16;
      corbPhys : Unsigned_64;
      rirbPhys : Unsigned_64;
      bar0Phys : Unsigned_64;
      ret      : Unsigned_64;
   begin
      --  Map BAR0 into our address space
      bar0Phys := getInfo (SYSINFO_HDA_BAR0);
      ret := syscall (SYSCALL_MAP_DEVICE, bar0Phys,
                       BAR_VIRT_BASE, BAR_MAP_PAGES);
      dmaPhysBase := getInfo (SYSINFO_HDA_DMA_PHYS);

      --  Reset the controller
      gctl := readReg32 (REG_GCTL);
      writeReg32 (REG_GCTL, gctl and (not GCTL_CRST));
      spinWait (5);

      --  Wait for reset to take effect
      for i in 1 .. 100 loop
         gctl := readReg32 (REG_GCTL);
         exit when (gctl and GCTL_CRST) = 0;
         spinWait (1);
      end loop;

      --  Bring controller out of reset
      writeReg32 (REG_GCTL, GCTL_CRST);

      --  Wait for controller ready
      for i in 1 .. 100 loop
         gctl := readReg32 (REG_GCTL);
         exit when (gctl and GCTL_CRST) /= 0;
         spinWait (1);
      end loop;

      --  Wait for codec enumeration
      spinWait (10);

      --  Check for codec(s) on STATESTS
      statests := readReg16 (REG_STATESTS);
      gcap := readReg16 (REG_GCAP);
      codecFound := (statests and 16#0001#) /= 0;

      if not codecFound then
         debugPrint ("hda: no codec found" & ASCII.LF);
         return;
      end if;

      --  Clear STATESTS by writing 1s
      writeReg16 (REG_STATESTS, statests);

      --  Read GCAP to find stream counts
      --  Bits 7:3=BSS, 11:8=ISS, 15:12=OSS
      gcap := readReg16 (REG_GCAP);
      numISS := Natural (Shift_Right (gcap, 8) and 16#0F#);
      numOSS := Natural (Shift_Right (gcap, 12) and 16#0F#);
      numOutputStreams := numOSS;

      --  Compute output stream 0 base register offset
      --  Streams start at offset 0x80, each is 0x20 bytes.
      --  Input streams come first, then output streams.
      outputStreamBase := 16#80# + Unsigned_64 (numISS) * 16#20#;

      debugPrint ("hda: GCAP=");
      printHex64 (Unsigned_64 (gcap));
      debugPrint (" ISS=");
      printHex64 (Unsigned_64 (numISS));
      debugPrint (" OSS=");
      printHex64 (Unsigned_64 (numOSS));
      debugPrint (" sdBase=");
      printHex64 (outputStreamBase);
      debugPrint ("" & ASCII.LF);

      --  Setup CORB
      corbPhys := dmaPhysBase + DMA_CORB_OFF;

      --  Stop CORB + RIRB
      writeReg8 (REG_CORBCTL, 0);
      writeReg8 (REG_RIRBCTL, 0);
      spinWait (2);

      --  Set CORB size to 256 entries (value 2)
      writeReg8 (REG_CORBSIZE, 2);

      --  Set CORB base address (phys for DMA)
      writeReg32 (REG_CORBLBASE,
         Unsigned_32 (corbPhys and 16#FFFF_FFFF#));
      writeReg32 (REG_CORBUBASE,
         Unsigned_32 (Shift_Right (corbPhys, 32)));


      --  Reset CORB read pointer
      writeReg16 (REG_CORBRP, 16#8000#);
      for i in 1 .. 1000 loop
         exit when
            (readReg16 (REG_CORBRP) and 16#8000#) /= 0;
         spinWait (1);
      end loop;
      writeReg16 (REG_CORBRP, 0);
      for i in 1 .. 1000 loop
         exit when
            (readReg16 (REG_CORBRP) and 16#8000#) = 0;
         spinWait (1);
      end loop;

      --  Reset CORB write pointer
      writeReg16 (REG_CORBWP, 0);
      corbWp := 0;

      --  Start CORB DMA
      writeReg8 (REG_CORBCTL, CORBCTL_RUN);
      spinWait (2);

      --  Setup RIRB
      rirbPhys := dmaPhysBase + DMA_RIRB_OFF;

      --  Set RIRB size to 256 entries
      writeReg8 (REG_RIRBSIZE, 2);

      --  Set RIRB base address (phys for DMA)
      writeReg32 (REG_RIRBLBASE,
         Unsigned_32 (rirbPhys and 16#FFFF_FFFF#));
      writeReg32 (REG_RIRBUBASE,
         Unsigned_32 (Shift_Right (rirbPhys, 32)));

      --  Reset RIRB write pointer
      writeReg16 (REG_RIRBWP, 16#8000#);
      spinWait (1);
      rirbRp := readReg16 (REG_RIRBWP) and 16#00FF#;

      --  Set RIRB interrupt count (must be non-zero
      --  for QEMU CORB to process commands)
      writeReg16 (REG_RINTCNT, 255);

      --  Start RIRB DMA + interrupts
      writeReg8 (REG_RIRBCTL, RIRBCTL_RUN or RIRBCTL_INT);
      spinWait (2);


      --  Let CORB/RIRB DMA engines stabilize
      spinWait (10);

      --  Enable controller + output stream 0 interrupts
      --  Bits 29:0 are per-stream enables; output stream 0 = bit numISS
      writeReg32 (REG_INTCTL,
         INTCTL_GIE or INTCTL_CIE or
         Shift_Left (1, Natural (numISS)));

      --  Read codec vendor ID
      codecVendor := sendVerb (
         0, VERB_GET_PARAM or PARAM_VENDOR_ID);

      debugPrint ("hda: controller init complete" & ASCII.LF);
   end initController;

   ---------------------------------------------------------------------------
   --  sendVerb - send a verb to codec 0 and poll for response
   ---------------------------------------------------------------------------
   function sendVerb (nid  : Unsigned_8;
                      verb : Unsigned_32) return Unsigned_32
   is
      cmd : Unsigned_32;
      wp  : Unsigned_16;
   begin
      --  Build command: codec=0 (bits 31-28),
      --  NID (bits 27-20), verb (bits 19-0)
      cmd := Shift_Left (Unsigned_32 (nid), 20) or
             (verb and 16#000F_FFFF#);

      --  Write command into CORB at next slot
      corbWp := (corbWp + 1) mod
                Unsigned_16 (CORB_SIZE);
      corb (Natural (corbWp)) := cmd;

      --  Update hardware write pointer
      writeReg16 (REG_CORBWP, corbWp);

      --  Poll RIRB for response
      for i in 1 .. 10_000 loop
         wp := readReg16 (REG_RIRBWP);
         if wp /= rirbRp then
            rirbRp := (rirbRp + 1) mod
                      Unsigned_16 (RIRB_SIZE);
            return rirb (Natural (rirbRp)).resp;
         end if;
         spinWait (1);
      end loop;

      debugPrint ("hda: verb timeout" & ASCII.LF);
      return 16#FFFF_FFFF#;
   end sendVerb;

   ---------------------------------------------------------------------------
   --  configureOutput - find and configure DAC and output pin
   ---------------------------------------------------------------------------
   procedure configureOutput is
      nodeCount   : Unsigned_32;
      startNID    : Unsigned_8;
      numNodes    : Unsigned_8;
      fgType      : Unsigned_32;
      afg_nid     : Unsigned_8 := 0;

      subNodeCount : Unsigned_32;
      subStart     : Unsigned_8;
      subNum       : Unsigned_8;

      audioCaps    : Unsigned_32;
      widgetType   : Unsigned_8;
      pinCaps      : Unsigned_32;
      connListLen  : Unsigned_32;
      connEntry    : Unsigned_32;
      ignore       : Unsigned_32;

      foundDAC     : Boolean := False;
      foundPin     : Boolean := False;
   begin
      --  Get root node's subordinate nodes
      nodeCount := sendVerb (
         0, VERB_GET_PARAM or PARAM_NODE_COUNT);
      startNID := Unsigned_8 (
         Shift_Right (nodeCount, 16) and 16#FF#);
      numNodes := Unsigned_8 (nodeCount and 16#FF#);

      --  Find Audio Function Group
      for i in 0 .. Natural (numNodes) - 1 loop
         fgType := sendVerb (
            startNID + Unsigned_8 (i),
            VERB_GET_PARAM or PARAM_FN_GROUP_TYPE);
         if (fgType and 16#FF#) = 1 then
            afg_nid := startNID + Unsigned_8 (i);
            exit;
         end if;
      end loop;

      if afg_nid = 0 then
         debugPrint (
            "hda: no Audio Function Group found"
            & ASCII.LF);
         return;
      end if;

      --  Power on the AFG
      ignore := sendVerb (afg_nid, VERB_SET_POWER_STATE or 0);

      --  Enumerate widgets under AFG
      subNodeCount := sendVerb (afg_nid, VERB_GET_PARAM or PARAM_NODE_COUNT);
      subStart := Unsigned_8 (Shift_Right (subNodeCount, 16) and 16#FF#);
      subNum := Unsigned_8 (subNodeCount and 16#FF#);

      --  First pass: find output pin widget
      for i in 0 .. Natural (subNum) - 1 loop
         audioCaps := sendVerb (subStart + Unsigned_8 (i),
                                VERB_GET_PARAM or PARAM_AUDIO_CAPS);
         widgetType := Unsigned_8 (Shift_Right (audioCaps, 20) and 16#0F#);

         if widgetType = 4 then  --  Pin Complex
            pinCaps := sendVerb (subStart + Unsigned_8 (i),
                                 VERB_GET_PARAM or PARAM_PIN_CAPS);
            --  Check for output capable (bit 4)
            if (pinCaps and 16#10#) /= 0 and not foundPin then
               pinNID := subStart + Unsigned_8 (i);
               foundPin := True;
            end if;
         elsif widgetType = 0 then  --  Audio Output (DAC)
            if not foundDAC then
               dacNID := subStart + Unsigned_8 (i);
               foundDAC := True;
            end if;
         end if;
      end loop;

      if not foundDAC or not foundPin then
         debugPrint ("hda: incomplete output path" & ASCII.LF);
         return;
      end if;

      --  Power on DAC and pin
      ignore := sendVerb (dacNID, VERB_SET_POWER_STATE or 0);
      ignore := sendVerb (pinNID, VERB_SET_POWER_STATE or 0);

      --  Set DAC stream format: 48kHz, 16-bit, stereo
      ignore := sendVerb (dacNID, VERB_SET_STREAM_FMT or
                           Unsigned_32 (FMT_48KHZ_16BIT_STEREO));

      --  Assign DAC to stream 1, channel 0
      --  Bits 7-4: stream ID (1), Bits 3-0: channel (0)
      ignore := sendVerb (dacNID, VERB_SET_CHAN_STREAM or 16#10#);

      --  Set DAC output amp: unmute, gain = max (0x7F)
      ignore := sendVerb (dacNID, VERB_SET_AMP_GAIN or
                           AMP_SET_OUTPUT or AMP_SET_LEFT or AMP_SET_RIGHT or
                           16#7F#);

      --  Enable pin output
      ignore := sendVerb (pinNID, VERB_SET_PIN_CTRL or
                           Unsigned_32 (PIN_OUT_ENABLE));

      --  Set pin amp: unmute, max gain
      ignore := sendVerb (pinNID, VERB_SET_AMP_GAIN or
                           AMP_SET_OUTPUT or AMP_SET_LEFT or AMP_SET_RIGHT or
                           16#7F#);

      --  Try to enable EAPD on pin (some codecs need this)
      ignore := sendVerb (pinNID, VERB_SET_EAPD or 16#02#);

      --  Check if pin has a connection list, set it to point at DAC
      connListLen := sendVerb (pinNID, VERB_GET_PARAM or PARAM_CONN_LIST_LEN);
      if (connListLen and 16#7F#) > 0 then
         --  Read first connection entry to find which index maps to our DAC
         connEntry := sendVerb (pinNID, VERB_GET_CONN_LIST or 0);
         --  If first entry matches our DAC, select index 0
         if Unsigned_8 (connEntry and 16#FF#) = dacNID then
            ignore := sendVerb (pinNID, VERB_SET_CONN_SEL or 0);
         elsif Unsigned_8 (Shift_Right (connEntry, 8) and 16#FF#) = dacNID then
            ignore := sendVerb (pinNID, VERB_SET_CONN_SEL or 1);
         else
            --  Default to index 0
            ignore := sendVerb (pinNID, VERB_SET_CONN_SEL or 0);
         end if;
      end if;

      debugPrint ("hda: output configured" & ASCII.LF);
   end configureOutput;

   ---------------------------------------------------------------------------
   --  startStream - configure BDL and start output stream DMA
   ---------------------------------------------------------------------------
   procedure startStream is
      sdBase   : Unsigned_64;
      bdlPhys  : Unsigned_64;
      pcmPhys  : Unsigned_64;
      ctl      : Unsigned_32;
      totalLen : Unsigned_32;
   begin
      sdBase := outputStreamBase;
      bdlPhys := dmaPhysBase + DMA_BDL_OFF;

      --  Stop stream first
      ctl := Unsigned_32 (readReg8 (sdBase + SD_CTL));
      writeReg8 (sdBase + SD_CTL, Unsigned_8 (ctl and (not SD_CTL_RUN)));
      spinWait (2);

      --  Reset stream
      writeReg8 (sdBase + SD_CTL, 1);  --  SRST
      spinWait (2);
      writeReg8 (sdBase + SD_CTL, 0);
      spinWait (2);

      --  Set up BDL entries pointing to PCM data buffers
      totalLen := 0;
      for i in 0 .. NUM_BDL_ENTRIES - 1 loop
         pcmPhys := dmaPhysBase + DMA_PCMBUF_OFF +
                     Unsigned_64 (i) * Unsigned_64 (PCM_PERIOD_BYTES);
         bdl (i) := (addr => pcmPhys,
                     len  => PCM_PERIOD_BYTES,
                     ioc  => 1);  --  IOC on every entry for refill
         totalLen := totalLen + PCM_PERIOD_BYTES;
      end loop;

      --  Set BDL base address
      writeReg32 (sdBase + SD_BDLPL,
                  Unsigned_32 (bdlPhys and 16#FFFF_FFFF#));
      writeReg32 (sdBase + SD_BDLPU,
                  Unsigned_32 (Shift_Right (bdlPhys, 32)));

      --  Set cyclic buffer length (total bytes across all BDL entries)
      writeReg32 (sdBase + SD_CBL, totalLen);

      --  Set last valid index
      writeReg16 (sdBase + SD_LVI, Unsigned_16 (NUM_BDL_ENTRIES - 1));

      --  Set stream format
      writeReg16 (sdBase + SD_FMT, FMT_48KHZ_16BIT_STEREO);

      --  Set stream ID = 1 in bits 23-20 of the 24-bit CTL register.
      --  CTL is at offset +0 (bits 0-7), +1 (bits 8-15), +2 (bits 16-23).
      writeReg8 (sdBase + SD_CTL + 2, 16#10#);  --  Stream ID = 1

      --  Start stream: set RUN + IOCE bits
      writeReg8 (sdBase + SD_CTL,
                 Unsigned_8 (SD_CTL_RUN or SD_CTL_IOCE));

      debugPrint ("hda: stream started" & ASCII.LF);
   end startStream;

   ---------------------------------------------------------------------------
   --  stopStream
   ---------------------------------------------------------------------------
   procedure stopStream is
      sdBase : Unsigned_64;
      ctl    : Unsigned_8;
   begin
      sdBase := outputStreamBase;
      ctl := readReg8 (sdBase + SD_CTL);
      writeReg8 (sdBase + SD_CTL, ctl and (not Unsigned_8 (SD_CTL_RUN)));
   end stopStream;

   ---------------------------------------------------------------------------
   --  fillBuffer - copy PCM data from staging area to a BDL buffer
   ---------------------------------------------------------------------------
   procedure fillBuffer (slotIdx  : Natural;
                         srcOff   : Unsigned_32;
                         numBytes : Unsigned_32)
   is
      dstAddr : constant Unsigned_64 :=
         DMA_VIRT_BASE + DMA_PCMBUF_OFF +
         Unsigned_64 (slotIdx) * Unsigned_64 (PCM_PERIOD_BYTES);

      srcAddr : constant Unsigned_64 :=
         stagingBase + Unsigned_64 (srcOff);

      type ByteArray is array (Natural range <>) of Unsigned_8
         with Convention => C;

      dst : ByteArray (0 .. Natural (numBytes) - 1)
         with Import, Address => To_Address (Integer_Address (dstAddr)),
              Volatile;

      src : ByteArray (0 .. Natural (numBytes) - 1)
         with Import, Address => To_Address (Integer_Address (srcAddr)),
              Volatile;
   begin
      for i in dst'Range loop
         dst (i) := src (i);
      end loop;
   end fillBuffer;

   ---------------------------------------------------------------------------
   --  getPosition
   ---------------------------------------------------------------------------
   function getPosition return Unsigned_32 is
   begin
      return readReg32 (outputStreamBase + SD_LPIB);
   end getPosition;

   ---------------------------------------------------------------------------
   --  writeSineTest - write a 440 Hz sine wave test pattern
   ---------------------------------------------------------------------------
   procedure writeSineTest (slotIdx : Natural) is
      dstAddr : constant Unsigned_64 :=
         DMA_VIRT_BASE + DMA_PCMBUF_OFF +
         Unsigned_64 (slotIdx) * Unsigned_64 (PCM_PERIOD_BYTES);

      type SampleArray is array (Natural range <>) of Integer_16
         with Convention => C;

      numSamples : constant Natural := Natural (PCM_PERIOD_BYTES) / 4;
      --  stereo 16-bit = 4 bytes per frame

      samples : SampleArray (0 .. numSamples * 2 - 1)
         with Import, Address => To_Address (Integer_Address (dstAddr));

      --  Simple approximation: 48000/440 ~= 109 samples per cycle.
      --  Use a piecewise linear triangle wave as a sine approximation.
      period   : constant := 109;
      quarter  : constant := 27;    --  period / 4 approx
      amplitude : constant := 16000;
      phase    : Natural := 0;
      val      : Integer_16;
   begin
      for i in 0 .. numSamples - 1 loop
         phase := i mod period;
         if phase < quarter then
            val := Integer_16 ((phase * amplitude) / quarter);
         elsif phase < quarter * 3 then
            val := Integer_16 (((quarter * 2 - phase) * amplitude) / quarter);
         else
            val := Integer_16 (((phase - period) * amplitude) / quarter);
         end if;

         --  Stereo: same sample on L and R
         samples (i * 2)     := val;
         samples (i * 2 + 1) := val;
      end loop;
   end writeSineTest;

end HDA;
