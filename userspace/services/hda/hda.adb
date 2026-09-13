------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Intel High Definition Audio controller driver implementation
------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with HDA_Amplifiers;

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
   transportFailed : Boolean := False;
   controllerMapped : Boolean := False;
   completedPeriods : Unsigned_32 := 0;
   firstCompletedPosition : Unsigned_32 := 0;
   playbackReported : Boolean := False;
   streamErrors : Unsigned_8 := 0;

   procedure diagnostic (name : String; value : Unsigned_32) is
      hex : constant String := "0123456789ABCDEF";
      encoded : String (1 .. 8);
      remaining : Unsigned_32 := value;
   begin
      for i in reverse encoded'Range loop
         encoded (i) := hex (Natural (remaining and 15) + 1);
         remaining := Shift_Right (remaining, 4);
      end loop;
      debugPrint ("hda: " & name & "=" & encoded & ASCII.LF);
   end diagnostic;

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
      codecFound := False;
      outputConfigured := False;
      transportFailed := False;
      --  Map BAR0 into our address space
      bar0Phys := getInfo (SYSINFO_HDA_BAR0);
      dmaPhysBase := getInfo (SYSINFO_HDA_DMA_PHYS);
      if bar0Phys = 0 or else dmaPhysBase = 0 then
         debugPrint ("hda: missing controller/DMA allocation" & ASCII.LF);
         return;
      end if;
      ret := syscall (SYSCALL_MAP_DEVICE, bar0Phys,
                       BAR_VIRT_BASE, BAR_MAP_PAGES);
      if ret = Unsigned_64'Last then
         debugPrint ("hda: controller mapping failed" & ASCII.LF);
         return;
      end if;
      controllerMapped := True;

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

      if (gctl and GCTL_CRST) /= 0 then
         debugPrint ("hda: controller reset assertion failed" & ASCII.LF);
         return;
      end if;

      --  Bring controller out of reset
      writeReg32 (REG_GCTL, GCTL_CRST);

      --  Wait for controller ready
      for i in 1 .. 100 loop
         gctl := readReg32 (REG_GCTL);
         exit when (gctl and GCTL_CRST) /= 0;
         spinWait (1);
      end loop;

      if (gctl and GCTL_CRST) = 0 then
         debugPrint ("hda: controller reset release failed" & ASCII.LF);
         return;
      end if;

      --  Wait for codec enumeration
      spinWait (10);

      --  Check for codec(s) on STATESTS
      statests := readReg16 (REG_STATESTS);
      diagnostic ("STATESTS", Unsigned_32 (statests));
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
      if numOSS = 0 then
         codecFound := False;
         debugPrint ("hda: no dedicated output stream" & ASCII.LF);
         return;
      end if;

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

      --  Enable only output stream 0 interrupts. Codec commands are polled
      --  during initialization; sendVerb owns RIRB response acknowledgement.
      --  Do not deliver those responses as playback-period notifications.
      --  Bits 29:0 are per-stream enables; output stream 0 = bit numISS
      writeReg32 (REG_INTCTL,
         INTCTL_GIE or
         Shift_Left (1, Natural (numISS)));

      --  Read codec vendor ID
      codecVendor := sendVerb (
         0, VERB_GET_PARAM or PARAM_VENDOR_ID);

      diagnostic ("codec 0 vendor", codecVendor);
      codecFound := not transportFailed and then codecVendor /= 0;

      debugPrint ("hda: controller init complete" & ASCII.LF);
   end initController;

   procedure abortInitialization is
   begin
      if controllerMapped then
         writeReg32 (REG_INTCTL, 0);
         writeReg8 (REG_CORBCTL, 0);
         writeReg8 (REG_RIRBCTL, 0);
         writeReg32 (REG_GCTL, 0);
         for attempt in 1 .. 100 loop
            exit when (readReg32 (REG_GCTL) and GCTL_CRST) = 0;
            spinWait (1);
         end loop;
         diagnostic ("aborted controller GCTL", readReg32 (REG_GCTL));
      end if;
      transportFailed := True;
      outputConfigured := False;
      codecFound := False;
   end abortInitialization;

   ---------------------------------------------------------------------------
   --  sendVerb - send a verb to codec 0 and poll for response
   ---------------------------------------------------------------------------
   function sendVerb (nid  : Unsigned_8;
                      verb : Unsigned_32) return Unsigned_32
   is
      cmd : Unsigned_32;
      wp  : Unsigned_16;
      response : Unsigned_32;
   begin
      --  A timed-out response could arrive late. Do not issue another verb
      --  and mistake that old response for the new command's result.
      if transportFailed then
         return Unsigned_32'Last;
      end if;
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
      for i in 1 .. 100 loop
         wp := readReg16 (REG_RIRBWP) and 16#00FF#;
         if wp /= rirbRp then
            rirbRp := (rirbRp + 1) mod
                      Unsigned_16 (RIRB_SIZE);
            --  Only a solicited response from codec zero belongs to this
            --  transport. Jack events and responses from other codecs do not.
            if (rirb (Natural (rirbRp)).respEx and 16#1F#) = 0 then
               response := rirb (Natural (rirbRp)).resp;
               --  W1C response status also resets the interrupt-response
               --  count. Leaving it asserted can stall the CORB at RINTCNT.
               writeReg8 (REG_RIRBSTS, 1);
               return response;
            end if;
         end if;
         spinWait (1);
      end loop;

      transportFailed := True;
      diagnostic ("verb timeout command", cmd);
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
      dacCaps      : Unsigned_32 := 0;
      selectedPinCaps : Unsigned_32 := 0;
      pinWidgetCaps : Unsigned_32 := 0;
      pinControl, converter, format, eapd : Unsigned_32;
      eapdOK : Boolean := True;

      function powerOn (nid : Unsigned_8) return Boolean is
         status : Unsigned_32;
      begin
         ignore := sendVerb (nid, VERB_SET_POWER_STATE);
         --  D0 transitions are asynchronous on real codecs.
         for attempt in 1 .. 100 loop
            status := sendVerb (nid, VERB_GET_POWER_STATE);
            exit when transportFailed or else (status and 16#1FF#) = 0;
            spinWait (1);
         end loop;
         diagnostic ("power node", Unsigned_32 (nid));
         diagnostic ("power requested/actual", status);
         return not transportFailed and then (status and 16#1FF#) = 0;
      end powerOn;

      function configureAmplifier
        (nid : Unsigned_8; widgetCaps : Unsigned_32) return Boolean
      is
         parameterNode : Unsigned_8 := afg_nid;
         rawCaps, gain, left, right : Unsigned_32;
      begin
         if (widgetCaps and WIDGET_OUTPUT_AMP) = 0 then
            return True;
         end if;
         --  Without AMP_OVERRIDE the widget inherits the AFG parameters.
         if (widgetCaps and WIDGET_AMP_OVERRIDE) /= 0 then
            parameterNode := nid;
         end if;
         rawCaps := sendVerb
           (parameterNode, VERB_GET_PARAM or PARAM_OUT_AMP_CAPS);
         if transportFailed then
            return False;
         end if;
         gain := Unsigned_32 (HDA_Amplifiers.Initial_Gain
           (HDA_Amplifiers.Decode (rawCaps)));
         ignore := sendVerb (nid, VERB_SET_AMP_GAIN or AMP_SET_OUTPUT or
           AMP_SET_LEFT or AMP_SET_RIGHT or gain);
         left := sendVerb
           (nid, VERB_GET_AMP_GAIN or AMP_GET_OUTPUT or AMP_GET_LEFT);
         right := sendVerb
           (nid, VERB_GET_AMP_GAIN or AMP_GET_OUTPUT or AMP_GET_RIGHT);
         diagnostic ("amp node", Unsigned_32 (nid));
         diagnostic ("amp capabilities", rawCaps);
         diagnostic ("amp requested gain", gain);
         diagnostic ("amp left gain/mute", left);
         diagnostic ("amp right gain/mute", right);
         return not transportFailed and then
           (left and 16#FF#) = gain and then (right and 16#FF#) = gain;
      end configureAmplifier;
   begin
      outputConfigured := False;
      --  Get root node's subordinate nodes
      nodeCount := sendVerb (
         0, VERB_GET_PARAM or PARAM_NODE_COUNT);
      startNID := Unsigned_8 (
         Shift_Right (nodeCount, 16) and 16#FF#);
      numNodes := Unsigned_8 (nodeCount and 16#FF#);
      if transportFailed or else
        Natural (startNID) + Natural (numNodes) > 256
      then
         debugPrint ("hda: invalid root node range" & ASCII.LF);
         return;
      end if;

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
      diagnostic ("codec subsystem", sendVerb (afg_nid, VERB_GET_SUBSYSTEM));
      if not powerOn (afg_nid) then
         debugPrint ("hda: function group did not reach D0" & ASCII.LF);
         return;
      end if;

      --  Enumerate widgets under AFG
      subNodeCount := sendVerb (afg_nid, VERB_GET_PARAM or PARAM_NODE_COUNT);
      subStart := Unsigned_8 (Shift_Right (subNodeCount, 16) and 16#FF#);
      subNum := Unsigned_8 (subNodeCount and 16#FF#);
      if transportFailed or else
        Natural (subStart) + Natural (subNum) > 256
      then
         debugPrint ("hda: invalid widget node range" & ASCII.LF);
         return;
      end if;

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
               selectedPinCaps := pinCaps;
               pinWidgetCaps := audioCaps;
               foundPin := True;
            end if;
         elsif widgetType = 0 then  --  Audio Output (DAC)
            if not foundDAC then
               dacNID := subStart + Unsigned_8 (i);
               dacCaps := audioCaps;
               foundDAC := True;
            end if;
         end if;
      end loop;

      if transportFailed or else not foundDAC or else not foundPin then
         debugPrint ("hda: incomplete output path" & ASCII.LF);
         return;
      end if;

      --  Power on DAC and pin
      diagnostic ("selected DAC", Unsigned_32 (dacNID));
      diagnostic ("selected pin", Unsigned_32 (pinNID));
      diagnostic ("pin default", sendVerb (pinNID, VERB_GET_PIN_DEFAULT));
      if (dacCaps and WIDGET_POWER) /= 0 and then not powerOn (dacNID) then
         debugPrint ("hda: DAC did not reach D0" & ASCII.LF);
         return;
      end if;
      if (pinWidgetCaps and WIDGET_POWER) /= 0 and then not powerOn (pinNID) then
         debugPrint ("hda: pin did not reach D0" & ASCII.LF);
         return;
      end if;

      --  Set DAC stream format: 48kHz, 16-bit, stereo
      ignore := sendVerb (dacNID, VERB_SET_STREAM_FMT or
                           Unsigned_32 (FMT_48KHZ_16BIT_STEREO));

      --  Assign DAC to stream 1, channel 0
      --  Bits 7-4: stream ID (1), Bits 3-0: channel (0)
      ignore := sendVerb (dacNID, VERB_SET_CHAN_STREAM or 16#10#);

      if not configureAmplifier (dacNID, dacCaps) then
         debugPrint ("hda: DAC amplifier read-back failed" & ASCII.LF);
         return;
      end if;

      --  Enable pin output
      ignore := sendVerb (pinNID, VERB_SET_PIN_CTRL or
                           Unsigned_32 (PIN_OUT_ENABLE));

      if not configureAmplifier (pinNID, pinWidgetCaps) then
         debugPrint ("hda: pin amplifier read-back failed" & ASCII.LF);
         return;
      end if;

      --  Preserve balanced-output/channel-swap bits; only enable a supported
      --  external amplifier. Do not send EAPD verbs to unsupported widgets.
      if (selectedPinCaps and PIN_CAP_EAPD) /= 0 then
         eapd := sendVerb (pinNID, VERB_GET_EAPD);
         ignore := sendVerb
           (pinNID, VERB_SET_EAPD or (eapd and 7) or EAPD_ENABLE);
         eapd := sendVerb (pinNID, VERB_GET_EAPD);
         diagnostic ("pin EAPD", eapd);
         eapdOK := (eapd and EAPD_ENABLE) /= 0;
      end if;

      --  Check if pin has a connection list, set it to point at DAC
      connListLen := sendVerb (pinNID, VERB_GET_PARAM or PARAM_CONN_LIST_LEN);
      diagnostic ("pin connection list length", connListLen);
      if (connListLen and 16#7F#) > 0 then
         --  Read first connection entry to find which index maps to our DAC
         connEntry := sendVerb (pinNID, VERB_GET_CONN_LIST or 0);
         diagnostic ("pin first connections", connEntry);
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

      pinControl := sendVerb (pinNID, VERB_GET_PIN_CTRL);
      converter := sendVerb (dacNID, VERB_GET_CHAN_STREAM);
      format := sendVerb (dacNID, VERB_GET_STREAM_FMT);
      diagnostic ("pin control", pinControl);
      diagnostic ("DAC stream/channel", converter);
      diagnostic ("DAC format", format);
      outputConfigured := not transportFailed and then eapdOK and then
        (pinControl and Unsigned_32 (PIN_OUT_ENABLE)) /= 0 and then
        (converter and 16#FF#) = 16#10# and then
        (format and 16#FFFF#) = Unsigned_32 (FMT_48KHZ_16BIT_STEREO);
      if outputConfigured then
         debugPrint ("hda: output configured and verified" & ASCII.LF);
      else
         debugPrint ("hda: output read-back failed" & ASCII.LF);
      end if;
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
      completedPeriods := 0;
      firstCompletedPosition := 0;
      streamErrors := 0;
      --  Clear stale completion/error status before enabling DMA.
      writeReg8 (sdBase + SD_STS, 16#1C#);
      writeReg8 (sdBase + SD_CTL,
                 Unsigned_8 (SD_CTL_RUN or SD_CTL_IOCE));

      if not playbackReported then
         diagnostic ("stream control", readReg32 (sdBase + SD_CTL) and 16#FF_FFFF#);
         diagnostic ("stream format", Unsigned_32 (readReg16 (sdBase + SD_FMT)));
         diagnostic ("stream cyclic bytes", readReg32 (sdBase + SD_CBL));
      end if;
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
      --  Report the first playback session at STOP, never in the refill/IRQ
      --  hot path. Counters are owned by this single-threaded driver service.
      if not playbackReported and then (ctl and Unsigned_8 (SD_CTL_RUN)) /= 0 then
         diagnostic ("DMA completed periods", completedPeriods);
         diagnostic ("DMA first completion position", firstCompletedPosition);
         diagnostic ("DMA stop position", readReg32 (sdBase + SD_LPIB));
         diagnostic ("DMA error bits", Unsigned_32 (streamErrors or
           (readReg8 (sdBase + SD_STS) and 16#18#)));
         playbackReported := True;
      end if;
   end stopStream;

   ---------------------------------------------------------------------------
   --  clearOutputBuffers
   ---------------------------------------------------------------------------
   procedure clearOutputBuffers
   is
      type ByteArray is array (Natural range <>) of Unsigned_8
         with Convention => C;

      pcm : ByteArray
        (0 .. NUM_BDL_ENTRIES * Natural (PCM_PERIOD_BYTES) - 1)
         with Import,
              Address => To_Address
                (Integer_Address (DMA_VIRT_BASE + DMA_PCMBUF_OFF)),
              Volatile;
   begin
      for i in pcm'Range loop
         pcm (i) := 0;
      end loop;
   end clearOutputBuffers;

   ---------------------------------------------------------------------------
   --  acknowledgePeriod
   ---------------------------------------------------------------------------
   procedure acknowledgePeriod
     (slot      : out Natural;
      position  : out Unsigned_32;
      completed : out Boolean)
   is
      status   : Unsigned_8;
      nextSlot : Natural;
   begin
      slot := 0;
      position := readReg32 (outputStreamBase + SD_LPIB);
      status := readReg8 (outputStreamBase + SD_STS);
      streamErrors := streamErrors or (status and 16#18#);
      completed := (status and SD_STS_BCIS) /= 0;

      --  Acknowledge error-only IRQs too, otherwise a level line can stay
      --  asserted forever. Preserve the error bits in our diagnostic state.
      if (status and 16#1C#) /= 0 then
         writeReg8 (outputStreamBase + SD_STS, status and 16#1C#);
      end if;

      if not completed then
         return;
      end if;

      if completedPeriods = 0 then
         firstCompletedPosition := position;
      end if;
      completedPeriods := completedPeriods + 1;

      --  LPIB identifies the next byte the device will consume.  Therefore
      --  the preceding BDL entry is exclusively available to the mixer.
      nextSlot := Natural (position / PCM_PERIOD_BYTES) mod NUM_BDL_ENTRIES;
      slot := (nextSlot + NUM_BDL_ENTRIES - 1) mod NUM_BDL_ENTRIES;
   end acknowledgePeriod;

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
