------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace PS/2 keyboard + mouse driver.
--
--  Handles both PS/2 keyboard (IRQ 33) and mouse (IRQ 44) in a single
--  process. Receives IRQ events from the kernel, reads PS/2 ports via
--  port I/O syscalls, and forwards decoded events to registered consumer
--  processes (shell, DOOM, etc.) via sendEvent.
--
--  Event format matches the former kernel services:
--    Keyboard: label=1, words(0) = scancode
--    Mouse:    label=2, words(0) = packed(buttons|dx|dy|dz|flags)
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Input; use CuBit.Input;

procedure main is
   use ASCII;

   --  PS/2 controller ports
   DATA_PORT    : constant Unsigned_16 := 16#60#;
   STATUS_PORT  : constant Unsigned_16 := 16#64#;
   COMMAND_PORT : constant Unsigned_16 := 16#64#;

   --  Mouse packet state
   packetBuf : array (0 .. 3) of Unsigned_8 := (others => 0);
   byteIdx   : Natural := 0;
   hasWheel  : Boolean := False;
   packetLen : Natural := 3;

   --  Consumer PIDs (looked up via sysinfo)
   kbdConsumer   : Unsigned_64 := 0;
   mouseConsumer : Unsigned_64 := 0;

   keyboardSequence : Source_Sequence := 0;
   pointerSequence  : Source_Sequence := 0;
   keyboardResyncPending : Boolean := False;
   pointerResyncPending  : Boolean := False;

   ---------------------------------------------------------------------------
   --  outb / inb wrappers
   ---------------------------------------------------------------------------
   procedure outb (port : Unsigned_16; val : Unsigned_8) is
      ignore : Unsigned_64;
   begin
      ignore := portOutp8 (port, val);
   end outb;

   function inb (port : Unsigned_16) return Unsigned_8 is
   begin
      return Unsigned_8 (portInp8 (port) and 16#FF#);
   end inb;

   ---------------------------------------------------------------------------
   --  waitForInput - poll until controller input buffer is empty
   ---------------------------------------------------------------------------
   procedure waitForInput is
      status : Unsigned_8;
   begin
      for i in 1 .. 10_000 loop
         status := inb (STATUS_PORT);
         if (status and 2) = 0 then
            return;
         end if;
      end loop;
   end waitForInput;

   ---------------------------------------------------------------------------
   --  waitForOutput - poll until controller output buffer has data
   ---------------------------------------------------------------------------
   procedure waitForOutput is
      status : Unsigned_8;
   begin
      for i in 1 .. 10_000 loop
         status := inb (STATUS_PORT);
         if (status and 1) /= 0 then
            return;
         end if;
      end loop;
   end waitForOutput;

   ---------------------------------------------------------------------------
   --  mouseWrite - send a byte to the mouse via the PS/2 controller
   ---------------------------------------------------------------------------
   procedure mouseWrite (val : Unsigned_8) is
      ack : Unsigned_8;
   begin
      --  Tell controller: next byte goes to auxiliary device (mouse)
      waitForInput;
      outb (COMMAND_PORT, 16#D4#);

      --  Send the byte
      waitForInput;
      outb (DATA_PORT, val);

      --  Read ACK (0xFA)
      waitForOutput;
      ack := inb (DATA_PORT);
   end mouseWrite;

   ---------------------------------------------------------------------------
   --  mouseRead - read a byte from the mouse
   ---------------------------------------------------------------------------
   function mouseRead return Unsigned_8 is
   begin
      waitForOutput;
      return inb (DATA_PORT);
   end mouseRead;

   ---------------------------------------------------------------------------
   --  setSampleRate - set mouse sample rate (used for wheel detection)
   ---------------------------------------------------------------------------
   procedure setSampleRate (rate : Unsigned_8) is
   begin
      mouseWrite (16#F3#);  --  Set Sample Rate command
      mouseWrite (rate);
   end setSampleRate;

   ---------------------------------------------------------------------------
   --  initMouse - enable auxiliary PS/2 port and configure mouse
   ---------------------------------------------------------------------------
   procedure initMouse is
      config   : Unsigned_8;
      deviceID : Unsigned_8;
   begin
      --  Enable auxiliary device (mouse port)
      waitForInput;
      outb (COMMAND_PORT, 16#A8#);

      --  Read controller configuration byte
      waitForInput;
      outb (COMMAND_PORT, 16#20#);
      waitForOutput;
      config := inb (DATA_PORT);

      --  Enable IRQ12 (bit 1) and clear auxiliary disable (bit 5)
      config := (config or 2) and (not 16#20#);

      --  Write updated configuration
      waitForInput;
      outb (COMMAND_PORT, 16#60#);
      waitForInput;
      outb (DATA_PORT, config);

      --  Reset mouse to defaults
      mouseWrite (16#F6#);

      --  Try to enable Intellimouse wheel extension:
      --  Magic sequence: sample rates 200, 100, 80, then read device ID
      setSampleRate (200);
      setSampleRate (100);
      setSampleRate (80);

      --  Read device ID to check for wheel support
      mouseWrite (16#F2#);
      deviceID := mouseRead;

      --  The protocol selected by the magic sequence determines the packet
      --  length.  Record it before enabling streaming: querying the ID again
      --  after F4 races with live motion bytes and can make a four-byte wheel
      --  mouse look like a three-byte mouse, corrupting all later framing.
      hasWheel := deviceID = 3 or else deviceID = 4;
      packetLen := (if hasWheel then 4 else 3);

      --  Enable data reporting
      mouseWrite (16#F4#);

      if deviceID = 3 or else deviceID = 4 then
         debugPrint ("ps2: Intellimouse detected (wheel support)" & LF);
      else
         debugPrint ("ps2: Standard PS/2 mouse detected" & LF);
      end if;
   end initMouse;

   ---------------------------------------------------------------------------
   --  flushPS2 - flush stale bytes from PS/2 output buffer
   ---------------------------------------------------------------------------
   procedure flushPS2 is
      status : Unsigned_8;
      ignore : Unsigned_8;
   begin
      loop
         status := inb (STATUS_PORT);
         exit when (status and 16#01#) = 0;
         ignore := inb (DATA_PORT);
      end loop;
   end flushPS2;

   ---------------------------------------------------------------------------
   --  refreshConsumers - re-read registered consumer PIDs from sysinfo
   ---------------------------------------------------------------------------
   procedure refreshConsumers is
   begin
      kbdConsumer := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_KEYBOARD);
      mouseConsumer := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_MOUSE);
   end refreshConsumers;

   ---------------------------------------------------------------------------
   --  handleKeyboard - read keyboard byte and forward to consumer
   ---------------------------------------------------------------------------
   procedure handleKeyboard (code : Unsigned_8) is
      accepted : Boolean;
      report   : Source_Report;
   begin
      if kbdConsumer /= 0 then
         keyboardSequence := Next_Sequence (keyboardSequence);
         report :=
           (sourceAuthorityTag => 0,
            sequence    => keyboardSequence,
            generation  => 1,
            device      => KEYBOARD,
            delivery    => ORDERED_TRANSITION,
            flags       =>
              (RESYNCHRONIZE => keyboardResyncPending),
            payload     => Unsigned_64 (code),
            snapshot    => 0);
         accepted := trySendEvent (kbdConsumer, Encode (report));
         keyboardResyncPending := not accepted;
      end if;
   end handleKeyboard;

   ---------------------------------------------------------------------------
   --  handleMouse - accumulate mouse bytes into packet, forward when complete
   ---------------------------------------------------------------------------
   procedure handleMouse (code : Unsigned_8) is
      buttons : Unsigned_64;
      dx      : Unsigned_64;
      dy      : Unsigned_64;
      dz      : Unsigned_64;
      flags   : Unsigned_64;
      packed  : Unsigned_64;
      accepted : Boolean;
      report   : Source_Report;
   begin
      --  Sync check: byte 0 has an always-one bit.  Overflow reports cannot
      --  express a trustworthy displacement, so discard them as well.  This
      --  also lets the decoder recover cleanly if an unexpected fourth wheel
      --  byte arrives after a protocol-negotiation failure: positive wheel
      --  bytes fail the sync bit and negative ones carry both overflow bits.
      if byteIdx = 0 then
         if (code and 8) = 0 or else (code and 16#C0#) /= 0 then
            return;
         end if;
      end if;

      packetBuf (byteIdx) := code;
      byteIdx := byteIdx + 1;

      if byteIdx >= packetLen then
         --  Complete packet received, decode and forward
         byteIdx := 0;

         if mouseConsumer /= 0 then
            --  Pack mouse event into words(0):
            --  Bits  0-7:   buttons (L=0, R=1, M=2)
            --  Bits  8-19:  dx (signed 12-bit)
            --  Bits 20-31:  dy (signed 12-bit)
            --  Bits 32-39:  dz (signed 8-bit scroll)
            --  Bits 40-47:  flags (bit 0 = hasWheel)
            buttons := Unsigned_64 (packetBuf (0) and 7);

            dx := Unsigned_64 (packetBuf (1));
            if (packetBuf (0) and 16#10#) /= 0 then
               dx := dx or 16#F00#;
            end if;

            dy := Unsigned_64 (packetBuf (2));
            if (packetBuf (0) and 16#20#) /= 0 then
               dy := dy or 16#F00#;
            end if;

            if hasWheel then
               --  IntelliMouse encodes the wheel as a signed four-bit value.
               --  Device ID 4 uses upper bits for buttons 4/5, so treating
               --  the whole byte as signed would turn an extra-button state
               --  into an enormous scroll delta.  PS/2 positive is toward
               --  the user (down); CuBit's UI convention is positive away
               --  from the user (up), so normalize direction at the driver
               --  boundary.
               dz := Unsigned_64 (packetBuf (3) and 16#0F#);
               if dz in 1 .. 7 then
                  dz := 256 - dz;
               elsif dz >= 8 then
                  dz := 16 - dz;
               end if;
            else
               dz := 0;
            end if;

            if hasWheel then
               flags := 1;
            else
               flags := 0;
            end if;

            packed := buttons
               or Shift_Left (dx and 16#FFF#, 8)
               or Shift_Left (dy and 16#FFF#, 20)
               or Shift_Left (dz and 16#FF#, 32)
               or Shift_Left (flags and 16#FF#, 40);

            pointerSequence := Next_Sequence (pointerSequence);
            report :=
              (sourceAuthorityTag => 0,
               sequence    => pointerSequence,
               generation  => 1,
               device      => RELATIVE_POINTER,
               delivery    => ACCUMULABLE_DISPLACEMENT,
               flags       => (RESYNCHRONIZE => pointerResyncPending),
               payload     => packed,
               snapshot    => buttons);
            accepted := trySendEvent (mouseConsumer, Encode (report));
            pointerResyncPending := not accepted;
         end if;
      end if;
   end handleMouse;

   --  Main loop variables
   event  : Message;
   status : Unsigned_8;
   code   : Unsigned_8;
   ignore : Unsigned_64;

   --  An IRQ notification means "controller work may be pending", not
   --  "consume exactly one byte". Work is drained in bounded decode batches;
   --  if a batch fills, the controller is checked again before blocking so
   --  coalesced IRQs cannot strand a partial mouse packet in port 0x60.
   MAX_INPUT_BYTES_PER_BATCH : constant Positive := 64;

begin
   debugPrint ("ps2: starting" & LF);

   --  Discard firmware/bootloader residue before enabling live reports.  A
   --  flush after F4 could consume only the first byte of a packet and leave
   --  the streaming decoder permanently out of phase.
   flushPS2;

   --  Initialize PS/2 mouse
   initMouse;

   --  Signal devmgr that we are ready
   declare
      CAP_SLOT_READY : constant Unsigned_64 := 15;
      OP_READY       : constant Unsigned_32 := 16#FF00#;
      rdyIgnore : MessageTag;
   begin
      rdyIgnore := capSend (CAP_SLOT_READY,
         (tag      => (label => OP_READY, length => 0,
                       flags => 0, reserved => 0),
          authorityTag => 0,
          words    => (others => 0)));
   end;

   --  Wait for at least one consumer to register before starting
   loop
      refreshConsumers;
      exit when kbdConsumer /= 0 or mouseConsumer /= 0;
      ignore := syscall (SYSCALL_SLEEP, 100);
   end loop;

   debugPrint ("ps2: consumer registered, entering event loop" & LF);

   --  Main event loop
   loop
      --  Block until IRQ 33 or 44 fires
      event := Wait_Event;

      refreshConsumers;

      loop
         for i in 1 .. MAX_INPUT_BYTES_PER_BATCH loop
            status := inb (STATUS_PORT);
            exit when (status and 16#01#) = 0;

            --  Read each byte exactly once, then route it according to the
            --  auxiliary-data bit in the same status sample.
            code := inb (DATA_PORT);
            if (status and 16#20#) = 0 then
               --  Bit 5 clear: keyboard data
               handleKeyboard (code);
            else
               --  Bit 5 set: mouse data
               handleMouse (code);
            end if;
         end loop;

         status := inb (STATUS_PORT);
         exit when (status and 16#01#) = 0;
      end loop;
   end loop;
end main;
