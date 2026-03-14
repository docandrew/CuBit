------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Intel HDA audio driver (userspace)
--
--  @description
--  Handles HDA hardware bring-up and PCM buffer management. Talks to the
--  mixer service via IPC: receives fill commands and sends buffer-consumed
--  notifications.  On startup, plays a sine wave test tone until the mixer
--  connects.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with HDA;

procedure main is
   use ASCII;

   --  IPC labels (must match kernel/src/ipc_labels.ads)
   OP_AUDIO_HW_INIT  : constant Unsigned_32 := 16#0510#;
   OP_AUDIO_HW_START : constant Unsigned_32 := 16#0511#;
   OP_AUDIO_HW_STOP  : constant Unsigned_32 := 16#0512#;
   OP_AUDIO_HW_FILL  : constant Unsigned_32 := 16#0513#;
   OP_AUDIO_HW_DRAIN : constant Unsigned_32 := 16#0514#;
   OP_AUDIO_HW_CAPS  : constant Unsigned_32 := 16#0515#;
   REPLY_OK           : constant Unsigned_32 := 16#F000#;
   REPLY_ERR          : constant Unsigned_32 := 16#F001#;

   --  Capability slots (assigned by devmgr)
   CAP_SLOT_MIXER : constant Unsigned_64 := 7;

   msg  : Message;
   from : ProcessID;
   ret  : Unsigned_64;

   --  Track which BDL slot to fill next
   nextSlot : Natural := 0;

   procedure sendReply (dest : ProcessID; label : Unsigned_32) is
      ignore : Unsigned_64;
   begin
      ignore := reply (dest,
         (tag => (label  => label,
                  length => 0,
                  flags  => 0,
                  badge  => 0),
          capBadge => 0,
          words => (others => 0)));
   end sendReply;

begin
   debugPrint ("hda: starting" & ASCII.LF);

   --  Initialize HDA controller
   HDA.initController;

   if not HDA.codecFound then
      debugPrint ("hda: no codec, exiting" & ASCII.LF);
      --  Signal devmgr that no hardware is present
      declare
         CAP_SLOT_READY  : constant Unsigned_64 := 15;
         OP_NOT_PRESENT  : constant Unsigned_32 := 16#FF01#;
         rdyIgnore : MessageTag;
      begin
         rdyIgnore := capSend (CAP_SLOT_READY,
            (tag      => (label => OP_NOT_PRESENT, length => 0,
                          flags => 0, badge => 0),
             capBadge => 0,
             words    => (others => 0)));
      end;
      ret := syscall (SYSCALL_EXIT);
      return;
   end if;

   --  Configure output path
   HDA.configureOutput;

   --  Start playback
   HDA.startStream;

   --  Register as HDA driver
   ret := registerDriver (DRIVER_HDA);

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

   debugPrint ("hda: registered, entering service loop" & ASCII.LF);

   --  Service loop: handle IPC from mixer
   loop
      receive (from, msg);

      case msg.tag.label is
         when OP_AUDIO_HW_INIT =>
            --  words(0) = staging grant ID
            HDA.stagingBase := HDA.GRANT_REGION_BASE +
               msg.words (0) * HDA.GRANT_SLOT_SIZE;
            sendReply (from, REPLY_OK);

         when OP_AUDIO_HW_START =>
            HDA.startStream;
            sendReply (from, REPLY_OK);

         when OP_AUDIO_HW_STOP =>
            HDA.stopStream;
            sendReply (from, REPLY_OK);

         when OP_AUDIO_HW_FILL =>
            --  words(0) = byte offset into grant buffer
            --  words(1) = number of bytes
            HDA.fillBuffer (nextSlot,
                            Unsigned_32 (msg.words (0)),
                            Unsigned_32 (msg.words (1)));
            nextSlot := (nextSlot + 1) mod HDA.NUM_BDL_ENTRIES;
            sendReply (from, REPLY_OK);

         when OP_AUDIO_HW_DRAIN =>
            --  Wait for current buffers to finish, then stop
            HDA.stopStream;
            sendReply (from, REPLY_OK);

         when OP_AUDIO_HW_CAPS =>
            --  Return hardware capabilities in reply
            --  words(0) = sample rate (48000)
            --  words(1) = channels (2)
            --  words(2) = format (0 = S16LE)
            ret := Unsigned_64 (reply (from,
               (tag => (label  => REPLY_OK,
                        length => 3,
                        flags  => 0,
                        badge  => 0),
                capBadge => 0,
                words => (0 => 48000,
                          1 => 2,
                          2 => 0,
                          3 => 0))));

         when others =>
            sendReply (from, REPLY_ERR);
      end case;
   end loop;
end main;
