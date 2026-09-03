------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Intel HDA audio driver (userspace)
--
--  @description
--  Handles HDA hardware bring-up and PCM DMA-period ownership.  It grants
--  only the PCM data page to the mixer, retains descriptors and controller
--  authority, and reports completed periods through one-way capability IPC.
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
   OP_AUDIO_HW_PERIOD : constant Unsigned_32 := 16#0516#;
   REPLY_OK           : constant Unsigned_32 := 16#F000#;
   REPLY_ERR          : constant Unsigned_32 := 16#F001#;

   --  Capability slots (assigned by devmgr)
   CAP_SLOT_MIXER : constant Unsigned_64 := 7;

   msg  : Message;
   from : ProcessID;
   ret  : Unsigned_64;
   buffersGranted : Boolean := False;
   buffersGrantId : Unsigned_64 := 0;
   streamRunning  : Boolean := False;
   periodSequence : Unsigned_64 := 0;

   procedure sendReply (label : Unsigned_32;
                        w0    : Unsigned_64 := 0;
                        w1    : Unsigned_64 := 0;
                        w2    : Unsigned_64 := 0;
                        w3    : Unsigned_64 := 0) is
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

      if from = 0 then
         --  Hardware IRQs arrive as one-way kernel events.  Acknowledge the
         --  device before notifying the mixer so the line cannot remain
         --  asserted while userspace is scheduled.
         irqEvent : declare
            completedSlot : Natural;
            position      : Unsigned_32;
            completed     : Boolean;
            submitted     : Boolean;
            periodMsg     : Message;
         begin
            HDA.acknowledgePeriod (completedSlot, position, completed);
            if completed and then streamRunning then
               periodSequence := periodSequence + 1;
               periodMsg :=
                 (tag => (label  => OP_AUDIO_HW_PERIOD,
                          length => 3,
                          flags  => 0,
                          badge  => 0),
                  capBadge => 0,
                  words =>
                    (0 => Unsigned_64 (completedSlot),
                     1 => periodSequence,
                     2 => Unsigned_64 (position),
                     3 => 0));
               submitted := capSubmit
                 (CAP_SLOT_MIXER, periodMsg, NO_COMPLETION_TOKEN);
               if not submitted then
                  debugPrint ("hda: mixer period notification dropped" & LF);
               end if;
            end if;
         end irqEvent;

      else
      case msg.tag.label is
         when OP_AUDIO_HW_INIT =>
            if not buffersGranted then
               createGrantViaCap
                 (slot      => CAP_SLOT_MIXER,
                  localAddr => To_Address
                    (Integer_Address
                       (HDA.DMA_VIRT_BASE + HDA.DMA_PCMBUF_OFF)),
                  numPages  => HDA.PCM_BUFFER_PAGES,
                  readWrite => True,
                  grantId   => buffersGrantId,
                  success   => buffersGranted);
            end if;

            if buffersGranted then
               HDA.clearOutputBuffers;
               sendReply
                 (REPLY_OK,
                  w0 => buffersGrantId,
                  w1 => Unsigned_64 (HDA.PCM_PERIOD_BYTES),
                  w2 => Unsigned_64 (HDA.NUM_BDL_ENTRIES),
                  w3 => 48_000);
            else
               sendReply (REPLY_ERR);
            end if;

         when OP_AUDIO_HW_START =>
            if buffersGranted then
               periodSequence := 0;
               HDA.startStream;
               streamRunning := True;
               sendReply (REPLY_OK);
            else
               sendReply (REPLY_ERR);
            end if;

         when OP_AUDIO_HW_STOP =>
            HDA.stopStream;
            streamRunning := False;
            sendReply (REPLY_OK);

         when OP_AUDIO_HW_FILL =>
            --  Payload copying is intentionally unsupported.  The mixer owns
            --  a restricted grant to the PCM period page and writes it
            --  directly.
            sendReply (REPLY_ERR);

         when OP_AUDIO_HW_DRAIN =>
            --  Wait for current buffers to finish, then stop
            HDA.stopStream;
            streamRunning := False;
            sendReply (REPLY_OK);

         when OP_AUDIO_HW_CAPS =>
            --  Return hardware capabilities in reply
            --  words(0) = sample rate (48000)
            --  words(1) = channels (2)
            --  words(2) = format (0 = S16LE)
            sendReply (REPLY_OK, w0 => 48_000, w1 => 2, w2 => 0);

         when others =>
            sendReply (REPLY_ERR);
      end case;
      end if;
   end loop;
end main;
