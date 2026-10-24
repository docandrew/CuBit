------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace NVMe block device driver.
--
--  Maps the NVMe controller BAR0 MMIO via SYSCALL_MAP_DEVICE, initializes
--  the controller and I/O queues, then offers the shared Block.Device.V1
--  protocol to authorized clients.
--
--  DMA region is pre-mapped by the kernel at 0x7000_0000_0000 (1 MiB).
--  BAR0 is mapped at 0x6000_0000_0000 (16KB) at startup.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Launch_Configurations;
with Launch_Configuration;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Block_Requests; use CuBit.Block_Requests;
with CuBit.Block_Device_Server;
with CuBit.Busy_Poll;
with CuBit.NVMe_Control;
with CuBit.Device_Interrupts;
with Startup;
with NVMe;

procedure main is
   use ASCII;

   ---------------------------------------------------------------------------
   --  sendReply - send a reply message
   ---------------------------------------------------------------------------
   procedure sendReply
     (dest   : Process_ID;
      label  : Unsigned_32;
      word0  : Unsigned_64)
   is
      replyMsg : Message;
      ignore   : Unsigned_64;
   begin
      replyMsg.tag := (label  => label,
                       length => 1,
                       protocol  => 0, ipc => <>,
                       reserved  => 0);
      replyMsg.words := [0 => word0, others => 0];
      ignore := reply (dest, replyMsg);
   end sendReply;

#if nvme_io_profile = "on" then
   --  Diagnostic: how completion waits end, every so many flushes.
   flushes : Unsigned_64 := 0;
   WAIT_REPORT_FLUSHES : constant := 256;
#end if;

   ---------------------------------------------------------------------------
   --  Block.Device.V1 through the shared front end (CuBit.Block_Requests).
   ---------------------------------------------------------------------------
   function deviceState return Device_State is
      maxBlocks : Unsigned_64;
   begin
      if NVMe.nsBlockCount = 0 or else
        NVMe.nsSectorSize not in Logical_Block_Size
      then
         return (Presence => Faulted);
      end if;
      maxBlocks := Unsigned_64'Min
        (NVMe.maxTransferBytes / Unsigned_64 (NVMe.nsSectorSize),
         Transfer_Limit'Last);
      if maxBlocks = 0 then
         return (Presence => Faulted);
      end if;
      return (Presence      => Available,
              Blocks        => NVMe.nsBlockCount,
              Logical_Size  => NVMe.nsSectorSize,
              Physical_Size => NVMe.nsSectorSize,
              Max_Transfer  => maxBlocks,
              Features      =>
                FEATURE_FLUSH or FEATURE_FUA or
                (if NVMe.volatileWriteCache then FEATURE_VOLATILE_CACHE
                 else 0),
              Media         => Fixed_Media);
   end deviceState;

   --  Count is at most Max_Transfer, which fits 32 bits.
   procedure transfer
     (state  : Device_State;
      item   : Transfer_Request;
      buffer : System.Address;
      moved  : out Unsigned_64)
   is
      pragma Unreferenced (state);
   begin
      if item.Direction = Device_Read then
         moved := NVMe.readBlocks
           (item.First_Block, Unsigned_32 (item.Count), buffer);
      else
         moved := NVMe.writeBlocks
           (item.First_Block, Unsigned_32 (item.Count), buffer,
            fua => item.Force_Unit_Access);
      end if;
   end transfer;

   --  Without a volatile write cache every completed write is already
   --  durable: the barrier needs no controller command.
   procedure flush (state : Device_State; durable : out Boolean) is
      pragma Unreferenced (state);
   begin
#if nvme_io_profile = "on" then
      flushes := flushes + 1;
      if flushes mod WAIT_REPORT_FLUSHES = 0 then
         NVMe.reportWaits;
      end if;
#end if;
      durable := not NVMe.volatileWriteCache or else NVMe.flush;
   end flush;

   package Block_Server is new CuBit.Block_Device_Server (transfer, flush);

   --  Main message loop variables
   sender   : Process_ID;
   msg      : Message;
   bar0Phys : Unsigned_64;
   dmaPhys  : Unsigned_64;
   --  devmgr's startup configuration (CuBit.NVMe_Control).
   configSender : Process_ID;
   configMessage : Message;
   --  devmgr enabled MSI-X and installed the completion vectors' capability.
   useInterrupts : Boolean := False;
   configValid : Boolean;
   queuesOk : Boolean;
begin
   debugPrint ("NVMe Driver: Starting..." & LF);

   --  0. The first message is devmgr's interrupt configuration, bound to
   --  the registered devmgr's kernel-supplied identity.
   receive (configSender, configMessage);
   declare
      use CuBit.NVMe_Control;
   begin
      configValid := configSender /= No_Process and then
        configSender = Registered_Driver (DRIVER_DEVMGR);
      if configValid and then
        configMessage.tag.label = Operation'Enum_Rep (Configure_Interrupts)
      then
         --  The vectors are the capability in the manifest's completion
         --  slot; devmgr programmed the table entries for them.
         configValid := configMessage.tag.length = 0 and then
           CuBit.Device_Interrupts.Block_Of (Startup.Completion).Valid;
         useInterrupts := configValid;
      elsif configValid then
         configValid :=
           configMessage.tag.label = Operation'Enum_Rep (Configure_Polled) and then
           configMessage.tag.length = 0;
      end if;
   end;

   --  1. BAR0 and the DMA region from devmgr's launch configuration
   declare
      Configuration : Launch_Configuration.Controller_Configuration;
      Configured : Boolean;
   begin
      CuBit.Launch_Configurations.Get
        (Launch_Configuration.NVMe_Controller, Configuration, Configured);
      bar0Phys := (if Configured then Configuration.Registers else 0);
      dmaPhys  := (if Configured then Configuration.DMA else 0);
   end;

   if bar0Phys = 0 then
      debugPrint ("NVMe Driver: No BAR0 configured, exiting." & LF);
      --  Signal devmgr that no hardware is present
      declare
         OP_NOT_PRESENT  : constant Unsigned_32 := 16#FF01#;
         rdyIgnore : MessageTag;
         ignore : Unsigned_64;
      begin
         rdyIgnore := capSend (Startup.Ready,
            (tag      => (label => OP_NOT_PRESENT, length => 0,
                          protocol => 0, ipc => <>, reserved => 0),
             authorityTag => 0,
             words    => [others => 0]), CuBit.Messages.Wait_Forever);
         ignore := syscall (SYSCALL_EXIT);
      end;
      return;
   end if;

   debugPrint ("NVMe Driver: Initializing controller..." & LF);

   --  2. Initialize controller (maps BAR0, resets, sets up admin queues)
   CuBit.Busy_Poll.Calibrate;   --  completion waits are timed by the TSC
   NVMe.initController (Startup.Registers, bar0Phys, dmaPhys);

   --  3. Identify controller and namespace
   NVMe.identifyController;
   NVMe.identifyNamespace;

   --  4. Create I/O queues (interrupting through MSI-X if configured),
   --  then answer devmgr, which unmasks the function on success.
   NVMe.createIOQueues (useInterrupts, queuesOk);
   if not queuesOk and then useInterrupts then
      useInterrupts := False;
      NVMe.createIOQueues (False, queuesOk);
   end if;
   if configSender /= No_Process then
      sendReply (configSender,
                 (if configValid and then NVMe.interruptsEnabled
                  then REPLY_OK else REPLY_ERROR), 0);
   end if;
   debugPrint ((if NVMe.interruptsEnabled
                then "NVMe Driver: completion interrupts (MSI-X)" & LF
                else "NVMe Driver: completions polled" & LF));

   --  5. Register as NVMe driver
   declare
      ignore : Unsigned_64;
   begin
      ignore := registerDriver (DRIVER_NVME, Startup.Registration);
   end;

   --  Signal devmgr that we are ready
   declare
      OP_READY       : constant Unsigned_32 := 16#FF00#;
      ignore : MessageTag;
   begin
      ignore := capSend (Startup.Ready,
         (tag      => (label => OP_READY, length => 0,
                       protocol => 0, ipc => <>, reserved => 0),
          authorityTag => 0,
          words    => [others => 0]), CuBit.Messages.Wait_Forever);
   end;

   debugPrint ("NVMe Driver: Ready, entering message loop." & LF);

   --  6. IPC receive loop
   loop
      receive (sender, msg);

      declare
         response : Message;
         ignore   : Unsigned_64;
      begin
         Block_Server.Handle (deviceState, sender, msg, response);
         ignore := reply (sender, response);
      end;
   end loop;
end main;
