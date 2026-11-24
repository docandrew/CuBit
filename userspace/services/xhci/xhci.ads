------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  xHCI controller driver. This package deliberately exposes no raw USB
--  device access to clients; BAR and DMA authority remain inside xhci.drv.
--
--  Initialize resets the controller and enumerates what is attached at
--  boot (bounded, before its bring-up report). After it, Service runs
--  everything from the event loop without blocking: the event ring, Port
--  Status Change handling (hotplug attach and detach), the enumeration of
--  hot-plugged root-port HID devices, halted-endpoint recovery and the
--  periodic health line.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with USB_Optical;
with SCSI;
with SCSI_Disks;
with XHCI_Capabilities;
with XHCI_DMA_Layout;
with XHCI_Controller_Identity;
with USB_Keyboards;
with CuBit.Block_Unit_Badges;
with CuBit.Disk_Presence;

package XHCI is

   type Runtime_Interrupt_Mode is
     (INTERRUPT_POLLING,
      INTERRUPT_MSI,
      INTERRUPT_MSIX);
   for Runtime_Interrupt_Mode use
     (INTERRUPT_POLLING => 0,
      INTERRUPT_MSI     => 1,
      INTERRUPT_MSIX    => 2);

   type Init_Result is
     (INIT_OK,
      INIT_MAP_FAILED,
      INIT_BAD_CAPABILITY,
      INIT_PAGE_SIZE_UNSUPPORTED,
      INIT_DMA_LAYOUT_MISMATCH,
      INIT_FIRMWARE_HANDOFF_FAILED,
      INIT_STOP_TIMEOUT,
      INIT_RESET_TIMEOUT,
      INIT_START_TIMEOUT,
      INIT_NO_DEVICE,
      INIT_PORT_RESET_TIMEOUT,
      INIT_COMMAND_TIMEOUT,
      INIT_COMMAND_FAILED,
      INIT_ADDRESS_FAILED,
      INIT_DESCRIPTOR_FAILED,
      INIT_NOT_BOOT_MOUSE,
      INIT_CONFIGURE_FAILED);

   --  Who this controller is (for log lines and the quirk table) and how
   --  devmgr wired its interrupter. Before Initialize.
   procedure Set_Identity
     (Location : String;
      Identity : XHCI_Controller_Identity.Identity;
      Mode     : Runtime_Interrupt_Mode);

   --  INIT_OK once the controller runs, even with nothing attached: the
   --  driver stays for hotplug. Other results are controller faults.
   procedure Initialize
     (barPhys  : Unsigned_64;
      barPages : Unsigned_64;
      dmaPhys  : Unsigned_64;
      expectedScratchpads : XHCI_Capabilities.Scratchpad_Buffer_Count;
      --  The device slots devmgr's DMA allocation covers (at most the
      --  controller's HCSPARAMS1 MaxSlots); the controller enables these.
      expectedSlots : XHCI_DMA_Layout.Slot_Count;
      result   : out Init_Result);

   function Port_Count return Natural;
   function Connected_Port_Count return Natural;
   function Device_Slot return Natural;
   function Has_Keyboard return Boolean;
   function Has_Mouse return Boolean;
   function Has_Storage return Boolean;

   --  Boot: every mass-storage unit, the boot medium first (Probe_Optical:
   --  the first unit whose medium holds ISO 9660 at sector 16 is the
   --  optical volume, read only), then the rest as disks (Probe_Disks).
   --  Probe before starting HID/runtime service delivery. Runtime reads below
   --  are nonblocking so the service can continue draining mouse reports.
   procedure Probe_Optical;
   procedure Probe_Disks;
   --  An optical medium was selected at boot (requests without a unit
   --  badge are its, Storage_Requests).
   function Optical_Selected return Boolean;
   --  The optical read's data was copied out: the bulk buffers are free.
   procedure Release_Optical_Read;
   function Optical_Block_Count return Unsigned_64;
   procedure Start_Optical_Read
     (First : Unsigned_32; Count : USB_Optical.Read_Block_Count;
      Accepted : out Boolean);
   procedure Poll_Optical_Read
     (Done : out Boolean; Result : out USB_Optical.Status_Result;
      Progressed : out Boolean);
   function Optical_Read_Buffer return System.Address;
   function Optical_Deadline return Unsigned_64;

   ---------------------------------------------------------------------------
   --  USB disks (usb_disk_service, FS-034): every unit USB_Disk_Probe finds
   --  Ready (never an ISO 9660 medium), at boot or plugged in later. Each
   --  is named by a unit (CuBit.Block_Unit_Badges) chosen here and never
   --  reused while the driver runs; devmgr learns of each through
   --  Next_Report (CuBit.Disk_Presence) and badges the filesystem's copy of
   --  the endpoint with it. A disk that goes away (detach, medium removed
   --  or changed, recovery failed) is gone for good: a stick plugged in
   --  again is a new unit.
   ---------------------------------------------------------------------------
   type Disk_Presence is (No_Disk, Disk_Ready, Disk_Gone);
   type Disk_Status is record
      Presence        : Disk_Presence := No_Disk;
      Geometry        : SCSI_Disks.Geometry;
      Write_Protected : Boolean := False;
      Removable       : Boolean := False;
      Identity        : SCSI.Inquiry_Data;
   end record;
   --  The disk Unit names (No_Disk for a unit never given, Disk_Gone for
   --  one that went away).
   function Disk (Unit : CuBit.Block_Unit_Badges.Unit) return Disk_Status;
   --  Some disk is ready.
   function Has_Disk return Boolean;
   --  The next report devmgr is owed (Due False: none), in order per
   --  device slot; Report_Sent once the kernel took it into devmgr's
   --  queue (until then the same report comes again).
   procedure Next_Report (Due : out Boolean; Report : out CuBit.Disk_Presence.Report);
   procedure Report_Sent;
   --  A probe or reset recovery runs: no request should be taken now.
   function Storage_Busy return Boolean;

   --  The commands' data: a write's before Start_Disk_Command, a read's
   --  after a Disk_Passed result.
   Disk_Buffer_Bytes : constant := 32_768;
   function Disk_Buffer return System.Address;

   type Disk_Outcome is
     (Disk_Passed,        --  the command passed and moved all its data
      Disk_Failed,        --  it failed (Sense says what the unit said)
      Disk_Unavailable);  --  no disk to run it on
   type Disk_Result is record
      Outcome : Disk_Outcome := Disk_Unavailable;
      Moved   : Unsigned_32 := 0;
      Sense   : SCSI_Disks.Sense_Meaning := SCSI_Disks.No_Error;
   end record;

   --  Runs Block on Unit's disk once the bulk buffers are free (one command
   --  at a time); a CHECK CONDITION is followed by REQUEST SENSE, a
   --  transport fault by a reset recovery, before it is done.
   procedure Start_Disk_Command (Unit : CuBit.Block_Unit_Badges.Unit; Block : SCSI.Command_Block)
     with Pre => SCSI.Well_Formed (Block) and then Block.Expected <= Disk_Buffer_Bytes;
   function Disk_Command_Done return Boolean;
   --  After Disk_Command_Done: the result (the command slot is free again).
   function Disk_Command_Result return Disk_Result;
   --  A command that outlived its caller's deadline: its disk is treated
   --  as gone (xHCI bounds every stage, so this only follows a hang).
   procedure Cancel_Disk_Command;

   --  Arm the interrupt-IN queues of the HID devices found at boot.
   procedure Start_Boot_Mouse_Transfers;
   procedure Start_Boot_Keyboard_Transfers;

   --  Enable the primary interrupter (devmgr programmed MSI or MSI-X entry
   --  0, docs/msi-interrupts.md), then arm hotplug.
   procedure Enable_Runtime_Interrupts
     (mode    : Runtime_Interrupt_Mode;
      enabled : out Boolean);
   --  The interrupt doorbell arrived: clear USBSTS.EINT and IMAN.IP (both
   --  RW1C); the next Service writes ERDP with EHB even if the ring is
   --  empty (xHCI 1.2, 4.17.2).
   procedure Acknowledge_Runtime_Interrupt;

   --  Everything that is not a HID report: drain the event ring into the
   --  completion mailboxes, ports, enumeration, recovery, health.
   procedure Service (Progressed : out Boolean);
   --  The next monotonic millisecond Service has timed work; Last if none.
   function Next_Deadline return Unsigned_64;
   --  False once events kept arriving with no interrupt (logged): the
   --  caller then polls every millisecond instead of waiting for one.
   function Interrupts_Trusted return Boolean;

   procedure Poll_Boot_Keyboard
     (Data : out USB_Keyboards.Report; Ready, Progressed : out Boolean);

   --  Bounded, aggregate observability for the interrupt hot path.
   --  lastReport holds the first four bytes in little-endian order.
   type Boot_Mouse_Diagnostics is record
      transferEvents    : Unsigned_64 := 0;
      decodedReports    : Unsigned_64 := 0;
      motionReports     : Unsigned_64 := 0;
      buttonTransitions : Unsigned_64 := 0;
      completionErrors  : Unsigned_64 := 0;
      shortReports      : Unsigned_64 := 0;
      unexpectedEvents  : Unsigned_64 := 0;
      lastReport        : Unsigned_32 := 0;
      lastLength        : Unsigned_8 := 0;
      lastCompletion    : Unsigned_8 := 0;
   end record;

   function Mouse_Diagnostics return Boot_Mouse_Diagnostics;

   procedure Poll_Boot_Mouse
     (buttons : out Unsigned_8;
      deltaX  : out Integer;
      deltaY  : out Integer;
      deltaZ  : out Integer;
      ready   : out Boolean;
      eventAvailable : out Boolean);

end XHCI;
