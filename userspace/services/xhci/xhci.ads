------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  xHCI controller driver. This package deliberately exposes no raw USB
--  device access to clients; BAR and DMA authority remain inside xhci.drv.
--
--  Initialize resets the controller and enumerates what is attached at
--  boot (bounded, before devmgr's configure reply). After it, Service runs
--  everything from the event loop without blocking: the event ring, Port
--  Status Change handling (hotplug attach and detach), the enumeration of
--  hot-plugged root-port HID devices, halted-endpoint recovery and the
--  periodic health line.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with USB_Optical;
with XHCI_Capabilities;
with XHCI_Controller_Identity;
with USB_Keyboards;

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
      result   : out Init_Result);

   function Port_Count return Natural;
   function Connected_Port_Count return Natural;
   function Device_Slot return Natural;
   function Has_Keyboard return Boolean;
   function Has_Mouse return Boolean;
   function Has_Storage return Boolean;

   --  Probe before starting HID/runtime service delivery. Runtime reads below
   --  are nonblocking so the service can continue draining mouse reports.
   procedure Probe_Optical;
   function Optical_Block_Count return Unsigned_64;
   procedure Start_Optical_Read
     (First : Unsigned_32; Count : USB_Optical.Read_Block_Count;
      Accepted : out Boolean);
   procedure Poll_Optical_Read
     (Done : out Boolean; Result : out USB_Optical.Status_Result;
      Progressed : out Boolean);
   function Optical_Read_Buffer return System.Address;
   function Optical_Deadline return Unsigned_64;

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
