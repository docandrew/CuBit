------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  Minimal xHCI controller foundation.  This package deliberately exposes
--  no raw USB device access to clients; BAR and DMA authority remain inside
--  xhci.drv.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with USB_Optical;
with XHCI_Capabilities;

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

   procedure Initialize
     (barPhys  : Unsigned_64;
      barPages : Unsigned_64;
      dmaPhys  : Unsigned_64;
      expectedScratchpads : XHCI_Capabilities.Scratchpad_Buffer_Count;
      result   : out Init_Result);

   function Port_Count return Natural;
   function Connected_Port_Count return Natural;
   function Device_Slot return Natural;

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

   --  Arm a bounded queue of interrupt-IN requests before entering the
   --  service loop. Keeping requests resident across scheduling gaps avoids
   --  losing whole USB service intervals between mouse reports.
   procedure Start_Boot_Mouse_Transfers;

   --  Enable and acknowledge the controller's primary MSI interrupter.
   procedure Enable_Runtime_Interrupts
     (mode        : Runtime_Interrupt_Mode;
      vector      : Unsigned_8;
      tableOffset : Unsigned_64;
      enabled     : out Boolean);
   procedure Acknowledge_Runtime_Interrupt;

   --  Bounded, aggregate observability for the interrupt hot path.  The
   --  driver can publish this at a low rate without printing per report or
   --  perturbing input latency.  lastReport contains the first four bytes in
   --  little-endian order so boot-protocol and unexpected report-ID layouts
   --  are distinguishable during hardware bring-up.
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
