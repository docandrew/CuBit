------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  Minimal xHCI controller foundation.  This package deliberately exposes
--  no raw USB device access to clients; BAR and DMA authority remain inside
--  xhci.drv.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

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
      INIT_SCRATCHPAD_LIMIT,
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
      result   : out Init_Result);

   function Port_Count return Natural;
   function Connected_Port_Count return Natural;
   function Device_Slot return Natural;

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

   procedure Poll_Boot_Mouse
     (buttons : out Unsigned_8;
      deltaX  : out Integer;
      deltaY  : out Integer;
      ready   : out Boolean;
      eventAvailable : out Boolean);

end XHCI;
