------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  xHCI controller reset, rings, boot enumeration, hotplug and recovery.
--
--  Reference: eXtensible Host Controller Interface for USB, revision 1.2
--  (sections cited where used), and USB 2.0 chapters 7 and 9.
------------------------------------------------------------------------------
with System; use System;
with System.Address_To_Access_Conversions;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Monotonic;
with CuBit.Handles.Devices;
with Boot_Log;
with Startup;
with USB_Configurations;
with USB_Device_Roles;
with XHCI_Completions;
with XHCI_DMA_Layout; use XHCI_DMA_Layout;
with XHCI_Ports;
with XHCI_Registers; use XHCI_Registers;
with XHCI_Rings;
with XHCI_Port_Machine;
with XHCI_Endpoint_Recovery;
with XHCI_Topology;
with USB_Hubs;
with XHCI_Legacy;

package body XHCI is

   package R renames XHCI_Rings;
   package PM renames XHCI_Port_Machine;
   package ER renames XHCI_Endpoint_Recovery;
   package CI renames XHCI_Controller_Identity;
   use type PM.Phase;
   use type PM.Action;
   use type ER.Step;
   use type ER.Disposition;
   use type USB_Device_Roles.Role;
   use type XHCI_Topology.Speed;

   DMA_VIRT_BASE : constant Unsigned_64 := 16#0000_7000_0000_0000#;
   BAR_VIRT_BASE : constant Unsigned_64 := 16#0000_6000_0000_0000#;

   ---------------------------------------------------------------------------
   --  Register offsets (5.3, 5.4, 5.5)
   ---------------------------------------------------------------------------
   REG_CAPLENGTH  : constant Storage_Offset := 16#00#;
   REG_HCSPARAMS1 : constant Storage_Offset := 16#04#;
   REG_HCSPARAMS2 : constant Storage_Offset := 16#08#;
   REG_HCCPARAMS1 : constant Storage_Offset := 16#10#;
   REG_DBOFF      : constant Storage_Offset := 16#14#;
   REG_RTSOFF     : constant Storage_Offset := 16#18#;
   DBOFF_MASK     : constant Unsigned_32 := 16#FFFF_FFFC#;
   RTSOFF_MASK    : constant Unsigned_32 := 16#FFFF_FFE0#;
   MINIMUM_CAPLENGTH : constant := 16#20#;
   PAGESIZE_4K    : constant Unsigned_32 := 1;

   OP_USBCMD   : constant Storage_Offset := 16#00#;
   OP_USBSTS   : constant Storage_Offset := 16#04#;
   OP_PAGESIZE : constant Storage_Offset := 16#08#;
   OP_CRCR     : constant Storage_Offset := 16#18#;
   OP_DCBAAP   : constant Storage_Offset := 16#30#;
   OP_CONFIG   : constant Storage_Offset := 16#38#;
   OP_PORTSC_BASE : constant Storage_Offset := 16#400#;
   OP_PORT_STRIDE : constant Storage_Offset := 16#10#;

   RT_INTR0    : constant Storage_Offset := 16#20#;
   INTR_IMAN   : constant Storage_Offset := 16#00#;
   INTR_IMOD   : constant Storage_Offset := 16#04#;
   INTR_ERSTSZ : constant Storage_Offset := 16#08#;
   INTR_ERSTBA : constant Storage_Offset := 16#10#;
   INTR_ERDP   : constant Storage_Offset := 16#18#;
   ONE_SEGMENT : constant Unsigned_32 := 1;

   --  Doorbell targets (5.6): 0 rings the command ring; a slot's doorbell
   --  takes the DCI. EP0's DCI is 1.
   COMMAND_DOORBELL : constant Unsigned_32 := 0;
   EP0_DCI : constant := 1;

   ---------------------------------------------------------------------------
   --  Contexts (6.2)
   ---------------------------------------------------------------------------
   --  Input Control Context (6.2.5.1) add flags: A0 slot, A1 EP0.
   ADD_SLOT : constant Unsigned_32 := 2 ** 0;
   ADD_EP0  : constant Unsigned_32 := 2 ** 1;
   --  Slot context (6.2.2).
   SLOT_SPEED_SHIFT   : constant := 20;
   SLOT_HUB           : constant Unsigned_32 := 2 ** 26;
   SLOT_ENTRIES_SHIFT : constant := 27;
   SLOT_ENTRIES_MASK  : constant Unsigned_32 := 16#F800_0000#;
   SLOT_ROOT_PORT_SHIFT : constant := 16;
   SLOT_PORTS_SHIFT   : constant := 24;
   SLOT_TT_THINK_SHIFT : constant := 16;
   LOW_24_BITS        : constant Unsigned_32 := 16#00FF_FFFF#;
   LOW_16_BITS        : constant Unsigned_32 := 16#0000_FFFF#;
   --  Endpoint context (6.2.3).
   EP_INTERVAL_SHIFT : constant := 16;
   EP_CERR_SHIFT     : constant := 1;
   EP_CERR_RETRIES   : constant Unsigned_32 := 3;
   EP_TYPE_SHIFT     : constant := 3;
   EP_TYPE_BULK_OUT  : constant Unsigned_32 := 2;
   EP_TYPE_CONTROL   : constant Unsigned_32 := 4;
   EP_TYPE_BULK_IN   : constant Unsigned_32 := 6;
   EP_TYPE_INTERRUPT_IN : constant Unsigned_32 := 7;
   EP_PACKET_SHIFT   : constant := 16;
   EP_ESIT_SHIFT     : constant := 16;
   EP0_AVERAGE_TRB   : constant Unsigned_32 := 8;
   BULK_AVERAGE_TRB  : constant Unsigned_32 := 32768;
   CONTEXT_DWORDS_32 : constant := 8;
   CONTEXT_DWORDS_64 : constant := 16;
   CONTEXT_COUNT     : constant := 33;   --  input control + slot + 31 EPs
   MAX_DCI           : constant := 31;

   ---------------------------------------------------------------------------
   --  USB requests (USB 2.0 9.3, 9.4; HID 1.11 7.2; hub 11.24; BOT 3.2)
   ---------------------------------------------------------------------------
   TYPE_DEVICE_IN      : constant Unsigned_8 := 16#80#;
   TYPE_DEVICE_OUT     : constant Unsigned_8 := 16#00#;
   TYPE_ENDPOINT_OUT   : constant Unsigned_8 := 16#02#;
   TYPE_CLASS_INTERFACE_OUT : constant Unsigned_8 := 16#21#;
   TYPE_CLASS_INTERFACE_IN  : constant Unsigned_8 := 16#A1#;
   TYPE_CLASS_DEVICE_IN : constant Unsigned_8 := 16#A0#;
   TYPE_CLASS_OTHER_IN  : constant Unsigned_8 := 16#A3#;
   TYPE_CLASS_OTHER_OUT : constant Unsigned_8 := 16#23#;
   DIRECTION_IN_BIT    : constant Unsigned_8 := 16#80#;
   REQUEST_GET_STATUS     : constant Unsigned_8 := 0;
   REQUEST_CLEAR_FEATURE  : constant Unsigned_8 := 1;
   REQUEST_SET_FEATURE    : constant Unsigned_8 := 3;
   REQUEST_GET_DESCRIPTOR : constant Unsigned_8 := 6;
   REQUEST_SET_CONFIGURATION : constant Unsigned_8 := 9;
   HID_SET_PROTOCOL       : constant Unsigned_8 := 16#0B#;
   BOT_GET_MAX_LUN        : constant Unsigned_8 := 16#FE#;
   DESCRIPTOR_DEVICE        : constant Unsigned_16 := 16#0100#;
   DESCRIPTOR_CONFIGURATION : constant Unsigned_16 := 16#0200#;
   DESCRIPTOR_HUB           : constant Unsigned_16 := 16#2900#;
   FEATURE_ENDPOINT_HALT : constant Unsigned_16 := 0;
   HUB_PORT_RESET        : constant Unsigned_16 := 4;
   HUB_PORT_POWER        : constant Unsigned_16 := 8;
   HUB_C_PORT_RESET      : constant Unsigned_16 := 20;
   HID_BOOT_PROTOCOL     : constant Unsigned_16 := 0;
   DEVICE_DESCRIPTOR_BYTES : constant := 18;
   DEVICE_DESCRIPTOR_TYPE  : constant Unsigned_8 := 1;
   CONFIG_DESCRIPTOR_TYPE  : constant Unsigned_8 := 2;
   CONFIG_HEADER_BYTES     : constant := 9;
   EP0_PROBE_BYTES         : constant := 8;
   HUB_PORT_STATUS_BYTES   : constant := 4;
   HUB_DESCRIPTOR_MINIMUM  : constant := 8;
   HUB_DESCRIPTOR_MAXIMUM  : constant := 71;
   HUB_PROTOCOL_MULTI_TT   : constant Unsigned_8 := 2;
   USB_ENDPOINT_NUMBER_MASK : constant Unsigned_8 := 16#0F#;
   BOOT_MOUSE_MINIMUM_REPORT : constant := 3;
   BOOT_KEYBOARD_REPORT     : constant := 8;

   ---------------------------------------------------------------------------
   --  Explicit time limits and the specification's minimum delays
   ---------------------------------------------------------------------------
   --  Linux XHCI_CMD_DEFAULT_TIMEOUT and USB_CTRL_GET_TIMEOUT.
   Command_Timeout_Ms : constant Unsigned_64 := 5_000;
   Control_Timeout_Ms : constant Unsigned_64 := 5_000;
   Bulk_Timeout_Ms    : constant Unsigned_64 := 2_000;
   --  Halt, reset and run handshakes (5.4.1, 5.4.2: HCH within 16 ms of
   --  R/S clear; Linux allows 10 s for HCRST/CNR on some hosts).
   Register_Wait_Ms   : constant Natural := 1_000;
   --  Boot: a port may still be finishing a reset or SuperSpeed link
   --  training it began before HCRST settled (the IdeaPad, tests/usb-optical
   --  port_tests); Linux retries an 800 ms reset five times (hub.c
   --  HUB_RESET_TIMEOUT, PORT_RESET_TRIES).
   Port_Reset_Wait_Ms : constant Natural := 5 * Natural (PM.Reset_Limit_Ms);
   --  USB 2.0 9.2.6.3: SET_ADDRESS recovery interval (Address Device
   --  issues SET_ADDRESS; 4.6.5). Linux waits 10 ms; 2 ms is the minimum.
   SetAddress_Recovery_Ms : constant Unsigned_64 := 2;
   --  USB 2.0 11.23.2.1: hub power-on to power-good, at least 100 ms.
   Hub_Power_Good_Ms  : constant Natural := 100;
   Hub_Reset_Poll_Ms  : constant Unsigned_64 := 10;
   Hub_Reset_Polls    : constant := 50;
   --  Bounded boot discovery window: devices attached before boot finish
   --  reconnecting after HCRST (USB2 companions after a failed SuperSpeed
   --  attempt). A device arriving later is handled as hotplug.
   Boot_Settle_Ms     : constant Natural := 3_000;
   Boot_Settle_Step_Ms : constant Unsigned_64 := 10;
   Health_Interval_Ms : constant Unsigned_64 := 5_000;
   Quiet_Health_Interval_Ms : constant Unsigned_64 := 60_000;
   Tick_Interval_Ms   : constant Unsigned_64 := 1_000;
   --  First transfer events logged one per line (bring-up evidence).
   First_Logged_Transfers : constant := 6;
   Port_Change_Rereads : constant := 4;

   HID_REPORT_STRIDE  : constant := 64;
   HID_TRANSFER_DEPTH : constant := XHCI_Completions.Queue_Depth;
   pragma Compile_Time_Error
     (HID_TRANSFER_DEPTH > XHCI_Completions.Queue_Depth,
      "a pipe's outstanding TRBs must fit its completion mailbox");
   pragma Compile_Time_Error
     (HID_REPORT_STRIDE * R.Ring_Entries > 4096,
      "one report page holds one buffer per ring TRB");
   MAX_ROOT_PORTS : constant := 255;

   subtype TRB is XHCI_Completions.Event;
   NULL_TRB : constant TRB := (others => 0);
   subtype Device_Index is Positive range 1 .. XHCI_Completions.Maximum_Slots;
   subtype Slot_Or_None is Natural range 0 .. XHCI_Completions.Maximum_Slots;
   subtype Root_Port_Number is Positive range 1 .. MAX_ROOT_PORTS;
   subtype Port_Or_None is Natural range 0 .. MAX_ROOT_PORTS;

   type Address_Array is array (Natural range <>) of Unsigned_64
     with Convention => C;
   type DWord_Array is array (Natural range <>) of Unsigned_32
     with Convention => C, Volatile_Components;
   type Byte_Array is array (Natural range <>) of Unsigned_8
     with Convention => C, Volatile_Components;
   type Page_TRBs is array (0 .. 255) of TRB with Convention => C;

   type ERST_Entry is record
      ringBase : Unsigned_64;
      ringSize : Unsigned_32;
      reserved : Unsigned_32;
   end record with Convention => C, Size => 128;
   for ERST_Entry use record
      ringBase at 0 range 0 .. 63;
      ringSize at 8 range 0 .. 31;
      reserved at 12 range 0 .. 31;
   end record;

   ---------------------------------------------------------------------------
   --  MMIO and DMA access
   ---------------------------------------------------------------------------
   --  Volatile_Full_Access on the pointed-to type gives both the compiler
   --  barrier and the indivisible access width xHCI registers require; the
   --  same accessors order TRB stores (control dword, with the cycle bit,
   --  last).
   type MMIO_Byte is mod 2 ** 8 with Size => 8, Volatile_Full_Access;
   type MMIO_DWord is mod 2 ** 32 with Size => 32, Volatile_Full_Access;
   package Byte_Access is new System.Address_To_Access_Conversions (MMIO_Byte);
   package DWord_Access is new System.Address_To_Access_Conversions (MMIO_DWord);

   function Read8 (base : System.Address; offset : Storage_Offset) return Unsigned_8 is
     (Unsigned_8 (Byte_Access.To_Pointer (base + offset).all));
   function Read32 (base : System.Address; offset : Storage_Offset) return Unsigned_32 is
     (Unsigned_32 (DWord_Access.To_Pointer (base + offset).all));
   procedure Write8 (base : System.Address; offset : Storage_Offset; value : Unsigned_8) is
   begin
      Byte_Access.To_Pointer (base + offset).all := MMIO_Byte (value);
   end Write8;
   procedure Write32 (base : System.Address; offset : Storage_Offset; value : Unsigned_32) is
   begin
      DWord_Access.To_Pointer (base + offset).all := MMIO_DWord (value);
   end Write32;
   procedure Write64 (base : System.Address; offset : Storage_Offset; value : Unsigned_64) is
   begin
      --  Naturally aligned 64-bit registers as two dwords, low first.
      Write32 (base, offset, Unsigned_32 (value and 16#FFFF_FFFF#));
      Write32 (base, offset + 4, Unsigned_32 (Shift_Right (value, 32)));
   end Write64;

   function DMA (Offset : Unsigned_64) return System.Address is
     (To_Address (Integer_Address (DMA_VIRT_BASE + Offset)));
   function Low (Value : Unsigned_64) return Unsigned_32 is
     (Unsigned_32 (Value and 16#FFFF_FFFF#));
   function High (Value : Unsigned_64) return Unsigned_32 is
     (Unsigned_32 (Shift_Right (Value, 32)));

   barBase      : System.Address := System.Null_Address;
   barMappedBytes : Unsigned_64 := 0;
   operational  : System.Address := System.Null_Address;
   runtimeBase  : System.Address := System.Null_Address;
   doorbellBase : System.Address := System.Null_Address;
   dmaPhysical  : Unsigned_64 := 0;

   function Phys (Offset : Unsigned_64) return Unsigned_64 is (dmaPhysical + Offset);
   function Device_Offset (Slot : Device_Index; Offset : Unsigned_64) return Unsigned_64 is
     (Offset + Unsigned_64 (Slot - 1) * DEVICE_STRIDE);
   function PORTSC (Port : Root_Port_Number) return Storage_Offset is
     (OP_PORTSC_BASE + Storage_Offset (Port - 1) * OP_PORT_STRIDE);
   function Read_Port (Port : Root_Port_Number) return Unsigned_32 is
     (Read32 (operational, PORTSC (Port)));
   procedure Ring_Doorbell (Target : Natural; Value : Unsigned_32) is
   begin
      Write32 (doorbellBase, Storage_Offset (Target * 4), Value);
   end Ring_Doorbell;

   procedure Store_TRB (Ring_Offset : Unsigned_64; Index : R.Ring_Index; Item : TRB) is
      Base : constant System.Address := DMA (Ring_Offset + Unsigned_64 (Index) * R.TRB_Bytes);
   begin
      Write32 (Base, 0, Item.ParameterLo);
      Write32 (Base, 4, Item.ParameterHi);
      Write32 (Base, 8, Item.Status);
      Write32 (Base, 12, Item.Control);
   end Store_TRB;

   --  An empty ring (every cycle bit zero, unlike the fresh producer's one)
   --  closed by a Link TRB to its start (4.11.5.1).
   procedure Init_Ring (Ring_Offset : Unsigned_64) is
   begin
      for I in R.Ring_Index loop
         Store_TRB (Ring_Offset, I, NULL_TRB);
      end loop;
      Store_TRB (Ring_Offset, R.Link_Index,
        (ParameterLo => Low (Phys (Ring_Offset)), ParameterHi => High (Phys (Ring_Offset)),
         Status => 0,
         Control => Type_Field (TRB_LINK) or TRB_TOGGLE_CYCLE or TRB_CYCLE));
   end Init_Ring;

   --  Write Item at P's enqueue position with P's cycle; rewrite the Link
   --  TRB after the last slot of a pass (4.11.5.1). Returns its address.
   procedure Enqueue
     (Ring_Offset : Unsigned_64; P : in out R.Producer; Item : TRB; Pointer : out Unsigned_64)
   is
      Placed : R.Placement;
      Value : TRB := Item;
   begin
      R.Advance (P, Placed);
      Value.Control := (Item.Control and not TRB_CYCLE) or Unsigned_32 (Placed.Cycle);
      Store_TRB (Ring_Offset, Placed.Index, Value);
      Pointer := R.Address (Phys (Ring_Offset), Placed.Index);
      if Placed.Link_After then
         Store_TRB (Ring_Offset, R.Link_Index,
           (ParameterLo => Low (Phys (Ring_Offset)), ParameterHi => High (Phys (Ring_Offset)),
            Status => 0,
            Control => Type_Field (TRB_LINK) or TRB_TOGGLE_CYCLE or Unsigned_32 (Placed.Link_Cycle)));
      end if;
   end Enqueue;

   type Device_DMA_Area is record
      deviceContext, inputContext : DWord_Array (0 .. 1023);
      ep0Ring, hidRing : Page_TRBs;
      descriptorBytes, hidReports : Byte_Array (0 .. 4095);
      bulkOutRing, bulkInRing : Page_TRBs;
   end record with Convention => C, Size => DEVICE_PAGES * 4096 * 8;
   type Device_DMA_Array is array (Device_Index) of Device_DMA_Area with Convention => C;
   pragma Compile_Time_Error
     (Device_DMA_Array'Size /= MAX_DEVICE_SLOTS * DEVICE_PAGES * 4096 * 8,
      "native device DMA array does not match shared allocation layout");
   deviceDMA : Device_DMA_Array with Import,
     Address => To_Address (Integer_Address (DMA_VIRT_BASE + DEVICE_CONTEXT_OFFSET));
   dcbaa : Address_Array (0 .. 255) with Import,
     Address => To_Address (Integer_Address (DMA_VIRT_BASE + DCBAA_OFFSET));
   erst : ERST_Entry with Import,
     Address => To_Address (Integer_Address (DMA_VIRT_BASE + ERST_OFFSET));

   ---------------------------------------------------------------------------
   --  Text for log lines
   ---------------------------------------------------------------------------
   Hex_Digits : constant String := "0123456789ABCDEF";
   function Hex (Value : Unsigned_64; Digits_Count : Positive) return String is
      Text : String (1 .. Digits_Count);
   begin
      for I in Text'Range loop
         Text (I) := Hex_Digits
           (Natural (Shift_Right (Value, (Digits_Count - I) * 4) and 16#F#) + 1);
      end loop;
      return Text;
   end Hex;
   function H32 (Value : Unsigned_32) return String is ("0x" & Hex (Unsigned_64 (Value), 8));
   function H16 (Value : Unsigned_16) return String is (Hex (Unsigned_64 (Value), 4));
   function H8 (Value : Unsigned_8) return String is (Hex (Unsigned_64 (Value), 2));
   function D (Value : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end D;
   function D (Value : Natural) return String is (D (Unsigned_64 (Value)));

   Location_Length : constant := 7;
   locationText : String (1 .. Location_Length) := "??:??.?";
   identity : CI.Identity;
   interruptMode : Runtime_Interrupt_Mode := INTERRUPT_POLLING;

   function Now_Ms return Unsigned_64 is (CuBit.Monotonic.Milliseconds);

   procedure Log (Text : String; Warning : Boolean := False) is
   begin
      Boot_Log.Line ("xhci: " & locationText & " " & Text, Warning);
   end Log;
   procedure Stage (Name : String; OK : Boolean; Detail : String) is
   begin
      Log ("stage " & Name & (if OK then " ok " else " fail ") & Detail, not OK);
   end Stage;
   procedure Trace (Text : String) is
   begin
      Boot_Log.Serial ("xhci: " & locationText & " " & Text);
   end Trace;

   function Speed_Name (S : Port_Speed) return String is
     (case S is
         when SPEED_FULL => "full", when SPEED_LOW => "low", when SPEED_HIGH => "high",
         when SPEED_SUPER => "super", when SPEED_SUPER_PLUS => "super+",
         when others => "none");
   function Link_Name (L : Link_State) return String is
     (case L is
         when LINK_U0 => "U0", when LINK_U3 => "U3", when LINK_DISABLED => "disabled",
         when LINK_RX_DETECT => "rxdetect", when LINK_INACTIVE => "inactive",
         when LINK_POLLING => "polling", when LINK_RECOVERY => "recovery",
         when LINK_HOT_RESET => "hotreset", when LINK_COMPLIANCE => "compliance",
         when LINK_RESUME => "resume", when others => "U" & D (Natural (L)));
   function Change_Names (Status : Unsigned_32) return String is
     ((if (Status and PORTSC_CSC) /= 0 then "c" else "") &
      (if (Status and PORTSC_PEC) /= 0 then "e" else "") &
      (if (Status and PORTSC_WRC) /= 0 then "w" else "") &
      (if (Status and PORTSC_OCC) /= 0 then "o" else "") &
      (if (Status and PORTSC_PRC) /= 0 then "r" else "") &
      (if (Status and PORTSC_PLC) /= 0 then "l" else "") &
      (if (Status and PORTSC_CEC) /= 0 then "f" else "") &
      (if Changes (Status) = 0 then "-" else ""));
   --  "portsc=0x.. ccs=1 ped=0 pls=polling spd=full chg=cr"
   function Port_Text (Status : Unsigned_32) return String is
     ("portsc=" & H32 (Status) &
      " ccs=" & (if Connected (Status) then "1" else "0") &
      " ped=" & (if Enabled (Status) then "1" else "0") &
      " pls=" & Link_Name (Link (Status)) &
      " spd=" & Speed_Name (Speed (Status)) &
      " chg=" & Change_Names (Status));

   ---------------------------------------------------------------------------
   --  Controller state
   ---------------------------------------------------------------------------
   maxPorts       : Natural := 0;
   connectedPorts : Natural := 0;
   contextStride  : Natural := CONTEXT_DWORDS_32;
   activeSlot     : Slot_Or_None := 0;

   events   : R.Consumer := R.Fresh_Consumer;
   commands : R.Producer := R.Fresh_Producer;
   completionMailboxes : XHCI_Completions.Mailboxes;
   ehbPending : Boolean := False;
   hotplugArmed : Boolean := False;

   type Control_State is record
      Ring   : R.Producer := R.Fresh_Producer;
      Failed : Boolean := False;
      Busy   : Boolean := False;
   end record;
   ep0State : array (Device_Index) of Control_State;
   slotPort    : array (Device_Index) of Port_Or_None := [others => 0];
   slotRetired : array (Device_Index) of Boolean := [others => False];
   ports : array (Root_Port_Number) of PM.Port_State;
   portResetStarted : array (Root_Port_Number) of Unsigned_64 := [others => 0];

   --  Health counters (one line every Health_Interval_Ms when they move).
   type Health_Counters is record
      Events, Transfers, Commands, Port_Changes, Controller_Events, Other_Events : Unsigned_64 := 0;
      Interrupts, Stalls, Babbles, Transaction_Errors, Other_Errors : Unsigned_64 := 0;
      Recoveries, Port_Resets, Given_Up, Ring_Full, Mailbox_Waits, EHB_Stuck, Late_Events : Unsigned_64 := 0;
      Dropped, Invalid_Targets, Attaches, Detaches, Enumeration_Failures : Unsigned_64 := 0;
      Last_Code : Completion_Code := CC_INVALID;
   end record;
   health, healthBefore : Health_Counters;
   healthDue, healthWindowStart, tickDue : Unsigned_64 := 0;
   transfersLogged : Natural := 0;
   --  Interrupt delivery check: Silent_Ticks_Limit ticks in a row with
   --  events waiting and no interrupt, and the driver stops trusting the
   --  interrupt (masked MSI, lost routing) and polls instead.
   Silent_Ticks_Limit : constant := 3;
   silentTicks : Natural := 0;
   interruptsAtTick : Unsigned_64 := 0;
   interruptsTrusted : Boolean := True;

   mouseDiagnostics : Boot_Mouse_Diagnostics;
   diagnosticButtons : Unsigned_8 := 0;

   procedure Count_Error (Code : Completion_Code) is
   begin
      if Code = CC_STALL then health.Stalls := health.Stalls + 1;
      elsif Code = CC_BABBLE then health.Babbles := health.Babbles + 1;
      elsif Code = CC_TRANSACTION_ERROR or else Code = CC_SPLIT_TRANSACTION_ERROR then
         health.Transaction_Errors := health.Transaction_Errors + 1;
      else health.Other_Errors := health.Other_Errors + 1;
      end if;
   end Count_Error;

   ---------------------------------------------------------------------------
   --  Event ring (4.9.4): the single owner. Events drain into per-endpoint
   --  completion mailboxes; port and controller events are handled here.
   ---------------------------------------------------------------------------
   procedure Handle_Port_Event (Port : Natural);

   function Event_At (Index : R.Event_Index; Control : Unsigned_32) return TRB is
      Base : constant System.Address :=
        DMA (EVENT_RING_OFFSET + Unsigned_64 (Index) * R.TRB_Bytes);
   begin
      return (ParameterLo => Read32 (Base, 0), ParameterHi => Read32 (Base, 4),
              Status => Read32 (Base, 8), Control => Control);
   end Event_At;

   procedure Write_Dequeue is
   begin
      Write64 (runtimeBase + RT_INTR0, INTR_ERDP,
               R.Dequeue_Register (Phys (EVENT_RING_OFFSET), events));
      ehbPending := False;
   end Write_Dequeue;

   procedure Dispatch (Item : TRB) is
      Result : XHCI_Completions.Route_Result;
      Kind : constant TRB_Type := Type_Of (Item.Control);
   begin
      health.Events := health.Events + 1;
      if Kind = TRB_TRANSFER_EVENT and then
        Slot_Of (Item.Control) in 1 .. XHCI_Completions.Maximum_Slots and then
        slotRetired (Device_Index (Slot_Of (Item.Control)))
      then
         --  A detached device's late completion: its slot is being disabled.
         health.Dropped := health.Dropped + 1;
         return;
      end if;
      XHCI_Completions.Route (completionMailboxes, Item, Result);
      case Result is
         when XHCI_Completions.Queued =>
            if Kind = TRB_TRANSFER_EVENT then
               health.Transfers := health.Transfers + 1;
               health.Last_Code := Code_Of (Item.Status);
               if hotplugArmed and then transfersLogged < First_Logged_Transfers then
                  transfersLogged := transfersLogged + 1;
                  Log ("event" & Natural'Image (transfersLogged) & " xfer slot=" &
                       D (Unsigned_64 (Slot_Of (Item.Control))) & " dci=" &
                       D (Unsigned_64 (Endpoint_Of (Item.Control))) & " cc=" &
                       Code_Name (Code_Of (Item.Status)) & " residual=" &
                       D (Unsigned_64 (Residual_Of (Item.Status))) & " deq=" &
                       D (events.Head) & "/" & D (Natural (events.Cycle)));
               end if;
            else
               health.Commands := health.Commands + 1;
            end if;
         when XHCI_Completions.Not_A_Completion =>
            if Kind = TRB_PORT_STATUS_CHANGE then
               health.Port_Changes := health.Port_Changes + 1;
               if hotplugArmed then
                  Handle_Port_Event (Natural (Port_Of (Item.ParameterLo)));
               end if;
            elsif Kind = TRB_HOST_CONTROLLER then
               health.Controller_Events := health.Controller_Events + 1;
               if Code_Of (Item.Status) = CC_EVENT_RING_FULL then
                  health.Ring_Full := health.Ring_Full + 1;
               end if;
               Log ("host controller event cc=" & Code_Name (Code_Of (Item.Status)) &
                    " usbsts=" & H32 (Read32 (operational, OP_USBSTS)), True);
            else
               health.Other_Events := health.Other_Events + 1;
            end if;
         when XHCI_Completions.Invalid_Target =>
            health.Invalid_Targets := health.Invalid_Targets + 1;
            Log ("event for no slot control=" & H32 (Item.Control) &
                 " status=" & H32 (Item.Status), True);
         when XHCI_Completions.Queue_Full =>
            --  Has_Room was checked; unreachable, but never silent.
            health.Mailbox_Waits := health.Mailbox_Waits + 1;
      end case;
   end Dispatch;

   --  Consume up to Budget events. ERDP (with EHB) is written when anything
   --  was consumed or an interrupt is being serviced.
   procedure Drain (Budget : Positive; Consumed : out Natural) is
      Control : Unsigned_32;
      Item : TRB;
   begin
      Consumed := 0;
      for I in 1 .. Budget loop
         Control := Read32 (DMA (EVENT_RING_OFFSET + Unsigned_64 (events.Head) * R.TRB_Bytes), 12);
         exit when not R.Owned (events, Control);
         Item := Event_At (events.Head, Control);
         if not XHCI_Completions.Has_Room (completionMailboxes, Item) then
            --  Left in the ring until its consumer takes its backlog: a
            --  mailbox never overflows and nothing is lost.
            health.Mailbox_Waits := health.Mailbox_Waits + 1;
            exit;
         end if;
         R.Consume (events);
         Consumed := Consumed + 1;
         Dispatch (Item);
      end loop;
      if R.Must_Write_Dequeue (Consumed, ehbPending) then
         Write_Dequeue;
      end if;
   end Drain;

   function Collect_Event return Boolean is
      Consumed : Natural;
   begin
      Drain (1, Consumed);
      return Consumed > 0;
   end Collect_Event;

   ---------------------------------------------------------------------------
   --  Commands (4.6): one in flight; the owner takes its completion.
   ---------------------------------------------------------------------------
   type Command_Owner is
     (No_Owner, Boot_Owner, Engine_Owner, Mouse_Owner, Keyboard_Owner,
      Release_Owner, Orphan_Owner);
   type Command_State is record
      Busy, Done : Boolean := False;
      Owner : Command_Owner := No_Owner;
      Pointer, Deadline : Unsigned_64 := 0;
      Code : Completion_Code := CC_INVALID;
      Slot : Natural := 0;
   end record;
   command : Command_State;

   procedure Issue_Command (Item : TRB; Owner : Command_Owner) is
   begin
      Enqueue (COMMAND_RING_OFFSET, commands, Item, command.Pointer);
      command := (Busy => True, Done => False, Owner => Owner, Pointer => command.Pointer,
                  Deadline => PM.Plus (Now_Ms, Command_Timeout_Ms), Code => CC_INVALID, Slot => 0);
      Ring_Doorbell (0, COMMAND_DOORBELL);
   end Issue_Command;

   function Command_Free return Boolean is (not command.Busy);
   function Command_Done_For (Owner : Command_Owner) return Boolean is
     (command.Busy and then command.Done and then command.Owner = Owner);
   procedure Release_Command is
   begin
      command := (others => <>);
   end Release_Command;

   procedure Poll_Command is
      Item : TRB;
      Found : Boolean;
      Pointer : Unsigned_64;
   begin
      loop
         XHCI_Completions.Take (completionMailboxes, 0, 0, Item, Found);
         exit when not Found;
         Pointer := (Unsigned_64 (Item.ParameterLo) or Shift_Left (Unsigned_64 (Item.ParameterHi), 32))
           and not Unsigned_64 (R.TRB_Bytes - 1);
         --  Command Ring Stopped (after an abort) names no command of ours.
         if command.Busy and then not command.Done and then Pointer = command.Pointer then
            command.Done := True;
            command.Code := Code_Of (Item.Status);
            command.Slot := Natural (Slot_Of (Item.Control));
         end if;
      end loop;
      if command.Busy and then not command.Done and then Now_Ms >= command.Deadline then
         --  4.6.1.2: abort the command ring; the controller completes the
         --  command as aborted and stops; the next doorbell restarts it.
         Write32 (operational, OP_CRCR, CRCR_CA);
         command.Done := True;
         command.Code := CC_COMMAND_ABORTED;
         Log ("command timeout ms=" & D (Command_Timeout_Ms) & " crcr-abort usbsts=" &
              H32 (Read32 (operational, OP_USBSTS)), True);
      end if;
      if command.Busy and then command.Done and then command.Owner = Orphan_Owner then
         Release_Command;
      end if;
   end Poll_Command;

   --  Boot path only: issue, then wait for the completion (bounded).
   procedure Submit_Command
     (Item : TRB; Code : out Completion_Code; Slot : out Natural; Completed : out Boolean)
   is
      Ignore : Unsigned_64;
      Progressed : Boolean;
   begin
      Code := CC_INVALID;
      Slot := 0;
      Completed := False;
      if command.Busy then
         return;
      end if;
      Issue_Command (Item, Boot_Owner);
      loop
         Progressed := Collect_Event;
         Poll_Command;
         if command.Done then
            Code := command.Code;
            Slot := command.Slot;
            Completed := Code /= CC_COMMAND_ABORTED;
            Release_Command;
            return;
         end if;
         if not Progressed then
            Ignore := syscall (SYSCALL_SLEEP, 1);
         end if;
      end loop;
   end Submit_Command;

   function Command_TRB (Kind : TRB_Type; Slot : Natural; Pointer : Unsigned_64 := 0;
                         Extra : Unsigned_32 := 0) return TRB is
     ((ParameterLo => Low (Pointer), ParameterHi => High (Pointer), Status => 0,
       Control => Type_Field (Kind) or Slot_Field (Unsigned_32 (Slot)) or Extra));

   ---------------------------------------------------------------------------
   --  Control transfers on EP0 (4.11.2.2): Setup, optional Data, Status.
   ---------------------------------------------------------------------------
   type Control_Result is (Control_Pending, Control_OK, Control_Error, Control_Timeout);
   type Control_Transfer is record
      Active : Boolean := False;
      Slot   : Slot_Or_None := 0;
      Setup_Pointer, Data_Pointer, Status_Pointer : Unsigned_64 := 0;
      Length, Actual : Natural := 0;
      Data_In, Data_Done : Boolean := False;
      Deadline : Unsigned_64 := 0;
      Result : Control_Result := Control_Pending;
      Code : Completion_Code := CC_INVALID;
   end record;

   procedure Start_Control
     (C : out Control_Transfer; Slot : Device_Index;
      Request_Type, Request : Unsigned_8; Value, Index : Unsigned_16;
      Length : Natural; Buffer_Offset : Unsigned_64)
   is
      Ring_Offset : constant Unsigned_64 := Device_Offset (Slot, EP0_RING_OFFSET);
      Data_In : constant Boolean := (Request_Type and DIRECTION_IN_BIT) /= 0;
      Transfer : constant Unsigned_32 :=
        (if Length = 0 then TRANSFER_NO_DATA elsif Data_In then TRANSFER_IN else TRANSFER_OUT);
      Buffer : constant Unsigned_64 := Phys (Buffer_Offset);
   begin
      C := (Slot => Slot, Length => Length, Data_In => Data_In, others => <>);
      if Length > 4096 or else ep0State (Slot).Failed or else ep0State (Slot).Busy then
         C.Result := Control_Error;
         return;
      end if;
      C.Active := True;
      ep0State (Slot).Busy := True;
      Enqueue (Ring_Offset, ep0State (Slot).Ring,
        (ParameterLo => Unsigned_32 (Request_Type) or Shift_Left (Unsigned_32 (Request), 8)
                        or Shift_Left (Unsigned_32 (Value), 16),
         ParameterHi => Unsigned_32 (Index) or Shift_Left (Unsigned_32 (Length), 16),
         Status => EP0_PROBE_BYTES,
         Control => Type_Field (TRB_SETUP_STAGE) or TRB_IDT or
                    Shift_Left (Transfer, TRB_TRT_SHIFT)), C.Setup_Pointer);
      if Length > 0 then
         Enqueue (Ring_Offset, ep0State (Slot).Ring,
           (ParameterLo => Low (Buffer), ParameterHi => High (Buffer),
            Status => Unsigned_32 (Length),
            Control => Type_Field (TRB_DATA_STAGE) or TRB_IOC or TRB_ISP or
                       (if Data_In then TRB_DIRECTION_IN else 0)), C.Data_Pointer);
      end if;
      Enqueue (Ring_Offset, ep0State (Slot).Ring,
        (ParameterLo => 0, ParameterHi => 0, Status => 0,
         Control => Type_Field (TRB_STATUS_STAGE) or TRB_IOC or
                    (if Length = 0 or else not Data_In then TRB_DIRECTION_IN else 0)),
         C.Status_Pointer);
      C.Deadline := PM.Plus (Now_Ms, Control_Timeout_Ms);
      Ring_Doorbell (Slot, EP0_DCI);
   end Start_Control;

   procedure Finish_Control (C : in out Control_Transfer; Result : Control_Result) is
   begin
      C.Result := Result;
      C.Active := False;
      if C.Slot in Device_Index then
         ep0State (C.Slot).Busy := False;
         if Result /= Control_OK then
            --  Quarantine the ring after an error or timeout; an endpoint
            --  recovery, not another Setup TRB, clears it.
            ep0State (C.Slot).Failed := True;
         end if;
      end if;
   end Finish_Control;

   procedure Poll_Control (C : in out Control_Transfer) is
      Item : TRB;
      Found : Boolean;
      Pointer : Unsigned_64;
      Code : Completion_Code;
      Residual : Unsigned_32;
   begin
      while C.Active loop
         XHCI_Completions.Take (completionMailboxes, C.Slot, EP0_DCI, Item, Found);
         exit when not Found;
         Pointer := Unsigned_64 (Item.ParameterLo) or Shift_Left (Unsigned_64 (Item.ParameterHi), 32);
         Code := Code_Of (Item.Status);
         Residual := Residual_Of (Item.Status);
         C.Code := Code;
         if (Item.Control and TRB_EVENT_DATA) /= 0 or else
           (Pointer /= C.Setup_Pointer and then Pointer /= C.Data_Pointer and then
            Pointer /= C.Status_Pointer)
         then
            Finish_Control (C, Control_Error);
         elsif Code /= CC_SUCCESS and then Code /= CC_SHORT_PACKET then
            Finish_Control (C, Control_Error);
         elsif C.Length > 0 and then Pointer = C.Data_Pointer then
            if C.Data_Done or else Residual > Unsigned_32 (C.Length) then
               Finish_Control (C, Control_Error);
            else
               C.Actual := C.Length - Natural (Residual);
               C.Data_Done := True;
            end if;
         elsif Pointer = C.Status_Pointer and then Code = CC_SUCCESS and then Residual = 0
           and then (C.Length = 0 or else C.Data_Done)
         then
            Finish_Control (C, Control_OK);
         else
            Finish_Control (C, Control_Error);
         end if;
      end loop;
      if C.Active and then Now_Ms >= C.Deadline then
         C.Code := CC_INVALID;
         Finish_Control (C, Control_Timeout);
      end if;
   end Poll_Control;

   lastControlCode : Completion_Code := CC_INVALID;

   --  Boot path only.
   procedure Control_Request
     (Slot : Device_Index; Request_Type, Request : Unsigned_8; Value, Index : Unsigned_16;
      Length : Natural; Buffer_Offset : Unsigned_64; Actual : out Natural; Success : out Boolean)
   is
      C : Control_Transfer;
      Progressed : Boolean;
      Ignore : Unsigned_64;
   begin
      Start_Control (C, Slot, Request_Type, Request, Value, Index, Length, Buffer_Offset);
      while C.Active loop
         Progressed := Collect_Event;
         Poll_Control (C);
         if C.Active and then not Progressed then
            Ignore := syscall (SYSCALL_SLEEP, 1);
         end if;
      end loop;
      lastControlCode := C.Code;
      Actual := C.Actual;
      Success := C.Result = Control_OK;
   end Control_Request;

   function Descriptor_Buffer (Slot : Device_Index) return Unsigned_64 is
     (Device_Offset (Slot, DESCRIPTOR_OFFSET));

   --  4.8.3: after a Stall, Reset Endpoint and Set TR Dequeue Pointer
   --  return EP0 to Stopped with a clean ring (boot path).
   function Recover_Control_Stall (Slot : Device_Index) return Boolean is
      Code : Completion_Code;
      Ignore_Slot : Natural;
      Done : Boolean;
   begin
      if lastControlCode /= CC_STALL or else not ep0State (Slot).Failed then
         return False;
      end if;
      Submit_Command (Command_TRB (TRB_RESET_ENDPOINT, Slot, Extra => Endpoint_Field (EP0_DCI)),
                      Code, Ignore_Slot, Done);
      if not Done or else Code /= CC_SUCCESS then return False; end if;
      Submit_Command (Command_TRB (TRB_SET_TR_DEQUEUE, Slot,
                        R.Restart_Pointer (Phys (Device_Offset (Slot, EP0_RING_OFFSET)),
                                           ep0State (Slot).Ring),
                        Endpoint_Field (EP0_DCI)), Code, Ignore_Slot, Done);
      if not Done or else Code /= CC_SUCCESS then return False; end if;
      ep0State (Slot).Failed := False;
      return True;
   end Recover_Control_Stall;

   ---------------------------------------------------------------------------
   --  Device contexts
   ---------------------------------------------------------------------------
   procedure Clear_Slot_DMA (Slot : Device_Index) is
      Bytes : array (0 .. Natural (DEVICE_STRIDE) - 1) of Unsigned_8
        with Import, Volatile, Address => DMA (Device_Offset (Slot, DEVICE_CONTEXT_OFFSET));
   begin
      for I in Bytes'Range loop
         Bytes (I) := 0;
      end loop;
   end Clear_Slot_DMA;

   procedure Clear_Input (Slot : Device_Index) is
   begin
      for I in 0 .. CONTEXT_COUNT * contextStride - 1 loop
         deviceDMA (Slot).inputContext (I) := 0;
      end loop;
   end Clear_Input;

   function Speed_Code (S : XHCI_Topology.Speed) return Unsigned_32 is
     (Unsigned_32 (XHCI_Topology.Speed'Enum_Rep (S)));
   function Default_Packet (S : XHCI_Topology.Speed) return Unsigned_32 is
     (case S is
        when XHCI_Topology.Full_Speed | XHCI_Topology.Low_Speed => 8,
        when XHCI_Topology.High_Speed => 64,
        when XHCI_Topology.Super_Speed => 512);

   --  4.3.3: an Input Context adding the slot and EP0, for Address Device.
   procedure Build_Address_Input (Slot : Device_Index; Path : XHCI_Topology.Path) is
      Stride : constant Natural := contextStride;
      EP0 : constant Natural := 2 * Stride;
      Ring : constant Unsigned_64 := Phys (Device_Offset (Slot, EP0_RING_OFFSET));
   begin
      Clear_Input (Slot);
      Init_Ring (Device_Offset (Slot, EP0_RING_OFFSET));
      ep0State (Slot) := (others => <>);
      deviceDMA (Slot).inputContext (1) := ADD_SLOT or ADD_EP0;
      deviceDMA (Slot).inputContext (Stride) :=
        XHCI_Topology.Route_String (Path) or
        Shift_Left (Speed_Code (XHCI_Topology.Device_Speed (Path)), SLOT_SPEED_SHIFT) or
        Shift_Left (1, SLOT_ENTRIES_SHIFT);
      deviceDMA (Slot).inputContext (Stride + 1) :=
        Shift_Left (Unsigned_32 (XHCI_Topology.Root_Port (Path)), SLOT_ROOT_PORT_SHIFT);
      deviceDMA (Slot).inputContext (Stride + 2) := XHCI_Topology.TT_Context (Path);
      deviceDMA (Slot).inputContext (EP0 + 1) :=
        Shift_Left (EP_CERR_RETRIES, EP_CERR_SHIFT) or
        Shift_Left (EP_TYPE_CONTROL, EP_TYPE_SHIFT) or
        Shift_Left (Default_Packet (XHCI_Topology.Device_Speed (Path)), EP_PACKET_SHIFT);
      deviceDMA (Slot).inputContext (EP0 + 2) := Low (Ring) or TRB_CYCLE;
      deviceDMA (Slot).inputContext (EP0 + 3) := High (Ring);
      deviceDMA (Slot).inputContext (EP0 + 4) := EP0_AVERAGE_TRB;
      dcbaa (Slot) := Phys (Device_Offset (Slot, DEVICE_CONTEXT_OFFSET));
   end Build_Address_Input;

   function Address_Device_TRB (Slot : Device_Index) return TRB is
     (Command_TRB (TRB_ADDRESS_DEVICE, Slot, Phys (Device_Offset (Slot, INPUT_CONTEXT_OFFSET))));
   function Input_Command (Kind : TRB_Type; Slot : Device_Index) return TRB is
     (Command_TRB (Kind, Slot, Phys (Device_Offset (Slot, INPUT_CONTEXT_OFFSET))));

   --  The EP0 max packet size the device descriptor's byte 7 encodes for
   --  the port's speed (USB 2.0 9.6.1; USB 3.2 9.6.1: 2**bMaxPacketSize0),
   --  or 0 when it is not one the speed allows.
   function EP0_Packet (S : XHCI_Topology.Speed; Encoded : Unsigned_8) return Unsigned_32 is
     (case S is
        when XHCI_Topology.Full_Speed =>
          (if Encoded in 8 | 16 | 32 | 64 then Unsigned_32 (Encoded) else 0),
        when XHCI_Topology.Low_Speed => (if Encoded = 8 then 8 else 0),
        when XHCI_Topology.High_Speed => (if Encoded = 64 then 64 else 0),
        when XHCI_Topology.Super_Speed => (if Encoded = 9 then 512 else 0));

   --  4.6.7: Evaluate Context with EP0's corrected max packet size.
   procedure Build_Evaluate_Packet (Slot : Device_Index; Packet : Unsigned_32) is
      Stride : constant Natural := contextStride;
   begin
      Clear_Input (Slot);
      deviceDMA (Slot).inputContext (1) := ADD_EP0;
      for I in 0 .. Stride - 1 loop
         deviceDMA (Slot).inputContext (2 * Stride + I) :=
           deviceDMA (Slot).deviceContext (Stride + I);
      end loop;
      deviceDMA (Slot).inputContext (2 * Stride + 1) :=
        (deviceDMA (Slot).inputContext (2 * Stride + 1) and LOW_16_BITS) or
        Shift_Left (Packet, EP_PACKET_SHIFT);
   end Build_Evaluate_Packet;

   --  A Configure Endpoint input context starting from the slot context.
   procedure Begin_Configure (Slot : Device_Index; Add : Unsigned_32; Last_DCI : Natural) is
      Stride : constant Natural := contextStride;
   begin
      Clear_Input (Slot);
      deviceDMA (Slot).inputContext (1) := ADD_SLOT or Add;
      for I in 0 .. Stride - 1 loop
         deviceDMA (Slot).inputContext (Stride + I) := deviceDMA (Slot).deviceContext (I);
      end loop;
      deviceDMA (Slot).inputContext (Stride) :=
        (deviceDMA (Slot).inputContext (Stride) and not SLOT_ENTRIES_MASK) or
        Shift_Left (Unsigned_32 (Last_DCI), SLOT_ENTRIES_SHIFT);
   end Begin_Configure;

   --  Table 6-12: the xHCI Interval (2**Interval x 125 us) of an interrupt
   --  endpoint. HS/SS bInterval is already an exponent plus one; FS/LS
   --  bInterval counts 1 ms frames: floor(log2(8 * bInterval)), 3 .. 10.
   function Interrupt_Interval (S : XHCI_Topology.Speed; Descriptor : Unsigned_8) return Unsigned_32 is
      Frames : Natural := Natural'Max (Natural (Descriptor), 1) * 8;
      Exponent : Unsigned_32 := 0;
   begin
      if S in XHCI_Topology.High_Speed | XHCI_Topology.Super_Speed then
         return Unsigned_32 (Natural'Min (Natural'Max (Natural (Descriptor), 1) - 1, 15));
      end if;
      while Frames > 1 and then Exponent < 15 loop
         Frames := Frames / 2;
         Exponent := Exponent + 1;
      end loop;
      return Unsigned_32'Max (3, Unsigned_32'Min (Exponent, 10));
   end Interrupt_Interval;

   function Interval_Us (Exponent : Unsigned_32) return Unsigned_64 is
     (Shift_Left (Unsigned_64 (125), Natural (Exponent)));

   ---------------------------------------------------------------------------
   --  Interrupt-IN pipes (boot mouse, boot keyboard)
   ---------------------------------------------------------------------------
   type Pipe_Kind is (Mouse_Pipe, Keyboard_Pipe);
   type Ring_Area is (Primary_Area, Secondary_Area);
   type Pipe_State is record
      Slot      : Slot_Or_None := 0;
      DCI       : Natural range 0 .. MAX_DCI := 0;
      Packet    : Natural range 0 .. HID_REPORT_STRIDE := 0;
      Address   : Unsigned_8 := 0;
      Interface_Number : Unsigned_8 := 0;
      Interval  : Unsigned_8 := 0;
      Area      : Ring_Area := Primary_Area;
      Ring      : R.Producer := R.Fresh_Producer;
      Started   : Boolean := False;
      Recovery  : ER.Pipe_Recovery;
      Control   : Control_Transfer;
      --  When a report last arrived, and whether the first halt was described.
      Last_Good : Unsigned_64 := 0;
      Described : Boolean := False;
      Gave_Up   : Boolean := False;
   end record;
   pipes : array (Pipe_Kind) of Pipe_State;

   function Pipe_Name (K : Pipe_Kind) return String is
     (case K is when Mouse_Pipe => "mouse", when Keyboard_Pipe => "keyboard");
   function Ring_Offset (P : Pipe_State) return Unsigned_64 is
     (Device_Offset (P.Slot, (if P.Area = Primary_Area then HID_RING_OFFSET
                              else SECOND_INTERRUPT_RING_OFFSET)));
   function Report_Offset (P : Pipe_State) return Unsigned_64 is
     (Device_Offset (P.Slot, (if P.Area = Primary_Area then HID_REPORT_OFFSET
                              else SECOND_INTERRUPT_REPORT_OFFSET)));
   function Owner_Of (K : Pipe_Kind) return Command_Owner is
     (case K is when Mouse_Pipe => Mouse_Owner, when Keyboard_Pipe => Keyboard_Owner);

   procedure Queue_Pipe (K : Pipe_Kind) is
      P : Pipe_State renames pipes (K);
      Index : constant R.Enqueue_Index := P.Ring.Tail;
      Buffer : constant Unsigned_64 := Report_Offset (P) + Unsigned_64 (Index) * HID_REPORT_STRIDE;
      Ignore_Pointer : Unsigned_64;
   begin
      --  Every live TRB owns a distinct report buffer; the queue is shorter
      --  than the ring, so a buffer is never cleared while being written.
      for I in 0 .. HID_REPORT_STRIDE - 1 loop
         Write8 (DMA (Buffer), Storage_Offset (I), 0);
      end loop;
      Enqueue (Ring_Offset (P), P.Ring,
        (ParameterLo => Low (Phys (Buffer)), ParameterHi => High (Phys (Buffer)),
         Status => Unsigned_32 (P.Packet),
         Control => Type_Field (TRB_NORMAL) or TRB_IOC), Ignore_Pointer);
   end Queue_Pipe;

   procedure Ring_Pipe (K : Pipe_Kind) is
   begin
      Ring_Doorbell (pipes (K).Slot, Unsigned_32 (pipes (K).DCI));
   end Ring_Pipe;

   procedure Fill_Pipe (K : Pipe_Kind) is
   begin
      for I in 1 .. HID_TRANSFER_DEPTH loop
         Queue_Pipe (K);
      end loop;
      Ring_Pipe (K);
   end Fill_Pipe;

   procedure Start_Pipe (K : Pipe_Kind) is
   begin
      if pipes (K).Slot = 0 or else pipes (K).Started or else pipes (K).DCI = 0 or else
        pipes (K).Packet = 0
      then
         return;
      end if;
      pipes (K).Started := True;
      Fill_Pipe (K);
   end Start_Pipe;

   --  Assign pipe K on Slot from interface Item; Area picks its pages.
   procedure Assign_Pipe
     (K : Pipe_Kind; Slot : Device_Index; Item : USB_Configurations.Interrupt_Interface;
      Area : Ring_Area)
   is
   begin
      pipes (K) := (Slot => Slot,
                    DCI => Natural (Item.Input.Address and USB_ENDPOINT_NUMBER_MASK) * 2 + 1,
                    Packet => Natural'Min (Item.Input.Packet_Bytes, HID_REPORT_STRIDE),
                    Address => Item.Input.Address, Interface_Number => Item.Number,
                    Interval => Item.Input.Interval, Area => Area, others => <>);
      Init_Ring (Ring_Offset (pipes (K)));
   end Assign_Pipe;

   --  Add pipe K's endpoint context to Slot's input context (6.2.3).
   procedure Add_Pipe_Context (K : Pipe_Kind; S : XHCI_Topology.Speed) is
      P : Pipe_State renames pipes (K);
      Base : constant Natural := (P.DCI + 1) * contextStride;
      Ring : constant Unsigned_64 := Phys (Ring_Offset (P));
      Exponent : constant Unsigned_32 := Interrupt_Interval (S, P.Interval);
   begin
      deviceDMA (P.Slot).inputContext (Base) := Shift_Left (Exponent, EP_INTERVAL_SHIFT);
      deviceDMA (P.Slot).inputContext (Base + 1) :=
        Shift_Left (EP_CERR_RETRIES, EP_CERR_SHIFT) or
        Shift_Left (EP_TYPE_INTERRUPT_IN, EP_TYPE_SHIFT) or
        Shift_Left (Unsigned_32 (P.Packet), EP_PACKET_SHIFT);
      deviceDMA (P.Slot).inputContext (Base + 2) := Low (Ring) or TRB_CYCLE;
      deviceDMA (P.Slot).inputContext (Base + 3) := High (Ring);
      deviceDMA (P.Slot).inputContext (Base + 4) :=
        Unsigned_32 (P.Packet) or Shift_Left (Unsigned_32 (P.Packet), EP_ESIT_SHIFT);
      Log ("slot " & D (Natural (P.Slot)) & " " & Pipe_Name (K) & " ok if=" &
           D (Natural (P.Interface_Number)) & " ep=0x" & H8 (P.Address) & " mps=" &
           D (P.Packet) & " bInterval=" & D (Natural (P.Interval)) & " xhci-interval=" &
           D (Unsigned_64 (Exponent)) & " (" & D (Interval_Us (Exponent)) & " us)");
   end Add_Pipe_Context;

   --  Build the Configure Endpoint input context for the pipes on Slot.
   procedure Build_Pipe_Configure (Slot : Device_Index; S : XHCI_Topology.Speed) is
      Add : Unsigned_32 := 0;
      Last : Natural := 0;
   begin
      for K in Pipe_Kind loop
         if pipes (K).Slot = Slot then
            Add := Add or Shift_Left (1, pipes (K).DCI);
            Last := Natural'Max (Last, pipes (K).DCI);
         end if;
      end loop;
      Begin_Configure (Slot, Add, Last);
      for K in Pipe_Kind loop
         if pipes (K).Slot = Slot then
            Add_Pipe_Context (K, S);
         end if;
      end loop;
   end Build_Pipe_Configure;

   function Context_Halted (P : Pipe_State) return Boolean is
     (P.Slot in Device_Index and then
      State_Of (deviceDMA (P.Slot).deviceContext (P.DCI * contextStride)) = EP_HALTED);

   PORTPMSC_OFFSET : constant Storage_Offset := 4;   --  5.4.9, after PORTSC
   PORTPMSC_HLE    : constant Unsigned_32 := 2 ** 16;  --  USB2 hardware LPM enable
   PORTPMSC_U1_U2  : constant Unsigned_32 := 16#FFFF#; --  USB3 U1/U2 timeouts
   SLOT_STATE_SHIFT : constant := 27;
   EP_INTERVAL_MASK : constant Unsigned_32 := 16#FF#;

   --  The first halt of a pipe, once: why do transfers fail at all? The
   --  port's link (PLS, PED, change bits, speed), link power management
   --  (PORTPMSC: this driver never enables USB2 LPM or U1/U2, so HLE and
   --  the U1/U2 timeouts must read zero), the slot and endpoint context
   --  states, the interval the controller runs, and the quiet time.
   procedure Describe_Halt (K : Pipe_Kind; Code : Completion_Code) is
      P : Pipe_State renames pipes (K);
      Port : constant Natural := slotPort (P.Slot);
      PMSC : Unsigned_32 := 0;
      EP0 : constant Unsigned_32 := deviceDMA (P.Slot).deviceContext (P.DCI * contextStride);
   begin
      if Port in 1 .. maxPorts then
         PMSC := Read32 (operational, PORTSC (Port) + PORTPMSC_OFFSET);
         Log ("slot " & D (Natural (P.Slot)) & " " & Pipe_Name (K) & " first-halt cc=" &
              Code_Name (Code) & " port " & D (Port) & " " & Port_Text (Read_Port (Port)), True);
      end if;
      Log ("slot " & D (Natural (P.Slot)) & " " & Pipe_Name (K) & " first-halt portpmsc=" &
           H32 (PMSC) & " hle=" & (if (PMSC and PORTPMSC_HLE) /= 0 then "1" else "0") &
           " u1u2=" & H32 (PMSC and PORTPMSC_U1_U2) & " slot-state=" &
           D (Unsigned_64 (Shift_Right (deviceDMA (P.Slot).deviceContext (3), SLOT_STATE_SHIFT))) &
           " ep-state=" & D (Unsigned_64 (State_Of (EP0))) & " ep-interval=" &
           D (Unsigned_64 (Shift_Right (EP0, EP_INTERVAL_SHIFT) and EP_INTERVAL_MASK)) &
           " quiet-ms=" & (if P.Last_Good = 0 then "never" else D (PM.Since (Now_Ms, P.Last_Good))), True);
   end Describe_Halt;

   --  Take pipe K's next completion: what it means and, for data, the
   --  report buffer and length.
   procedure Take_Pipe_Event
     (K : Pipe_Kind; Found : out Boolean; Disposition : out ER.Disposition;
      Buffer : out Unsigned_64; Actual : out Natural; Code : out Completion_Code)
   is
      P : Pipe_State renames pipes (K);
      Item : TRB;
      Pointer, Base : Unsigned_64;
      Residual : Natural;
   begin
      Disposition := ER.Ignore;
      Buffer := 0;
      Actual := 0;
      Code := CC_INVALID;
      Found := False;
      if P.Slot = 0 or else not P.Started then
         return;
      end if;
      XHCI_Completions.Take (completionMailboxes, P.Slot, P.DCI, Item, Found);
      if not Found then
         return;
      end if;
      Code := Code_Of (Item.Status);
      Base := Phys (Ring_Offset (P));
      Pointer := (Unsigned_64 (Item.ParameterLo) or Shift_Left (Unsigned_64 (Item.ParameterHi), 32))
        and not Unsigned_64 (R.TRB_Bytes - 1);
      if P.Recovery.Current /= ER.Running then
         return;   --  queued behind the error; recovery restarts the ring
      end if;
      Disposition := ER.Classify (Code, Context_Halted (P));
      if Disposition = ER.Deliver then
         if R.Names_TRB (Base, Pointer) then
            Buffer := Report_Offset (P) + Unsigned_64 (R.Index_Of (Base, Pointer)) * HID_REPORT_STRIDE;
            Residual := Natural (Residual_Of (Item.Status));
            Actual := (if Residual <= P.Packet then P.Packet - Residual else 0);
         else
            Disposition := ER.Replace;   --  unknown TRB: replace it, no data
         end if;
      end if;
      case Disposition is
         when ER.Deliver | ER.Replace =>
            if Disposition = ER.Deliver then
               P.Last_Good := Now_Ms;
            end if;
            --  Replenish before decoding so the controller always has work.
            Queue_Pipe (K);
            Ring_Pipe (K);
            if Disposition = ER.Replace and then Code /= CC_SUCCESS then
               Count_Error (Code);
            end if;
         when ER.Recover =>
            Count_Error (Code);
            if not P.Described then
               P.Described := True;
               Describe_Halt (K, Code);
            end if;
            ER.Halted (P.Recovery, Code);
            Log ("slot " & D (Natural (P.Slot)) & " " & Pipe_Name (K) & " halted cc=" &
                 Code_Name (Code) & " next=" &
                 (case P.Recovery.Current is
                     when ER.Need_Soft_Reset => "soft-retry " & D (P.Recovery.Soft) & "/" & D (Natural (ER.Soft_Limit)),
                     when ER.Need_Reset => "hard-reset " & D (P.Recovery.Hard) & "/" & D (Natural (ER.Hard_Limit)),
                     when others => "port-reset"), True);
         when ER.Ignore =>
            null;
      end case;
   end Take_Pipe_Event;

   --  Drive one pipe's halted-endpoint recovery (XHCI_Endpoint_Recovery).
   procedure Run_Recovery (K : Pipe_Kind) is
      P : Pipe_State renames pipes (K);
   begin
      if P.Slot = 0 or else not P.Started then
         return;
      end if;
      case P.Recovery.Current is
         when ER.Running | ER.Need_Port_Reset => null;   --  Run_Port_Recovery
         when ER.Need_Soft_Reset =>
            if Command_Free then
               --  4.6.8.1, TSP=1: keep the toggle and the queued transfer.
               Issue_Command (Command_TRB (TRB_RESET_ENDPOINT, P.Slot,
                                Extra => Endpoint_Field (Unsigned_32 (P.DCI)) or TRB_TSP),
                              Owner_Of (K));
               ER.Issued (P.Recovery);
            end if;
         when ER.Need_Reset =>
            if Command_Free then
               --  4.6.8, TSP=0: Halted -> Stopped, host toggle DATA0.
               Issue_Command (Command_TRB (TRB_RESET_ENDPOINT, P.Slot,
                                Extra => Endpoint_Field (Unsigned_32 (P.DCI))), Owner_Of (K));
               ER.Issued (P.Recovery);
            end if;
         when ER.Need_Dequeue =>
            if Command_Free then
               --  4.6.10: restart at the next enqueue position.
               Issue_Command (Command_TRB (TRB_SET_TR_DEQUEUE, P.Slot,
                                R.Restart_Pointer (Phys (Ring_Offset (P)), P.Ring),
                                Endpoint_Field (Unsigned_32 (P.DCI))), Owner_Of (K));
               ER.Issued (P.Recovery);
            end if;
         when ER.Await_Soft_Reset | ER.Await_Reset | ER.Await_Dequeue =>
            if Command_Done_For (Owner_Of (K)) then
               ER.Completed (P.Recovery, command.Code);
               Release_Command;
            elsif command.Busy and then command.Owner /= Owner_Of (K) then
               ER.Escalate (P.Recovery);   --  lost its command: cannot happen
            end if;
         when ER.Need_Clear_Halt =>
            if not ep0State (P.Slot).Busy and then not ep0State (P.Slot).Failed then
               --  USB 2.0 9.4.5: clears the device's halt and data toggle.
               Start_Control (P.Control, P.Slot, TYPE_ENDPOINT_OUT, REQUEST_CLEAR_FEATURE,
                              FEATURE_ENDPOINT_HALT, Unsigned_16 (P.Address), 0, 0);
               ER.Issued (P.Recovery);
            elsif ep0State (P.Slot).Failed then
               ER.Escalate (P.Recovery);   --  EP0 quarantined: replug
            end if;
         when ER.Await_Clear_Halt =>
            Poll_Control (P.Control);
            if not P.Control.Active then
               ER.Completed (P.Recovery, P.Control.Code);
            end if;
         when ER.Need_Doorbell =>
            Ring_Pipe (K);
            ER.Rang (P.Recovery);
            health.Recoveries := health.Recoveries + 1;
            Log ("slot " & D (Natural (P.Slot)) & " " & Pipe_Name (K) & " soft-retry " &
                 D (P.Recovery.Soft) & " done");
         when ER.Need_Requeue =>
            Fill_Pipe (K);
            ER.Rang (P.Recovery);
            health.Recoveries := health.Recoveries + 1;
            Log ("slot " & D (Natural (P.Slot)) & " " & Pipe_Name (K) & " hard-reset " &
                 D (P.Recovery.Hard) & " done clear-halt cc=" & Code_Name (P.Control.Code));
      end case;
   end Run_Recovery;

   ---------------------------------------------------------------------------
   --  Bulk storage (the optical drive)
   ---------------------------------------------------------------------------
   storageSlot : Slot_Or_None := 0;
   storageInDCI, storageOutDCI : Natural := 0;
   storageInterface : Unsigned_8 := 0;
   type Bulk_Phase is (Bulk_Idle, Command_Out, Data_In, Status_In, Bulk_Finished, Bulk_Faulted);
   bulkPhase : Bulk_Phase := Bulk_Idle;
   bulkIn, bulkOut : R.Producer := R.Fresh_Producer;
   bulkExpectedPointer : Unsigned_64 := 0;
   bulkExpectedLength : Unsigned_32 := 0;
   bulkDeadline : Unsigned_64 := 0;
   bulkTag : Unsigned_32 := 0;
   bulkCommand : USB_Optical.Command;
   bulkReceived : Unsigned_32 := 0;
   bulkResult : USB_Optical.Status_Result := USB_Optical.Invalid_Status;
   opticalLUN : USB_Optical.Logical_Unit := 0;
   opticalBlocks : Unsigned_64 := 0;
   storageFormat : USB_Optical.Sector_Format := USB_Optical.Sector_2048;
   CBW_BYTES : constant := 31;
   CSW_BYTES : constant := 13;
   bulkData : USB_Optical.Bytes (1 .. 32768) with Import, Volatile,
     Address => To_Address (Integer_Address (DMA_VIRT_BASE + BULK_DATA_OFFSET));
   bulkCBW : USB_Optical.Command_Wrapper with Import, Volatile,
     Address => To_Address (Integer_Address (DMA_VIRT_BASE + BULK_CBW_OFFSET));
   bulkCSW : USB_Optical.Bytes (1 .. CSW_BYTES) with Import, Volatile,
     Address => To_Address (Integer_Address (DMA_VIRT_BASE + BULK_CSW_OFFSET));

   procedure Queue_Bulk (Input : Boolean; Offset : Unsigned_64; Length : Unsigned_32) is
      Ring : constant Unsigned_64 :=
        Device_Offset (storageSlot, (if Input then BULK_IN_RING_OFFSET else BULK_OUT_RING_OFFSET));
      Item : constant TRB :=
        (ParameterLo => Low (Phys (Offset)), ParameterHi => High (Phys (Offset)),
         Status => Length,
         Control => Type_Field (TRB_NORMAL) or TRB_IOC or (if Input then TRB_ISP else 0));
   begin
      bulkExpectedLength := Length;
      bulkDeadline := PM.Plus (Now_Ms, Bulk_Timeout_Ms);
      if Input then
         Enqueue (Ring, bulkIn, Item, bulkExpectedPointer);
      else
         Enqueue (Ring, bulkOut, Item, bulkExpectedPointer);
      end if;
      Ring_Doorbell (storageSlot, Unsigned_32 (if Input then storageInDCI else storageOutDCI));
   end Queue_Bulk;

   procedure Begin_Bulk
     (Item : USB_Optical.Command; LUN : USB_Optical.Logical_Unit; Accepted : out Boolean) is
   begin
      Accepted := False;
      if storageSlot = 0 or else
         bulkPhase not in Bulk_Idle | Bulk_Finished or else bulkTag = Unsigned_32'Last
      then return; end if;
      bulkTag := bulkTag + 1;
      bulkCommand := Item;
      bulkReceived := 0;
      bulkResult := USB_Optical.Invalid_Status;
      bulkCBW := USB_Optical.Encode (Item, bulkTag, LUN);
      bulkPhase := Command_Out;
      Queue_Bulk (False, BULK_CBW_OFFSET, CBW_BYTES);
      Accepted := True;
   end Begin_Bulk;

   procedure Poll_Optical_Read
     (Done : out Boolean; Result : out USB_Optical.Status_Result; Progressed : out Boolean)
   is
      Item : TRB;
      Found : Boolean;
      Pointer : Unsigned_64;
      Residual, Actual : Unsigned_32;
      Code : Completion_Code;
      use type USB_Optical.Status_Result;
      procedure Fault is
      begin
         --  No caller memory is exposed to this DMA. Retain all private DMA
         --  buffers and forbid further submissions.
         bulkPhase := Bulk_Faulted;
         opticalBlocks := 0;
         bulkResult := USB_Optical.Reset_Recovery_Required;
         Log ("storage transport quarantined cc=" & Code_Name (Code), True);
      end Fault;
   begin
      Done := False;
      Progressed := False;
      Result := bulkResult;
      Code := CC_INVALID;
      if bulkPhase in Command_Out | Data_In | Status_In then
         if storageSlot = 0 then
            Fault;
         else
            XHCI_Completions.Take (completionMailboxes, storageSlot,
              (if bulkPhase = Command_Out then storageOutDCI else storageInDCI), Item, Found);
            if not hotplugArmed then
               Progressed := Collect_Event or else Found;
            else
               Progressed := Found;
            end if;
            if Found then
               Pointer := Unsigned_64 (Item.ParameterLo) or Shift_Left (Unsigned_64 (Item.ParameterHi), 32);
               Residual := Residual_Of (Item.Status);
               Code := Code_Of (Item.Status);
               if Pointer /= bulkExpectedPointer or else (Item.Control and TRB_EVENT_DATA) /= 0
                 or else Residual > bulkExpectedLength
                 or else (Code /= CC_SUCCESS and then Code /= CC_SHORT_PACKET)
               then
                  Fault;
               else
                  Actual := bulkExpectedLength - Residual;
                  case bulkPhase is
                     when Command_Out =>
                        if Actual /= CBW_BYTES then Fault;
                        elsif USB_Optical.Transfer_Bytes (bulkCommand) = 0 then
                           bulkPhase := Status_In;
                           Queue_Bulk (True, BULK_CSW_OFFSET, CSW_BYTES);
                        else
                           bulkPhase := Data_In;
                           Queue_Bulk (True, BULK_DATA_OFFSET, USB_Optical.Transfer_Bytes (bulkCommand));
                        end if;
                     when Data_In =>
                        bulkReceived := Actual;
                        bulkPhase := Status_In;
                        Queue_Bulk (True, BULK_CSW_OFFSET, CSW_BYTES);
                     when Status_In =>
                        if Actual /= CSW_BYTES then Fault;
                        else
                           declare
                              Snapshot : constant USB_Optical.Bytes (1 .. CSW_BYTES) := bulkCSW;
                           begin
                              bulkResult := USB_Optical.Decode_Status
                                (Snapshot, bulkTag, USB_Optical.Transfer_Bytes (bulkCommand), bulkReceived);
                           end;
                           if bulkResult in USB_Optical.Invalid_Status | USB_Optical.Reset_Recovery_Required then
                              Fault;
                           elsif opticalBlocks /= 0 and then bulkResult /= USB_Optical.Command_Passed then
                              --  Once mounted, a failed READ invalidates the
                              --  media session; never reuse cached extents
                              --  against a replacement disc.
                              Fault;
                           else
                              bulkPhase := Bulk_Finished;
                           end if;
                        end if;
                     when others => null;
                  end case;
               end if;
            elsif Now_Ms >= bulkDeadline then
               Log ("storage transfer timeout ms=" & D (Bulk_Timeout_Ms), True);
               Fault;
            end if;
         end if;
      end if;
      Done := bulkPhase in Bulk_Finished | Bulk_Faulted;
      Result := bulkResult;
   end Poll_Optical_Read;

   ---------------------------------------------------------------------------
   --  Slot release (hotplug detach): Disable Slot (4.6.4) then forget it.
   ---------------------------------------------------------------------------
   disablingSlot : Slot_Or_None := 0;

   --  Stop using Slot now; Run_Releases disables it in the controller.
   procedure Retire_Slot (Slot : Device_Index) is
   begin
      slotRetired (Slot) := True;
      XHCI_Completions.Clear_Slot (completionMailboxes, Slot);
      for K in Pipe_Kind loop
         if pipes (K).Slot = Slot then
            if command.Busy and then command.Owner = Owner_Of (K) then
               command.Owner := Orphan_Owner;
            end if;
            pipes (K) := (others => <>);
         end if;
      end loop;
      if storageSlot = Slot then
         storageSlot := 0;
         if bulkPhase in Command_Out | Data_In | Status_In then
            bulkResult := USB_Optical.Reset_Recovery_Required;
         end if;
         bulkPhase := Bulk_Faulted;
         opticalBlocks := 0;
      end if;
   end Retire_Slot;

   procedure Free_Slot (Slot : Device_Index) is
   begin
      dcbaa (Slot) := 0;
      ep0State (Slot) := (others => <>);
      slotPort (Slot) := 0;
      slotRetired (Slot) := False;
      XHCI_Completions.Clear_Slot (completionMailboxes, Slot);
   end Free_Slot;

   function Slots_Of (Port : Root_Port_Number) return Natural is
      Count : Natural := 0;
   begin
      for S in Device_Index loop
         if slotPort (S) = Port then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Slots_Of;

   ---------------------------------------------------------------------------
   --  Boot enumeration (bounded; before devmgr's configure reply)
   ---------------------------------------------------------------------------
   procedure Disable_Slot_Now (Slot : Device_Index) is
      Code : Completion_Code;
      Ignore_Slot : Natural;
      Done : Boolean;
   begin
      Retire_Slot (Slot);
      Submit_Command (Command_TRB (TRB_DISABLE_SLOT, Slot), Code, Ignore_Slot, Done);
      Log ("slot " & D (Natural (Slot)) & " disable " & (if Done then "ok" else "fail") &
           " cc=" & Code_Name (Code), not Done);
      Free_Slot (Slot);
   end Disable_Slot_Now;

   procedure Enumerate_Device (Path : XHCI_Topology.Path; Result : out Init_Result);

   --  Configure a hub at Slot and enumerate its connected children (boot).
   procedure Enumerate_Hub
     (Slot : Device_Index; Path : XHCI_Topology.Path;
      Configuration : USB_Configurations.Configuration; Result : out Init_Result)
   is
      use type USB_Hubs.Decode_Result;
      use type USB_Hubs.Power_Mode;
      use type USB_Hubs.Port_Action;
      use type XHCI_Topology.Attach_Result;
      Hub : USB_Hubs.Descriptor;
      Decoded : USB_Hubs.Decode_Result;
      Status : Unsigned_16 := 0;
      Actual : Natural;
      OK : Boolean;
      Code : Completion_Code;
      Ignore_Slot : Natural;
      Done : Boolean;
      Child_Path : XHCI_Topology.Path;
      Attached : XHCI_Topology.Attach_Result;
      Child_Result : Init_Result;
      Ignore : Unsigned_64;
      Stride : constant Natural := contextStride;
      function Read_Hub_Port (Port : Positive) return Boolean is
      begin
         Control_Request (Slot, TYPE_CLASS_OTHER_IN, REQUEST_GET_STATUS, 0, Unsigned_16 (Port),
                          HUB_PORT_STATUS_BYTES, Descriptor_Buffer (Slot), Actual, OK);
         if not OK or else Actual /= HUB_PORT_STATUS_BYTES then return False; end if;
         Status := Unsigned_16 (deviceDMA (Slot).descriptorBytes (0)) or
           Shift_Left (Unsigned_16 (deviceDMA (Slot).descriptorBytes (1)), 8);
         return True;
      end Read_Hub_Port;
   begin
      Control_Request (Slot, TYPE_DEVICE_OUT, REQUEST_SET_CONFIGURATION,
                       Unsigned_16 (Configuration.Value), 0, 0, 0, Actual, OK);
      if not OK then Result := INIT_CONFIGURE_FAILED; return; end if;
      Control_Request (Slot, TYPE_CLASS_DEVICE_IN, REQUEST_GET_DESCRIPTOR, DESCRIPTOR_HUB, 0,
                       HUB_DESCRIPTOR_MAXIMUM, Descriptor_Buffer (Slot), Actual, OK);
      if not OK or else Actual not in HUB_DESCRIPTOR_MINIMUM .. HUB_DESCRIPTOR_MAXIMUM then
         Result := INIT_DESCRIPTOR_FAILED; return;
      end if;
      declare
         Data : USB_Hubs.Bytes (1 .. Actual);
      begin
         for I in Data'Range loop
            Data (I) := deviceDMA (Slot).descriptorBytes (I - 1);
         end loop;
         USB_Hubs.Decode (Data, Hub, Decoded);
      end;
      if Decoded /= USB_Hubs.Decoded or else Configuration.Hub.Protocol = HUB_PROTOCOL_MULTI_TT then
         Log ("slot " & D (Natural (Slot)) & " hub fail unsupported descriptor bytes=" & D (Actual) &
              " protocol=" & D (Natural (Configuration.Hub.Protocol)), True);
         Result := INIT_DESCRIPTOR_FAILED; return;
      end if;
      --  Tell the controller this slot routes downstream traffic (6.2.2).
      Begin_Configure (Slot, 0, 0);
      deviceDMA (Slot).inputContext (1) := ADD_SLOT;
      deviceDMA (Slot).inputContext (Stride) := deviceDMA (Slot).deviceContext (0) or SLOT_HUB;
      deviceDMA (Slot).inputContext (Stride + 1) :=
        (deviceDMA (Slot).deviceContext (1) and LOW_24_BITS) or
        Shift_Left (Unsigned_32 (Hub.Ports), SLOT_PORTS_SHIFT);
      if XHCI_Topology.Device_Speed (Path) = XHCI_Topology.High_Speed then
         deviceDMA (Slot).inputContext (Stride + 2) := deviceDMA (Slot).deviceContext (2) or
           Shift_Left (Unsigned_32 (Hub.TT_Think_Time), SLOT_TT_THINK_SHIFT);
      end if;
      Submit_Command (Input_Command (TRB_CONFIGURE_ENDPOINT, Slot), Code, Ignore_Slot, Done);
      if not Done or else Code /= CC_SUCCESS then Result := INIT_CONFIGURE_FAILED; return; end if;
      Log ("slot " & D (Natural (Slot)) & " hub ok ports=" & D (Hub.Ports));
      if Hub.Power /= USB_Hubs.Always_On then
         for Port in 1 .. Hub.Ports loop
            Control_Request (Slot, TYPE_CLASS_OTHER_OUT, REQUEST_SET_FEATURE, HUB_PORT_POWER,
                             Unsigned_16 (Port), 0, 0, Actual, OK);
            if not OK then Result := INIT_CONFIGURE_FAILED; return; end if;
         end loop;
      end if;
      Ignore := syscall (SYSCALL_SLEEP, Unsigned_64 (Natural'Max (Hub.Power_Delay_MS, Hub_Power_Good_Ms)));
      for Port in 1 .. Hub.Ports loop
         if not Read_Hub_Port (Port) then Result := INIT_DESCRIPTOR_FAILED; return; end if;
         if USB_Hubs.Action (Status) = USB_Hubs.Reset_Required then
            Control_Request (Slot, TYPE_CLASS_OTHER_OUT, REQUEST_SET_FEATURE, HUB_PORT_RESET,
                             Unsigned_16 (Port), 0, 0, Actual, OK);
            if not OK then Result := INIT_CONFIGURE_FAILED; return; end if;
            for Attempt in 1 .. Hub_Reset_Polls loop
               Ignore := syscall (SYSCALL_SLEEP, Hub_Reset_Poll_Ms);
               if not Read_Hub_Port (Port) then Result := INIT_DESCRIPTOR_FAILED; return; end if;
               exit when USB_Hubs.Action (Status) not in USB_Hubs.Resetting | USB_Hubs.Reset_Required;
            end loop;
         end if;
         if USB_Hubs.Action (Status) /= USB_Hubs.Disconnected then
            Log ("slot " & D (Natural (Slot)) & " hub port " & D (Port) & " status=0x" &
                 H16 (Status) & " " & "action=" & D (Natural (USB_Hubs.Port_Action'Pos (USB_Hubs.Action (Status)))));
         end if;
         if USB_Hubs.Action (Status) = USB_Hubs.Ready then
            Control_Request (Slot, TYPE_CLASS_OTHER_OUT, REQUEST_CLEAR_FEATURE, HUB_C_PORT_RESET,
                             Unsigned_16 (Port), 0, 0, Actual, OK);
            if not OK then Result := INIT_CONFIGURE_FAILED; return; end if;
            Ignore := syscall (SYSCALL_SLEEP, PM.Reset_Recovery_Ms);
            XHCI_Topology.Child (Path, Slot, Port, USB_Hubs.Rate (Status), Child_Path, Attached);
            if Attached /= XHCI_Topology.Attached then
               Result := INIT_ADDRESS_FAILED; return;
            end if;
            --  Address this child before resetting any sibling.
            Enumerate_Device (Child_Path, Child_Result);
            if Child_Result in INIT_COMMAND_TIMEOUT | INIT_COMMAND_FAILED then
               Result := Child_Result; return;
            end if;
         end if;
      end loop;
      Result := INIT_OK;
   end Enumerate_Hub;

   procedure Configure_Storage
     (Slot : Device_Index; S : XHCI_Topology.Speed;
      Configuration : USB_Configurations.Configuration; Result : out Init_Result)
   is
      Storage : constant USB_Configurations.Storage_Interface := Configuration.Storage;
      In_DCI : constant Natural := Natural (Storage.Input.Address and USB_ENDPOINT_NUMBER_MASK) * 2 + 1;
      Out_DCI : constant Natural := Natural (Storage.Output.Address and USB_ENDPOINT_NUMBER_MASK) * 2;
      Actual : Natural;
      OK : Boolean;
      Code : Completion_Code;
      Ignore_Slot : Natural;
      Done : Boolean;
      Super_Packet : constant := 1024;
      High_Packet : constant := 512;
      procedure Add_Endpoint (DCI : Natural; EP : USB_Configurations.Endpoint;
                              EP_Type : Unsigned_32; Ring_Page : Unsigned_64) is
         Base : constant Natural := (DCI + 1) * contextStride;
         Ring : constant Unsigned_64 := Phys (Device_Offset (Slot, Ring_Page));
      begin
         Init_Ring (Device_Offset (Slot, Ring_Page));
         deviceDMA (Slot).inputContext (Base + 1) :=
           Shift_Left (EP_CERR_RETRIES, EP_CERR_SHIFT) or Shift_Left (EP_Type, EP_TYPE_SHIFT) or
           Shift_Left (Unsigned_32 (EP.Packet_Bytes), EP_PACKET_SHIFT);
         deviceDMA (Slot).inputContext (Base + 2) := Low (Ring) or TRB_CYCLE;
         deviceDMA (Slot).inputContext (Base + 3) := High (Ring);
         deviceDMA (Slot).inputContext (Base + 4) := BULK_AVERAGE_TRB;
      end Add_Endpoint;
   begin
      if S not in XHCI_Topology.High_Speed | XHCI_Topology.Super_Speed or else
        Storage.Input.Packet_Bytes /= (if S = XHCI_Topology.High_Speed then High_Packet else Super_Packet)
        or else Storage.Output.Packet_Bytes /= Storage.Input.Packet_Bytes
      then
         Result := INIT_CONFIGURE_FAILED; return;
      end if;
      Control_Request (Slot, TYPE_DEVICE_OUT, REQUEST_SET_CONFIGURATION,
                       Unsigned_16 (Configuration.Value), 0, 0, 0, Actual, OK);
      if not OK then Result := INIT_CONFIGURE_FAILED; return; end if;
      Begin_Configure (Slot, Shift_Left (1, In_DCI) or Shift_Left (1, Out_DCI), Natural'Max (In_DCI, Out_DCI));
      Add_Endpoint (In_DCI, Storage.Input, EP_TYPE_BULK_IN, BULK_IN_RING_OFFSET);
      Add_Endpoint (Out_DCI, Storage.Output, EP_TYPE_BULK_OUT, BULK_OUT_RING_OFFSET);
      bulkIn := R.Fresh_Producer;
      bulkOut := R.Fresh_Producer;
      Submit_Command (Input_Command (TRB_CONFIGURE_ENDPOINT, Slot), Code, Ignore_Slot, Done);
      if not Done or else Code /= CC_SUCCESS then
         Log ("slot " & D (Natural (Slot)) & " configure-ep fail cc=" & Code_Name (Code), True);
         Result := INIT_CONFIGURE_FAILED; return;
      end if;
      storageSlot := Slot;
      storageInDCI := In_DCI;
      storageOutDCI := Out_DCI;
      storageInterface := Storage.Number;
      bulkPhase := Bulk_Idle;
      Log ("slot " & D (Natural (Slot)) & " storage ok if=" & D (Natural (Storage.Number)) &
           " in=0x" & H8 (Storage.Input.Address) & " out=0x" & H8 (Storage.Output.Address));
      Result := INIT_OK;
   end Configure_Storage;

   procedure Configure_HID
     (Slot : Device_Index; S : XHCI_Topology.Speed;
      Configuration : USB_Configurations.Configuration;
      Roles : USB_Device_Roles.Selection; Result : out Init_Result)
   is
      Actual : Natural;
      OK : Boolean;
      Code : Completion_Code;
      Ignore_Slot : Natural;
      Done : Boolean;
   begin
      Control_Request (Slot, TYPE_DEVICE_OUT, REQUEST_SET_CONFIGURATION,
                       Unsigned_16 (Configuration.Value), 0, 0, 0, Actual, OK);
      if not OK then Result := INIT_CONFIGURE_FAILED; return; end if;
      if Roles.Mouse then
         Assign_Pipe (Mouse_Pipe, Slot, Configuration.Mouse, Primary_Area);
      end if;
      if Roles.Keyboard then
         Assign_Pipe (Keyboard_Pipe, Slot, Configuration.Keyboard,
                      (if Roles.Mouse then Secondary_Area else Primary_Area));
      end if;
      Build_Pipe_Configure (Slot, S);
      Submit_Command (Input_Command (TRB_CONFIGURE_ENDPOINT, Slot), Code, Ignore_Slot, Done);
      if not Done or else Code /= CC_SUCCESS then
         Log ("slot " & D (Natural (Slot)) & " configure-ep fail cc=" & Code_Name (Code), True);
         Result := INIT_CONFIGURE_FAILED; return;
      end if;
      --  The fixed boot reports, independent of report descriptors.
      for K in Pipe_Kind loop
         if pipes (K).Slot = Slot then
            Control_Request (Slot, TYPE_CLASS_INTERFACE_OUT, HID_SET_PROTOCOL, HID_BOOT_PROTOCOL,
                             Unsigned_16 (pipes (K).Interface_Number), 0, 0, Actual, OK);
            if not OK then
               Log ("slot " & D (Natural (Slot)) & " set-protocol fail " & Pipe_Name (K) &
                    " cc=" & Code_Name (lastControlCode), True);
               Result := INIT_CONFIGURE_FAILED; return;
            end if;
         end if;
      end loop;
      Log ("slot " & D (Natural (Slot)) & " configured roles=" &
           (if Roles.Mouse then "mouse" else "") & (if Roles.Mouse and Roles.Keyboard then "+" else "") &
           (if Roles.Keyboard then "keyboard" else ""));
      Result := INIT_OK;
   end Configure_HID;

   procedure Enumerate_Device (Path : XHCI_Topology.Path; Result : out Init_Result) is
      Code : Completion_Code;
      Slot_Id : Natural;
      Done : Boolean;
      Slot : Device_Index;
      S : constant XHCI_Topology.Speed := XHCI_Topology.Device_Speed (Path);
      Actual, Total : Natural;
      OK : Boolean;
      Ignore : Unsigned_64;
      Configuration : USB_Configurations.Configuration;
      Decoded : USB_Configurations.Decode_Result;
      Roles : USB_Device_Roles.Selection;
      use type USB_Configurations.Decode_Result;
      procedure Fail (Why : Init_Result; Text : String) is
      begin
         Log ("slot " & D (Natural (Slot)) & " " & Text, True);
         Result := Why;
      end Fail;
   begin
      Submit_Command (Command_TRB (TRB_ENABLE_SLOT, 0), Code, Slot_Id, Done);
      if not Done then
         Stage ("enable-slot", False, "timeout usbsts=" & H32 (Read32 (operational, OP_USBSTS)) &
                " crcr=" & H32 (Read32 (operational, OP_CRCR)));
         Result := INIT_COMMAND_TIMEOUT; return;
      elsif Code /= CC_SUCCESS or else Slot_Id not in Device_Index then
         Stage ("enable-slot", False, "cc=" & Code_Name (Code) & " slot=" & D (Slot_Id));
         Result := INIT_COMMAND_FAILED; return;
      end if;
      Slot := Slot_Id;
      activeSlot := Slot;
      slotPort (Slot) := XHCI_Topology.Root_Port (Path);
      Clear_Slot_DMA (Slot);
      Log ("slot " & D (Natural (Slot)) & " enable ok port=" & D (Natural (slotPort (Slot))) &
           " route=" & H32 (XHCI_Topology.Route_String (Path)));
      Build_Address_Input (Slot, Path);
      Submit_Command (Address_Device_TRB (Slot), Code, Slot_Id, Done);
      if not Done or else Code /= CC_SUCCESS then
         Fail (INIT_ADDRESS_FAILED, "address fail cc=" & Code_Name (Code));
         Disable_Slot_Now (Slot);
         return;
      end if;
      Ignore := syscall (SYSCALL_SLEEP, SetAddress_Recovery_Ms);
      --  Learn EP0's packet size before a multi-packet descriptor read.
      Control_Request (Slot, TYPE_DEVICE_IN, REQUEST_GET_DESCRIPTOR, DESCRIPTOR_DEVICE, 0,
                       EP0_PROBE_BYTES, Descriptor_Buffer (Slot), Actual, OK);
      if not OK or else Actual /= EP0_PROBE_BYTES or else
        deviceDMA (Slot).descriptorBytes (0) /= DEVICE_DESCRIPTOR_BYTES or else
        deviceDMA (Slot).descriptorBytes (1) /= DEVICE_DESCRIPTOR_TYPE
      then
         Fail (INIT_DESCRIPTOR_FAILED, "device8 fail cc=" & Code_Name (lastControlCode) &
               " len=" & D (Actual));
         Disable_Slot_Now (Slot);
         return;
      end if;
      declare
         Packet : constant Unsigned_32 := EP0_Packet (S, deviceDMA (Slot).descriptorBytes (7));
      begin
         Log ("slot " & D (Natural (Slot)) & " address ok addr=" &
              D (Unsigned_64 (deviceDMA (Slot).deviceContext (3) and 16#FF#)) & " mps0=" &
              D (Unsigned_64 (Packet)));
         if Packet = 0 then
            Fail (INIT_DESCRIPTOR_FAILED, "device8 fail mps0-code=" &
                  D (Natural (deviceDMA (Slot).descriptorBytes (7))));
            Disable_Slot_Now (Slot);
            return;
         elsif Packet /= Default_Packet (S) then
            Build_Evaluate_Packet (Slot, Packet);
            Submit_Command (Input_Command (TRB_EVALUATE_CONTEXT, Slot), Code, Slot_Id, Done);
            if not Done or else Code /= CC_SUCCESS then
               Fail (INIT_CONFIGURE_FAILED, "evaluate fail cc=" & Code_Name (Code));
               Disable_Slot_Now (Slot);
               return;
            end if;
         end if;
      end;
      Control_Request (Slot, TYPE_DEVICE_IN, REQUEST_GET_DESCRIPTOR, DESCRIPTOR_DEVICE, 0,
                       DEVICE_DESCRIPTOR_BYTES, Descriptor_Buffer (Slot), Actual, OK);
      if not OK or else Actual < DEVICE_DESCRIPTOR_BYTES then
         Fail (INIT_DESCRIPTOR_FAILED, "device fail cc=" & Code_Name (lastControlCode));
         Disable_Slot_Now (Slot);
         return;
      end if;
      Log ("slot " & D (Natural (Slot)) & " device ok id=" &
           H8 (deviceDMA (Slot).descriptorBytes (9)) & H8 (deviceDMA (Slot).descriptorBytes (8)) & ":" &
           H8 (deviceDMA (Slot).descriptorBytes (11)) & H8 (deviceDMA (Slot).descriptorBytes (10)) &
           " class=" & H8 (deviceDMA (Slot).descriptorBytes (4)) & " usb=" &
           H8 (deviceDMA (Slot).descriptorBytes (3)) & H8 (deviceDMA (Slot).descriptorBytes (2)));
      Control_Request (Slot, TYPE_DEVICE_IN, REQUEST_GET_DESCRIPTOR, DESCRIPTOR_CONFIGURATION, 0,
                       CONFIG_HEADER_BYTES, Descriptor_Buffer (Slot), Actual, OK);
      if not OK or else Actual < CONFIG_HEADER_BYTES or else
        deviceDMA (Slot).descriptorBytes (1) /= CONFIG_DESCRIPTOR_TYPE
      then
         Fail (INIT_DESCRIPTOR_FAILED, "config fail cc=" & Code_Name (lastControlCode));
         Disable_Slot_Now (Slot);
         return;
      end if;
      Total := Natural (deviceDMA (Slot).descriptorBytes (2)) +
        Natural (deviceDMA (Slot).descriptorBytes (3)) * 256;
      if Total < CONFIG_HEADER_BYTES or else Total > USB_Configurations.Maximum_Descriptor_Bytes then
         Fail (INIT_DESCRIPTOR_FAILED, "config fail total=" & D (Total));
         Disable_Slot_Now (Slot);
         return;
      end if;
      Control_Request (Slot, TYPE_DEVICE_IN, REQUEST_GET_DESCRIPTOR, DESCRIPTOR_CONFIGURATION, 0,
                       Total, Descriptor_Buffer (Slot), Actual, OK);
      if not OK or else Actual < Total then
         Fail (INIT_DESCRIPTOR_FAILED, "config fail cc=" & Code_Name (lastControlCode) &
               " len=" & D (Actual) & "/" & D (Total));
         Disable_Slot_Now (Slot);
         return;
      end if;
      declare
         Data : USB_Configurations.Bytes (1 .. Total);
      begin
         for I in Data'Range loop
            Data (I) := deviceDMA (Slot).descriptorBytes (I - 1);
         end loop;
         USB_Configurations.Decode (Data, Configuration, Decoded);
      end;
      if Decoded /= USB_Configurations.Decoded then
         Fail (INIT_DESCRIPTOR_FAILED, "config fail malformed len=" & D (Total));
         Disable_Slot_Now (Slot);
         return;
      end if;
      Roles := USB_Device_Roles.Select_Roles
        (Configuration, Mouse_Free => pipes (Mouse_Pipe).Slot = 0,
         Keyboard_Free => pipes (Keyboard_Pipe).Slot = 0, Storage_Free => storageSlot = 0);
      Log ("slot " & D (Natural (Slot)) & " config ok len=" & D (Total) & " mouse-if=" &
           (if Configuration.Mouse.Present then "1" else "0") & " keyboard-if=" &
           (if Configuration.Keyboard.Present then "1" else "0") & " storage-if=" &
           (if Configuration.Storage.Present then "1" else "0") & " hub=" &
           (if Configuration.Hub.Present then "1" else "0") & " role=" &
           USB_Device_Roles.Role_Name (Roles.Kind));
      case Roles.Kind is
         when USB_Device_Roles.Hub_Role =>
            Enumerate_Hub (Slot, Path, Configuration, Result);
         when USB_Device_Roles.Storage_Role =>
            Configure_Storage (Slot, S, Configuration, Result);
         when USB_Device_Roles.HID_Role =>
            Configure_HID (Slot, S, Configuration, Roles, Result);
         when USB_Device_Roles.Unsupported =>
            Result := INIT_NOT_BOOT_MOUSE;
      end case;
      if Result /= INIT_OK and then Result not in INIT_COMMAND_TIMEOUT | INIT_COMMAND_FAILED then
         for K in Pipe_Kind loop
            if pipes (K).Slot = Slot then
               pipes (K) := (others => <>);
            end if;
         end loop;
         Disable_Slot_Now (Slot);
      end if;
   end Enumerate_Device;

   procedure Probe_Optical is
      Actual : Natural;
      OK, Valid : Boolean;
      Last_LUN : USB_Optical.Logical_Unit;
      Outcome : USB_Optical.Control_Result;
      Count : Unsigned_64 := 0;
      Capacity : USB_Optical.Capacity_Result;
      Format : USB_Optical.Sector_Format;
      use type USB_Optical.Status_Result;
      use type USB_Optical.Capacity_Result;
      function Run (Kind : USB_Optical.Probe_Kind; LUN : USB_Optical.Logical_Unit) return Boolean is
         Accepted, Done, Progressed : Boolean;
         Result : USB_Optical.Status_Result;
         Ignore : Unsigned_64;
      begin
         Begin_Bulk (USB_Optical.Probe (Kind), LUN, Accepted);
         if not Accepted then return False; end if;
         loop
            Poll_Optical_Read (Done, Result, Progressed);
            exit when Done;
            if not Progressed then Ignore := syscall (SYSCALL_SLEEP, 1); end if;
         end loop;
         return Result = USB_Optical.Command_Passed;
      end Run;
      ISO_Primary_Sector : constant := 16;
      function Has_ISO (LUN : USB_Optical.Logical_Unit) return Boolean is
         Accepted, Done, Progressed : Boolean;
         Result : USB_Optical.Status_Result;
         Ignore : Unsigned_64;
      begin
         if Count <= ISO_Primary_Sector then return False; end if;
         Begin_Bulk (USB_Optical.Read_Request (ISO_Primary_Sector, 1, Format), LUN, Accepted);
         if not Accepted then return False; end if;
         loop
            Poll_Optical_Read (Done, Result, Progressed);
            exit when Done;
            if not Progressed then Ignore := syscall (SYSCALL_SLEEP, 1); end if;
         end loop;
         --  Admission signature only ("CD001", version 1); the filesystem
         --  validates the descriptor with its hostile-input decoder.
         return Result = USB_Optical.Command_Passed and then
           bulkData (1) in 0 | 1 and then
           bulkData (2) = Character'Pos ('C') and then bulkData (3) = Character'Pos ('D') and then
           bulkData (4) = Character'Pos ('0') and then bulkData (5) = Character'Pos ('0') and then
           bulkData (6) = Character'Pos ('1') and then bulkData (7) = 1;
      end Has_ISO;
   begin
      if storageSlot = 0 then return; end if;
      Control_Request (storageSlot, TYPE_CLASS_INTERFACE_IN, BOT_GET_MAX_LUN, 0,
                       Unsigned_16 (storageInterface), 1, Descriptor_Buffer (storageSlot), Actual, OK);
      Outcome := (if OK then USB_Optical.Control_Success else USB_Optical.Control_Error);
      if not OK and then lastControlCode = CC_STALL and then Recover_Control_Stall (storageSlot) then
         Outcome := USB_Optical.Control_Stall;
      end if;
      declare
         Data : USB_Optical.Bytes (1 .. Actual);
      begin
         for I in Data'Range loop
            Data (I) := deviceDMA (storageSlot).descriptorBytes (I - 1);
         end loop;
         USB_Optical.Decode_Max_LUN (Outcome, Data, Last_LUN, Valid);
      end;
      if not Valid then
         Log ("slot " & D (Natural (storageSlot)) & " storage fail invalid LUN discovery", True);
         return;
      end if;
      for LUN in USB_Optical.Logical_Unit range 0 .. Last_LUN loop
         if Run (USB_Optical.Inquiry, LUN) then
            declare
               Inquiry : constant USB_Optical.Bytes (1 .. 36) := bulkData (1 .. 36);
            begin
               if USB_Optical.Is_Supported_Inquiry (Inquiry) then
                  for Attempt in 1 .. 5 loop
                     exit when Run (USB_Optical.Test_Unit_Ready, LUN);
                     OK := Run (USB_Optical.Request_Sense, LUN);
                  end loop;
                  if Run (USB_Optical.Read_Capacity, LUN) then
                     declare
                        Response : constant USB_Optical.Bytes (1 .. 8) := bulkData (1 .. 8);
                     begin
                        USB_Optical.Decode_Capacity (Response, Count, Capacity, Format);
                     end;
                     if Capacity = USB_Optical.Capacity_Valid and then Has_ISO (LUN) then
                        opticalLUN := LUN;
                        opticalBlocks := Count;
                        storageFormat := Format;
                        Log ("slot " & D (Natural (storageSlot)) & " optical ok lun=" &
                             D (Natural (LUN)) & " blocks=" & D (Count) & " sector=" &
                             D (Natural (2048 / USB_Optical.Sectors_Per_Block (Format))));
                        --  The live-boot regression's marker.
                        Trace ("optical LUN=" & D (Natural (LUN)));
                        return;
                     end if;
                  end if;
               end if;
            end;
         end if;
      end loop;
      Log ("slot " & D (Natural (storageSlot)) & " optical fail no supported medium", True);
   end Probe_Optical;

   function Optical_Block_Count return Unsigned_64 is (opticalBlocks);
   function Optical_Read_Buffer return System.Address is
     (To_Address (Integer_Address (DMA_VIRT_BASE + BULK_DATA_OFFSET)));
   function Optical_Deadline return Unsigned_64 is
     (if bulkPhase in Command_Out | Data_In | Status_In then bulkDeadline else Unsigned_64'Last);
   procedure Start_Optical_Read
     (First : Unsigned_32; Count : USB_Optical.Read_Block_Count; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not USB_Optical.Read_Fits (First, Count, opticalBlocks) then return; end if;
      if First > Unsigned_32'Last / USB_Optical.Sectors_Per_Block (storageFormat) then return; end if;
      Begin_Bulk (USB_Optical.Read_Request (First, Count, storageFormat), opticalLUN, Accepted);
   end Start_Optical_Read;

   ---------------------------------------------------------------------------
   --  Controller bring-up
   ---------------------------------------------------------------------------
   procedure Set_Identity
     (Location : String; Identity : XHCI_Controller_Identity.Identity;
      Mode : Runtime_Interrupt_Mode) is
   begin
      if Location'Length = Location_Length then
         locationText := Location;
      end if;
      XHCI.identity := Identity;
      interruptMode := Mode;
   end Set_Identity;

   --  Poll a register until (value and Mask) = Expected, at most Limit_Ms
   --  milliseconds; Elapsed is how long it took (or the limit).
   function Wait_For_Bits
     (Base : System.Address; Offset : Storage_Offset; Mask, Expected : Unsigned_32;
      Limit_Ms : Natural; Elapsed : out Unsigned_64) return Boolean
   is
      Started : constant Unsigned_64 := Now_Ms;
      Ignore : Unsigned_64;
   begin
      Elapsed := 0;
      for Attempt in 0 .. Limit_Ms loop
         if (Read32 (Base, Offset) and Mask) = Expected then
            Elapsed := PM.Since (Now_Ms, Started);
            return True;
         end if;
         exit when PM.Since (Now_Ms, Started) >= Unsigned_64 (Limit_Ms);
         Ignore := syscall (SYSCALL_SLEEP, 1);
      end loop;
      Elapsed := PM.Since (Now_Ms, Started);
      --  A terminal observation at the timeout boundary.
      return (Read32 (Base, Offset) and Mask) = Expected;
   end Wait_For_Bits;

   function Firmware_Handoff (HCC : Unsigned_32) return Boolean is
      Offset : Unsigned_64 := XHCI_Legacy.First_Offset (HCC);
      Header, Before : Unsigned_32;
      Distance, Waited : Unsigned_64;
      Capability_Id_Mask : constant Unsigned_32 := 16#FF#;
   begin
      for Entry_Number in 1 .. Natural (barMappedBytes / 4) loop
         if Offset = 0 then
            Stage ("handoff", True, "legacy=absent");
            return True;
         elsif not XHCI_Legacy.Fits (Offset, 4, barMappedBytes) then
            Stage ("handoff", False, "xecp-out-of-range offset=" & H32 (Unsigned_32 (Offset)));
            return False;
         end if;
         Header := Read32 (barBase, Storage_Offset (Offset));
         if Header = Unsigned_32'Last or else (Header and Capability_Id_Mask) = 0 then
            Stage ("handoff", False, "xecp-invalid header=" & H32 (Header));
            return False;
         end if;
         if (Header and Capability_Id_Mask) = XHCI_Legacy.LEGACY_ID then
            if not XHCI_Legacy.Fits (Offset, 8, barMappedBytes) then
               return False;
            end if;
            Before := Header;
            --  7.1.1: request OS ownership; never forcibly clear BIOS's.
            Write32 (barBase, Storage_Offset (Offset), Header or XHCI_Legacy.OS_OWNED);
            if not Wait_For_Bits (barBase, Storage_Offset (Offset),
              XHCI_Legacy.BIOS_OWNED or XHCI_Legacy.OS_OWNED, XHCI_Legacy.OS_OWNED,
              Register_Wait_Ms, Waited)
            then
               Stage ("handoff", False, "timeout before=" & H32 (Before) & " now=" &
                      H32 (Read32 (barBase, Storage_Offset (Offset))));
               return False;
            end if;
            Write32 (barBase, Storage_Offset (Offset + 4),
                     XHCI_Legacy.Disable_SMIs (Read32 (barBase, Storage_Offset (Offset + 4))));
            Stage ("handoff", True, "legacy=acquired before=" & H32 (Before) & " after=" &
                   H32 (Read32 (barBase, Storage_Offset (Offset))) & " ms=" & D (Waited));
            --  The live-boot regression's marker.
            Trace ("firmware ownership acquired=" & H32 (Read32 (barBase, Storage_Offset (Offset))));
            return True;
         end if;
         Distance := XHCI_Legacy.Next_Distance (Header);
         Offset := (if Distance = 0 then 0 else Offset + Distance);
      end loop;
      Stage ("handoff", False, "xecp-walk-exhausted");
      return False;
   end Firmware_Handoff;

   procedure Enumerate_Root_Port (Port : Root_Port_Number; Result : out Init_Result) is
      Status : Unsigned_32 := Read_Port (Port);
      Path : XHCI_Topology.Path;
      Waited : Unsigned_64 := 0;
      Ignore : Unsigned_64;
   begin
      case XHCI_Ports.Before_Enumeration (Status) is
         when XHCI_Ports.Disconnected =>
            Result := INIT_NO_DEVICE;
            return;
         when XHCI_Ports.Start_Reset =>
            Write32 (operational, PORTSC (Port), XHCI_Ports.Reset_Write (Status));
         when XHCI_Ports.Wait_For_Reset | XHCI_Ports.Already_Enabled =>
            null;
      end case;
      if not Wait_For_Bits (operational, PORTSC (Port), PORTSC_WPR or PORTSC_PR or PORTSC_PED,
                            PORTSC_PED, Port_Reset_Wait_Ms, Waited)
      then
         Log ("port " & D (Natural (Port)) & " reset fail ms=" & D (Waited) & " " &
              Port_Text (Read_Port (Port)), True);
         Result := INIT_PORT_RESET_TIMEOUT;
         return;
      end if;
      --  USB 2.0 7.1.7.5: reset recovery before the first request.
      Ignore := syscall (SYSCALL_SLEEP, PM.Reset_Recovery_Ms);
      Status := Read_Port (Port);
      Log ("port " & D (Natural (Port)) & " reset ok ms=" & D (Waited) & " " & Port_Text (Status));
      case Speed (Status) is
         when SPEED_FULL => Path := XHCI_Topology.Root (Port, XHCI_Topology.Full_Speed);
         when SPEED_LOW => Path := XHCI_Topology.Root (Port, XHCI_Topology.Low_Speed);
         when SPEED_HIGH => Path := XHCI_Topology.Root (Port, XHCI_Topology.High_Speed);
         when SPEED_SUPER | SPEED_SUPER_PLUS => Path := XHCI_Topology.Root (Port, XHCI_Topology.Super_Speed);
         when others =>
            Log ("port " & D (Natural (Port)) & " speed fail " & Port_Text (Status), True);
            Result := INIT_ADDRESS_FAILED;
            return;
      end case;
      Enumerate_Device (Path, Result);
   end Enumerate_Root_Port;

   procedure Initialize
     (barPhys  : Unsigned_64;
      barPages : Unsigned_64;
      dmaPhys  : Unsigned_64;
      expectedScratchpads : XHCI_Capabilities.Scratchpad_Buffer_Count;
      result   : out Init_Result)
   is
      Cap_Dword, HCS1, HCS2, HCC1, Page_Sizes : Unsigned_32;
      Max_Slots_Value : Natural;
      Scratchpads : XHCI_Capabilities.Scratchpad_Buffer_Count;
      Layout : constant Allocation := Plan (expectedScratchpads);
      Halt_Ms, Reset_Ms, Ready_Ms, Run_Ms : Unsigned_64 := 0;
      Settle_Events : Natural := 0;
      Ignore : Unsigned_64;
      Quirk_Set : CI.Quirk_Set;
      Quirk_Text : String (1 .. 80) := [others => ' '];
      Quirk_Length : Natural := 0;
      Port_Result : Init_Result;
   begin
      result := INIT_BAD_CAPABILITY;
      maxPorts := 0;
      connectedPorts := 0;
      activeSlot := 0;
      dmaPhysical := dmaPhys;
      commands := R.Fresh_Producer;
      events := R.Fresh_Consumer;
      command := (others => <>);
      XHCI_Completions.Clear (completionMailboxes);
      ep0State := [others => (others => <>)];
      pipes := [others => (others => <>)];
      storageSlot := 0;
      mouseDiagnostics := (others => <>);
      barMappedBytes := 0;

      if barPhys = 0 or else barPages = 0 or else barPages > 256 or else dmaPhys = 0 then
         Stage ("identify", False, "configure bar=" & H32 (Low (barPhys)) & " pages=" & D (barPages));
         return;
      end if;
      --  Through its register handle (its manifest's "registers" binding).
      if not CuBit.Handles.Devices.Map_Device
        (Startup.Registers, barPhys, BAR_VIRT_BASE, barPages, Read_Only => False)
      then
         Stage ("identify", False, "map refused bar=" & H32 (Low (barPhys)));
         result := INIT_MAP_FAILED;
         return;
      end if;
      barBase := To_Address (Integer_Address (BAR_VIRT_BASE));
      barMappedBytes := barPages * PAGE_SIZE;
      Cap_Dword := Read32 (barBase, REG_CAPLENGTH);
      if Read8 (barBase, REG_CAPLENGTH) < MINIMUM_CAPLENGTH then
         Stage ("identify", False, "caplength=" & H32 (Cap_Dword));
         return;
      end if;
      operational := barBase + Storage_Offset (Read8 (barBase, REG_CAPLENGTH));
      HCS1 := Read32 (barBase, REG_HCSPARAMS1);
      HCS2 := Read32 (barBase, REG_HCSPARAMS2);
      HCC1 := Read32 (barBase, REG_HCCPARAMS1);
      Page_Sizes := Read32 (operational, OP_PAGESIZE);
      doorbellBase := barBase + Storage_Offset (Read32 (barBase, REG_DBOFF) and DBOFF_MASK);
      runtimeBase := barBase + Storage_Offset (Read32 (barBase, REG_RTSOFF) and RTSOFF_MASK);
      Max_Slots_Value := Natural (Max_Slots (HCS1));
      maxPorts := Natural'Min (Natural (Max_Ports (HCS1)), MAX_ROOT_PORTS);
      contextStride := (if (HCC1 and HCCPARAMS1_CSZ) = 0 then CONTEXT_DWORDS_32 else CONTEXT_DWORDS_64);
      Scratchpads := XHCI_Capabilities.Scratchpad_Count (HCS2);
      Stage ("identify", True,
             "pci=" & H16 (identity.Vendor) & ":" & H16 (identity.Device) & " rev=" &
             H8 (identity.Revision) & " hci=" & H16 (HCI_Version (Cap_Dword)) & " ports=" &
             D (maxPorts) & " slots=" & D (Max_Slots_Value) & " intrs=" &
             D (Unsigned_64 (Max_Interrupters (HCS1))) & " ctx=" & D (contextStride * 4) &
             " scratch=" & D (Natural (Scratchpads)) & " irq=" &
             (case interruptMode is
                 when INTERRUPT_POLLING => "polled", when INTERRUPT_MSI => "msi",
                 when INTERRUPT_MSIX => "msix"));
      Trace ("hcsparams1=" & H32 (HCS1) & " hcsparams2=" & H32 (HCS2) & " hccparams1=" &
             H32 (HCC1) & " pagesize=" & H32 (Page_Sizes));
      Quirk_Set := CI.Quirks (identity, HCI_Version (Cap_Dword));
      for Q in CI.Quirk loop
         if Quirk_Set (Q) then
            declare
               Name : constant String := (if Quirk_Length = 0 then "" else ",") & CI.Quirk_Name (Q);
            begin
               if Quirk_Length + Name'Length <= Quirk_Text'Length then
                  Quirk_Text (Quirk_Length + 1 .. Quirk_Length + Name'Length) := Name;
                  Quirk_Length := Quirk_Length + Name'Length;
               end if;
            end;
         end if;
      end loop;
      Stage ("quirks", True, (if Quirk_Length = 0 then "none" else Quirk_Text (1 .. Quirk_Length)) &
             " runtime-effect=" & (if CI.Changes_Runtime (Quirk_Set) then "yes" else "none"));

      if Max_Slots_Value = 0 or else maxPorts = 0 or else (HCC1 and HCCPARAMS1_AC64) = 0 then
         Stage ("identify", False, "unsupported slots/ports/ac64 hccparams1=" & H32 (HCC1));
         return;
      end if;
      if (Page_Sizes and PAGESIZE_4K) = 0 then
         Stage ("identify", False, "no 4k pages pagesize=" & H32 (Page_Sizes));
         result := INIT_PAGE_SIZE_UNSUPPORTED;
         return;
      end if;
      if Scratchpads /= expectedScratchpads then
         Stage ("identify", False, "scratchpads=" & D (Natural (Scratchpads)) & " allocated-for=" &
                D (Natural (expectedScratchpads)));
         result := INIT_DMA_LAYOUT_MISMATCH;
         return;
      end if;

      if not Firmware_Handoff (HCC1) then
         result := INIT_FIRMWARE_HANDOFF_FAILED;
         return;
      end if;

      --  5.4.1: clear R/S and wait for HCH before HCRST.
      if (Read32 (operational, OP_USBSTS) and USBSTS_HCH) = 0 then
         Write32 (operational, OP_USBCMD, Read32 (operational, OP_USBCMD) and not USBCMD_RUN);
         if not Wait_For_Bits (operational, OP_USBSTS, USBSTS_HCH, USBSTS_HCH, Register_Wait_Ms, Halt_Ms) then
            Stage ("reset", False, "halt-timeout usbsts=" & H32 (Read32 (operational, OP_USBSTS)));
            result := INIT_STOP_TIMEOUT;
            return;
         end if;
      end if;
      Write32 (operational, OP_USBCMD, USBCMD_HCRST);
      if not Wait_For_Bits (operational, OP_USBCMD, USBCMD_HCRST, 0, Register_Wait_Ms, Reset_Ms) or else
         not Wait_For_Bits (operational, OP_USBSTS, USBSTS_CNR, 0, Register_Wait_Ms, Ready_Ms)
      then
         Stage ("reset", False, "hcrst-timeout usbcmd=" & H32 (Read32 (operational, OP_USBCMD)) &
                " usbsts=" & H32 (Read32 (operational, OP_USBSTS)));
         result := INIT_RESET_TIMEOUT;
         return;
      end if;

      declare
         Bytes : array (Natural range 0 .. Pages (Layout) * 4096 - 1) of Unsigned_8
           with Import, Address => DMA (0);
      begin
         for I in Bytes'Range loop
            Bytes (I) := 0;
         end loop;
      end;
      if Scratchpads > 0 then
         declare
            Pointers : Address_Array (0 .. Scratchpads - 1)
              with Import, Address => DMA (SCRATCH_ARRAY_OFFSET);
         begin
            for I in Pointers'Range loop
               Pointers (I) := dmaPhys + Layout.Scratch_First + Unsigned_64 (I) * PAGE_SIZE;
            end loop;
         end;
         dcbaa (0) := Phys (SCRATCH_ARRAY_OFFSET);
      end if;
      Init_Ring (COMMAND_RING_OFFSET);
      erst := (ringBase => Phys (EVENT_RING_OFFSET), ringSize => R.Event_Entries, reserved => 0);
      Write64 (operational, OP_DCBAAP, Phys (DCBAA_OFFSET));
      Write64 (operational, OP_CRCR, Phys (COMMAND_RING_OFFSET) or Unsigned_64 (CRCR_RCS));
      Write32 (runtimeBase + RT_INTR0, INTR_IMAN, 0);
      Write32 (runtimeBase + RT_INTR0, INTR_IMOD, 0);
      Write32 (runtimeBase + RT_INTR0, INTR_ERSTSZ, ONE_SEGMENT);
      Write64 (runtimeBase + RT_INTR0, INTR_ERDP, Phys (EVENT_RING_OFFSET));
      Write64 (runtimeBase + RT_INTR0, INTR_ERSTBA, Phys (ERST_OFFSET));
      Write32 (operational, OP_CONFIG,
               Unsigned_32 (Natural'Min (Max_Slots_Value, XHCI_Completions.Maximum_Slots)));
      Write32 (operational, OP_USBCMD, USBCMD_RUN);
      if not Wait_For_Bits (operational, OP_USBSTS, USBSTS_HCH, 0, Register_Wait_Ms, Run_Ms) then
         Stage ("reset", False, "run-timeout usbsts=" & H32 (Read32 (operational, OP_USBSTS)));
         result := INIT_START_TIMEOUT;
         return;
      end if;
      Stage ("reset", True, "halt-ms=" & D (Halt_Ms) & " reset-ms=" & D (Reset_Ms) & " cnr-ms=" &
             D (Ready_Ms) & " run-ms=" & D (Run_Ms) & " usbsts=" & H32 (Read32 (operational, OP_USBSTS)));
      Stage ("interrupter", True,
             "iman=" & H32 (Read32 (runtimeBase + RT_INTR0, INTR_IMAN)) & " imod=" &
             H32 (Read32 (runtimeBase + RT_INTR0, INTR_IMOD)) & " erstsz=" &
             D (Unsigned_64 (Read32 (runtimeBase + RT_INTR0, INTR_ERSTSZ))) & " erdp=" &
             H32 (Read32 (runtimeBase + RT_INTR0, INTR_ERDP)) & " events=" & D (Natural (R.Event_Entries)) &
             " slots-enabled=" & D (Unsigned_64 (Read32 (operational, OP_CONFIG))));

      --  Bounded boot discovery window (see Boot_Settle_Ms); the event ring
      --  is drained so port-change events cannot fill it meanwhile.
      for Step in 1 .. Boot_Settle_Ms / Natural (Boot_Settle_Step_Ms) loop
         while Collect_Event loop
            Settle_Events := Settle_Events + 1;
         end loop;
         Ignore := syscall (SYSCALL_SLEEP, Boot_Settle_Step_Ms);
      end loop;
      connectedPorts := 0;
      declare
         Empty_Ports : Natural := 0;
      begin
         for Port in 1 .. maxPorts loop
            if Connected (Read_Port (Port)) then
               connectedPorts := connectedPorts + 1;
               Log ("port " & D (Port) & " start " & Port_Text (Read_Port (Port)));
            else
               Empty_Ports := Empty_Ports + 1;
               Trace ("port " & D (Port) & " start " & Port_Text (Read_Port (Port)));
            end if;
         end loop;
         Stage ("settle", True, "ms=" & D (Boot_Settle_Ms) & " events=" & D (Settle_Events) &
                " connected=" & D (connectedPorts) & " empty=" & D (Empty_Ports));
      end;

      for Port in 1 .. maxPorts loop
         if Connected (Read_Port (Port)) then
            Enumerate_Root_Port (Port, Port_Result);
            if Port_Result in INIT_COMMAND_TIMEOUT | INIT_COMMAND_FAILED then
               result := Port_Result;
               return;
            end if;
         end if;
      end loop;
      activeSlot := (if pipes (Mouse_Pipe).Slot /= 0 then pipes (Mouse_Pipe).Slot
                     elsif pipes (Keyboard_Pipe).Slot /= 0 then pipes (Keyboard_Pipe).Slot
                     else storageSlot);
      result := INIT_OK;
   end Initialize;

   function Port_Count return Natural is (maxPorts);
   function Connected_Port_Count return Natural is (connectedPorts);
   function Device_Slot return Natural is (activeSlot);
   function Has_Keyboard return Boolean is (pipes (Keyboard_Pipe).Slot /= 0);
   function Has_Mouse return Boolean is (pipes (Mouse_Pipe).Slot /= 0);
   function Has_Storage return Boolean is (storageSlot /= 0);

   procedure Start_Boot_Mouse_Transfers is
   begin
      Start_Pipe (Mouse_Pipe);
   end Start_Boot_Mouse_Transfers;

   procedure Start_Boot_Keyboard_Transfers is
   begin
      Start_Pipe (Keyboard_Pipe);
   end Start_Boot_Keyboard_Transfers;

   ---------------------------------------------------------------------------
   --  Ports (hotplug): Port Status Change events and deadlines
   ---------------------------------------------------------------------------
   type Engine_Step is
     (Engine_Idle, Engine_Enable, Engine_Address, Engine_Address_Recovery,
      Engine_Device8, Engine_Evaluate, Engine_Device18, Engine_Config9,
      Engine_Config_Full, Engine_Set_Config, Engine_Configure,
      Engine_Protocol_Mouse, Engine_Protocol_Keyboard);
   type Engine_State is record
      Step      : Engine_Step := Engine_Idle;
      Port      : Port_Or_None := 0;
      Slot      : Slot_Or_None := 0;
      Path      : XHCI_Topology.Path;
      Issued    : Boolean := False;
      Cancelled : Boolean := False;
      Wait_Until, Started_At : Unsigned_64 := 0;
      Total     : Natural := 0;
      Packet0   : Unsigned_32 := 0;
      Configuration : USB_Configurations.Configuration;
      Roles     : USB_Device_Roles.Selection;
      Control   : Control_Transfer;
   end record;
   engine : Engine_State;

   procedure Try_Complete_Release (Port : Root_Port_Number) is
   begin
      if ports (Port).Current = PM.Releasing and then Slots_Of (Port) = 0 and then
        not (engine.Step /= Engine_Idle and then engine.Port = Port)
      then
         PM.Released (ports (Port), Connected (Read_Port (Port)), Now_Ms);
         Log ("port " & D (Natural (Port)) & " released next=" & PM.Phase_Name (ports (Port).Current));
      end if;
   end Try_Complete_Release;

   procedure Execute (Port : Root_Port_Number; A : PM.Action; Status : Unsigned_32; Was : PM.Phase) is
      Count : Natural := 0;
   begin
      case A is
         when PM.No_Action => null;
         when PM.Start_Reset =>
            portResetStarted (Port) := Now_Ms;
            Write32 (operational, PORTSC (Port), XHCI_Ports.Reset_Write (Status));
            Log ("port " & D (Natural (Port)) & " debounce ok ms=" & D (PM.Debounce_Stable_Ms) &
                 " reset start " & Port_Text (Status));
         when PM.Give_Up =>
            Log ("port " & D (Natural (Port)) & " " & PM.Phase_Name (Was) & " fail gave up " &
                 Port_Text (Status), True);
         when PM.Release_Slots =>
            for S in Device_Index loop
               if slotPort (S) = Port and then not slotRetired (S) then
                  Retire_Slot (S);
                  Count := Count + 1;
               end if;
            end loop;
            if engine.Step /= Engine_Idle and then engine.Port = Port then
               engine.Cancelled := True;
            end if;
            health.Detaches := health.Detaches + 1;
            Log ("port " & D (Natural (Port)) & " detach slots=" & D (Count) & " " & Port_Text (Status));
      end case;
   end Execute;

   --  Read PORTSC, acknowledge its change bits, step the port's machine;
   --  repeat while new change bits appear (bounded).
   procedure Port_Step (Port : Root_Port_Number; Changed : Boolean) is
      Status : Unsigned_32;
      A : PM.Action;
      Was : PM.Phase;
   begin
      for Reading in 1 .. Port_Change_Rereads loop
         Status := Read_Port (Port);
         exit when Reading > 1 and then Changes (Status) = 0;
         if Changes (Status) /= 0 then
            --  5.4.8 / 4.19.2: RW1C, so further changes raise new events.
            Write32 (operational, PORTSC (Port), Acknowledge_Changes (Status));
         end if;
         Was := ports (Port).Current;
         PM.Step (ports (Port), PM.Observe (Status), Now_Ms, A);
         if Was /= ports (Port).Current or else A /= PM.No_Action then
            if Was = PM.Empty and then ports (Port).Current = PM.Debouncing then
               Log ("port " & D (Natural (Port)) & " connect " & Port_Text (Status));
            elsif Was = PM.Resetting and then ports (Port).Current = PM.Recovering then
               Log ("port " & D (Natural (Port)) & " reset ok ms=" &
                    D (PM.Since (Now_Ms, portResetStarted (Port))) & " " & Port_Text (Status));
            elsif Was = PM.Debouncing and then ports (Port).Current = PM.Recovering then
               Log ("port " & D (Natural (Port)) & " debounce ok enabled " & Port_Text (Status));
            elsif ports (Port).Current = PM.Empty and then Was /= PM.Releasing then
               Log ("port " & D (Natural (Port)) & " disconnect during " & PM.Phase_Name (Was));
            end if;
            Execute (Port, A, Status, Was);
         end if;
         Try_Complete_Release (Port);
         exit when not Changed and then Changes (Status) = 0;
      end loop;
   end Port_Step;

   procedure Handle_Port_Event (Port : Natural) is
   begin
      if Port in 1 .. maxPorts then
         Port_Step (Port, True);
      end if;
   end Handle_Port_Event;

   ---------------------------------------------------------------------------
   --  The enumeration engine: one hot-plugged root-port device at a time,
   --  one command or control transfer per step, never waiting.
   ---------------------------------------------------------------------------
   procedure Engine_Done (Success : Boolean; Text : String) is
      Port : constant Port_Or_None := engine.Port;
   begin
      if not Success then
         health.Enumeration_Failures := health.Enumeration_Failures + 1;
         Log ("slot " & D (Natural (engine.Slot)) & " " & Text, True);
         for K in Pipe_Kind loop
            if engine.Slot /= 0 and then pipes (K).Slot = engine.Slot then
               pipes (K) := (others => <>);
            end if;
         end loop;
         if engine.Slot /= 0 then
            Retire_Slot (engine.Slot);
         end if;
      end if;
      engine.Step := Engine_Idle;
      if Port /= 0 then
         if ports (Port).Current = PM.Enumerating then
            PM.Finished (ports (Port), Success);
            if Success then
               health.Attaches := health.Attaches + 1;
               Log ("port " & D (Natural (Port)) & " attached slot=" & D (Natural (engine.Slot)) &
                    " ms=" & D (PM.Since (Now_Ms, engine.Started_At)) & " " & Text);
            end if;
         else
            Try_Complete_Release (Port);
         end if;
      end if;
   end Engine_Done;

   --  Begin step Next: commands and controls are issued by Run_Engine.
   procedure Engine_Next (Next : Engine_Step) is
   begin
      engine.Step := Next;
      engine.Issued := False;
   end Engine_Next;

   procedure Engine_Control (Request_Type, Request : Unsigned_8; Value, Index : Unsigned_16;
                             Length : Natural; Buffer : Unsigned_64) is
   begin
      if not engine.Issued then
         if not ep0State (engine.Slot).Busy then
            Start_Control (engine.Control, engine.Slot, Request_Type, Request, Value, Index,
                           Length, Buffer);
            engine.Issued := True;
         end if;
      else
         Poll_Control (engine.Control);
      end if;
   end Engine_Control;

   function Engine_Control_Finished return Boolean is
     (engine.Issued and then not engine.Control.Active);
   function Engine_Control_OK return Boolean is
     (engine.Control.Result = Control_OK);

   procedure Engine_Command (Item : TRB) is
   begin
      if not engine.Issued and then Command_Free then
         Issue_Command (Item, Engine_Owner);
         engine.Issued := True;
      end if;
   end Engine_Command;

   function Engine_Command_Finished return Boolean is (Command_Done_For (Engine_Owner));

   procedure Run_Engine is
      Now : constant Unsigned_64 := Now_Ms;
      Code : Completion_Code;
      Slot_Id : Natural;
      Status : Unsigned_32;
      Desc : Unsigned_64;
      Decoded : USB_Configurations.Decode_Result;
      use type USB_Configurations.Decode_Result;
   begin
      if engine.Step = Engine_Idle then
         for Port in 1 .. maxPorts loop
            if ports (Port).Current = PM.Ready then
               Status := Read_Port (Port);
               PM.Claim (ports (Port));
               engine := (Step => Engine_Enable, Port => Port, Started_At => Now, others => <>);
               case Speed (Status) is
                  when SPEED_FULL => engine.Path := XHCI_Topology.Root (Port, XHCI_Topology.Full_Speed);
                  when SPEED_LOW => engine.Path := XHCI_Topology.Root (Port, XHCI_Topology.Low_Speed);
                  when SPEED_HIGH => engine.Path := XHCI_Topology.Root (Port, XHCI_Topology.High_Speed);
                  when SPEED_SUPER | SPEED_SUPER_PLUS =>
                     engine.Path := XHCI_Topology.Root (Port, XHCI_Topology.Super_Speed);
                  when others =>
                     Engine_Done (False, "speed fail " & Port_Text (Status));
                     return;
               end case;
               Log ("port " & D (Natural (Port)) & " enumerate start " & Port_Text (Status));
               exit;
            end if;
         end loop;
         if engine.Step = Engine_Idle then
            return;
         end if;
      end if;

      --  A detach while enumerating: finish the outstanding operation, then
      --  give the slot back.
      if engine.Cancelled then
         if engine.Issued and then
           ((engine.Step in Engine_Enable | Engine_Address | Engine_Evaluate | Engine_Configure
             and then not Engine_Command_Finished)
            or else (engine.Step in Engine_Device8 | Engine_Device18 | Engine_Config9 |
                       Engine_Config_Full | Engine_Set_Config | Engine_Protocol_Mouse |
                       Engine_Protocol_Keyboard and then engine.Control.Active))
         then
            if engine.Control.Active then
               Poll_Control (engine.Control);
            end if;
            return;
         end if;
         if Engine_Command_Finished then
            if engine.Step = Engine_Enable and then command.Code = CC_SUCCESS and then
              command.Slot in Device_Index
            then
               engine.Slot := command.Slot;
               slotPort (engine.Slot) := engine.Port;
            end if;
            Release_Command;
         end if;
         Engine_Done (False, "enumerate cancelled by detach");
         return;
      end if;

      Desc := (if engine.Slot in Device_Index then Descriptor_Buffer (engine.Slot) else 0);
      case engine.Step is
         when Engine_Idle => null;

         when Engine_Enable =>
            Engine_Command (Command_TRB (TRB_ENABLE_SLOT, 0));
            if Engine_Command_Finished then
               Code := command.Code;
               Slot_Id := command.Slot;
               Release_Command;
               if Code /= CC_SUCCESS or else Slot_Id not in Device_Index then
                  Engine_Done (False, "enable fail cc=" & Code_Name (Code));
                  return;
               end if;
               engine.Slot := Slot_Id;
               slotPort (engine.Slot) := engine.Port;
               Clear_Slot_DMA (engine.Slot);
               Log ("slot " & D (Slot_Id) & " enable ok port=" & D (Natural (engine.Port)));
               Build_Address_Input (engine.Slot, engine.Path);
               Engine_Next (Engine_Address);
            end if;

         when Engine_Address =>
            Engine_Command (Address_Device_TRB (engine.Slot));
            if Engine_Command_Finished then
               Code := command.Code;
               Release_Command;
               if Code /= CC_SUCCESS then
                  Engine_Done (False, "address fail cc=" & Code_Name (Code));
                  return;
               end if;
               engine.Wait_Until := PM.Plus (Now, SetAddress_Recovery_Ms);
               Engine_Next (Engine_Address_Recovery);
            end if;

         when Engine_Address_Recovery =>
            if Now >= engine.Wait_Until then
               Engine_Next (Engine_Device8);
            end if;

         when Engine_Device8 =>
            Engine_Control (TYPE_DEVICE_IN, REQUEST_GET_DESCRIPTOR, DESCRIPTOR_DEVICE, 0,
                            EP0_PROBE_BYTES, Desc);
            if Engine_Control_Finished then
               if not Engine_Control_OK or else engine.Control.Actual /= EP0_PROBE_BYTES or else
                 deviceDMA (engine.Slot).descriptorBytes (0) /= DEVICE_DESCRIPTOR_BYTES or else
                 deviceDMA (engine.Slot).descriptorBytes (1) /= DEVICE_DESCRIPTOR_TYPE
               then
                  Engine_Done (False, "device8 fail cc=" & Code_Name (engine.Control.Code));
                  return;
               end if;
               engine.Packet0 := EP0_Packet (XHCI_Topology.Device_Speed (engine.Path),
                                             deviceDMA (engine.Slot).descriptorBytes (7));
               Log ("slot " & D (Natural (engine.Slot)) & " address ok addr=" &
                    D (Unsigned_64 (deviceDMA (engine.Slot).deviceContext (3) and 16#FF#)) &
                    " mps0=" & D (Unsigned_64 (engine.Packet0)));
               if engine.Packet0 = 0 then
                  Engine_Done (False, "device8 fail mps0");
               elsif engine.Packet0 /= Default_Packet (XHCI_Topology.Device_Speed (engine.Path)) then
                  Build_Evaluate_Packet (engine.Slot, engine.Packet0);
                  Engine_Next (Engine_Evaluate);
               else
                  Engine_Next (Engine_Device18);
               end if;
            end if;

         when Engine_Evaluate =>
            Engine_Command (Input_Command (TRB_EVALUATE_CONTEXT, engine.Slot));
            if Engine_Command_Finished then
               Code := command.Code;
               Release_Command;
               if Code /= CC_SUCCESS then
                  Engine_Done (False, "evaluate fail cc=" & Code_Name (Code));
                  return;
               end if;
               Engine_Next (Engine_Device18);
            end if;

         when Engine_Device18 =>
            Engine_Control (TYPE_DEVICE_IN, REQUEST_GET_DESCRIPTOR, DESCRIPTOR_DEVICE, 0,
                            DEVICE_DESCRIPTOR_BYTES, Desc);
            if Engine_Control_Finished then
               if not Engine_Control_OK or else engine.Control.Actual < DEVICE_DESCRIPTOR_BYTES then
                  Engine_Done (False, "device fail cc=" & Code_Name (engine.Control.Code));
                  return;
               end if;
               Log ("slot " & D (Natural (engine.Slot)) & " device ok id=" &
                    H8 (deviceDMA (engine.Slot).descriptorBytes (9)) &
                    H8 (deviceDMA (engine.Slot).descriptorBytes (8)) & ":" &
                    H8 (deviceDMA (engine.Slot).descriptorBytes (11)) &
                    H8 (deviceDMA (engine.Slot).descriptorBytes (10)) & " class=" &
                    H8 (deviceDMA (engine.Slot).descriptorBytes (4)));
               Engine_Next (Engine_Config9);
            end if;

         when Engine_Config9 =>
            Engine_Control (TYPE_DEVICE_IN, REQUEST_GET_DESCRIPTOR, DESCRIPTOR_CONFIGURATION, 0,
                            CONFIG_HEADER_BYTES, Desc);
            if Engine_Control_Finished then
               engine.Total := Natural (deviceDMA (engine.Slot).descriptorBytes (2)) +
                 Natural (deviceDMA (engine.Slot).descriptorBytes (3)) * 256;
               if not Engine_Control_OK or else engine.Control.Actual < CONFIG_HEADER_BYTES or else
                 deviceDMA (engine.Slot).descriptorBytes (1) /= CONFIG_DESCRIPTOR_TYPE or else
                 engine.Total not in CONFIG_HEADER_BYTES .. USB_Configurations.Maximum_Descriptor_Bytes
               then
                  Engine_Done (False, "config fail cc=" & Code_Name (engine.Control.Code) &
                               " total=" & D (engine.Total));
                  return;
               end if;
               Engine_Next (Engine_Config_Full);
            end if;

         when Engine_Config_Full =>
            Engine_Control (TYPE_DEVICE_IN, REQUEST_GET_DESCRIPTOR, DESCRIPTOR_CONFIGURATION, 0,
                            engine.Total, Desc);
            if Engine_Control_Finished then
               if not Engine_Control_OK or else engine.Control.Actual < engine.Total then
                  Engine_Done (False, "config fail cc=" & Code_Name (engine.Control.Code));
                  return;
               end if;
               declare
                  Data : USB_Configurations.Bytes (1 .. engine.Total);
               begin
                  for I in Data'Range loop
                     Data (I) := deviceDMA (engine.Slot).descriptorBytes (I - 1);
                  end loop;
                  USB_Configurations.Decode (Data, engine.Configuration, Decoded);
               end;
               if Decoded /= USB_Configurations.Decoded then
                  Engine_Done (False, "config fail malformed");
                  return;
               end if;
               --  Hubs and storage are boot-time devices for now.
               engine.Roles := USB_Device_Roles.Select_Roles
                 (engine.Configuration, Mouse_Free => pipes (Mouse_Pipe).Slot = 0,
                  Keyboard_Free => pipes (Keyboard_Pipe).Slot = 0, Storage_Free => False);
               Log ("slot " & D (Natural (engine.Slot)) & " config ok len=" & D (engine.Total) &
                    " mouse-if=" & (if engine.Configuration.Mouse.Present then "1" else "0") &
                    " keyboard-if=" & (if engine.Configuration.Keyboard.Present then "1" else "0") &
                    " role=" & USB_Device_Roles.Role_Name (engine.Roles.Kind));
               if engine.Roles.Kind /= USB_Device_Roles.HID_Role then
                  Engine_Done (False, "config fail unsupported on hotplug (hub, storage or no free role)");
                  return;
               end if;
               Engine_Next (Engine_Set_Config);
            end if;

         when Engine_Set_Config =>
            Engine_Control (TYPE_DEVICE_OUT, REQUEST_SET_CONFIGURATION,
                            Unsigned_16 (engine.Configuration.Value), 0, 0, 0);
            if Engine_Control_Finished then
               if not Engine_Control_OK then
                  Engine_Done (False, "set-config fail cc=" & Code_Name (engine.Control.Code));
                  return;
               end if;
               if engine.Roles.Mouse then
                  Assign_Pipe (Mouse_Pipe, engine.Slot, engine.Configuration.Mouse, Primary_Area);
               end if;
               if engine.Roles.Keyboard then
                  Assign_Pipe (Keyboard_Pipe, engine.Slot, engine.Configuration.Keyboard,
                               (if engine.Roles.Mouse then Secondary_Area else Primary_Area));
               end if;
               Build_Pipe_Configure (engine.Slot, XHCI_Topology.Device_Speed (engine.Path));
               Engine_Next (Engine_Configure);
            end if;

         when Engine_Configure =>
            Engine_Command (Input_Command (TRB_CONFIGURE_ENDPOINT, engine.Slot));
            if Engine_Command_Finished then
               Code := command.Code;
               Release_Command;
               if Code /= CC_SUCCESS then
                  Engine_Done (False, "configure-ep fail cc=" & Code_Name (Code));
                  return;
               end if;
               Engine_Next (if engine.Roles.Mouse then Engine_Protocol_Mouse else Engine_Protocol_Keyboard);
            end if;

         when Engine_Protocol_Mouse | Engine_Protocol_Keyboard =>
            declare
               K : constant Pipe_Kind :=
                 (if engine.Step = Engine_Protocol_Mouse then Mouse_Pipe else Keyboard_Pipe);
            begin
               Engine_Control (TYPE_CLASS_INTERFACE_OUT, HID_SET_PROTOCOL, HID_BOOT_PROTOCOL,
                               Unsigned_16 (pipes (K).Interface_Number), 0, 0);
               if Engine_Control_Finished then
                  if not Engine_Control_OK then
                     Engine_Done (False, "set-protocol fail " & Pipe_Name (K) & " cc=" &
                                  Code_Name (engine.Control.Code));
                     return;
                  end if;
                  if K = Mouse_Pipe and then engine.Roles.Keyboard then
                     Engine_Next (Engine_Protocol_Keyboard);
                  else
                     for P in Pipe_Kind loop
                        if pipes (P).Slot = engine.Slot then
                           Start_Pipe (P);
                        end if;
                     end loop;
                     Engine_Done (True, "roles=" & (if engine.Roles.Mouse then "mouse" else "") &
                                  (if engine.Roles.Mouse and engine.Roles.Keyboard then "+" else "") &
                                  (if engine.Roles.Keyboard then "keyboard" else ""));
                  end if;
               end if;
            end;
      end case;
   end Run_Engine;

   --  Disable retired slots, one command at a time.
   procedure Run_Releases is
      Code : Completion_Code;
      Slot : Device_Index;
   begin
      if disablingSlot /= 0 then
         if Command_Done_For (Release_Owner) then
            Code := command.Code;
            Release_Command;
            Slot := disablingSlot;
            disablingSlot := 0;
            Log ("slot " & D (Natural (Slot)) & " disable " &
                 (if Code = CC_SUCCESS then "ok" else "fail") & " cc=" & Code_Name (Code),
                 Code /= CC_SUCCESS);
            declare
               Port : constant Port_Or_None := slotPort (Slot);
            begin
               Free_Slot (Slot);
               if Port /= 0 then
                  Try_Complete_Release (Port);
               end if;
            end;
         end if;
         return;
      end if;
      if not Command_Free then
         return;
      end if;
      for S in Device_Index loop
         if slotRetired (S) and then not (engine.Step /= Engine_Idle and then engine.Slot = S) then
            disablingSlot := S;
            Issue_Command (Command_TRB (TRB_DISABLE_SLOT, S), Release_Owner);
            return;
         end if;
      end loop;
   end Run_Releases;

   ---------------------------------------------------------------------------
   --  The ladder's last rung: reset and re-enumerate the port (software
   --  replug), Port_Reset_Limit times per port with backoff.
   ---------------------------------------------------------------------------
   portResets : array (Root_Port_Number) of ER.Port_Reset_Count := [others => 0];
   portResetAt : array (Root_Port_Number) of Unsigned_64 := [others => 0];
   portRecoveryDue : Unsigned_64 := Unsigned_64'Last;

   procedure Run_Port_Recovery is
      Now : constant Unsigned_64 := Now_Ms;
      Port : Port_Or_None;
      Count : Natural;
   begin
      portRecoveryDue := Unsigned_64'Last;
      for K in Pipe_Kind loop
         if pipes (K).Slot /= 0 and then pipes (K).Recovery.Current = ER.Need_Port_Reset then
            Port := slotPort (pipes (K).Slot);
            if Port /= 0 then
               if portResets (Port) > 0 and then
                 PM.Since (Now, portResetAt (Port)) >= ER.Port_Reset_Forget_Ms
               then
                  portResets (Port) := 0;
               end if;
               if portResets (Port) = ER.Port_Reset_Limit then
                  if not pipes (K).Gave_Up then
                     pipes (K).Gave_Up := True;
                     health.Given_Up := health.Given_Up + 1;
                     Log ("port " & D (Natural (Port)) & " " & Pipe_Name (K) &
                          " recovery gave up after" & Natural'Image (ER.Port_Reset_Limit) &
                          " port resets; replug the device", True);
                  end if;
               elsif portResets (Port) > 0 and then
                 Now < portResetAt (Port) + ER.Port_Reset_Delay (portResets (Port) - 1)
               then
                  portRecoveryDue := Unsigned_64'Min
                    (portRecoveryDue, portResetAt (Port) + ER.Port_Reset_Delay (portResets (Port) - 1));
               elsif ports (Port).Current = PM.Attached then
                  portResets (Port) := portResets (Port) + 1;
                  portResetAt (Port) := Now;
                  health.Port_Resets := health.Port_Resets + 1;
                  Count := 0;
                  PM.Recover (ports (Port));
                  for S in Device_Index loop
                     if slotPort (S) = Port and then not slotRetired (S) then
                        Retire_Slot (S);
                        Count := Count + 1;
                     end if;
                  end loop;
                  Log ("port " & D (Natural (Port)) & " recovery port-reset " &
                       D (Natural (portResets (Port))) & "/" & D (Natural (ER.Port_Reset_Limit)) &
                       " (software replug) slots=" & D (Count) & " " & Port_Text (Read_Port (Port)), True);
                  Try_Complete_Release (Port);
               end if;
            end if;
         end if;
      end loop;
   end Run_Port_Recovery;

   ---------------------------------------------------------------------------
   --  Interrupts and service
   ---------------------------------------------------------------------------
   procedure Enable_Runtime_Interrupts
     (mode    : Runtime_Interrupt_Mode;
      enabled : out Boolean)
   is
      Now : constant Unsigned_64 := Now_Ms;
   begin
      enabled := False;
      --  Port machines from what boot enumeration left; acknowledge every
      --  change bit so the next change raises a Port Status Change event
      --  (4.19.2). A connected port without a slot (it arrived late, or its
      --  boot enumeration failed and released the slot) starts debouncing.
      for Port in 1 .. maxPorts loop
         declare
            Status : constant Unsigned_32 := Read_Port (Port);
         begin
            if Changes (Status) /= 0 then
               Write32 (operational, PORTSC (Port), Acknowledge_Changes (Status));
            end if;
            ports (Port) := PM.Booted (Connected (Status), Slots_Of (Port) > 0, Now);
         end;
      end loop;
      hotplugArmed := True;
      healthDue := PM.Plus (Now, Health_Interval_Ms);
      healthWindowStart := Now;
      tickDue := PM.Plus (Now, Tick_Interval_Ms);
      healthBefore := health;
      if mode /= INTERRUPT_POLLING then
         --  Clear a pending indication while enabling the interrupter, then
         --  let the controller signal (5.5.2.1, 5.4.1 INTE).
         Write32 (runtimeBase + RT_INTR0, INTR_IMAN, IMAN_IP or IMAN_IE);
         Write32 (operational, OP_USBCMD, Read32 (operational, OP_USBCMD) or USBCMD_INTE);
         enabled := True;
      end if;
      ehbPending := True;
      Stage ("irq", True, "mode=" &
             (case mode is when INTERRUPT_POLLING => "polled", when INTERRUPT_MSI => "msi",
                           when INTERRUPT_MSIX => "msix") &
             " iman=" & H32 (Read32 (runtimeBase + RT_INTR0, INTR_IMAN)) & " imod=" &
             H32 (Read32 (runtimeBase + RT_INTR0, INTR_IMOD)) & " usbcmd=" &
             H32 (Read32 (operational, OP_USBCMD)) & " hotplug=armed");
   end Enable_Runtime_Interrupts;

   procedure Acknowledge_Runtime_Interrupt is
   begin
      --  Linux's order (xhci_irq): USBSTS.EINT, then IMAN.IP (both RW1C;
      --  with MSI the controller clears IP itself, 5.5.2.1, and writing one
      --  to a clear bit does nothing), then the events, then ERDP with EHB.
      Write32 (operational, OP_USBSTS, USBSTS_EINT);
      Write32 (runtimeBase + RT_INTR0, INTR_IMAN, IMAN_IP or IMAN_IE);
      health.Interrupts := health.Interrupts + 1;
      ehbPending := True;
   end Acknowledge_Runtime_Interrupt;

   function Per_Second (Delta_Value, Window : Unsigned_64) return Unsigned_64 is
     (if Window = 0 then 0 else Delta_Value * 1000 / Window);

   procedure Health_Line (Now : Unsigned_64) is
      Window : constant Unsigned_64 := PM.Since (Now, healthWindowStart);
      Status : constant Unsigned_32 := Read32 (operational, OP_USBSTS);
   begin
      Log ("health ev/s=" & D (Per_Second (health.Events - healthBefore.Events, Window)) &
           " irq/s=" & D (Per_Second (health.Interrupts - healthBefore.Interrupts, Window)) &
           " xfer=" & D (health.Transfers) & " cc=" & Code_Name (health.Last_Code) &
           " err=" & D (health.Stalls) & "/" & D (health.Babbles) & "/" &
           D (health.Transaction_Errors) & "/" & D (health.Other_Errors) &
           " rec=" & D (health.Recoveries) & " port-resets=" & D (health.Port_Resets) & " gaveup=" & D (health.Given_Up) &
           " psc=" & D (health.Port_Changes) & " att=" & D (health.Attaches) &
           " det=" & D (health.Detaches) & " full=" & D (health.Ring_Full) &
           " ehb=" & D (health.EHB_Stuck) & " late=" & D (health.Late_Events) &
           " wait=" & D (health.Mailbox_Waits) & " deq=" & D (events.Head) &
           (if (Status and (USBSTS_HSE or USBSTS_HCE or USBSTS_HCH)) /= 0
            then " usbsts=" & H32 (Status) else ""),
           (Status and (USBSTS_HSE or USBSTS_HCE)) /= 0);
      healthBefore := health;
      healthWindowStart := Now;
   end Health_Line;

   procedure Service (Progressed : out Boolean) is
      Now : constant Unsigned_64 := Now_Ms;
      Consumed : Natural;
      Interrupted : constant Boolean := ehbPending;
      Before : constant Unsigned_64 := health.Events;
   begin
      Progressed := False;
      if not hotplugArmed then
         return;
      end if;
      if Now >= tickDue then
         --  Safety net, once per tick: an interrupt-driven loop that finds
         --  events nobody signalled, or EHB still set with nothing pending,
         --  is evidence of a lost interrupt; servicing recovers it.
         if interruptMode /= INTERRUPT_POLLING and then not Interrupted then
            if (Read32 (runtimeBase + RT_INTR0, INTR_ERDP) and Low (R.EHB_Clear)) /= 0 then
               health.EHB_Stuck := health.EHB_Stuck + 1;
               ehbPending := True;
            end if;
            if R.Owned (events, Read32 (DMA (EVENT_RING_OFFSET + Unsigned_64 (events.Head) *
                                             R.TRB_Bytes), 12))
            then
               health.Late_Events := health.Late_Events + 1;
               --  Events waiting and not one interrupt since the last tick.
               if interruptsTrusted and then health.Interrupts = interruptsAtTick then
                  silentTicks := silentTicks + 1;
                  if silentTicks >= Silent_Ticks_Limit then
                     interruptsTrusted := False;
                     Stage ("irq", False, "no interrupt for" & Natural'Image (Silent_Ticks_Limit) &
                            " ticks with events pending; polling every 1 ms iman=" &
                            H32 (Read32 (runtimeBase + RT_INTR0, INTR_IMAN)) & " usbsts=" &
                            H32 (Read32 (operational, OP_USBSTS)) & " irqs=" & D (health.Interrupts));
                  end if;
               end if;
            elsif health.Interrupts /= interruptsAtTick then
               silentTicks := 0;
            end if;
         end if;
         interruptsAtTick := health.Interrupts;
         tickDue := PM.Plus (Now, Tick_Interval_Ms);
      end if;
      Drain (R.Event_Entries, Consumed);
      Poll_Command;
      Run_Releases;
      for K in Pipe_Kind loop
         Run_Recovery (K);
      end loop;
      Run_Port_Recovery;
      Run_Engine;
      for Port in 1 .. maxPorts loop
         if PM.Next_Deadline (ports (Port)) <= Now then
            Port_Step (Port, False);
         end if;
      end loop;
      Poll_Command;
      if Now >= healthDue then
         if health /= healthBefore or else PM.Since (Now, healthWindowStart) >= Quiet_Health_Interval_Ms then
            Health_Line (Now);
         end if;
         healthDue := PM.Plus (Now, Health_Interval_Ms);
      end if;
      Progressed := Consumed > 0 or else health.Events /= Before;
   end Service;

   function Interrupts_Trusted return Boolean is (interruptsTrusted);

   function Next_Deadline return Unsigned_64 is
      Deadline : Unsigned_64 := Unsigned_64'Min (healthDue, tickDue);
      --  Work waiting for the command ring or EP0: look again in a tick.
      Soon : constant Unsigned_64 := PM.Plus (Now_Ms, 1);
   begin
      if not hotplugArmed then
         return Unsigned_64'Last;
      end if;
      for Port in 1 .. maxPorts loop
         Deadline := Unsigned_64'Min (Deadline, PM.Next_Deadline (ports (Port)));
      end loop;
      if command.Busy then
         Deadline := Unsigned_64'Min (Deadline, command.Deadline);
      end if;
      Deadline := Unsigned_64'Min (Deadline, portRecoveryDue);
      if engine.Step = Engine_Address_Recovery then
         Deadline := Unsigned_64'Min (Deadline, engine.Wait_Until);
      end if;
      if engine.Control.Active then
         Deadline := Unsigned_64'Min (Deadline, engine.Control.Deadline);
      end if;
      if (engine.Step /= Engine_Idle and then not engine.Issued) or else
        (disablingSlot = 0 and then (for some S in Device_Index => slotRetired (S)))
      then
         Deadline := Unsigned_64'Min (Deadline, Soon);
      end if;
      for K in Pipe_Kind loop
         if ER.Issuing (pipes (K).Recovery) or else
           pipes (K).Recovery.Current in ER.Need_Doorbell | ER.Need_Requeue
         then
            Deadline := Unsigned_64'Min (Deadline, Soon);
         elsif pipes (K).Control.Active then
            Deadline := Unsigned_64'Min (Deadline, pipes (K).Control.Deadline);
         end if;
      end loop;
      for Port in 1 .. maxPorts loop
         if ports (Port).Current = PM.Ready and then engine.Step = Engine_Idle then
            Deadline := Unsigned_64'Min (Deadline, Soon);
         end if;
      end loop;
      return Deadline;
   end Next_Deadline;

   ---------------------------------------------------------------------------
   --  HID reports
   ---------------------------------------------------------------------------
   procedure Poll_Boot_Keyboard
     (Data : out USB_Keyboards.Report; Ready, Progressed : out Boolean)
   is
      Found : Boolean;
      Disposition : ER.Disposition;
      Buffer : Unsigned_64;
      Actual : Natural;
      Code : Completion_Code;
   begin
      Data := [others => 0];
      Ready := False;
      Take_Pipe_Event (Keyboard_Pipe, Found, Disposition, Buffer, Actual, Code);
      Progressed := Found;
      if Found and then Disposition = ER.Deliver then
         ER.Delivered (pipes (Keyboard_Pipe).Recovery);
         if Actual = BOOT_KEYBOARD_REPORT then
            for I in Data'Range loop
               Data (I) := Read8 (DMA (Buffer), Storage_Offset (I - 1));
            end loop;
            Ready := True;
         end if;
      end if;
   end Poll_Boot_Keyboard;

   function Mouse_Diagnostics return Boot_Mouse_Diagnostics is (mouseDiagnostics);

   function Signed (Value : Unsigned_8) return Integer is
     (if Value < 128 then Integer (Value) else Integer (Value) - 256);

   procedure Poll_Boot_Mouse
     (buttons : out Unsigned_8;
      deltaX  : out Integer;
      deltaY  : out Integer;
      deltaZ  : out Integer;
      ready   : out Boolean;
      eventAvailable : out Boolean)
   is
      Found : Boolean;
      Disposition : ER.Disposition;
      Buffer : Unsigned_64;
      Actual : Natural;
      Code : Completion_Code;
      Button_Mask : constant Unsigned_8 := 7;
      function Byte (I : Natural) return Unsigned_8 is (Read8 (DMA (Buffer), Storage_Offset (I)));
   begin
      buttons := 0;
      deltaX := 0;
      deltaY := 0;
      deltaZ := 0;
      ready := False;
      Take_Pipe_Event (Mouse_Pipe, Found, Disposition, Buffer, Actual, Code);
      eventAvailable := Found;
      if not Found then
         return;
      end if;
      mouseDiagnostics.transferEvents := mouseDiagnostics.transferEvents + 1;
      mouseDiagnostics.lastCompletion := Unsigned_8 (Code);
      if Disposition /= ER.Deliver then
         if Code /= CC_SUCCESS then
            mouseDiagnostics.completionErrors := mouseDiagnostics.completionErrors + 1;
         end if;
         return;
      end if;
      ER.Delivered (pipes (Mouse_Pipe).Recovery);
      mouseDiagnostics.lastLength := Unsigned_8 (Natural'Min (Actual, Natural (Unsigned_8'Last)));
      mouseDiagnostics.lastReport :=
        Unsigned_32 (Byte (0)) or Shift_Left (Unsigned_32 (Byte (1)), 8) or
        Shift_Left (Unsigned_32 (Byte (2)), 16) or Shift_Left (Unsigned_32 (Byte (3)), 24);
      if Actual < BOOT_MOUSE_MINIMUM_REPORT then
         mouseDiagnostics.shortReports := mouseDiagnostics.shortReports + 1;
         return;
      end if;
      buttons := Byte (0) and Button_Mask;
      deltaX := Signed (Byte (1));
      deltaY := Signed (Byte (2));
      --  Wheel mice commonly append one signed byte to the boot report.
      if Actual > BOOT_MOUSE_MINIMUM_REPORT then
         deltaZ := Signed (Byte (3));
      end if;
      mouseDiagnostics.decodedReports := mouseDiagnostics.decodedReports + 1;
      if deltaX /= 0 or else deltaY /= 0 then
         mouseDiagnostics.motionReports := mouseDiagnostics.motionReports + 1;
      end if;
      if buttons /= diagnosticButtons then
         mouseDiagnostics.buttonTransitions := mouseDiagnostics.buttonTransitions + 1;
         diagnosticButtons := buttons;
      end if;
      ready := True;
   end Poll_Boot_Mouse;

end XHCI;
