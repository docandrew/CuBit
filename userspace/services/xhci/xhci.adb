------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  xHCI controller reset and ring initialization.
--
--  Reference: Intel xHCI Requirements Specification, revision 1.2b,
--  sections 4.2, 5.3, 5.4, 5.5, 6.4, and 6.5.
------------------------------------------------------------------------------
with System; use System;
with System.Address_To_Access_Conversions;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with USB_Configurations;
with XHCI_Completions;
with XHCI_Capabilities;
with XHCI_DMA_Layout; use XHCI_DMA_Layout;
with XHCI_Ports;
with XHCI_Legacy;
with USB_Optical;

package body XHCI is

   DMA_VIRT_BASE : constant Unsigned_64 := 16#0000_7000_0000_0000#;
   BAR_VIRT_BASE : constant Unsigned_64 := 16#0000_6000_0000_0000#;

   --  Capability registers.
   REG_CAPLENGTH  : constant Storage_Offset := 16#00#;
   REG_HCSPARAMS1 : constant Storage_Offset := 16#04#;
   REG_HCSPARAMS2 : constant Storage_Offset := 16#08#;
   REG_HCCPARAMS1 : constant Storage_Offset := 16#10#;
   REG_DBOFF      : constant Storage_Offset := 16#14#;
   REG_RTSOFF     : constant Storage_Offset := 16#18#;

   --  Operational registers, relative to CAPLENGTH.
   OP_USBCMD  : constant Storage_Offset := 16#00#;
   OP_USBSTS  : constant Storage_Offset := 16#04#;
   OP_PAGESIZE : constant Storage_Offset := 16#08#;
   OP_CRCR    : constant Storage_Offset := 16#18#;
   OP_DCBAAP  : constant Storage_Offset := 16#30#;
   OP_CONFIG  : constant Storage_Offset := 16#38#;
   OP_PORTSC_BASE : constant Storage_Offset := 16#400#;
   OP_PORT_STRIDE : constant Storage_Offset := 16#10#;

   --  Primary interrupter registers, relative to RTSOFF + 0x20.
   RT_INTR0   : constant Storage_Offset := 16#20#;
   INTR_IMAN  : constant Storage_Offset := 16#00#;
   INTR_IMOD  : constant Storage_Offset := 16#04#;
   INTR_ERSTSZ : constant Storage_Offset := 16#08#;
   INTR_ERSTBA : constant Storage_Offset := 16#10#;
   INTR_ERDP   : constant Storage_Offset := 16#18#;

   USBCMD_RUN  : constant Unsigned_32 := 16#0000_0001#;
   USBCMD_HCRST : constant Unsigned_32 := 16#0000_0002#;
   USBCMD_INTE : constant Unsigned_32 := 16#0000_0004#;
   IMAN_IP     : constant Unsigned_32 := 16#0000_0001#;
   IMAN_IE     : constant Unsigned_32 := 16#0000_0002#;
   USBSTS_HCH  : constant Unsigned_32 := 16#0000_0001#;
   USBSTS_CNR  : constant Unsigned_32 := 16#0000_0800#;
   PORTSC_CCS  : constant Unsigned_32 := 16#0000_0001#;
   PORTSC_PED  : constant Unsigned_32 := 16#0000_0002#;
   PORTSC_PR   : constant Unsigned_32 := 16#0000_0010#;
   PORTSC_CHANGE_BITS : constant Unsigned_32 := 16#00FE_0000#;

   TRB_CYCLE       : constant Unsigned_32 := 1;
   TRB_TOGGLE_CYCLE : constant Unsigned_32 := 2;
   TRB_TYPE_SHIFT  : constant Natural := 10;
   TRB_TYPE_LINK   : constant Unsigned_32 := 6;
   TRB_TYPE_ENABLE_SLOT : constant Unsigned_32 := 9;
   TRB_TYPE_ADDRESS_DEVICE : constant Unsigned_32 := 11;
   TRB_TYPE_CONFIGURE_ENDPOINT : constant Unsigned_32 := 12;
   TRB_TYPE_NORMAL : constant Unsigned_32 := 1;
   TRB_TYPE_SETUP_STAGE : constant Unsigned_32 := 2;
   TRB_TYPE_DATA_STAGE : constant Unsigned_32 := 3;
   TRB_TYPE_STATUS_STAGE : constant Unsigned_32 := 4;
   TRB_TYPE_TRANSFER_EVENT : constant Unsigned_32 := 32;
   TRB_TYPE_COMMAND_COMPLETION : constant Unsigned_32 := 33;
   TRB_TYPE_MASK    : constant Unsigned_32 := 16#0000_FC00#;
   COMPLETION_SUCCESS : constant Unsigned_32 := 1;
   COMPLETION_SHORT_PACKET : constant Unsigned_32 := 13;
   TRB_IOC : constant Unsigned_32 := 16#20#;
   TRB_IDT : constant Unsigned_32 := 16#40#;
   TRB_DIRECTION_IN : constant Unsigned_32 := 16#0001_0000#;

   COMMAND_RING_ENTRIES : constant Natural := 64;
   EVENT_RING_ENTRIES   : constant Natural := 64;
   HID_REPORT_STRIDE    : constant Natural := 64;
   HID_TRANSFER_DEPTH   : constant Natural := 8;

   subtype TRB is XHCI_Completions.Event;

   NULL_TRB : constant TRB :=
     (parameterLo => 0, parameterHi => 0, status => 0, control => 0);

   type TRB_Array is array (Natural range <>) of TRB
     with Convention => C, Volatile_Components;

   type Address_Array is array (Natural range <>) of Unsigned_64
     with Convention => C;

   type DWord_Array is array (Natural range <>) of Unsigned_32
     with Convention => C, Volatile_Components;

   type Byte_Array is array (Natural range <>) of Unsigned_8
     with Convention => C, Volatile_Components;

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

   barBase     : System.Address := System.Null_Address;
   barMappedBytes : Unsigned_64 := 0;
   operational : System.Address := System.Null_Address;
   runtimeBase : System.Address := System.Null_Address;
   doorbellBase : System.Address := System.Null_Address;
   maxPorts    : Natural := 0;
   connectedPorts : Natural := 0;
   activePort   : Natural := 0;
   activeSlot   : Natural := 0; -- Enumeration/control context, never HID identity.
   mouseSlot    : Natural := 0;
   storageSlot : Natural := 0;
   storageInDCI, storageOutDCI : Natural := 0;
   storageInterface : Unsigned_8 := 0;
   dmaPhysical  : Unsigned_64 := 0;
   commandTail  : Natural := 0;
   commandCycle : Unsigned_32 := TRB_CYCLE;
   eventHead    : Natural := 0;
   eventCycle   : Unsigned_32 := TRB_CYCLE;
   completionMailboxes : XHCI_Completions.Mailboxes;
   completionRoutingFailed : Boolean := False;
   subtype Device_Index is Positive range 1 .. XHCI_Completions.Maximum_Slots;
   type Control_State is record
      Tail : Natural := 0;
      Cycle : Unsigned_32 := TRB_CYCLE;
      Failed : Boolean := False;
   end record;
   lastControlCompletion : Unsigned_32 := 0;
   ep0State : array (Device_Index) of Control_State := [others => (others => <>)];
   hidTail      : Natural := 0;
   hidCycle     : Unsigned_32 := TRB_CYCLE;
   hidEndpointDCI : Natural := 0;
   hidMaxPacket : Natural := 0;
   hidTransfersStarted : Boolean := False;
   mouseDiagnostics : Boot_Mouse_Diagnostics;
   diagnosticButtons : Unsigned_8 := 0;
   type Bulk_Phase is (Bulk_Idle, Command_Out, Data_In, Status_In,
                       Bulk_Finished, Bulk_Faulted);
   bulkPhase : Bulk_Phase := Bulk_Idle;
   type Bulk_Cursor is record
      Tail : Natural range 0 .. COMMAND_RING_ENTRIES - 2 := 0;
      Cycle : Unsigned_32 := TRB_CYCLE;
   end record;
   bulkIn, bulkOut : Bulk_Cursor;
   bulkExpectedPointer : Unsigned_64 := 0;
   bulkExpectedLength : Unsigned_32 := 0;
   bulkDeadline : Unsigned_64 := 0;
   bulkTag : Unsigned_32 := 0;
   bulkCommand : USB_Optical.Command;
   bulkReceived : Unsigned_32 := 0;
   bulkResult : USB_Optical.Status_Result := USB_Optical.Invalid_Status;
   opticalLUN : USB_Optical.Logical_Unit := 0;
   opticalBlocks : Unsigned_64 := 0;
   bulkData : USB_Optical.Bytes (1 .. 32768) with Import, Volatile,
     Address => To_Address (Integer_Address (DMA_VIRT_BASE + BULK_DATA_OFFSET));
   bulkCBW : USB_Optical.Command_Wrapper with Import, Volatile,
     Address => To_Address (Integer_Address (DMA_VIRT_BASE + BULK_CBW_OFFSET));
   bulkCSW : USB_Optical.Bytes (1 .. 13) with Import, Volatile,
     Address => To_Address (Integer_Address (DMA_VIRT_BASE + BULK_CSW_OFFSET));

   dcbaa : Address_Array (0 .. 255) with Import,
     Address => To_Address (Integer_Address (DMA_VIRT_BASE + DCBAA_OFFSET));
   commandRing : TRB_Array (0 .. COMMAND_RING_ENTRIES - 1) with Import,
     Address => To_Address
       (Integer_Address (DMA_VIRT_BASE + COMMAND_RING_OFFSET));
   eventRing : TRB_Array (0 .. EVENT_RING_ENTRIES - 1) with Import,
     Address => To_Address
       (Integer_Address (DMA_VIRT_BASE + EVENT_RING_OFFSET));
   erst : ERST_Entry with Import,
     Address => To_Address (Integer_Address (DMA_VIRT_BASE + ERST_OFFSET));
   type Device_DMA_Area is record
      deviceContext, inputContext : DWord_Array (0 .. 1023);
      ep0Ring, hidRing : TRB_Array (0 .. 255);
      descriptorBytes, hidReports : Byte_Array (0 .. 4095);
      bulkOutRing, bulkInRing : TRB_Array (0 .. 255);
   end record with Convention => C, Size => DEVICE_PAGES * 4096 * 8;
   type Device_DMA_Array is array (Device_Index) of Device_DMA_Area
     with Convention => C;
   pragma Compile_Time_Error
     (Device_DMA_Array'Size /= MAX_DEVICE_SLOTS * DEVICE_PAGES * 4096 * 8,
      "native device DMA array does not match shared allocation layout");
   deviceDMA : Device_DMA_Array with Import,
     Address => To_Address
       (Integer_Address (DMA_VIRT_BASE + DEVICE_CONTEXT_OFFSET));

   function Device_Offset (Slot : Device_Index; Offset : Unsigned_64)
      return Unsigned_64 is
     (Offset + Unsigned_64 (Slot - 1) * DEVICE_STRIDE);

   --  MMIO must remain an observable operation at every call site.  An
   --  imported local object with a run-time Address aspect can be hoisted by
   --  the optimizer when used inside a polling loop.  Volatile_Full_Access on
   --  the pointed-to type gives both the compiler barrier and the indivisible
   --  access width required by xHCI registers.
   type MMIO_Byte is mod 2 ** 8
     with Size => 8, Volatile_Full_Access;
   type MMIO_DWord is mod 2 ** 32
     with Size => 32, Volatile_Full_Access;

   package Byte_Access is new
     System.Address_To_Access_Conversions (MMIO_Byte);
   package DWord_Access is new
     System.Address_To_Access_Conversions (MMIO_DWord);

   function Read8
     (base : System.Address; offset : Storage_Offset) return Unsigned_8
   is (Unsigned_8 (Byte_Access.To_Pointer (base + offset).all));

   function Read32
     (base : System.Address; offset : Storage_Offset) return Unsigned_32
   is (Unsigned_32 (DWord_Access.To_Pointer (base + offset).all));

   procedure Write32
     (base : System.Address; offset : Storage_Offset; value : Unsigned_32)
   is
   begin
      DWord_Access.To_Pointer (base + offset).all := MMIO_DWord (value);
   end Write32;

   procedure Write64
     (base : System.Address; offset : Storage_Offset; value : Unsigned_64)
   is
   begin
      --  xHCI permits naturally aligned 64-bit register accesses as two
      --  32-bit operations, low dword first.
      Write32 (base, offset, Unsigned_32 (value and 16#FFFF_FFFF#));
      Write32 (base, offset + 4, Unsigned_32 (Shift_Right (value, 32)));
   end Write64;

   procedure Clear_DMA (Layout : Allocation) is
      bytes : array (Natural range 0 .. Pages (Layout) * 4096 - 1) of Unsigned_8
        with Import, Address => To_Address (Integer_Address (DMA_VIRT_BASE));
   begin
      for i in bytes'Range loop
         bytes (i) := 0;
      end loop;
   end Clear_DMA;

   procedure Debug_Hex32 (labelText : String; value : Unsigned_32);

   function Wait_For_Bits
     (base      : System.Address;
      offset    : Storage_Offset;
      mask      : Unsigned_32;
      expected  : Unsigned_32;
      attempts  : Positive) return Boolean
   is
      ignore : Unsigned_64;
   begin
      if (Read32 (base, offset) and mask) = expected then
         return True;
      end if;
      for attempt in 1 .. attempts loop
         if attempt = 1 then
            Debug_Hex32 ("xhci: wait register offset=", Unsigned_32 (offset));
            debugPrint ("xhci: wait entering sleep(1ms)" & ASCII.LF);
         end if;
         ignore := syscall (SYSCALL_SLEEP, 1);
         if attempt = 1 then
            debugPrint ("xhci: wait resumed from sleep" & ASCII.LF);
         elsif attempt mod 1000 = 0 then
            Debug_Hex32 ("xhci: wait iterations completed=", Unsigned_32 (attempt));
            Debug_Hex32 ("xhci: wait register value=", Read32 (base, offset));
         end if;
         if (Read32 (base, offset) and mask) = expected then
            return True;
         end if;
      end loop;
      --  Include a terminal observation outside the tight polling loop.
      --  Besides defining the timeout boundary precisely, this lets an
      --  emulator commit a deferred MMIO state transition at the loop edge.
      return (Read32 (base, offset) and mask) = expected;
   end Wait_For_Bits;

   procedure Acknowledge_Event is
      dequeue : constant Unsigned_64 :=
        dmaPhysical + EVENT_RING_OFFSET + Unsigned_64 (eventHead * 16);
   begin
      --  EHB is RW1C.  Advancing ERDP and setting EHB acknowledges the event
      --  without enabling interrupts; this first driver deliberately polls.
      Write64 (runtimeBase + RT_INTR0, INTR_ERDP, dequeue or 16#8#);
   end Acknowledge_Event;

   function Poll_Hardware_Event (event : out TRB) return Boolean is
      control : Unsigned_32;
   begin
      control := eventRing (eventHead).control;
      if (control and TRB_CYCLE) /= eventCycle then
         event := NULL_TRB;
         return False;
      end if;

      event := eventRing (eventHead);
      if eventHead = EVENT_RING_ENTRIES - 1 then
         eventHead := 0;
         eventCycle := eventCycle xor TRB_CYCLE;
      else
         eventHead := eventHead + 1;
      end if;
      Acknowledge_Event;
      return True;
   end Poll_Hardware_Event;

   --  All hardware events pass through one owner. A command/control wait
   --  leaves unrelated endpoint completions available to their consumer.
   function Collect_Event return Boolean is
      event : TRB;
      result : XHCI_Completions.Route_Result;
   begin
      if completionRoutingFailed or else not Poll_Hardware_Event (event) then
         return False;
      end if;
      XHCI_Completions.Route (completionMailboxes, event, result);
      case result is
         when XHCI_Completions.Queued | XHCI_Completions.Not_A_Completion =>
            null;
         when XHCI_Completions.Invalid_Target | XHCI_Completions.Queue_Full =>
            --  Never silently overwrite a completion or reuse its DMA page.
            --  Stop new submissions and request controller halt. The private
            --  DMA allocation remains retained, even if halt itself fails.
            completionRoutingFailed := True;
            Write32 (operational, OP_USBCMD, 0);
            debugPrint ("xhci: completion routing fault; halt requested" & ASCII.LF);
      end case;
      return True;
   end Collect_Event;

   procedure Next_Completion
     (slot : XHCI_Completions.Slot_Number;
      endpoint : XHCI_Completions.Endpoint_Number;
      event : out TRB; found, progressed : out Boolean)
   is
   begin
      event := NULL_TRB;
      found := False;
      progressed := False;
      if completionRoutingFailed then
         return;
      end if;
      XHCI_Completions.Take (completionMailboxes, slot, endpoint, event, found);
      progressed := found;
      if not found then
         progressed := Collect_Event;
         if not completionRoutingFailed then
            XHCI_Completions.Take
              (completionMailboxes, slot, endpoint, event, found);
         end if;
      end if;
   end Next_Completion;

   procedure Submit_Command
     (command        : TRB;
      completionCode : out Unsigned_32;
      slotId         : out Natural;
      completed      : out Boolean)
   is
      pending       : TRB := command;
      event         : TRB;
      ignore        : Unsigned_64;
      commandPhys   : Unsigned_64;
      eventType     : Unsigned_32;
      eventPointer  : Unsigned_64;
      found, progressed : Boolean;
   begin
      completionCode := 0;
      slotId := 0;
      completed := False;
      if completionRoutingFailed then
         return;
      end if;
      commandPhys :=
        dmaPhysical + COMMAND_RING_OFFSET + Unsigned_64 (commandTail * 16);
      pending.control :=
        (pending.control and not TRB_CYCLE) or commandCycle;
      commandRing (commandTail) := pending;

      if commandTail = COMMAND_RING_ENTRIES - 2 then
         commandRing (COMMAND_RING_ENTRIES - 1).control :=
           Shift_Left (TRB_TYPE_LINK, TRB_TYPE_SHIFT) or
           TRB_TOGGLE_CYCLE or commandCycle;
         commandTail := 0;
         commandCycle := commandCycle xor TRB_CYCLE;
      else
         commandTail := commandTail + 1;
      end if;

      Write32 (doorbellBase, 0, 0);
      for attempt in 1 .. 10_000 loop
         Next_Completion (0, 0, event, found, progressed);
         if completionRoutingFailed then
            return;
         end if;
         if found then
            eventType := Shift_Right (event.control and TRB_TYPE_MASK, 10);
            if eventType = TRB_TYPE_COMMAND_COMPLETION then
               eventPointer := Unsigned_64 (event.parameterLo) or
                 Shift_Left (Unsigned_64 (event.parameterHi), 32);
               if (eventPointer and not Unsigned_64 (16#F#)) = commandPhys then
                  completionCode := Shift_Right (event.status, 24);
                  slotId := Natural (Shift_Right (event.control, 24));
                  completed := True;
                  return;
               end if;
            end if;
         elsif not progressed then
            ignore := syscall (SYSCALL_SLEEP, 1);
         end if;
      end loop;
   end Submit_Command;

   procedure Debug_Hex32 (labelText : String; value : Unsigned_32) is
      hex : constant String := "0123456789ABCDEF";
      text : String (1 .. 8);
   begin
      for i in text'Range loop
         text (i) :=
           hex
             (Natural
                (Shift_Right (value, (text'Last - i) * 4) and 16#F#) + 1);
      end loop;
      debugPrint (labelText & text & ASCII.LF);
   end Debug_Hex32;

   function Firmware_Handoff (HCC : Unsigned_32) return Boolean is
      Offset : Unsigned_64 := XHCI_Legacy.First_Offset (HCC);
      Header, Control : Unsigned_32;
      Distance : Unsigned_64;
   begin
      --  Every nonterminal link advances by at least one dword. A BAR-sized
      --  bound prevents an unbounded walk; validate each access before MMIO.
      for Entry_Number in 1 .. Natural (barMappedBytes / 4) loop
         if Offset = 0 then
            debugPrint ("xhci: no firmware ownership capability" & ASCII.LF);
            return True;
         elsif not XHCI_Legacy.Fits (Offset, 4, barMappedBytes) then
            debugPrint ("xhci: invalid extended capability range" & ASCII.LF);
            return False;
         end if;
         Header := Read32 (barBase, Storage_Offset (Offset));
         if Header = Unsigned_32'Last or else (Header and 255) = 0 then
            debugPrint ("xhci: invalid extended capability header" & ASCII.LF);
            return False;
         end if;
         if (Header and 255) = XHCI_Legacy.LEGACY_ID then
            if not XHCI_Legacy.Fits (Offset, 8, barMappedBytes) then
               return False;
            end if;
            Debug_Hex32 ("xhci: firmware ownership before=", Header);
            --  Request OS ownership; never forcibly clear BIOS ownership.
            Write32 (barBase, Storage_Offset (Offset), Header or XHCI_Legacy.OS_OWNED);
            if not Wait_For_Bits (barBase, Storage_Offset (Offset),
              XHCI_Legacy.BIOS_OWNED or XHCI_Legacy.OS_OWNED,
              XHCI_Legacy.OS_OWNED, 1000)
            then
               Debug_Hex32 ("xhci: firmware ownership timeout=",
                 Read32 (barBase, Storage_Offset (Offset)));
               --  Do not forcibly clear firmware ownership and race the BIOS.
               return False;
            end if;
            Control := Read32 (barBase, Storage_Offset (Offset + 4));
            Write32 (barBase, Storage_Offset (Offset + 4),
              XHCI_Legacy.Disable_SMIs (Control));
            Debug_Hex32 ("xhci: firmware ownership acquired=",
              Read32 (barBase, Storage_Offset (Offset)));
            return True;
         end if;
         Distance := XHCI_Legacy.Next_Distance (Header);
         Offset := (if Distance = 0 then 0 else Offset + Distance);
      end loop;
      debugPrint ("xhci: extended capability walk exhausted" & ASCII.LF);
      return False;
   end Firmware_Handoff;

   procedure Queue_EP0 (item : TRB) is
      pending : TRB := item;
   begin
      pending.control := (pending.control and not TRB_CYCLE) or ep0State (activeSlot).Cycle;
      deviceDMA (activeSlot).ep0Ring (ep0State (activeSlot).Tail) := pending;
      if ep0State (activeSlot).Tail = COMMAND_RING_ENTRIES - 2 then
         deviceDMA (activeSlot).ep0Ring (COMMAND_RING_ENTRIES - 1).control :=
           Shift_Left (TRB_TYPE_LINK, TRB_TYPE_SHIFT) or
           TRB_TOGGLE_CYCLE or ep0State (activeSlot).Cycle;
         ep0State (activeSlot).Tail := 0;
         ep0State (activeSlot).Cycle := ep0State (activeSlot).Cycle xor TRB_CYCLE;
      else
         ep0State (activeSlot).Tail := ep0State (activeSlot).Tail + 1;
      end if;
   end Queue_EP0;

   procedure Control_Request
     (requestType   : Unsigned_8;
      request       : Unsigned_8;
      value         : Unsigned_16;
      index         : Unsigned_16;
      length        : Natural;
      bufferPhys    : Unsigned_64;
      actualLength  : out Natural;
      success       : out Boolean)
   is
      setupLo       : Unsigned_32;
      setupHi       : Unsigned_32;
      setupControl  : Unsigned_32;
      statusControl : Unsigned_32;
      completion    : Unsigned_32;
      setupPhys, dataPhys, statusPhys, eventPhys : Unsigned_64 := 0;
      event : TRB;
      found, progressed, dataCompleted : Boolean := False;
      residual : Unsigned_32;
      ignore : Unsigned_64;
      dataIn        : constant Boolean := (requestType and 16#80#) /= 0;
   begin
      actualLength := 0;
      success := False;
      lastControlCompletion := 0;
      if length > 4096 or else completionRoutingFailed or else
         ep0State (activeSlot).Failed
      then
         return;
      end if;

      setupLo := Unsigned_32 (requestType) or
        Shift_Left (Unsigned_32 (request), 8) or
        Shift_Left (Unsigned_32 (value), 16);
      setupHi := Unsigned_32 (index) or
        Shift_Left (Unsigned_32 (length), 16);
      setupControl := Shift_Left (TRB_TYPE_SETUP_STAGE, TRB_TYPE_SHIFT) or
        TRB_IDT;
      if length > 0 then
         if dataIn then
            setupControl := setupControl or
              Shift_Left (Unsigned_32 (3), 16);
         else
            setupControl := setupControl or
              Shift_Left (Unsigned_32 (2), 16);
         end if;
      end if;
      setupPhys := dmaPhysical + Device_Offset (activeSlot, EP0_RING_OFFSET) +
        Unsigned_64 (ep0State (activeSlot).Tail * 16);
      Queue_EP0
        ((parameterLo => setupLo, parameterHi => setupHi, status => 8,
          control => setupControl));

      if length > 0 then
         dataPhys := dmaPhysical + Device_Offset (activeSlot, EP0_RING_OFFSET) +
           Unsigned_64 (ep0State (activeSlot).Tail * 16);
         Queue_EP0
           ((parameterLo => Unsigned_32 (bufferPhys and 16#FFFF_FFFF#),
             parameterHi => Unsigned_32 (Shift_Right (bufferPhys, 32)),
             status => Unsigned_32 (length),
             control => Shift_Left (TRB_TYPE_DATA_STAGE, TRB_TYPE_SHIFT) or
               TRB_IOC or 4 or (if dataIn then TRB_DIRECTION_IN else 0)));
      end if;

      statusControl := Shift_Left (TRB_TYPE_STATUS_STAGE, TRB_TYPE_SHIFT) or
        TRB_IOC;
      if length = 0 or else not dataIn then
         statusControl := statusControl or TRB_DIRECTION_IN;
      end if;
      statusPhys := dmaPhysical + Device_Offset (activeSlot, EP0_RING_OFFSET) +
        Unsigned_64 (ep0State (activeSlot).Tail * 16);
      Queue_EP0
        ((parameterLo => 0, parameterHi => 0, status => 0,
          control => statusControl));

      Write32 (doorbellBase, Storage_Offset (activeSlot * 4), 1);
      for attempt in 1 .. 2_000 loop
         Next_Completion (activeSlot, 1, event, found, progressed);
         exit when completionRoutingFailed;
         if found then
            eventPhys := Unsigned_64 (event.parameterLo) or
              Shift_Left (Unsigned_64 (event.parameterHi), 32);
            completion := Shift_Right (event.status, 24);
            residual := event.status and 16#00FF_FFFF#;
            lastControlCompletion := completion;
            if (event.control and 4) /= 0 or else
               eventPhys not in setupPhys | dataPhys | statusPhys
            then
               exit;
            elsif completion /= COMPLETION_SUCCESS and then
                  completion /= COMPLETION_SHORT_PACKET
            then
               Debug_Hex32 ("xhci: control completion=", completion);
               exit;
            elsif length > 0 and then eventPhys = dataPhys then
               exit when dataCompleted or else residual > Unsigned_32 (length);
               actualLength := length - Natural (residual);
               dataCompleted := True;
            elsif eventPhys = statusPhys and then
                  completion = COMPLETION_SUCCESS and then residual = 0 and then
                  (length = 0 or else dataCompleted)
            then
               success := True;
               return;
            else
               exit;
            end if;
         elsif not progressed then
            ignore := syscall (SYSCALL_SLEEP, 1);
         end if;
      end loop;
      --  Keep this slot's DMA/ring state quarantined after a timeout/error.
      --  An acknowledged endpoint recovery, not another Setup TRB, clears it.
      ep0State (activeSlot).Failed := True;
   end Control_Request;

   function Recover_Control_Stall return Boolean is
      code : Unsigned_32;
      slot : Natural;
      done : Boolean;
      pointer : constant Unsigned_64 :=
        dmaPhysical + Device_Offset (activeSlot, EP0_RING_OFFSET) +
        Unsigned_64 (ep0State (activeSlot).Tail * 16);
   begin
      if lastControlCompletion /= 6 or else not ep0State (activeSlot).Failed then
         return False;
      end if;
      Submit_Command ((0, 0, 0, Shift_Left (14, 10) or Shift_Left (1, 16) or
        Shift_Left (Unsigned_32 (activeSlot), 24)), code, slot, done);
      if not done or else code /= COMPLETION_SUCCESS then return False; end if;
      Submit_Command
        ((Unsigned_32 (pointer and 16#FFFF_FFFF#) or ep0State (activeSlot).Cycle,
          Unsigned_32 (Shift_Right (pointer, 32)), 0,
          Shift_Left (16, 10) or Shift_Left (1, 16) or
          Shift_Left (Unsigned_32 (activeSlot), 24)), code, slot, done);
      if not done or else code /= COMPLETION_SUCCESS then return False; end if;
      ep0State (activeSlot).Failed := False;
      return True;
   end Recover_Control_Stall;

   procedure Queue_Bulk (Input : Boolean; Offset : Unsigned_64; Length : Unsigned_32) is
      cursor : Bulk_Cursor := (if Input then bulkIn else bulkOut);
      ringOffset : constant Unsigned_64 :=
        (if Input then BULK_IN_RING_OFFSET else BULK_OUT_RING_OFFSET);
      ringPhys : constant Unsigned_64 := dmaPhysical + Device_Offset (storageSlot, ringOffset);
      bufferPhys : constant Unsigned_64 := dmaPhysical + Offset;
      item : TRB;
   begin
      bulkExpectedPointer := ringPhys + Unsigned_64 (cursor.Tail * 16);
      bulkExpectedLength := Length;
      bulkDeadline := syscall (SYSCALL_GETTIME) + 2_000;
      item := (Unsigned_32 (bufferPhys and 16#FFFF_FFFF#),
        Unsigned_32 (Shift_Right (bufferPhys, 32)), Length,
        Shift_Left (TRB_TYPE_NORMAL, TRB_TYPE_SHIFT) or TRB_IOC or
        (if Input then 4 else 0) or cursor.Cycle);
      if Input then deviceDMA (storageSlot).bulkInRing (cursor.Tail) := item;
      else deviceDMA (storageSlot).bulkOutRing (cursor.Tail) := item; end if;
      if cursor.Tail = COMMAND_RING_ENTRIES - 2 then
         if Input then
            deviceDMA (storageSlot).bulkInRing (COMMAND_RING_ENTRIES - 1).control :=
              Shift_Left (TRB_TYPE_LINK, TRB_TYPE_SHIFT) or TRB_TOGGLE_CYCLE or cursor.Cycle;
         else
            deviceDMA (storageSlot).bulkOutRing (COMMAND_RING_ENTRIES - 1).control :=
              Shift_Left (TRB_TYPE_LINK, TRB_TYPE_SHIFT) or TRB_TOGGLE_CYCLE or cursor.Cycle;
         end if;
         cursor.Tail := 0;
         cursor.Cycle := cursor.Cycle xor TRB_CYCLE;
      else cursor.Tail := cursor.Tail + 1; end if;
      if Input then bulkIn := cursor; else bulkOut := cursor; end if;
      Write32 (doorbellBase, Storage_Offset (storageSlot * 4),
        Unsigned_32 (if Input then storageInDCI else storageOutDCI));
   end Queue_Bulk;

   procedure Begin_Bulk
     (Item : USB_Optical.Command; LUN : USB_Optical.Logical_Unit;
      Accepted : out Boolean)
   is
   begin
      Accepted := False;
      if storageSlot = 0 or else completionRoutingFailed or else
         bulkPhase not in Bulk_Idle | Bulk_Finished or else bulkTag = Unsigned_32'Last
      then return; end if;
      bulkTag := bulkTag + 1;
      bulkCommand := Item;
      bulkReceived := 0;
      bulkResult := USB_Optical.Invalid_Status;
      bulkCBW := USB_Optical.Encode (Item, bulkTag, LUN);
      bulkPhase := Command_Out;
      Queue_Bulk (False, BULK_CBW_OFFSET, 31);
      Accepted := True;
   end Begin_Bulk;

   procedure Poll_Optical_Read
     (Done : out Boolean; Result : out USB_Optical.Status_Result;
      Progressed : out Boolean)
   is
      event : TRB;
      found : Boolean;
      pointer : Unsigned_64;
      residual, actual, completion : Unsigned_32;
      use type USB_Optical.Status_Result;
      procedure Fault is
      begin
         --  No caller memory is exposed to this DMA. Retain all private DMA
         --  buffers and forbid further submissions; recovery requires a
         --  quiesced device, not recycling an outstanding transfer's page.
         bulkPhase := Bulk_Faulted;
         opticalBlocks := 0;
         bulkResult := USB_Optical.Reset_Recovery_Required;
         debugPrint ("xhci: optical transport quarantined" & ASCII.LF);
      end Fault;
   begin
      Done := False;
      Progressed := False;
      Result := bulkResult;
      if bulkPhase in Command_Out | Data_In | Status_In then
         Next_Completion (storageSlot,
           (if bulkPhase = Command_Out then storageOutDCI else storageInDCI),
           event, found, progressed);
         if completionRoutingFailed then Fault;
         elsif found then
            pointer := Unsigned_64 (event.parameterLo) or
              Shift_Left (Unsigned_64 (event.parameterHi), 32);
            residual := event.status and 16#00FF_FFFF#;
            completion := Shift_Right (event.status, 24);
            if pointer /= bulkExpectedPointer or else (event.control and 4) /= 0 or else
               residual > bulkExpectedLength or else
               completion not in COMPLETION_SUCCESS | COMPLETION_SHORT_PACKET
            then
               Debug_Hex32 ("xhci: bulk completion=", completion);
               Fault;
            else
               actual := bulkExpectedLength - residual;
               case bulkPhase is
                  when Command_Out =>
                     if actual /= 31 then Fault;
                     elsif USB_Optical.Transfer_Bytes (bulkCommand) = 0 then
                        bulkPhase := Status_In;
                        Queue_Bulk (True, BULK_CSW_OFFSET, 13);
                     else
                        bulkPhase := Data_In;
                        Queue_Bulk (True, BULK_DATA_OFFSET,
                          USB_Optical.Transfer_Bytes (bulkCommand));
                     end if;
                  when Data_In =>
                     bulkReceived := actual;
                     bulkPhase := Status_In;
                     Queue_Bulk (True, BULK_CSW_OFFSET, 13);
                  when Status_In =>
                     if actual /= 13 then Fault;
                     else
                        declare
                           snapshot : constant USB_Optical.Bytes (1 .. 13) := bulkCSW;
                        begin
                           bulkResult := USB_Optical.Decode_Status
                             (snapshot, bulkTag,
                              USB_Optical.Transfer_Bytes (bulkCommand), bulkReceived);
                        end;
                        if bulkResult in USB_Optical.Invalid_Status |
                          USB_Optical.Reset_Recovery_Required
                        then Fault;
                        elsif opticalBlocks /= 0 and then
                          bulkResult /= USB_Optical.Command_Passed
                        then
                           --  Once mounted, any failed/short READ invalidates
                           --  this media session. In particular do not clear a
                           --  Unit Attention and silently reuse cached extents
                           --  against a replacement CD. Remount needs a fresh
                           --  device/session generation (future hotplug work).
                           Fault;
                        else bulkPhase := Bulk_Finished; end if;
                     end if;
                  when others => null;
               end case;
            end if;
         elsif syscall (SYSCALL_GETTIME) >= bulkDeadline then
            debugPrint ("xhci: optical transfer timeout" & ASCII.LF);
            Fault;
         end if;
      end if;
      Done := bulkPhase in Bulk_Finished | Bulk_Faulted;
      Result := bulkResult;
   end Poll_Optical_Read;

   procedure Probe_Optical is
      actual : Natural;
      ok, valid : Boolean;
      lastLUN : USB_Optical.Logical_Unit;
      outcome : USB_Optical.Control_Result;
      count : Unsigned_64;
      capacity : USB_Optical.Capacity_Result;
      savedSlot : constant Natural := activeSlot;
      use type USB_Optical.Status_Result;
      use type USB_Optical.Capacity_Result;
      function Run (Kind : USB_Optical.Probe_Kind; LUN : USB_Optical.Logical_Unit)
         return Boolean
      is
         accepted, done, progressed : Boolean;
         result : USB_Optical.Status_Result;
         ignore : Unsigned_64;
      begin
         Begin_Bulk (USB_Optical.Probe (Kind), LUN, accepted);
         if not accepted then return False; end if;
         loop
            Poll_Optical_Read (done, result, progressed);
            exit when done;
            ignore := syscall (SYSCALL_SLEEP, 1);
         end loop;
         return result = USB_Optical.Command_Passed;
      end Run;
   begin
      if storageSlot = 0 then return; end if;
      activeSlot := storageSlot;
      Control_Request (16#A1#, 16#FE#, 0, Unsigned_16 (storageInterface), 1,
        dmaPhysical + Device_Offset (storageSlot, DESCRIPTOR_OFFSET), actual, ok);
      outcome := (if ok then USB_Optical.Control_Success else USB_Optical.Control_Error);
      if not ok and then lastControlCompletion = 6 and then Recover_Control_Stall then
         outcome := USB_Optical.Control_Stall;
      end if;
      declare
         data : USB_Optical.Bytes (1 .. actual);
      begin
         for i in data'Range loop data (i) := deviceDMA (storageSlot).descriptorBytes (i - 1); end loop;
         USB_Optical.Decode_Max_LUN (outcome, data, lastLUN, valid);
      end;
      activeSlot := savedSlot;
      if not valid then
         debugPrint ("xhci: invalid optical LUN discovery" & ASCII.LF);
         return;
      end if;
      for lun in USB_Optical.Logical_Unit range 0 .. lastLUN loop
         if Run (USB_Optical.Inquiry, lun) then
            declare
               inquiry : constant USB_Optical.Bytes (1 .. 36) := bulkData (1 .. 36);
            begin
               if USB_Optical.Is_Optical_Inquiry (inquiry) then
                  for attempt in 1 .. 5 loop
                     exit when Run (USB_Optical.Test_Unit_Ready, lun);
                     ok := Run (USB_Optical.Request_Sense, lun);
                  end loop;
                  if Run (USB_Optical.Read_Capacity, lun) then
                     declare
                        response : constant USB_Optical.Bytes (1 .. 8) := bulkData (1 .. 8);
                     begin
                        USB_Optical.Decode_Capacity (response, count, capacity);
                     end;
                     if capacity = USB_Optical.Capacity_Valid then
                        opticalLUN := lun;
                        opticalBlocks := count;
                        Debug_Hex32 ("xhci: optical LUN=", Unsigned_32 (lun));
                        Debug_Hex32 ("xhci: optical blocks=", Unsigned_32 (count));
                        return;
                     end if;
                  end if;
               end if;
            end;
         end if;
      end loop;
      debugPrint ("xhci: no supported optical medium" & ASCII.LF);
   end Probe_Optical;

   function Optical_Block_Count return Unsigned_64 is (opticalBlocks);
   function Optical_Read_Buffer return System.Address is
     (To_Address (Integer_Address (DMA_VIRT_BASE + BULK_DATA_OFFSET)));
   function Optical_Deadline return Unsigned_64 is
     (if bulkPhase in Command_Out | Data_In | Status_In then bulkDeadline
      else Unsigned_64'Last);
   procedure Start_Optical_Read
     (First : Unsigned_32; Count : USB_Optical.Read_Block_Count;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      if not USB_Optical.Read_Fits (First, Count, opticalBlocks) then return; end if;
      Begin_Bulk (USB_Optical.Read_Request (First, Count), opticalLUN, Accepted);
   end Start_Optical_Read;

   function Boot_Interval
     (speed : Unsigned_32; descriptorInterval : Unsigned_8)
      return Unsigned_32
   is
      interval : Natural := Natural (descriptorInterval);
      exponent : Unsigned_32 := 0;
   begin
      if speed = 3 or else speed = 4 then
         if interval > 0 then
            return Unsigned_32 (Natural'Min (interval - 1, 15));
         end if;
         return 0;
      end if;

      --  Low/full-speed bInterval is measured in 1 ms frames.  xHCI uses
      --  125 us exponent form, so choose floor(log2(8 * bInterval)).
      interval := Natural'Max (interval, 1) * 8;
      while interval > 1 and then exponent < 15 loop
         interval := interval / 2;
         exponent := exponent + 1;
      end loop;
      return exponent;
   end Boot_Interval;

   procedure Initialize
     (barPhys  : Unsigned_64;
      barPages : Unsigned_64;
      dmaPhys  : Unsigned_64;
      expectedScratchpads : XHCI_Capabilities.Scratchpad_Buffer_Count;
      result   : out Init_Result)
   is
      ignore     : Unsigned_64;
      capLength  : Unsigned_8;
      hcsParams1 : Unsigned_32;
      hcsParams2 : Unsigned_32;
      hccParams1 : Unsigned_32;
      maxSlots   : Natural;
      enabledSlots : Natural;
      scratchpads : XHCI_Capabilities.Scratchpad_Buffer_Count;
      Layout : constant Allocation := Plan (expectedScratchpads);
      dbOffset   : Unsigned_32;
      rtOffset   : Unsigned_32;
      pageSizes  : Unsigned_32;
      usbCommand : Unsigned_32;
      usbStatus  : Unsigned_32;
      portStatus : Unsigned_32;
      commandCompletion : Unsigned_32;
      commandDone : Boolean;
      slotId : Natural;
      contextStride : Natural;
      endpointBase  : Natural;
      portSpeed     : Unsigned_32;
      maxPacketSize : Unsigned_32;

      procedure Enumerate_Selected_Port (result : out Init_Result) is
      begin
      Debug_Hex32 ("xhci: enumerate root port=", Unsigned_32 (activePort));
      portStatus := Read32
        (operational,
         OP_PORTSC_BASE + Storage_Offset (activePort - 1) * OP_PORT_STRIDE);
      Debug_Hex32 ("xhci: PORTSC before enumeration=", portStatus);
      case XHCI_Ports.Before_Enumeration (portStatus) is
         when XHCI_Ports.Disconnected =>
            result := INIT_NO_DEVICE;
            return;
         when XHCI_Ports.Start_Reset =>
            debugPrint ("xhci: starting port reset" & ASCII.LF);
            Write32
              (operational,
               OP_PORTSC_BASE + Storage_Offset (activePort - 1) * OP_PORT_STRIDE,
               XHCI_Ports.Reset_Write (portStatus));
         when XHCI_Ports.Wait_For_Reset =>
            debugPrint ("xhci: waiting for existing port reset" & ASCII.LF);
         when XHCI_Ports.Already_Enabled =>
            null;
      end case;
      if not Wait_For_Bits
        (operational,
         OP_PORTSC_BASE + Storage_Offset (activePort - 1) * OP_PORT_STRIDE,
         16#8000_0000# or PORTSC_PR or PORTSC_PED, PORTSC_PED, 10_000)
      then
         Debug_Hex32
           ("xhci: PORTSC reset timeout=",
            Read32
              (operational,
               OP_PORTSC_BASE +
                 Storage_Offset (activePort - 1) * OP_PORT_STRIDE));
         result := INIT_PORT_RESET_TIMEOUT;
         return;
      end if;
      Debug_Hex32 ("xhci: PORTSC ready=", Read32 (operational,
        OP_PORTSC_BASE + Storage_Offset (activePort - 1) * OP_PORT_STRIDE));
      debugPrint ("xhci: submitting Enable Slot" & ASCII.LF);
      Submit_Command
        ((parameterLo => 0, parameterHi => 0, status => 0,
          control => Shift_Left (TRB_TYPE_ENABLE_SLOT, TRB_TYPE_SHIFT)),
         commandCompletion, slotId, commandDone);
      if not commandDone then
         Debug_Hex32 ("xhci: Enable Slot timeout USBSTS=", Read32 (operational, OP_USBSTS));
         Debug_Hex32 ("xhci: Enable Slot timeout CRCR=", Read32 (operational, OP_CRCR));
         result := INIT_COMMAND_TIMEOUT;
         return;
      elsif commandCompletion /= COMPLETION_SUCCESS or else
         slotId not in 1 .. XHCI_Completions.Maximum_Slots
      then
         Debug_Hex32 ("xhci: Enable Slot completion=", commandCompletion);
         result := INIT_COMMAND_FAILED;
         return;
      end if;
      activeSlot := slotId;
      Debug_Hex32 ("xhci: enabled device slot=", Unsigned_32 (activeSlot));

      --  Give the slot an output Device Context, then describe the route and
      --  default control endpoint in an Input Context.  Contexts are 32 or 64
      --  bytes according to HCCPARAMS1.CSZ; each lives in its own DMA page.
      if (hccParams1 and 16#4#) = 0 then
         contextStride := 8;
      else
         contextStride := 16;
      end if;

      portStatus := Read32
        (operational,
         OP_PORTSC_BASE + Storage_Offset (activePort - 1) * OP_PORT_STRIDE);
      portSpeed := Shift_Right (portStatus, 10) and 16#F#;
      case portSpeed is
         when 1 | 2 =>
            maxPacketSize := 8;
         when 3 =>
            maxPacketSize := 64;
         when 4 =>
            maxPacketSize := 512;
         when others =>
            result := INIT_ADDRESS_FAILED;
            return;
      end case;

      dcbaa (activeSlot) := dmaPhys + Device_Offset (activeSlot, DEVICE_CONTEXT_OFFSET);
      --  Input Control Context: add Slot and Endpoint 0 contexts.
      deviceDMA (activeSlot).inputContext (0) := 0;
      deviceDMA (activeSlot).inputContext (1) := 3;
      --  Input Slot Context is context index 1.
      deviceDMA (activeSlot).inputContext (contextStride) :=
        Shift_Left (portSpeed, 20) or Shift_Left (1, 27);
      deviceDMA (activeSlot).inputContext (contextStride + 1) :=
        Shift_Left (Unsigned_32 (activePort), 16);
      --  Input Endpoint 0 Context is context index 2.
      endpointBase := 2 * contextStride;
      deviceDMA (activeSlot).inputContext (endpointBase + 1) :=
        Shift_Left (3, 1) or Shift_Left (4, 3) or
        Shift_Left (maxPacketSize, 16);
      deviceDMA (activeSlot).inputContext (endpointBase + 2) :=
        Unsigned_32 ((dmaPhys + Device_Offset (activeSlot, EP0_RING_OFFSET)) and 16#FFFF_FFFF#) or
        TRB_CYCLE;
      deviceDMA (activeSlot).inputContext (endpointBase + 3) :=
        Unsigned_32 (Shift_Right (dmaPhys + Device_Offset (activeSlot, EP0_RING_OFFSET), 32));
      deviceDMA (activeSlot).inputContext (endpointBase + 4) := 8;

      deviceDMA (activeSlot).ep0Ring (COMMAND_RING_ENTRIES - 1) :=
        (parameterLo =>
           Unsigned_32 ((dmaPhys + Device_Offset (activeSlot, EP0_RING_OFFSET)) and 16#FFFF_FFFF#),
         parameterHi =>
           Unsigned_32 (Shift_Right (dmaPhys + Device_Offset (activeSlot, EP0_RING_OFFSET), 32)),
         status => 0,
         control => Shift_Left (TRB_TYPE_LINK, TRB_TYPE_SHIFT) or
           TRB_TOGGLE_CYCLE or TRB_CYCLE);

      Submit_Command
        ((parameterLo =>
            Unsigned_32 ((dmaPhys + Device_Offset (activeSlot, INPUT_CONTEXT_OFFSET)) and 16#FFFF_FFFF#),
          parameterHi =>
            Unsigned_32 (Shift_Right (dmaPhys + Device_Offset (activeSlot, INPUT_CONTEXT_OFFSET), 32)),
          status => 0,
          control => Shift_Left (TRB_TYPE_ADDRESS_DEVICE, TRB_TYPE_SHIFT) or
            Shift_Left (Unsigned_32 (activeSlot), 24)),
         commandCompletion, slotId, commandDone);
      if not commandDone then
         result := INIT_COMMAND_TIMEOUT;
         return;
      elsif commandCompletion /= COMPLETION_SUCCESS then
         Debug_Hex32 ("xhci: Address Device completion=", commandCompletion);
         result := INIT_ADDRESS_FAILED;
         return;
      end if;
      Debug_Hex32
        ("xhci: assigned USB address=",
         deviceDMA (activeSlot).deviceContext (3) and 16#FF#);

      declare
         actualLength       : Natural;
         requestOK          : Boolean;
         totalLength        : Natural;
         configValue        : Unsigned_8 := 0;
         interfaceNumber    : Unsigned_8 := 0;
         endpointAddress    : Unsigned_8 := 0;
         endpointPacketSize : Natural := 0;
         endpointInterval   : Unsigned_8 := 0;
         selectedStorage : USB_Configurations.Storage_Interface;
         endpointNumber     : Natural;
         endpointDCI        : Natural;
         endpointInputBase  : Natural;
         endpointIntervalXHCI : Unsigned_32;
         vendorProduct      : Unsigned_32;
      begin
         --  Read only bounded standard descriptors into a private DMA page.
         Control_Request
           (16#80#, 6, 16#0100#, 0, 18,
            dmaPhys + Device_Offset (activeSlot, DESCRIPTOR_OFFSET), actualLength, requestOK);
         if not requestOK or else actualLength < 18 or else
            deviceDMA (activeSlot).descriptorBytes (0) < 18 or else deviceDMA (activeSlot).descriptorBytes (1) /= 1
         then
            result := INIT_DESCRIPTOR_FAILED;
            return;
         end if;
         vendorProduct := Unsigned_32 (deviceDMA (activeSlot).descriptorBytes (8)) or
           Shift_Left (Unsigned_32 (deviceDMA (activeSlot).descriptorBytes (9)), 8) or
           Shift_Left (Unsigned_32 (deviceDMA (activeSlot).descriptorBytes (10)), 16) or
           Shift_Left (Unsigned_32 (deviceDMA (activeSlot).descriptorBytes (11)), 24);
         Debug_Hex32 ("xhci: USB vendor/product=", vendorProduct);

         Control_Request
           (16#80#, 6, 16#0200#, 0, 9,
            dmaPhys + Device_Offset (activeSlot, DESCRIPTOR_OFFSET), actualLength, requestOK);
         if not requestOK or else actualLength < 9 or else
            deviceDMA (activeSlot).descriptorBytes (0) < 9 or else deviceDMA (activeSlot).descriptorBytes (1) /= 2
         then
            result := INIT_DESCRIPTOR_FAILED;
            return;
         end if;
         totalLength := Natural (deviceDMA (activeSlot).descriptorBytes (2)) +
           Natural (deviceDMA (activeSlot).descriptorBytes (3)) * 256;
         configValue := deviceDMA (activeSlot).descriptorBytes (5);
         if totalLength < 9 or else totalLength > deviceDMA (activeSlot).descriptorBytes'Length or else
            configValue = 0
         then
            result := INIT_DESCRIPTOR_FAILED;
            return;
         end if;

         Control_Request
           (16#80#, 6, 16#0200#, 0, totalLength,
            dmaPhys + Device_Offset (activeSlot, DESCRIPTOR_OFFSET), actualLength, requestOK);
         if not requestOK or else actualLength < totalLength then
            result := INIT_DESCRIPTOR_FAILED;
            return;
         end if;

         --  Snapshot completed DMA before the pure decoder sees it. The
         --  interface number and endpoint are retained as one object, never
         --  assembled from unrelated interfaces in a composite device.
         declare
            data : USB_Configurations.Bytes (1 .. totalLength);
            configuration : USB_Configurations.Configuration;
            decoded : USB_Configurations.Decode_Result;
            use type USB_Configurations.Decode_Result;
         begin
            for i in data'Range loop
               data (i) := deviceDMA (activeSlot).descriptorBytes (i - 1);
            end loop;
            USB_Configurations.Decode (data, configuration, decoded);
            if decoded /= USB_Configurations.Decoded then
               result := INIT_DESCRIPTOR_FAILED;
               return;
            end if;
            configValue := configuration.Value;
            if configuration.Storage.Present and then storageSlot = 0 then
               selectedStorage := configuration.Storage;
            elsif not configuration.Mouse.Present or else mouseSlot /= 0 then
               result := INIT_NOT_BOOT_MOUSE;
               return;
            else
               interfaceNumber := configuration.Mouse.Number;
               endpointAddress := configuration.Mouse.Input.Address;
               endpointPacketSize := configuration.Mouse.Input.Packet_Bytes;
               endpointInterval := configuration.Mouse.Input.Interval;
            end if;
         end;

         if selectedStorage.Present then
            if portSpeed not in 3 .. 4 then
               result := INIT_CONFIGURE_FAILED;
               return;
            end if;
            Control_Request
              (0, 9, Unsigned_16 (configValue), 0, 0, 0, actualLength, requestOK);
            if not requestOK then
               result := INIT_CONFIGURE_FAILED;
               return;
            end if;
            declare
               inDCI : constant Natural :=
                 Natural (selectedStorage.Input.Address and 15) * 2 + 1;
               outDCI : constant Natural :=
                 Natural (selectedStorage.Output.Address and 15) * 2;
               procedure Add_Endpoint
                 (dci : Natural; ep : USB_Configurations.Endpoint;
                  epType : Unsigned_32; ringOffset : Unsigned_64)
               is
                  base : constant Natural := (dci + 1) * contextStride;
                  phys : constant Unsigned_64 :=
                    dmaPhys + Device_Offset (activeSlot, ringOffset);
               begin
                  deviceDMA (activeSlot).inputContext (base + 1) :=
                    Shift_Left (3, 1) or Shift_Left (epType, 3) or
                    Shift_Left (Unsigned_32 (ep.Packet_Bytes), 16);
                  deviceDMA (activeSlot).inputContext (base + 2) :=
                    Unsigned_32 (phys and 16#FFFF_FFFF#) or TRB_CYCLE;
                  deviceDMA (activeSlot).inputContext (base + 3) :=
                    Unsigned_32 (Shift_Right (phys, 32));
                  deviceDMA (activeSlot).inputContext (base + 4) := 32768;
               end Add_Endpoint;
               function Link (offset : Unsigned_64) return TRB is
                  phys : constant Unsigned_64 :=
                    dmaPhys + Device_Offset (activeSlot, offset);
               begin
                  return (Unsigned_32 (phys and 16#FFFF_FFFF#),
                    Unsigned_32 (Shift_Right (phys, 32)), 0,
                    Shift_Left (TRB_TYPE_LINK, TRB_TYPE_SHIFT) or
                    TRB_TOGGLE_CYCLE or TRB_CYCLE);
               end Link;
            begin
               if selectedStorage.Input.Packet_Bytes /=
                    (if portSpeed = 3 then 512 else 1024) or else
                  selectedStorage.Output.Packet_Bytes /= selectedStorage.Input.Packet_Bytes
               then
                  result := INIT_CONFIGURE_FAILED;
                  return;
               end if;
               for i in 0 .. 33 * contextStride - 1 loop
                  deviceDMA (activeSlot).inputContext (i) := 0;
               end loop;
               deviceDMA (activeSlot).inputContext (1) := 1 or
                 Shift_Left (1, inDCI) or Shift_Left (1, outDCI);
               for i in 0 .. contextStride - 1 loop
                  deviceDMA (activeSlot).inputContext (contextStride + i) :=
                    deviceDMA (activeSlot).deviceContext (i);
               end loop;
               deviceDMA (activeSlot).inputContext (contextStride) :=
                 (deviceDMA (activeSlot).inputContext (contextStride) and not 16#F800_0000#) or
                 Shift_Left (Unsigned_32 (Natural'Max (inDCI, outDCI)), 27);
               Add_Endpoint (inDCI, selectedStorage.Input, 6, BULK_IN_RING_OFFSET);
               Add_Endpoint (outDCI, selectedStorage.Output, 2, BULK_OUT_RING_OFFSET);
               deviceDMA (activeSlot).bulkInRing (COMMAND_RING_ENTRIES - 1) := Link (BULK_IN_RING_OFFSET);
               deviceDMA (activeSlot).bulkOutRing (COMMAND_RING_ENTRIES - 1) := Link (BULK_OUT_RING_OFFSET);
               Submit_Command
                 ((Unsigned_32 ((dmaPhys + Device_Offset (activeSlot, INPUT_CONTEXT_OFFSET)) and 16#FFFF_FFFF#),
                   Unsigned_32 (Shift_Right (dmaPhys + Device_Offset (activeSlot, INPUT_CONTEXT_OFFSET), 32)),
                   0, Shift_Left (TRB_TYPE_CONFIGURE_ENDPOINT, TRB_TYPE_SHIFT) or
                   Shift_Left (Unsigned_32 (activeSlot), 24)),
                  commandCompletion, slotId, commandDone);
               if not commandDone or else commandCompletion /= COMPLETION_SUCCESS then
                  result := INIT_CONFIGURE_FAILED;
                  return;
               end if;
               storageSlot := activeSlot;
               storageInDCI := inDCI;
               storageOutDCI := outDCI;
               storageInterface := selectedStorage.Number;
               Debug_Hex32 ("xhci: BOT storage slot=", Unsigned_32 (storageSlot));
               result := INIT_OK;
               return;
            end;
         end if;

         if endpointAddress = 0 or else endpointPacketSize = 0 or else
            endpointPacketSize > HID_REPORT_STRIDE
         then
            result := INIT_NOT_BOOT_MOUSE;
            return;
         end if;

         --  Select the USB configuration while EP0 is the only active
         --  endpoint, then add precisely the discovered interrupt endpoint.
         Control_Request
           (0, 9, Unsigned_16 (configValue), 0, 0, 0,
            actualLength, requestOK);
         if not requestOK then
            result := INIT_CONFIGURE_FAILED;
            return;
         end if;

         endpointNumber := Natural (endpointAddress and 16#0F#);
         endpointDCI := endpointNumber * 2 + 1;
         if endpointDCI > 31 then
            result := INIT_NOT_BOOT_MOUSE;
            return;
         end if;
         hidEndpointDCI := endpointDCI;
         hidMaxPacket := endpointPacketSize;

         for i in 0 .. 33 * contextStride - 1 loop
            deviceDMA (activeSlot).inputContext (i) := 0;
         end loop;
         deviceDMA (activeSlot).inputContext (1) := Unsigned_32 (1) or
           Shift_Left (Unsigned_32 (1), endpointDCI);
         for i in 0 .. contextStride - 1 loop
            deviceDMA (activeSlot).inputContext (contextStride + i) := deviceDMA (activeSlot).deviceContext (i);
         end loop;
         deviceDMA (activeSlot).inputContext (contextStride) :=
           (deviceDMA (activeSlot).inputContext (contextStride) and not 16#F800_0000#) or
           Shift_Left (Unsigned_32 (endpointDCI), 27);

         endpointInputBase := (endpointDCI + 1) * contextStride;
         endpointIntervalXHCI :=
           Boot_Interval (portSpeed, endpointInterval);
         deviceDMA (activeSlot).inputContext (endpointInputBase) :=
           Shift_Left (endpointIntervalXHCI, 16);
         deviceDMA (activeSlot).inputContext (endpointInputBase + 1) :=
           Shift_Left (Unsigned_32 (3), 1) or
           Shift_Left (Unsigned_32 (7), 3) or
           Shift_Left (Unsigned_32 (endpointPacketSize), 16);
         deviceDMA (activeSlot).inputContext (endpointInputBase + 2) :=
           Unsigned_32 ((dmaPhys + Device_Offset (activeSlot, HID_RING_OFFSET)) and 16#FFFF_FFFF#) or
           TRB_CYCLE;
         deviceDMA (activeSlot).inputContext (endpointInputBase + 3) :=
           Unsigned_32 (Shift_Right (dmaPhys + Device_Offset (activeSlot, HID_RING_OFFSET), 32));
         deviceDMA (activeSlot).inputContext (endpointInputBase + 4) :=
           Unsigned_32 (endpointPacketSize) or
           Shift_Left (Unsigned_32 (endpointPacketSize), 16);

         deviceDMA (activeSlot).hidRing (COMMAND_RING_ENTRIES - 1) :=
           (parameterLo =>
              Unsigned_32 ((dmaPhys + Device_Offset (activeSlot, HID_RING_OFFSET)) and 16#FFFF_FFFF#),
            parameterHi =>
              Unsigned_32 (Shift_Right (dmaPhys + Device_Offset (activeSlot, HID_RING_OFFSET), 32)),
            status => 0,
            control => Shift_Left (TRB_TYPE_LINK, TRB_TYPE_SHIFT) or
              TRB_TOGGLE_CYCLE or TRB_CYCLE);

         Submit_Command
           ((parameterLo =>
               Unsigned_32
                 ((dmaPhys + Device_Offset (activeSlot, INPUT_CONTEXT_OFFSET)) and 16#FFFF_FFFF#),
             parameterHi =>
               Unsigned_32
                 (Shift_Right (dmaPhys + Device_Offset (activeSlot, INPUT_CONTEXT_OFFSET), 32)),
             status => 0,
             control =>
               Shift_Left (TRB_TYPE_CONFIGURE_ENDPOINT, TRB_TYPE_SHIFT) or
               Shift_Left (Unsigned_32 (activeSlot), 24)),
            commandCompletion, slotId, commandDone);
         if not commandDone or else
            commandCompletion /= COMPLETION_SUCCESS
         then
            Debug_Hex32
              ("xhci: Configure Endpoint completion=", commandCompletion);
            result := INIT_CONFIGURE_FAILED;
            return;
         end if;

         --  Use the fixed three-byte boot report, independent of arbitrary
         --  HID report-descriptor contents.
         Control_Request
           (16#21#, 16#0B#, 0, Unsigned_16 (interfaceNumber), 0, 0,
            actualLength, requestOK);
         if not requestOK then
            result := INIT_CONFIGURE_FAILED;
            return;
         end if;
         Debug_Hex32
           ("xhci: boot mouse endpoint DCI=", Unsigned_32 (endpointDCI));
      end;

      mouseSlot := activeSlot;
      result := INIT_OK;
      end Enumerate_Selected_Port;
   begin
      result := INIT_BAD_CAPABILITY;
      maxPorts := 0;
      connectedPorts := 0;
      activePort := 0;
      activeSlot := 0;
      dmaPhysical := dmaPhys;
      commandTail := 0;
      commandCycle := TRB_CYCLE;
      eventHead := 0;
      eventCycle := TRB_CYCLE;
      XHCI_Completions.Clear (completionMailboxes);
      completionRoutingFailed := False;
      ep0State := [others => (others => <>)];
      mouseSlot := 0;
      storageSlot := 0;
      hidTail := 0;
      hidCycle := TRB_CYCLE;
      hidEndpointDCI := 0;
      hidMaxPacket := 0;
      hidTransfersStarted := False;
      mouseDiagnostics :=
        (transferEvents    => 0,
         decodedReports    => 0,
         motionReports     => 0,
         buttonTransitions => 0,
         completionErrors  => 0,
         shortReports      => 0,
         unexpectedEvents  => 0,
         lastReport        => 0,
         lastLength        => 0,
         lastCompletion    => 0);
      diagnosticButtons := 0;
      barMappedBytes := 0;

      if barPhys = 0 or else barPages = 0 or else barPages > 256 or else
         dmaPhys = 0
      then
         return;
      end if;

      ignore := syscall
        (SYSCALL_MAP_DEVICE, barPhys, BAR_VIRT_BASE, barPages);
      if ignore = Unsigned_64'Last then
         result := INIT_MAP_FAILED;
         return;
      end if;

      barBase := To_Address (Integer_Address (BAR_VIRT_BASE));
      barMappedBytes := barPages * PAGE_SIZE;
      capLength := Read8 (barBase, REG_CAPLENGTH);
      if capLength < 16#20# then
         return;
      end if;

      operational := barBase + Storage_Offset (capLength);
      hcsParams1 := Read32 (barBase, REG_HCSPARAMS1);
      hcsParams2 := Read32 (barBase, REG_HCSPARAMS2);
      hccParams1 := Read32 (barBase, REG_HCCPARAMS1);
      pageSizes := Read32 (operational, OP_PAGESIZE);
      Debug_Hex32 ("xhci: CAPLENGTH=", Unsigned_32 (capLength));
      Debug_Hex32 ("xhci: HCSPARAMS1=", hcsParams1);
      Debug_Hex32 ("xhci: HCSPARAMS2=", hcsParams2);
      Debug_Hex32 ("xhci: HCCPARAMS1=", hccParams1);
      Debug_Hex32 ("xhci: PAGESIZE=", pageSizes);
      dbOffset := Read32 (barBase, REG_DBOFF) and 16#FFFF_FFFC#;
      rtOffset := Read32 (barBase, REG_RTSOFF) and 16#FFFF_FFE0#;
      doorbellBase := barBase + Storage_Offset (dbOffset);
      runtimeBase := barBase + Storage_Offset (rtOffset);

      maxSlots := Natural (hcsParams1 and 16#FF#);
      maxPorts := Natural (Shift_Right (hcsParams1, 24) and 16#FF#);
      if maxSlots = 0 or else maxPorts = 0 or else
         (hccParams1 and 1) = 0
      then
         --  CuBit currently requires 64-bit DMA addressing.
         return;
      end if;

      if (pageSizes and 1) = 0 then
         result := INIT_PAGE_SIZE_UNSUPPORTED;
         return;
      end if;

      scratchpads := XHCI_Capabilities.Scratchpad_Count (hcsParams2);
      Debug_Hex32 ("xhci: scratchpad buffers required=", Unsigned_32 (scratchpads));
      if scratchpads /= expectedScratchpads then
         Debug_Hex32 ("xhci: scratchpad buffers allocated for=", Unsigned_32 (expectedScratchpads));
         result := INIT_DMA_LAYOUT_MISMATCH;
         return;
      end if;
      Debug_Hex32 ("xhci: DMA pages allocated=", Unsigned_32 (Pages (Layout)));

      debugPrint ("xhci: pre-reset sleep(1ms) begin" & ASCII.LF);
      ignore := syscall (SYSCALL_SLEEP, 1);
      debugPrint ("xhci: pre-reset sleep resumed" & ASCII.LF);

      if not Firmware_Handoff (hccParams1) then
         result := INIT_FIRMWARE_HANDOFF_FAILED;
         return;
      end if;

      --  Stop before reset if firmware left the controller running.
      usbCommand := Read32 (operational, OP_USBCMD);
      usbStatus := Read32 (operational, OP_USBSTS);
      Debug_Hex32 ("xhci: USBCMD before stop=", usbCommand);
      Debug_Hex32 ("xhci: USBSTS before stop=", usbStatus);
      if (usbStatus and USBSTS_HCH) = 0 then
         Write32
           (operational, OP_USBCMD, usbCommand and not USBCMD_RUN);
         usbCommand := Read32 (operational, OP_USBCMD);
         Debug_Hex32 ("xhci: USBCMD after stop=", usbCommand);
         if not Wait_For_Bits
           (operational, OP_USBSTS, USBSTS_HCH, USBSTS_HCH, 1_000)
         then
            usbStatus := Read32 (operational, OP_USBSTS);
            Debug_Hex32 ("xhci: USBSTS stop timeout=", usbStatus);
            result := INIT_STOP_TIMEOUT;
            return;
         end if;
      end if;

      Write32 (operational, OP_USBCMD, USBCMD_HCRST);
      if not Wait_For_Bits
        (operational, OP_USBCMD, USBCMD_HCRST, 0, 1_000) or else
         not Wait_For_Bits
           (operational, OP_USBSTS, USBSTS_CNR, 0, 1_000)
      then
         result := INIT_RESET_TIMEOUT;
         return;
      end if;

      Clear_DMA (Layout);

      if scratchpads > 0 then
         declare
            scratchPointers : Address_Array (0 .. scratchpads - 1)
              with Import, Address => To_Address
                (Integer_Address (DMA_VIRT_BASE + SCRATCH_ARRAY_OFFSET));
         begin
            for i in scratchPointers'Range loop
               scratchPointers (i) :=
                 dmaPhys + Layout.Scratch_First + Unsigned_64 (i) * PAGE_SIZE;
            end loop;
         end;
         dcbaa (0) := dmaPhys + SCRATCH_ARRAY_OFFSET;
      end if;

      commandRing (COMMAND_RING_ENTRIES - 1) :=
        (parameterLo =>
           Unsigned_32 ((dmaPhys + COMMAND_RING_OFFSET) and 16#FFFF_FFFF#),
         parameterHi =>
           Unsigned_32 (Shift_Right (dmaPhys + COMMAND_RING_OFFSET, 32)),
         status      => 0,
         control     =>
           Shift_Left (TRB_TYPE_LINK, TRB_TYPE_SHIFT) or
           TRB_TOGGLE_CYCLE or TRB_CYCLE);

      erst :=
        (ringBase => dmaPhys + EVENT_RING_OFFSET,
         ringSize => Unsigned_32 (EVENT_RING_ENTRIES),
         reserved => 0);

      enabledSlots := Natural'Min (maxSlots, XHCI_Completions.Maximum_Slots);
      Write64 (operational, OP_DCBAAP, dmaPhys + DCBAA_OFFSET);
      Write64
        (operational, OP_CRCR,
         dmaPhys + COMMAND_RING_OFFSET or Unsigned_64 (TRB_CYCLE));
      Write32
        (runtimeBase + RT_INTR0, INTR_IMAN, 0);
      Write32
        (runtimeBase + RT_INTR0, INTR_IMOD, 0);
      Write32
        (runtimeBase + RT_INTR0, INTR_ERSTSZ, 1);
      Write64
        (runtimeBase + RT_INTR0, INTR_ERSTBA, dmaPhys + ERST_OFFSET);
      Write64
        (runtimeBase + RT_INTR0, INTR_ERDP, dmaPhys + EVENT_RING_OFFSET);
      Write32 (operational, OP_CONFIG, Unsigned_32 (enabledSlots));

      Write32 (operational, OP_USBCMD, USBCMD_RUN);
      if not Wait_For_Bits
        (operational, OP_USBSTS, USBSTS_HCH, 0, 1_000)
      then
         result := INIT_START_TIMEOUT;
         return;
      end if;

      --  A device attached before boot reconnects asynchronously after HCRST.
      --  Wait for that architected state transition rather than taking a
      --  one-shot snapshot immediately after Run/Stop is asserted.
      for attempt in 1 .. 1_000 loop
         connectedPorts := 0;
         for port in 0 .. maxPorts - 1 loop
            portStatus := Read32
              (operational,
               OP_PORTSC_BASE + Storage_Offset (port) * OP_PORT_STRIDE);
            if (portStatus and PORTSC_CCS) /= 0 then
               connectedPorts := connectedPorts + 1;
            end if;
         end loop;
         exit when connectedPorts > 0;
         ignore := syscall (SYSCALL_SLEEP, 1);
      end loop;

      connectedPorts := 0;
      for port in 0 .. maxPorts - 1 loop
         portStatus := Read32
           (operational,
            OP_PORTSC_BASE + Storage_Offset (port) * OP_PORT_STRIDE);
         if (portStatus and PORTSC_CCS) /= 0 then
            connectedPorts := connectedPorts + 1;
            Debug_Hex32 ("xhci: connected root port=", Unsigned_32 (port + 1));
            Debug_Hex32 ("xhci: connected PORTSC=", portStatus);
            if activePort = 0 then
               activePort := port + 1;
            end if;
         end if;
      end loop;

      if activePort = 0 then
         result := INIT_NO_DEVICE;
         return;
      end if;

      declare
         portResult : Init_Result;
      begin
         for port in 1 .. maxPorts loop
            portStatus := Read32 (operational,
              OP_PORTSC_BASE + Storage_Offset (port - 1) * OP_PORT_STRIDE);
            if (portStatus and PORTSC_CCS) /= 0 then
               activePort := port;
               Enumerate_Selected_Port (portResult);
               if portResult /= INIT_OK then
                  Debug_Hex32 ("xhci: port initialization result=",
                    Unsigned_32 (Init_Result'Pos (portResult)));
               end if;
               if portResult in INIT_COMMAND_TIMEOUT | INIT_COMMAND_FAILED then
                  result := portResult;
                  return;
               end if;
            end if;
         end loop;
      end;
      activeSlot := (if mouseSlot /= 0 then mouseSlot else storageSlot);
      result := (if activeSlot /= 0 then INIT_OK else INIT_NO_DEVICE);
   end Initialize;

   function Port_Count return Natural is
   begin
      return maxPorts;
   end Port_Count;

   function Connected_Port_Count return Natural is
   begin
      return connectedPorts;
   end Connected_Port_Count;

   function Device_Slot return Natural is
   begin
      return activeSlot;
   end Device_Slot;

   procedure Queue_HID_Transfer is
      reportOffset : constant Natural := hidTail * HID_REPORT_STRIDE;
      reportPhys   : constant Unsigned_64 :=
        dmaPhysical + Device_Offset (mouseSlot, HID_REPORT_OFFSET) + Unsigned_64 (reportOffset);
      pending      : TRB;
   begin
      --  Every live TRB owns a distinct cache-line-sized report buffer.  The
      --  queue depth is smaller than the transfer ring, so a buffer is never
      --  cleared while the controller may still be writing it.
      for i in reportOffset .. reportOffset + HID_REPORT_STRIDE - 1 loop
         deviceDMA (mouseSlot).hidReports (i) := 0;
      end loop;

      pending :=
        (parameterLo => Unsigned_32 (reportPhys and 16#FFFF_FFFF#),
         parameterHi => Unsigned_32 (Shift_Right (reportPhys, 32)),
         status      => Unsigned_32 (hidMaxPacket),
         control     => Shift_Left (TRB_TYPE_NORMAL, TRB_TYPE_SHIFT) or
           TRB_IOC or hidCycle);
      deviceDMA (mouseSlot).hidRing (hidTail) := pending;

      if hidTail = COMMAND_RING_ENTRIES - 2 then
         deviceDMA (mouseSlot).hidRing (COMMAND_RING_ENTRIES - 1).control :=
           Shift_Left (TRB_TYPE_LINK, TRB_TYPE_SHIFT) or
           TRB_TOGGLE_CYCLE or hidCycle;
         hidTail := 0;
         hidCycle := hidCycle xor TRB_CYCLE;
      else
         hidTail := hidTail + 1;
      end if;
   end Queue_HID_Transfer;

   procedure Ring_HID_Doorbell is
   begin
      Write32
        (doorbellBase, Storage_Offset (mouseSlot * 4),
         Unsigned_32 (hidEndpointDCI));
   end Ring_HID_Doorbell;

   procedure Start_Boot_Mouse_Transfers is
   begin
      if hidTransfersStarted or else mouseSlot = 0 or else
         hidEndpointDCI = 0 or else hidMaxPacket = 0 or else
         hidMaxPacket > HID_REPORT_STRIDE
      then
         return;
      end if;

      --  A permanently populated queue covers scheduler and IPC handoff
      --  delays without skipping USB interrupt-endpoint service intervals.
      for transfer in 1 .. HID_TRANSFER_DEPTH loop
         Queue_HID_Transfer;
      end loop;
      hidTransfersStarted := True;
      Ring_HID_Doorbell;
   end Start_Boot_Mouse_Transfers;

   procedure Enable_Runtime_Interrupts
     (mode        : Runtime_Interrupt_Mode;
      vector      : Unsigned_8;
      tableOffset : Unsigned_64;
      enabled     : out Boolean)
   is
      usbCommand : Unsigned_32;
   begin
      enabled := False;
      if mode = INTERRUPT_POLLING then
         return;
      elsif mode = INTERRUPT_MSIX then
         --  One MSI-X table entry is four dwords. The table location is
         --  supplied by devmgr only after it has validated BIR=BAR0; repeat
         --  the byte-range and alignment checks at the authority boundary.
         if tableOffset mod 8 /= 0 or else barMappedBytes < 16 or else
            tableOffset > barMappedBytes - 16
         then
            return;
         end if;
         Write32
           (barBase, Storage_Offset (tableOffset), 16#FEE0_0000#);
         Write32
           (barBase, Storage_Offset (tableOffset + 4), 0);
         Write32
           (barBase, Storage_Offset (tableOffset + 8), Unsigned_32 (vector));
         --  Vector Control bit 0 clear means this entry is unmasked. The
         --  PCI function mask remains asserted by devmgr until setup replies.
         Write32
           (barBase, Storage_Offset (tableOffset + 12), 0);
      end if;

      --  Clear any initialization-time pending indication while enabling the
      --  primary interrupter, then permit the controller to assert MSI.
      Write32 (runtimeBase + RT_INTR0, INTR_IMAN, IMAN_IP or IMAN_IE);
      usbCommand := Read32 (operational, OP_USBCMD);
      Write32 (operational, OP_USBCMD, usbCommand or USBCMD_INTE);
      enabled := True;
   end Enable_Runtime_Interrupts;

   procedure Acknowledge_Runtime_Interrupt is
   begin
      --  IP is RW1C and IE is ordinary RW.  Writing both acknowledges the
      --  observed interrupt without accidentally disabling its source.
      Write32 (runtimeBase + RT_INTR0, INTR_IMAN, IMAN_IP or IMAN_IE);
   end Acknowledge_Runtime_Interrupt;

   function Mouse_Diagnostics return Boot_Mouse_Diagnostics is
   begin
      return mouseDiagnostics;
   end Mouse_Diagnostics;

   procedure Poll_Boot_Mouse
     (buttons : out Unsigned_8;
      deltaX  : out Integer;
      deltaY  : out Integer;
      deltaZ  : out Integer;
      ready   : out Boolean;
      eventAvailable : out Boolean)
   is
      event          : TRB;
      eventType      : Unsigned_32;
      eventSlot      : Natural;
      eventEP        : Natural;
      eventPointer   : Unsigned_64;
      transferBase, transferLimit : Unsigned_64;
      transferIndex  : Natural;
      reportOffset   : Natural;
      completion     : Unsigned_32;
      residual       : Natural;
      actualLength   : Natural := 0;
      found          : Boolean;
   begin
      buttons := 0;
      deltaX := 0;
      deltaY := 0;
      deltaZ := 0;
      ready := False;
      eventAvailable := False;
      if mouseSlot not in 1 .. XHCI_Completions.Maximum_Slots or else
         hidEndpointDCI not in 1 .. 31
      then
         return;
      end if;
      transferBase := dmaPhysical + Device_Offset (mouseSlot, HID_RING_OFFSET);
      transferLimit := transferBase + Unsigned_64 ((COMMAND_RING_ENTRIES - 1) * 16);
      Next_Completion
        (mouseSlot, hidEndpointDCI, event, found, eventAvailable);
      if not found then
         return;
      end if;

      eventType := Shift_Right (event.control and TRB_TYPE_MASK, 10);
      if eventType /= TRB_TYPE_TRANSFER_EVENT then
         mouseDiagnostics.unexpectedEvents :=
           mouseDiagnostics.unexpectedEvents + 1;
         return;
      end if;

      mouseDiagnostics.transferEvents :=
        mouseDiagnostics.transferEvents + 1;

      eventSlot := Natural (Shift_Right (event.control, 24));
      eventEP := Natural (Shift_Right (event.control, 16) and 16#1F#);
      if eventSlot /= mouseSlot or else eventEP /= hidEndpointDCI then
         return;
      end if;

      eventPointer :=
        (Unsigned_64 (event.parameterLo) or
         Shift_Left (Unsigned_64 (event.parameterHi), 32)) and
        not Unsigned_64 (16#F#);
      if eventPointer < transferBase or else
         eventPointer >= transferLimit or else
         ((eventPointer - transferBase) mod 16) /= 0
      then
         return;
      end if;

      transferIndex := Natural ((eventPointer - transferBase) / 16);
      reportOffset := transferIndex * HID_REPORT_STRIDE;
      completion := Shift_Right (event.status, 24);
      residual := Natural (event.status and 16#00FF_FFFF#);
      if residual <= hidMaxPacket then
         actualLength := hidMaxPacket - residual;
      end if;

      --  Replenish before decoding or publishing so the hardware always has
      --  work available even if downstream input handling is preempted.
      Queue_HID_Transfer;
      Ring_HID_Doorbell;

      mouseDiagnostics.lastCompletion := Unsigned_8 (completion and 16#FF#);
      mouseDiagnostics.lastLength :=
        Unsigned_8
          (Natural'Min (actualLength, Natural (Unsigned_8'Last)));
      mouseDiagnostics.lastReport :=
        Unsigned_32 (deviceDMA (mouseSlot).hidReports (reportOffset)) or
        Shift_Left (Unsigned_32 (deviceDMA (mouseSlot).hidReports (reportOffset + 1)), 8) or
        Shift_Left (Unsigned_32 (deviceDMA (mouseSlot).hidReports (reportOffset + 2)), 16) or
        Shift_Left (Unsigned_32 (deviceDMA (mouseSlot).hidReports (reportOffset + 3)), 24);

      if completion /= COMPLETION_SUCCESS and then
         completion /= COMPLETION_SHORT_PACKET
      then
         mouseDiagnostics.completionErrors :=
           mouseDiagnostics.completionErrors + 1;
         return;
      elsif actualLength < 3 then
         mouseDiagnostics.shortReports := mouseDiagnostics.shortReports + 1;
         return;
      end if;

      buttons := deviceDMA (mouseSlot).hidReports (reportOffset) and 7;
      if deviceDMA (mouseSlot).hidReports (reportOffset + 1) < 128 then
         deltaX := Integer (deviceDMA (mouseSlot).hidReports (reportOffset + 1));
      else
         deltaX := Integer (deviceDMA (mouseSlot).hidReports (reportOffset + 1)) - 256;
      end if;
      if deviceDMA (mouseSlot).hidReports (reportOffset + 2) < 128 then
         deltaY := Integer (deviceDMA (mouseSlot).hidReports (reportOffset + 2));
      else
         deltaY := Integer (deviceDMA (mouseSlot).hidReports (reportOffset + 2)) - 256;
      end if;
      --  The HID boot-mouse minimum is three bytes.  Wheel mice commonly
      --  append one signed byte while retaining that prefix, so accept it
      --  when present without requiring it from strict boot-only devices.
      if actualLength >= 4 then
         if deviceDMA (mouseSlot).hidReports (reportOffset + 3) < 128 then
            deltaZ := Integer (deviceDMA (mouseSlot).hidReports (reportOffset + 3));
         else
            deltaZ := Integer (deviceDMA (mouseSlot).hidReports (reportOffset + 3)) - 256;
         end if;
      end if;
      mouseDiagnostics.decodedReports :=
        mouseDiagnostics.decodedReports + 1;
      if deltaX /= 0 or else deltaY /= 0 then
         mouseDiagnostics.motionReports :=
           mouseDiagnostics.motionReports + 1;
      end if;
      if buttons /= diagnosticButtons then
         mouseDiagnostics.buttonTransitions :=
           mouseDiagnostics.buttonTransitions + 1;
         diagnosticButtons := buttons;
      end if;
      ready := True;
   end Poll_Boot_Mouse;

end XHCI;
