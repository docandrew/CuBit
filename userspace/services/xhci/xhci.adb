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

package body XHCI is

   PAGE_SIZE     : constant Unsigned_64 := 4096;
   DMA_VIRT_BASE : constant Unsigned_64 := 16#0000_7000_0000_0000#;
   BAR_VIRT_BASE : constant Unsigned_64 := 16#0000_6000_0000_0000#;
   DMA_PAGES     : constant Natural := 64;

   --  DMA layout.  Every controller-visible object begins on its own page.
   DCBAA_OFFSET       : constant Unsigned_64 := 0 * PAGE_SIZE;
   COMMAND_RING_OFFSET : constant Unsigned_64 := 1 * PAGE_SIZE;
   EVENT_RING_OFFSET  : constant Unsigned_64 := 2 * PAGE_SIZE;
   ERST_OFFSET        : constant Unsigned_64 := 3 * PAGE_SIZE;
   SCRATCH_ARRAY_OFFSET : constant Unsigned_64 := 4 * PAGE_SIZE;
   SCRATCH_FIRST_OFFSET : constant Unsigned_64 := 5 * PAGE_SIZE;
   MAX_SCRATCHPADS    : constant Natural := 16;
   DEVICE_CONTEXT_OFFSET : constant Unsigned_64 := 21 * PAGE_SIZE;
   INPUT_CONTEXT_OFFSET  : constant Unsigned_64 := 22 * PAGE_SIZE;
   EP0_RING_OFFSET       : constant Unsigned_64 := 23 * PAGE_SIZE;
   HID_RING_OFFSET       : constant Unsigned_64 := 24 * PAGE_SIZE;
   DESCRIPTOR_OFFSET     : constant Unsigned_64 := 25 * PAGE_SIZE;
   HID_REPORT_OFFSET     : constant Unsigned_64 := 26 * PAGE_SIZE;

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

   type TRB is record
      parameterLo : Unsigned_32;
      parameterHi : Unsigned_32;
      status      : Unsigned_32;
      control     : Unsigned_32;
   end record with Convention => C, Size => 128;

   for TRB use record
      parameterLo at  0 range 0 .. 31;
      parameterHi at  4 range 0 .. 31;
      status      at  8 range 0 .. 31;
      control     at 12 range 0 .. 31;
   end record;

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
   activeSlot   : Natural := 0;
   dmaPhysical  : Unsigned_64 := 0;
   commandTail  : Natural := 0;
   commandCycle : Unsigned_32 := TRB_CYCLE;
   eventHead    : Natural := 0;
   eventCycle   : Unsigned_32 := TRB_CYCLE;
   ep0Tail      : Natural := 0;
   ep0Cycle     : Unsigned_32 := TRB_CYCLE;
   hidTail      : Natural := 0;
   hidCycle     : Unsigned_32 := TRB_CYCLE;
   hidEndpointDCI : Natural := 0;
   hidMaxPacket : Natural := 0;
   hidTransfersStarted : Boolean := False;

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
   scratchPointers : Address_Array (0 .. MAX_SCRATCHPADS - 1) with Import,
     Address => To_Address
       (Integer_Address (DMA_VIRT_BASE + SCRATCH_ARRAY_OFFSET));
   deviceContext : DWord_Array (0 .. 1023) with Import,
     Address => To_Address
       (Integer_Address (DMA_VIRT_BASE + DEVICE_CONTEXT_OFFSET));
   inputContext : DWord_Array (0 .. 1023) with Import,
     Address => To_Address
       (Integer_Address (DMA_VIRT_BASE + INPUT_CONTEXT_OFFSET));
   ep0Ring : TRB_Array (0 .. COMMAND_RING_ENTRIES - 1) with Import,
     Address => To_Address
       (Integer_Address (DMA_VIRT_BASE + EP0_RING_OFFSET));
   hidRing : TRB_Array (0 .. COMMAND_RING_ENTRIES - 1) with Import,
     Address => To_Address
       (Integer_Address (DMA_VIRT_BASE + HID_RING_OFFSET));
   descriptorBytes : Byte_Array (0 .. 4095) with Import,
     Address => To_Address
       (Integer_Address (DMA_VIRT_BASE + DESCRIPTOR_OFFSET));
   hidReports : Byte_Array (0 .. 4095) with Import,
     Address => To_Address
       (Integer_Address (DMA_VIRT_BASE + HID_REPORT_OFFSET));

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

   procedure Clear_DMA is
      bytes : array (Natural range 0 .. DMA_PAGES * 4096 - 1) of Unsigned_8
        with Import, Address => To_Address (Integer_Address (DMA_VIRT_BASE));
   begin
      for i in bytes'Range loop
         bytes (i) := 0;
      end loop;
   end Clear_DMA;

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
         ignore := syscall (SYSCALL_SLEEP, 1);
         if (Read32 (base, offset) and mask) = expected then
            return True;
         end if;
      end loop;
      --  Include a terminal observation outside the tight polling loop.
      --  Besides defining the timeout boundary precisely, this lets an
      --  emulator commit a deferred MMIO state transition at the loop edge.
      return (Read32 (base, offset) and mask) = expected;
   end Wait_For_Bits;

   function Scratchpad_Count (hcsParams2 : Unsigned_32) return Natural is
      low  : constant Unsigned_32 := Shift_Right (hcsParams2, 21) and 16#1F#;
      high : constant Unsigned_32 := Shift_Right (hcsParams2, 27) and 16#1F#;
   begin
      return Natural (Shift_Left (high, 5) or low);
   end Scratchpad_Count;

   procedure Acknowledge_Event is
      dequeue : constant Unsigned_64 :=
        dmaPhysical + EVENT_RING_OFFSET + Unsigned_64 (eventHead * 16);
   begin
      --  EHB is RW1C.  Advancing ERDP and setting EHB acknowledges the event
      --  without enabling interrupts; this first driver deliberately polls.
      Write64 (runtimeBase + RT_INTR0, INTR_ERDP, dequeue or 16#8#);
   end Acknowledge_Event;

   function Poll_Event (event : out TRB) return Boolean is
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
   end Poll_Event;

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
   begin
      completionCode := 0;
      slotId := 0;
      completed := False;
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
         if Poll_Event (event) then
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
         else
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

   procedure Queue_EP0 (item : TRB) is
      pending : TRB := item;
   begin
      pending.control := (pending.control and not TRB_CYCLE) or ep0Cycle;
      ep0Ring (ep0Tail) := pending;
      if ep0Tail = COMMAND_RING_ENTRIES - 2 then
         ep0Ring (COMMAND_RING_ENTRIES - 1).control :=
           Shift_Left (TRB_TYPE_LINK, TRB_TYPE_SHIFT) or
           TRB_TOGGLE_CYCLE or ep0Cycle;
         ep0Tail := 0;
         ep0Cycle := ep0Cycle xor TRB_CYCLE;
      else
         ep0Tail := ep0Tail + 1;
      end if;
   end Queue_EP0;

   procedure Wait_For_Transfer
     (endpoint       : Natural;
      requested      : Natural;
      completionCode : out Unsigned_32;
      actualLength   : out Natural;
      completed      : out Boolean)
   is
      event      : TRB;
      eventType  : Unsigned_32;
      residual   : Natural;
      eventSlot  : Natural;
      eventEP    : Natural;
      ignore     : Unsigned_64;
   begin
      completionCode := 0;
      actualLength := 0;
      completed := False;
      for attempt in 1 .. 10_000 loop
         if Poll_Event (event) then
            eventType := Shift_Right (event.control and TRB_TYPE_MASK, 10);
            if eventType = TRB_TYPE_TRANSFER_EVENT then
               eventSlot := Natural (Shift_Right (event.control, 24));
               eventEP := Natural
                 (Shift_Right (event.control, 16) and 16#1F#);
               if eventSlot = activeSlot and then eventEP = endpoint then
                  completionCode := Shift_Right (event.status, 24);
                  residual := Natural (event.status and 16#00FF_FFFF#);
                  if residual <= requested then
                     actualLength := requested - residual;
                  end if;
                  completed := True;
                  return;
               end if;
            end if;
         else
            ignore := syscall (SYSCALL_SLEEP, 1);
         end if;
      end loop;
   end Wait_For_Transfer;

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
      completed     : Boolean;
      dataIn        : constant Boolean := (requestType and 16#80#) /= 0;
   begin
      actualLength := 0;
      success := False;
      if length > 4096 then
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
      Queue_EP0
        ((parameterLo => setupLo, parameterHi => setupHi, status => 8,
          control => setupControl));

      if length > 0 then
         Queue_EP0
           ((parameterLo => Unsigned_32 (bufferPhys and 16#FFFF_FFFF#),
             parameterHi => Unsigned_32 (Shift_Right (bufferPhys, 32)),
             status => Unsigned_32 (length),
             control => Shift_Left (TRB_TYPE_DATA_STAGE, TRB_TYPE_SHIFT) or
               (if dataIn then TRB_DIRECTION_IN else 0)));
      end if;

      statusControl := Shift_Left (TRB_TYPE_STATUS_STAGE, TRB_TYPE_SHIFT) or
        TRB_IOC;
      if length = 0 or else not dataIn then
         statusControl := statusControl or TRB_DIRECTION_IN;
      end if;
      Queue_EP0
        ((parameterLo => 0, parameterHi => 0, status => 0,
          control => statusControl));

      Write32 (doorbellBase, Storage_Offset (activeSlot * 4), 1);
      Wait_For_Transfer
        (1, length, completion, actualLength, completed);
      success := completed and then
        (completion = COMPLETION_SUCCESS or else
         completion = COMPLETION_SHORT_PACKET);
      if completed and then not success then
         Debug_Hex32 ("xhci: control completion=", completion);
      end if;
   end Control_Request;

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
      result   : out Init_Result)
   is
      ignore     : Unsigned_64;
      capLength  : Unsigned_8;
      hcsParams1 : Unsigned_32;
      hcsParams2 : Unsigned_32;
      hccParams1 : Unsigned_32;
      maxSlots   : Natural;
      enabledSlots : Natural;
      scratchpads : Natural;
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
      ep0Tail := 0;
      ep0Cycle := TRB_CYCLE;
      hidTail := 0;
      hidCycle := TRB_CYCLE;
      hidEndpointDCI := 0;
      hidMaxPacket := 0;
      hidTransfersStarted := False;
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

      scratchpads := Scratchpad_Count (hcsParams2);
      if scratchpads > MAX_SCRATCHPADS then
         result := INIT_SCRATCHPAD_LIMIT;
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

      Clear_DMA;

      if scratchpads > 0 then
         for i in 0 .. scratchpads - 1 loop
            scratchPointers (i) :=
              dmaPhys + SCRATCH_FIRST_OFFSET + Unsigned_64 (i) * PAGE_SIZE;
         end loop;
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

      enabledSlots := Natural'Min (maxSlots, 8);
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

      --  Reset the selected USB2 root port.  Status-change fields are RW1C,
      --  so write them as zero while preserving the remaining port state.
      portStatus := Read32
        (operational,
         OP_PORTSC_BASE + Storage_Offset (activePort - 1) * OP_PORT_STRIDE);
      Write32
        (operational,
         OP_PORTSC_BASE + Storage_Offset (activePort - 1) * OP_PORT_STRIDE,
         (portStatus and not PORTSC_CHANGE_BITS) or PORTSC_PR);
      if not Wait_For_Bits
        (operational,
         OP_PORTSC_BASE + Storage_Offset (activePort - 1) * OP_PORT_STRIDE,
         PORTSC_PR or PORTSC_PED, PORTSC_PED, 10_000)
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

      Submit_Command
        ((parameterLo => 0, parameterHi => 0, status => 0,
          control => Shift_Left (TRB_TYPE_ENABLE_SLOT, TRB_TYPE_SHIFT)),
         commandCompletion, slotId, commandDone);
      if not commandDone then
         result := INIT_COMMAND_TIMEOUT;
         return;
      elsif commandCompletion /= COMPLETION_SUCCESS or else slotId = 0 then
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

      dcbaa (activeSlot) := dmaPhys + DEVICE_CONTEXT_OFFSET;
      --  Input Control Context: add Slot and Endpoint 0 contexts.
      inputContext (0) := 0;
      inputContext (1) := 3;
      --  Input Slot Context is context index 1.
      inputContext (contextStride) :=
        Shift_Left (portSpeed, 20) or Shift_Left (1, 27);
      inputContext (contextStride + 1) :=
        Shift_Left (Unsigned_32 (activePort), 16);
      --  Input Endpoint 0 Context is context index 2.
      endpointBase := 2 * contextStride;
      inputContext (endpointBase + 1) :=
        Shift_Left (3, 1) or Shift_Left (4, 3) or
        Shift_Left (maxPacketSize, 16);
      inputContext (endpointBase + 2) :=
        Unsigned_32 ((dmaPhys + EP0_RING_OFFSET) and 16#FFFF_FFFF#) or
        TRB_CYCLE;
      inputContext (endpointBase + 3) :=
        Unsigned_32 (Shift_Right (dmaPhys + EP0_RING_OFFSET, 32));
      inputContext (endpointBase + 4) := 8;

      ep0Ring (COMMAND_RING_ENTRIES - 1) :=
        (parameterLo =>
           Unsigned_32 ((dmaPhys + EP0_RING_OFFSET) and 16#FFFF_FFFF#),
         parameterHi =>
           Unsigned_32 (Shift_Right (dmaPhys + EP0_RING_OFFSET, 32)),
         status => 0,
         control => Shift_Left (TRB_TYPE_LINK, TRB_TYPE_SHIFT) or
           TRB_TOGGLE_CYCLE or TRB_CYCLE);

      Submit_Command
        ((parameterLo =>
            Unsigned_32 ((dmaPhys + INPUT_CONTEXT_OFFSET) and 16#FFFF_FFFF#),
          parameterHi =>
            Unsigned_32 (Shift_Right (dmaPhys + INPUT_CONTEXT_OFFSET, 32)),
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
         deviceContext (3) and 16#FF#);

      declare
         actualLength       : Natural;
         requestOK          : Boolean;
         totalLength        : Natural;
         position           : Natural;
         descriptorLength   : Natural;
         configValue        : Unsigned_8 := 0;
         interfaceNumber    : Unsigned_8 := 0;
         endpointAddress    : Unsigned_8 := 0;
         endpointPacketSize : Natural := 0;
         endpointInterval   : Unsigned_8 := 0;
         inBootMouse        : Boolean := False;
         endpointNumber     : Natural;
         endpointDCI        : Natural;
         endpointInputBase  : Natural;
         endpointIntervalXHCI : Unsigned_32;
         vendorProduct      : Unsigned_32;
      begin
         --  Read only bounded standard descriptors into a private DMA page.
         Control_Request
           (16#80#, 6, 16#0100#, 0, 18,
            dmaPhys + DESCRIPTOR_OFFSET, actualLength, requestOK);
         if not requestOK or else actualLength < 18 or else
            descriptorBytes (0) < 18 or else descriptorBytes (1) /= 1
         then
            result := INIT_DESCRIPTOR_FAILED;
            return;
         end if;
         vendorProduct := Unsigned_32 (descriptorBytes (8)) or
           Shift_Left (Unsigned_32 (descriptorBytes (9)), 8) or
           Shift_Left (Unsigned_32 (descriptorBytes (10)), 16) or
           Shift_Left (Unsigned_32 (descriptorBytes (11)), 24);
         Debug_Hex32 ("xhci: USB vendor/product=", vendorProduct);

         Control_Request
           (16#80#, 6, 16#0200#, 0, 9,
            dmaPhys + DESCRIPTOR_OFFSET, actualLength, requestOK);
         if not requestOK or else actualLength < 9 or else
            descriptorBytes (0) < 9 or else descriptorBytes (1) /= 2
         then
            result := INIT_DESCRIPTOR_FAILED;
            return;
         end if;
         totalLength := Natural (descriptorBytes (2)) +
           Natural (descriptorBytes (3)) * 256;
         configValue := descriptorBytes (5);
         if totalLength < 9 or else totalLength > descriptorBytes'Length or else
            configValue = 0
         then
            result := INIT_DESCRIPTOR_FAILED;
            return;
         end if;

         Control_Request
           (16#80#, 6, 16#0200#, 0, totalLength,
            dmaPhys + DESCRIPTOR_OFFSET, actualLength, requestOK);
         if not requestOK or else actualLength < totalLength then
            result := INIT_DESCRIPTOR_FAILED;
            return;
         end if;

         --  Accept one HID boot-mouse interface and one interrupt-IN
         --  endpoint.  Every descriptor length is validated before fields
         --  are inspected, so malformed device data cannot escape the page.
         position := 0;
         while position + 2 <= totalLength loop
            descriptorLength := Natural (descriptorBytes (position));
            if descriptorLength < 2 or else
               descriptorLength > totalLength - position
            then
               result := INIT_DESCRIPTOR_FAILED;
               return;
            end if;

            if descriptorBytes (position + 1) = 4 and then
               descriptorLength >= 9
            then
               inBootMouse :=
                 descriptorBytes (position + 5) = 3 and then
                 descriptorBytes (position + 6) = 1 and then
                 descriptorBytes (position + 7) = 2;
               if inBootMouse then
                  interfaceNumber := descriptorBytes (position + 2);
               end if;
            elsif descriptorBytes (position + 1) = 5 and then
                  descriptorLength >= 7 and then inBootMouse and then
                  (descriptorBytes (position + 2) and 16#80#) /= 0 and then
                  (descriptorBytes (position + 3) and 3) = 3 and then
                  endpointAddress = 0
            then
               endpointAddress := descriptorBytes (position + 2);
               endpointPacketSize :=
                 Natural
                   ((Unsigned_16 (descriptorBytes (position + 4)) or
                     Shift_Left
                       (Unsigned_16 (descriptorBytes (position + 5)), 8)) and
                    16#07FF#);
               endpointInterval := descriptorBytes (position + 6);
            end if;
            position := position + descriptorLength;
         end loop;

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
            inputContext (i) := 0;
         end loop;
         inputContext (1) := Unsigned_32 (1) or
           Shift_Left (Unsigned_32 (1), endpointDCI);
         for i in 0 .. contextStride - 1 loop
            inputContext (contextStride + i) := deviceContext (i);
         end loop;
         inputContext (contextStride) :=
           (inputContext (contextStride) and not 16#F800_0000#) or
           Shift_Left (Unsigned_32 (endpointDCI), 27);

         endpointInputBase := (endpointDCI + 1) * contextStride;
         endpointIntervalXHCI :=
           Boot_Interval (portSpeed, endpointInterval);
         inputContext (endpointInputBase) :=
           Shift_Left (endpointIntervalXHCI, 16);
         inputContext (endpointInputBase + 1) :=
           Shift_Left (Unsigned_32 (3), 1) or
           Shift_Left (Unsigned_32 (7), 3) or
           Shift_Left (Unsigned_32 (endpointPacketSize), 16);
         inputContext (endpointInputBase + 2) :=
           Unsigned_32 ((dmaPhys + HID_RING_OFFSET) and 16#FFFF_FFFF#) or
           TRB_CYCLE;
         inputContext (endpointInputBase + 3) :=
           Unsigned_32 (Shift_Right (dmaPhys + HID_RING_OFFSET, 32));
         inputContext (endpointInputBase + 4) :=
           Unsigned_32 (endpointPacketSize) or
           Shift_Left (Unsigned_32 (endpointPacketSize), 16);

         hidRing (COMMAND_RING_ENTRIES - 1) :=
           (parameterLo =>
              Unsigned_32 ((dmaPhys + HID_RING_OFFSET) and 16#FFFF_FFFF#),
            parameterHi =>
              Unsigned_32 (Shift_Right (dmaPhys + HID_RING_OFFSET, 32)),
            status => 0,
            control => Shift_Left (TRB_TYPE_LINK, TRB_TYPE_SHIFT) or
              TRB_TOGGLE_CYCLE or TRB_CYCLE);

         Submit_Command
           ((parameterLo =>
               Unsigned_32
                 ((dmaPhys + INPUT_CONTEXT_OFFSET) and 16#FFFF_FFFF#),
             parameterHi =>
               Unsigned_32
                 (Shift_Right (dmaPhys + INPUT_CONTEXT_OFFSET, 32)),
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

      result := INIT_OK;
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
        dmaPhysical + HID_REPORT_OFFSET + Unsigned_64 (reportOffset);
      pending      : TRB;
   begin
      --  Every live TRB owns a distinct cache-line-sized report buffer.  The
      --  queue depth is smaller than the transfer ring, so a buffer is never
      --  cleared while the controller may still be writing it.
      for i in reportOffset .. reportOffset + HID_REPORT_STRIDE - 1 loop
         hidReports (i) := 0;
      end loop;

      pending :=
        (parameterLo => Unsigned_32 (reportPhys and 16#FFFF_FFFF#),
         parameterHi => Unsigned_32 (Shift_Right (reportPhys, 32)),
         status      => Unsigned_32 (hidMaxPacket),
         control     => Shift_Left (TRB_TYPE_NORMAL, TRB_TYPE_SHIFT) or
           TRB_IOC or hidCycle);
      hidRing (hidTail) := pending;

      if hidTail = COMMAND_RING_ENTRIES - 2 then
         hidRing (COMMAND_RING_ENTRIES - 1).control :=
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
        (doorbellBase, Storage_Offset (activeSlot * 4),
         Unsigned_32 (hidEndpointDCI));
   end Ring_HID_Doorbell;

   procedure Start_Boot_Mouse_Transfers is
   begin
      if hidTransfersStarted or else activeSlot = 0 or else
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

   procedure Poll_Boot_Mouse
     (buttons : out Unsigned_8;
      deltaX  : out Integer;
      deltaY  : out Integer;
      ready   : out Boolean;
      eventAvailable : out Boolean)
   is
      event          : TRB;
      eventType      : Unsigned_32;
      eventSlot      : Natural;
      eventEP        : Natural;
      eventPointer   : Unsigned_64;
      transferBase   : constant Unsigned_64 :=
        dmaPhysical + HID_RING_OFFSET;
      transferLimit  : constant Unsigned_64 :=
        transferBase + Unsigned_64 ((COMMAND_RING_ENTRIES - 1) * 16);
      transferIndex  : Natural;
      reportOffset   : Natural;
      completion     : Unsigned_32;
      residual       : Natural;
      actualLength   : Natural := 0;
   begin
      buttons := 0;
      deltaX := 0;
      deltaY := 0;
      ready := False;
      eventAvailable := Poll_Event (event);
      if not eventAvailable then
         return;
      end if;

      eventType := Shift_Right (event.control and TRB_TYPE_MASK, 10);
      if eventType /= TRB_TYPE_TRANSFER_EVENT then
         return;
      end if;

      eventSlot := Natural (Shift_Right (event.control, 24));
      eventEP := Natural (Shift_Right (event.control, 16) and 16#1F#);
      if eventSlot /= activeSlot or else eventEP /= hidEndpointDCI then
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

      if (completion /= COMPLETION_SUCCESS and then
          completion /= COMPLETION_SHORT_PACKET) or else
         actualLength < 3
      then
         return;
      end if;

      buttons := hidReports (reportOffset) and 7;
      if hidReports (reportOffset + 1) < 128 then
         deltaX := Integer (hidReports (reportOffset + 1));
      else
         deltaX := Integer (hidReports (reportOffset + 1)) - 256;
      end if;
      if hidReports (reportOffset + 2) < 128 then
         deltaY := Integer (hidReports (reportOffset + 2));
      else
         deltaY := Integer (hidReports (reportOffset + 2)) - 256;
      end if;
      ready := True;
   end Poll_Boot_Mouse;

end XHCI;
