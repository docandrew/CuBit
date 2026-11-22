with Interfaces; use Interfaces;
with XHCI_Capabilities;

--  One allocation contract for devmgr and the controller driver. All offsets
--  are derived so enlarging the device-slot or scratch storage cannot overlap
--  the controller rings.
--
--  The allocation is sized from the controller's own capability registers
--  (LIM-001): one device area per slot it reports (HCSPARAMS1 MaxSlots,
--  xHCI 1.2 section 5.3.3) and the scratchpad buffers it asks for
--  (HCSPARAMS2). Fixed controller pages come first, then the device areas,
--  then the scratchpad array and buffers.
package XHCI_DMA_Layout with SPARK_Mode => On, Pure is
   PAGE_SIZE : constant Unsigned_64 := 4096;
   --  MaxSlots is an 8-bit field and slot ID 0 is the scratchpad entry of
   --  the device context base address array, so 255 is the protocol's limit.
   PROTOCOL_MAX_SLOTS : constant := 255;
   subtype Slot_Count is Natural range 1 .. PROTOCOL_MAX_SLOTS;
   DEVICE_PAGES : constant := 8;
   BULK_DATA_PAGES : constant := 8;

   DCBAA_OFFSET : constant Unsigned_64 := 0;
   COMMAND_RING_OFFSET : constant Unsigned_64 := DCBAA_OFFSET + PAGE_SIZE;
   EVENT_RING_OFFSET : constant Unsigned_64 := COMMAND_RING_OFFSET + PAGE_SIZE;
   ERST_OFFSET : constant Unsigned_64 := EVENT_RING_OFFSET + PAGE_SIZE;
   BULK_DATA_OFFSET : constant Unsigned_64 := ERST_OFFSET + PAGE_SIZE;
   BULK_CBW_OFFSET : constant Unsigned_64 :=
     BULK_DATA_OFFSET + BULK_DATA_PAGES * PAGE_SIZE;
   BULK_CSW_OFFSET : constant Unsigned_64 := BULK_CBW_OFFSET + PAGE_SIZE;

   --  Slot 1's device area; slot S's is DEVICE_STRIDE * (S - 1) further.
   DEVICE_CONTEXT_OFFSET : constant Unsigned_64 := BULK_CSW_OFFSET + PAGE_SIZE;
   INPUT_CONTEXT_OFFSET : constant Unsigned_64 := DEVICE_CONTEXT_OFFSET + PAGE_SIZE;
   EP0_RING_OFFSET : constant Unsigned_64 := INPUT_CONTEXT_OFFSET + PAGE_SIZE;
   HID_RING_OFFSET : constant Unsigned_64 := EP0_RING_OFFSET + PAGE_SIZE;
   DESCRIPTOR_OFFSET : constant Unsigned_64 := HID_RING_OFFSET + PAGE_SIZE;
   HID_REPORT_OFFSET : constant Unsigned_64 := DESCRIPTOR_OFFSET + PAGE_SIZE;
   BULK_OUT_RING_OFFSET : constant Unsigned_64 := HID_REPORT_OFFSET + PAGE_SIZE;
   BULK_IN_RING_OFFSET : constant Unsigned_64 := BULK_OUT_RING_OFFSET + PAGE_SIZE;
   DEVICE_STRIDE : constant Unsigned_64 := DEVICE_PAGES * PAGE_SIZE;
   --  A composite HID device (boot keyboard and boot mouse interfaces on one
   --  slot) needs a second interrupt-IN ring and report page. A slot is
   --  never both a HID device and the bulk-storage device, so the second
   --  pipe uses the bulk ring pages: ring in the bulk-IN page, reports in
   --  the bulk-OUT page.
   SECOND_INTERRUPT_RING_OFFSET   : constant Unsigned_64 := BULK_IN_RING_OFFSET;
   SECOND_INTERRUPT_REPORT_OFFSET : constant Unsigned_64 := BULK_OUT_RING_OFFSET;

   --  Controller pages before the first device area.
   FIXED_PAGES : constant := 14;
   MAX_POINTER_PAGES : constant := 2;
   --  The most any controller can ask for: every slot, every scratchpad.
   MAX_USED_PAGES : constant :=
     FIXED_PAGES + PROTOCOL_MAX_SLOTS * DEVICE_PAGES + MAX_POINTER_PAGES +
     XHCI_Capabilities.Scratchpad_Buffer_Count'Last;
   --  The smallest power of two holding one slot and nothing else, up to
   --  the smallest holding MAX_USED_PAGES.
   subtype Allocation_Order is Natural range 5 .. 12;
   subtype Page_Count is Natural range FIXED_PAGES + DEVICE_PAGES .. 2 ** Allocation_Order'Last;
   type Allocation is record
      Slots : Slot_Count;
      Pointer_Pages : Natural range 0 .. MAX_POINTER_PAGES;
      Scratch_Array : Unsigned_64;
      Scratch_First : Unsigned_64;
      Used_Pages : Page_Count;
      Order : Allocation_Order;
   end record;
   function Plan
     (Count : XHCI_Capabilities.Scratchpad_Buffer_Count; Slots : Slot_Count)
     return Allocation
   with Post => Plan'Result.Slots = Slots
                and then Plan'Result.Used_Pages <= 2 ** Plan'Result.Order;
   function Pages (Item : Allocation) return Page_Count is (2 ** Item.Order);
   function Bytes (Item : Allocation) return Unsigned_64 is
     (Unsigned_64 (Pages (Item)) * PAGE_SIZE);

   --  The most slots whose allocation (with Count scratchpads) fits in
   --  Ceiling bytes; 0 when not even one slot fits.
   function Slots_Within
     (Count : XHCI_Capabilities.Scratchpad_Buffer_Count; Wanted : Slot_Count;
      Ceiling : Unsigned_64) return Natural
   with Post => Slots_Within'Result <= Wanted
                and then (if Slots_Within'Result > 0 then
                            Bytes (Plan (Count, Slots_Within'Result)) <= Ceiling);

   --  Configure-message word 1 (devmgr to the driver): BAR pages in the
   --  low 32 bits, then the allocation's scratchpad count and slot count.
   CONFIG_PAGES_MASK : constant Unsigned_64 := 16#FFFF_FFFF#;
   CONFIG_SCRATCHPAD_SHIFT : constant := 32;
   CONFIG_SCRATCHPAD_MASK : constant Unsigned_64 := 16#FFFF#;
   CONFIG_SLOTS_SHIFT : constant := 48;
   CONFIG_SLOTS_MASK : constant Unsigned_64 := 16#FFFF#;
   function Configuration_Word
     (Bar_Pages : Unsigned_32; Item : Allocation;
      Count : XHCI_Capabilities.Scratchpad_Buffer_Count) return Unsigned_64 is
     (Unsigned_64 (Bar_Pages) or Shift_Left (Unsigned_64 (Count), CONFIG_SCRATCHPAD_SHIFT)
      or Shift_Left (Unsigned_64 (Item.Slots), CONFIG_SLOTS_SHIFT));
   function Scratchpads_Of (Word : Unsigned_64) return Unsigned_64 is
     (Shift_Right (Word, CONFIG_SCRATCHPAD_SHIFT) and CONFIG_SCRATCHPAD_MASK);
   function Slots_Of (Word : Unsigned_64) return Unsigned_64 is
     (Shift_Right (Word, CONFIG_SLOTS_SHIFT) and CONFIG_SLOTS_MASK);

   --  Compile-time checks, not kernel/userspace runtime assertions.
   pragma Compile_Time_Error
     (BULK_IN_RING_OFFSET + PAGE_SIZE /= DEVICE_CONTEXT_OFFSET + DEVICE_STRIDE,
      "device layout does not match its stride");
   pragma Compile_Time_Error
     (DEVICE_CONTEXT_OFFSET /= FIXED_PAGES * PAGE_SIZE,
      "fixed DMA page count does not match ring layout");
   pragma Compile_Time_Error
     (MAX_USED_PAGES > 2 ** Allocation_Order'Last
      or else MAX_USED_PAGES <= 2 ** (Allocation_Order'Last - 1),
      "the largest allocation order is not the smallest holding every slot");
   pragma Compile_Time_Error
     (FIXED_PAGES + DEVICE_PAGES > 2 ** Allocation_Order'First
      or else FIXED_PAGES + DEVICE_PAGES <= 2 ** (Allocation_Order'First - 1),
      "the smallest allocation order is not the smallest holding one slot");
end XHCI_DMA_Layout;
