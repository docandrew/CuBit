with Interfaces; use Interfaces;
with XHCI_Capabilities;

--  One allocation contract for devmgr and the controller driver. All offsets
--  are derived so enlarging scratch storage cannot overlap the device rings.
package XHCI_DMA_Layout with SPARK_Mode => On, Pure is
   PAGE_SIZE : constant Unsigned_64 := 4096;
   MAX_DEVICE_SLOTS : constant := 8;
   DEVICE_PAGES : constant := 8;
   BULK_DATA_PAGES : constant := 8;

   DCBAA_OFFSET : constant Unsigned_64 := 0;
   COMMAND_RING_OFFSET : constant Unsigned_64 := DCBAA_OFFSET + PAGE_SIZE;
   EVENT_RING_OFFSET : constant Unsigned_64 := COMMAND_RING_OFFSET + PAGE_SIZE;
   ERST_OFFSET : constant Unsigned_64 := EVENT_RING_OFFSET + PAGE_SIZE;
   DEVICE_CONTEXT_OFFSET : constant Unsigned_64 := ERST_OFFSET + PAGE_SIZE;
   INPUT_CONTEXT_OFFSET : constant Unsigned_64 := DEVICE_CONTEXT_OFFSET + PAGE_SIZE;
   EP0_RING_OFFSET : constant Unsigned_64 := INPUT_CONTEXT_OFFSET + PAGE_SIZE;
   HID_RING_OFFSET : constant Unsigned_64 := EP0_RING_OFFSET + PAGE_SIZE;
   DESCRIPTOR_OFFSET : constant Unsigned_64 := HID_RING_OFFSET + PAGE_SIZE;
   HID_REPORT_OFFSET : constant Unsigned_64 := DESCRIPTOR_OFFSET + PAGE_SIZE;
   BULK_OUT_RING_OFFSET : constant Unsigned_64 := HID_REPORT_OFFSET + PAGE_SIZE;
   BULK_IN_RING_OFFSET : constant Unsigned_64 := BULK_OUT_RING_OFFSET + PAGE_SIZE;
   DEVICE_STRIDE : constant Unsigned_64 := DEVICE_PAGES * PAGE_SIZE;
   BULK_DATA_OFFSET : constant Unsigned_64 :=
     DEVICE_CONTEXT_OFFSET + MAX_DEVICE_SLOTS * DEVICE_STRIDE;
   BULK_CBW_OFFSET : constant Unsigned_64 :=
     BULK_DATA_OFFSET + BULK_DATA_PAGES * PAGE_SIZE;
   BULK_CSW_OFFSET : constant Unsigned_64 := BULK_CBW_OFFSET + PAGE_SIZE;
   --  Variable scratchpad storage is last: runtime ring addresses stay fixed.
   SCRATCH_ARRAY_OFFSET : constant Unsigned_64 := BULK_CSW_OFFSET + PAGE_SIZE;
   FIXED_PAGES : constant := 78;
   subtype Allocation_Order is Natural range 7 .. 11;
   subtype Page_Count is Natural range FIXED_PAGES .. 2048;
   type Allocation is record
      Pointer_Pages : Natural range 0 .. 2;
      Scratch_First : Unsigned_64;
      Used_Pages : Page_Count;
      Order : Allocation_Order;
   end record;
   function Plan (Count : XHCI_Capabilities.Scratchpad_Buffer_Count)
     return Allocation;
   function Pages (Item : Allocation) return Page_Count is (2 ** Item.Order);
   function Bytes (Item : Allocation) return Unsigned_64 is
     (Unsigned_64 (Pages (Item)) * PAGE_SIZE);

   --  Compile-time checks, not kernel/userspace runtime assertions.
   pragma Compile_Time_Error
     (BULK_IN_RING_OFFSET + PAGE_SIZE /= DEVICE_CONTEXT_OFFSET + DEVICE_STRIDE,
      "device layout does not match its stride");
   pragma Compile_Time_Error
     (SCRATCH_ARRAY_OFFSET /= FIXED_PAGES * PAGE_SIZE,
      "fixed DMA page count does not match ring layout");
end XHCI_DMA_Layout;
