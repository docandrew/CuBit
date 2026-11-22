package body XHCI_DMA_Layout with SPARK_Mode => On is
   function Plan
     (Count : XHCI_Capabilities.Scratchpad_Buffer_Count; Slots : Slot_Count)
     return Allocation
   is
      --  Each pointer is eight bytes; each controller page is 4096 bytes.
      Pointer_Pages : constant Natural := (Count + 511) / 512;
      Slot_Pages : constant Natural := Slots * DEVICE_PAGES;
      Required : constant Page_Count :=
        FIXED_PAGES + Slot_Pages + Pointer_Pages + Count;
      Order : Allocation_Order := Allocation_Order'First;
      Scratch_Array : constant Unsigned_64 :=
        DEVICE_CONTEXT_OFFSET + Unsigned_64 (Slot_Pages) * PAGE_SIZE;
   begin
      while 2 ** Order < Required loop
         pragma Loop_Invariant (2 ** Order < Required);
         pragma Loop_Variant (Increases => Order);
         Order := Order + 1;
      end loop;
      return
        (Slots => Slots,
         Pointer_Pages => Pointer_Pages,
         Scratch_Array => Scratch_Array,
         Scratch_First => Scratch_Array + Unsigned_64 (Pointer_Pages) * PAGE_SIZE,
         Used_Pages => Required, Order => Order);
   end Plan;

   function Slots_Within
     (Count : XHCI_Capabilities.Scratchpad_Buffer_Count; Wanted : Slot_Count;
      Ceiling : Unsigned_64) return Natural
   is
   begin
      for Slots in reverse 1 .. Wanted loop
         if Bytes (Plan (Count, Slots)) <= Ceiling then
            return Slots;
         end if;
      end loop;
      return 0;
   end Slots_Within;
end XHCI_DMA_Layout;
