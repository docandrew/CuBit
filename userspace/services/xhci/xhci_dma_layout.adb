package body XHCI_DMA_Layout with SPARK_Mode => On is
   function Plan (Count : XHCI_Capabilities.Scratchpad_Buffer_Count)
     return Allocation
   is
      --  Each pointer is eight bytes; each controller page is 4096 bytes.
      Pointer_Pages : constant Natural := (Count + 511) / 512;
      Required : constant Page_Count := FIXED_PAGES + Pointer_Pages + Count;
      Order : constant Allocation_Order :=
        (if Required <= 128 then 7
         elsif Required <= 256 then 8
         elsif Required <= 512 then 9
         elsif Required <= 1024 then 10
         else 11);
   begin
      return
        (Pointer_Pages => Pointer_Pages,
         Scratch_First => SCRATCH_ARRAY_OFFSET +
           Unsigned_64 (Pointer_Pages) * PAGE_SIZE,
         Used_Pages => Required, Order => Order);
   end Plan;
end XHCI_DMA_Layout;
