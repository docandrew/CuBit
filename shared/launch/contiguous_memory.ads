------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The largest physically contiguous block the kernel hands out, which the
--  kernel and the user runtimes agree on (LIM-001).
--
--  @description
--  The kernel's buddy allocator tracks blocks up to 2 ** Maximum_Order
--  pages (Config.MAX_BUDDY_ORDER is this constant) and ALLOC_DMA refuses a
--  larger order. devmgr sizes every DMA allocation by this order, and the
--  manifest compiler's DMA ceiling (CCL.Resource_Sections.MAX_DMA_BYTES) is
--  checked against Maximum_Bytes where both are seen, so a manifest can
--  never declare a DMA region the kernel cannot allocate.
--
--  Raising Maximum_Order costs physical memory on every machine (each
--  usable region is trimmed to the largest block's alignment, see
--  BuddyAllocator) to admit one contiguous region no driver asks for.
------------------------------------------------------------------------------
pragma Ada_2022;

package Contiguous_Memory with Pure, SPARK_Mode is

   Page_Bytes : constant := 4096;
   Maximum_Order : constant := 13;
   Maximum_Pages : constant := 2 ** Maximum_Order;
   Maximum_Bytes : constant := Maximum_Pages * Page_Bytes;

   subtype Order is Natural range 0 .. Maximum_Order;
   subtype Page_Count is Positive range 1 .. Maximum_Pages;

   --  The smallest order holding Pages pages.
   function Order_For (Pages : Page_Count) return Order
   with Post => 2 ** Order_For'Result >= Pages
                and then (Order_For'Result = 0 or else 2 ** (Order_For'Result - 1) < Pages);

end Contiguous_Memory;
