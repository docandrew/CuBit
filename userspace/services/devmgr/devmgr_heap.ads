------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  devmgr's process heap: CuAlloc (userspace/allocator), THE allocator, as
--  System.Memory uses it. Devmgr_Growable stores are instantiated over
--  these two calls.
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with Interfaces; use Interfaces;

package Devmgr_Heap is

   --  A block of Bytes aligned to Alignment, or Null_Address (refused).
   function Allocate (Bytes, Alignment : Unsigned_64) return System.Address;
   procedure Release (Block : System.Address);

end Devmgr_Heap;
