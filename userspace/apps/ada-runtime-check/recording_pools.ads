------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A CuBit.CuAlloc_Pool that records every Allocate and checks that each
--  Deallocate names the same address, size and alignment (RT-008 slice 3).
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.CuAlloc_Pool;

package Recording_Pools is

   Max_Live : constant := 32;
   subtype Live_Count is Natural range 0 .. Max_Live;

   type Block is record
      Address   : System.Address := System.Null_Address;
      Size      : Storage_Count := 0;
      Alignment : Storage_Count := 0;
   end record;
   type Block_Table is array (1 .. Max_Live) of Block;

   type Recording_Pool is new CuBit.CuAlloc_Pool.CuAlloc_Storage_Pool with record
      Live        : Block_Table;
      Allocations : Natural := 0;
      Frees       : Natural := 0;
      Exact_Frees : Natural := 0;   --  size and alignment as allocated
      Last_Size   : Storage_Count := 0;
   end record;

   overriding procedure Allocate
     (Pool                     : in out Recording_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count);

   overriding procedure Deallocate
     (Pool                     : in out Recording_Pool;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count);

end Recording_Pools;
