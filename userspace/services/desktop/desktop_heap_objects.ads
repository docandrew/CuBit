------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Desktop's growable tables in the process heap, without stopping Desktop
--  when memory runs out.
--
--  @description
--  An Ada allocator that the heap cannot satisfy ends the process
--  (System.Memory). Desktop's surface table grows while clients ask for
--  windows, so it takes its memory from CuAlloc directly, which answers
--  null when it is out: the request is refused and Desktop says so. The
--  caller declares the new object at the address (default initialization,
--  so a fresh table starts empty), copies the old one into it, then frees
--  the old one. Not SPARK: address-to-access conversion is the boundary.
------------------------------------------------------------------------------
with System;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;

generic
   type Object (<>) is limited private;
package Desktop_Heap_Objects is
   package Conversions is new System.Address_To_Access_Conversions (Object);
   subtype Reference is Conversions.Object_Pointer;
   use type Reference;

   --  Room for Bytes (the constrained subtype's Max_Size_In_Storage_Elements),
   --  or Null_Address when the heap is out.
   function Allocate (Bytes : System.Storage_Elements.Storage_Count) return System.Address;

   function To_Reference (Where : System.Address) return Reference
     renames Conversions.To_Pointer;

   --  Returns room from Allocate (Null_Address: nothing).
   procedure Free (Where : System.Address);
   procedure Free (Item : in out Reference)
     with Post => Item = null;
end Desktop_Heap_Objects;
