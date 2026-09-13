with System;
with System.Storage_Elements; use System.Storage_Elements;
package BuddyAllocator is
   type Order is range 0 .. 10;
   Allow_Allocation : Boolean := True;
   Live_Blocks : Natural := 0;
   Attempts : Natural := 0;
   function blockSize (O : Order) return Storage_Count is (4096 * 2 ** Natural (O));
   function getOrder (Bytes : Storage_Count) return Order;
   procedure alloc (O : Order; Addr : out System.Address);
   procedure free (O : Order; Addr : System.Address);
end BuddyAllocator;
