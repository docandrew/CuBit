with System;
with System.Storage_Elements; use System.Storage_Elements;
package BuddyAllocator is
    type Order is range 0 .. 12;
    function blockSize (O : Order) return Storage_Count;
    function getOrder (Bytes : Storage_Count) return Order;
    procedure alloc (O : Order; Address : out System.Address);
    procedure free (O : Order; Address : System.Address);
end BuddyAllocator;
