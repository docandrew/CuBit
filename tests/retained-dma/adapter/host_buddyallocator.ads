with Interfaces;
with System;
with System.Storage_Elements;
with Virtmem;
package BuddyAllocator is
   type Charge_Refund_Handler is access procedure
     (Identity, Pages : Interfaces.Unsigned_64);
   procedure installChargeRefundHandler
     (Handler : not null Charge_Refund_Handler; Success : out Boolean);
   function getTotalBytes return System.Storage_Elements.Storage_Count;
   procedure alloc (Order : Natural; Page : out System.Address);
   procedure free (Order : Natural; Page : System.Address);
   procedure allocFrame (Frame : out Virtmem.PhysAddress);
   procedure freeFrame (Frame : Virtmem.PhysAddress);
   procedure bindKernelFrameCharge
     (Frame : Virtmem.PhysAddress; Charge : Interfaces.Unsigned_64; OK : out Boolean);
   procedure Set_Bind_Failure (Enabled : Boolean);
   procedure Set_Failure (Nth : Natural);
   function Live_Bytes return Interfaces.Unsigned_64;
   procedure Complete_Physical (Charge, Pages : Interfaces.Unsigned_64);
end BuddyAllocator;
