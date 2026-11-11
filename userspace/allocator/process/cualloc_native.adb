pragma Ada_2022;
with CuAlloc;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;

package body CuAlloc_Native is
   package ABI renames CuBit.Kernel_ABI;

   --  RESERVE_OWNED_MEMORY (bytes, alignment): the kernel places the
   --  reservation at a multiple of the alignment.
   function Reserve (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
     (CuBit.Kernel_Calls.Call (ABI.Reserve_Owned_Memory, Bytes, Alignment));
   function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean is
     (CuBit.Kernel_Calls.Call (ABI.Commit_Owned_Memory_Prefix, Base, Offset, Bytes) = 0);
   function Release (Base, Bytes : Unsigned_64) return Boolean is
     (CuBit.Kernel_Calls.Call (ABI.Release_Owned_Reservation, Base, Bytes) = 0);
   --  CuBit's thread exits call cualloc_thread_exit themselves.
   procedure Attached is null;

   package Heap is new CuAlloc (Reserve, Commit, Release, ABI.Maximum_Owned_Commit_Bytes, Attached);

   function Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
     (Heap.Allocate (Bytes, Alignment));

   function Allocate_Zeroed (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
     (Heap.Allocate_Zeroed (Bytes, Alignment));

   function Allocate_Quick (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
     (Heap.Allocate_Quick (Bytes, Alignment));
   function Allocate_Quick (Bytes, Alignment : Unsigned_64; Untouched : out Boolean) return Unsigned_64 is
      Item : Unsigned_64;
   begin
      Item := Heap.Allocate_Quick (Bytes, Alignment, Untouched);
      return Item;
   end Allocate_Quick;
   function Allocate_Rest (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
     (Heap.Allocate_Rest (Bytes, Alignment));
   function Allocate_Zeroed_Rest (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
     (Heap.Allocate_Zeroed_Rest (Bytes, Alignment));

   function Reallocate_Quick (Item, Bytes : Unsigned_64) return Unsigned_64 is
     (Heap.Reallocate_Quick (Item, Bytes));

   procedure Free (Item : Unsigned_64) is
   begin
      Heap.Free (Item);
   end Free;

   procedure Free_Sized (Item, Bytes, Alignment : Unsigned_64) is
   begin
      Heap.Free_Sized (Item, Bytes, Alignment);
   end Free_Sized;
   function Usable_Size (Item : Unsigned_64) return Unsigned_64 is (Heap.Usable_Size (Item));

   function Reallocate (Item, Bytes : Unsigned_64) return Unsigned_64 is (Heap.Reallocate (Item, Bytes));

   procedure Thread_Exit is
   begin
      Heap.Thread_Exit;
   end Thread_Exit;
end CuAlloc_Native;
