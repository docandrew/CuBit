pragma Ada_2022;
with CuAlloc;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with Owned_Memory_ABI;

package body CuAlloc_Native is
   package ABI renames CuBit.Kernel_ABI;

   --  The kernel's owned regions (ABI-002), one call each. Reserve: the
   --  region's base is a multiple of the alignment. Commit: CuAlloc asks
   --  for the next prefix of a region, [Base + Offset, + Bytes), fresh
   --  zero pages. Release: the whole region with everything committed.
   --  A refusal (Owned_Memory_ABI) is no base: 0.
   function Reserve (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
     (declare Answer : constant Unsigned_64 :=
        CuBit.Kernel_Calls.Call (ABI.Reserve_Owned_Region, Bytes, Alignment);
      begin (if Owned_Memory_ABI.Is_Refusal_Word (Answer) then 0 else Answer));
   function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean is
     (CuBit.Kernel_Calls.Call
        (ABI.Change_Owned_Range, Base + Offset, Bytes,
         ABI.Owned_Range_Operation'Pos (ABI.Commit)) = 0);
   function Release (Base, Bytes : Unsigned_64) return Boolean is
     (CuBit.Kernel_Calls.Call (ABI.Release_Owned_Region, Base, Bytes) = 0);
   --  CuBit's thread exits call cualloc_thread_exit themselves.
   procedure Attached is null;

   --  One commit backs any range (MEM-007): CuAlloc's commits are bounded
   --  only by what it wants, here the largest page multiple.
   Any_Commit : constant Unsigned_64 := Unsigned_64'Last - (ABI.Page_Bytes - 1);

   package Heap is new CuAlloc (Reserve, Commit, Release, Any_Commit, Attached);

   function Decommit (Address, Bytes : Unsigned_64) return Boolean is
     (CuBit.Kernel_Calls.Call
        (ABI.Change_Owned_Range, Address, Bytes,
         ABI.Owned_Range_Operation'Pos (ABI.Decommit)) = 0);

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
