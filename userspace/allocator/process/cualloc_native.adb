pragma Ada_2022;
with System;
with CuAlloc;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;

package body CuAlloc_Native is
   package ABI renames CuBit.Kernel_ABI;

   function Reserve (Bytes : Unsigned_64) return Unsigned_64 is
     (CuBit.Kernel_Calls.Call (ABI.Reserve_Owned_Memory, Bytes));
   function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean is
     (CuBit.Kernel_Calls.Call (ABI.Commit_Owned_Memory_Prefix, Base, Offset, Bytes) = 0);
   function Release (Base, Bytes : Unsigned_64) return Boolean is
     (CuBit.Kernel_Calls.Call (ABI.Release_Owned_Reservation, Base, Bytes) = 0);

   package Heap is new CuAlloc (Reserve, Commit, Release, ABI.Maximum_Owned_Commit_Bytes);

   Locked : aliased Unsigned_8 := 0;
   function Exchange (Ptr : System.Address; Value : Unsigned_8; Model : Integer) return Unsigned_8
     with Import, Convention => Intrinsic, External_Name => "__atomic_exchange_1";
   procedure Store (Ptr : System.Address; Value : Unsigned_8; Model : Integer)
     with Import, Convention => Intrinsic, External_Name => "__atomic_store_1";
   procedure Pause with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_pause";
   ACQUIRE : constant := 2;
   RELEASE_ORDER : constant := 3;
   procedure Lock is
   begin
      while Exchange (Locked'Address, 1, ACQUIRE) /= 0 loop
         Pause;
      end loop;
   end Lock;
   procedure Unlock is
   begin
      Store (Locked'Address, 0, RELEASE_ORDER);
   end Unlock;

   function Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
      Item : Unsigned_64;
   begin
      Lock;
      Item := Heap.Allocate (Bytes, Alignment);
      Unlock;
      return Item;
   end Allocate;

   function Allocate_Zeroed (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
      Item : Unsigned_64;
   begin
      Lock;
      Item := Heap.Allocate_Zeroed (Bytes, Alignment);
      Unlock;
      return Item;
   end Allocate_Zeroed;

   procedure Free (Item : Unsigned_64) is
   begin
      Lock;
      Heap.Free (Item);
      Unlock;
   end Free;

   function Usable_Size (Item : Unsigned_64) return Unsigned_64 is
      Bytes : Unsigned_64;
   begin
      Lock;
      Bytes := Heap.Usable_Size (Item);
      Unlock;
      return Bytes;
   end Usable_Size;

   function Reallocate (Item, Bytes : Unsigned_64) return Unsigned_64 is
      Moved : Unsigned_64;
   begin
      Lock;
      Moved := Heap.Reallocate (Item, Bytes);
      Unlock;
      return Moved;
   end Reallocate;
end CuAlloc_Native;
