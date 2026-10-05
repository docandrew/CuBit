pragma Ada_2022;
with System;
with CuAlloc;
with Linux_Provider;

package body CuAlloc_Host is
   package Heap is new CuAlloc
     (Linux_Provider.Reserve, Linux_Provider.Commit, Linux_Provider.Release,
      Linux_Provider.Maximum_Commit);

   Locked : aliased Unsigned_8 := 0;
   function Exchange (Ptr : System.Address; Value : Unsigned_8; Model : Integer) return Unsigned_8
     with Import, Convention => Intrinsic, External_Name => "__atomic_exchange_1";
   procedure Store (Ptr : System.Address; Value : Unsigned_8; Model : Integer)
     with Import, Convention => Intrinsic, External_Name => "__atomic_store_1";
   ACQUIRE : constant := 2;
   RELEASE_ORDER : constant := 3;
   procedure Lock is
   begin
      while Exchange (Locked'Address, 1, ACQUIRE) /= 0 loop
         null;
      end loop;
   end Lock;
   procedure Unlock is
   begin
      Store (Locked'Address, 0, RELEASE_ORDER);
   end Unlock;

   function Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
      Item : Unsigned_64;
   begin
      Lock; Item := Heap.Allocate (Bytes, Alignment); Unlock;
      return Item;
   end Allocate;
   function Allocate_Zeroed (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
      Item : Unsigned_64;
   begin
      Lock; Item := Heap.Allocate_Zeroed (Bytes, Alignment); Unlock;
      return Item;
   end Allocate_Zeroed;
   procedure Free (Item : Unsigned_64) is
   begin
      Lock; Heap.Free (Item); Unlock;
   end Free;
   function Usable_Size (Item : Unsigned_64) return Unsigned_64 is
      Bytes : Unsigned_64;
   begin
      Lock; Bytes := Heap.Usable_Size (Item); Unlock;
      return Bytes;
   end Usable_Size;
   function Reallocate (Item, Bytes : Unsigned_64) return Unsigned_64 is
      Moved : Unsigned_64;
   begin
      Lock; Moved := Heap.Reallocate (Item, Bytes); Unlock;
      return Moved;
   end Reallocate;
   procedure Set_Quota (Bytes : Unsigned_64) is
   begin
      Lock; Linux_Provider.Quota := Bytes; Unlock;
   end Set_Quota;
   function Committed return Unsigned_64 is (Linux_Provider.Committed);
end CuAlloc_Host;
