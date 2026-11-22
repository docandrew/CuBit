pragma Ada_2022;
with CuAlloc;
with Linux_Provider;
with Thread_Exit_Key;

package body CuAlloc_Host is
   --  The heap, as on CuBit (CuAlloc_Native); a thread that takes a heap
   --  arms the thread-exit destructor.
   package Heap is new CuAlloc
     (Linux_Provider.Reserve, Linux_Provider.Commit, Linux_Provider.Release,
      Linux_Provider.Maximum_Commit, Thread_Exit_Key.Arm);

   function Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
     (Heap.Allocate (Bytes, Alignment));
   function Allocate_Zeroed (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
     (Heap.Allocate_Zeroed (Bytes, Alignment));
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
   procedure Set_Quota (Bytes : Unsigned_64) is
   begin
      Linux_Provider.Set_Quota (Bytes);
   end Set_Quota;
   function Committed return Unsigned_64 is (Linux_Provider.Committed);
   procedure Statistics (Arenas, Reserved, Committed : out Unsigned_64) is
   begin
      Heap.Statistics (Arenas, Reserved, Committed);
   end Statistics;
end CuAlloc_Host;
