pragma Ada_2022;
with CuAlloc;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Messages;

package body CuAlloc_Native is
   package ABI renames CuBit.Kernel_ABI;

   --  The kernel's owned regions (ABI-002), one call each. Reserve: the
   --  region's base is a multiple of the alignment. Commit: CuAlloc asks
   --  for the next prefix of a region, [Base + Offset, + Bytes), fresh
   --  zero pages. Release: the whole region with everything committed.
   --  A refused reservation or commit is said once per process, here and
   --  in the kernel's log line (BUG-009: an allocator null reached libgcc
   --  with no trace of why). Later ones are counted, not printed: the
   --  kernel's line has them, rate-limited.
   Refusals : Natural := 0 with Atomic;
   procedure Say_Refused (What : String; Bytes, Answer : Unsigned_64) is
      Seen : constant Natural := Refusals;
   begin
      if Seen < Natural'Last then
         Refusals := Seen + 1;
      end if;
      if Seen = 0 then
         CuBit.Messages.debugPrint
           ("cualloc: heap growth refused: " & What & " of" & Unsigned_64'Image (Bytes) &
            " bytes, kernel answer" & Unsigned_64'Image (Answer) & ASCII.LF);
      end if;
   end Say_Refused;

   function Reserve (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
      Base : constant Unsigned_64 :=
        CuBit.Kernel_Calls.Call (ABI.Reserve_Owned_Region, Bytes, Alignment);
   begin
      if Base = 0 or else Base = ABI.Failed then
         Say_Refused ("reserve", Bytes, Base);
      end if;
      return Base;
   end Reserve;
   function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean is
      Answer : constant Unsigned_64 := CuBit.Kernel_Calls.Call
        (ABI.Change_Owned_Range, Base + Offset, Bytes,
         ABI.Owned_Range_Operation'Pos (ABI.Commit));
   begin
      if Answer /= 0 then
         Say_Refused ("commit", Bytes, Answer);
      end if;
      return Answer = 0;
   end Commit;
   function Release (Base, Bytes : Unsigned_64) return Boolean is
     (CuBit.Kernel_Calls.Call (ABI.Release_Owned_Region, Base, Bytes) = 0);
   --  CuBit's thread exits call cualloc_thread_exit themselves.
   procedure Attached is null;

   package Heap is new CuAlloc (Reserve, Commit, Release, ABI.Maximum_Owned_Commit_Bytes, Attached);

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
