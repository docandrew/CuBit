pragma Ada_2022;
with Interfaces; use Interfaces;
with Heap;

package body Parked_Twice is
   task type Helper is
      entry Free_It (Item : Unsigned_64);
      entry Leave;
   end Helper;

   task body Helper is
      Held : Unsigned_64;
   begin
      accept Free_It (Item : Unsigned_64) do
         Held := Item;
      end Free_It;
      --  The second free, remote: into a batch for the owner.
      Heap.Free (Held);
      accept Leave;
      --  Publish the batch.
      Heap.Thread_Exit;
   end Helper;

   function Run return Boolean is
      type Blocks is array (Positive range <>) of Unsigned_64;
      Classes : constant Blocks := [16, 32, 48, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 384, 448, 512, 640, 768, 896, 1_024, 1_280, 1_536, 1_792, 2_048, 2_560, 3_072, 3_584, 4_096];
      Drained : Blocks (Classes'Range);
      Item, First, Second, Third : Unsigned_64;
      H : Helper;
   begin
      --  One block of every class: the heap's spare block, whatever its
      --  class, comes back to one of them; freeing Item then parks it.
      for K in Classes'Range loop
         Drained (K) := Heap.Allocate (Classes (K), 16);
      end loop;
      Item := Heap.Allocate (40, 16);
      Heap.Free (Item);
      H.Free_It (Item);
      H.Leave;
      while not H'Terminated loop
         delay 0.001;
      end loop;
      --  Take the remote free back (Thread_Exit drains the inbox; the next
      --  allocation adopts this thread's heap again).
      Heap.Thread_Exit;
      First := Heap.Allocate (40, 16);
      Second := Heap.Allocate (40, 16);
      Third := Heap.Allocate (40, 16);
      Heap.Free (First);
      Heap.Free (Second);
      Heap.Free (Third);
      for Block of Drained loop
         Heap.Free (Block);
      end loop;
      return First /= 0 and then Second /= 0 and then Third /= 0
        and then First /= Second and then Second /= Third and then First /= Third;
   end Run;
end Parked_Twice;
