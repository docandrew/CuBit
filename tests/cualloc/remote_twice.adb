pragma Ada_2022;
with Interfaces; use Interfaces;
with Heap;

package body Remote_Twice is
   --  A batch holds 62 blocks (CuAlloc's NODE_CAPACITY).
   BATCH : constant := 62;
   type Blocks is array (1 .. BATCH) of Unsigned_64;

   task type Helper is
      entry Free_All (Items : Blocks);
      entry Freed;
      entry Leave;
   end Helper;

   task body Helper is
      Held : Blocks;
   begin
      accept Free_All (Items : Blocks) do
         Held := Items;
      end Free_All;
      --  Remote frees: the 62nd fills the batch, which is published.
      for Item of Held loop
         Heap.Free (Item);
      end loop;
      accept Freed;
      accept Leave;
      --  Publish whatever is still batched here.
      Heap.Thread_Exit;
   end Helper;

   function Run return Boolean is
      First, Again : Blocks;
      Extra : Unsigned_64;
      Fresh : Boolean := True;
      H : Helper;
   begin
      for I in First'Range loop
         First (I) := Heap.Allocate (64, 16);
      end loop;
      H.Free_All (First);
      H.Freed;
      --  Take them back (Thread_Exit drains the inbox; the next allocation
      --  adopts this thread's own heap again), and reuse them.
      Heap.Thread_Exit;
      for I in Again'Range loop
         Again (I) := Heap.Allocate (64, 16);
      end loop;
      H.Leave;
      --  Wait for the helper's exit (its batches published), then drain.
      while not H'Terminated loop
         delay 0.001;
      end loop;
      Heap.Thread_Exit;
      Extra := Heap.Allocate (64, 16);
      for Item of Again loop
         Fresh := Fresh and then Item /= Extra;
      end loop;
      for Item of Again loop
         Heap.Free (Item);
      end loop;
      Heap.Free (Extra);
      return Fresh;
   end Run;
end Remote_Twice;
