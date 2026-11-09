--  CuAlloc's process heap on Linux memory (Linux_Provider): every size path,
--  growth past one arena, alignment, realloc, zeroing, huge blocks given
--  back, running out of quota without losing live blocks, and a random
--  stress whose shadow model checks that live blocks never overlap and keep
--  their contents; then threads: a thread's exit and re-entry, and a stress
--  of generations of tasks freeing each other's blocks, exiting, and the
--  next generation adopting their heaps.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Numerics.Discrete_Random;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Heap;
with Linux_Provider;
with Thread_Stress;
with Remote_Twice;

procedure Main is
   Failures, Checks : Natural := 0;
   procedure Check (Good : Boolean; Name : String) is
   begin
      Checks := Checks + 1;
      if not Good then
         Put_Line ("FAIL " & Name);
         Failures := Failures + 1;
      end if;
   end Check;

   function Addr (V : Unsigned_64) return System.Address is (To_Address (Integer_Address (V)));
   procedure Fill (Item, Bytes : Unsigned_64; Value : Storage_Element) is
      B : Storage_Array (1 .. Storage_Offset (Bytes)) with Import, Address => Addr (Item);
   begin
      B := [others => Value];
   end Fill;
   function Holds (Item, Bytes : Unsigned_64; Value : Storage_Element) return Boolean is
      B : Storage_Array (1 .. Storage_Offset (Bytes)) with Import, Address => Addr (Item);
   begin
      return (for all X of B => X = Value);
   end Holds;

   A, B, C : Unsigned_64;
   --  Just over the medium limit: a reservation of its own.
   HUGE_BYTES : constant := 1_048_577;
   HUGE_USABLE : constant := 1_052_672;
begin
   --  Each path.
   A := Heap.Allocate (24, 16);
   Check (A /= 0 and then A mod 16 = 0 and then Heap.Usable_Size (A) = 32, "small: 24 bytes from the 32 class");
   B := Heap.Allocate (10_000, 16);
   Check (B /= 0 and then B mod 4096 = 0 and then Heap.Usable_Size (B) = 12_288, "medium: whole pages");
   C := Heap.Allocate (3_000_000, 16);
   Check (C /= 0 and then Heap.Usable_Size (C) >= 3_000_000, "huge: its own reservation");
   Fill (A, 24, 1); Fill (B, 10_000, 2); Fill (C, 3_000_000, 3);
   Check (Holds (A, 24, 1) and Holds (B, 10_000, 2) and Holds (C, 3_000_000, 3), "each holds what was written");
   declare
      Reserved : constant Unsigned_64 := Heap.Reserved_Bytes;
   begin
      Heap.Free (C);
      Check (Heap.Reserved_Bytes < Reserved and then Heap.Usable_Size (C) = 0, "a freed huge block goes back");
   end;
   Heap.Free (A); Heap.Free (B);
   Check (Heap.Allocate (24, 16) = A, "a freed small block is reused");
   Check (Heap.Allocate (10_000, 16) = B, "a freed medium run is reused");

   --  Alignment.
   for Shift in 4 .. 30 loop
      declare
         Align : constant Unsigned_64 := 2 ** Shift;
         Item : constant Unsigned_64 := Heap.Allocate (100, Align);
      begin
         Check (Item /= 0 and then Item mod Align = 0, "alignment" & Align'Image);
         Heap.Free (Item);
      end;
   end loop;
   Check (Heap.Allocate (16, 48) = 0, "a non-power-of-two alignment is refused");

   --  Growth past one arena: 40 MiB of 1 KiB blocks.
   declare
      Count : constant := 40 * 1024;
      Items : array (1 .. Count) of Unsigned_64;
      Ok : Boolean := True;
   begin
      for I in Items'Range loop
         Items (I) := Heap.Allocate (1000, 16);
         Ok := Ok and Items (I) /= 0;
      end loop;
      Check (Ok and then Heap.Arena_Count >= 4, "40 MiB of small blocks spread over new arenas");
      for I in Items'Range loop Heap.Free (Items (I)); end loop;
   end;

   --  Realloc and zeroing.
   A := Heap.Allocate (100, 16);
   Fill (A, 100, 9);
   B := Heap.Reallocate (A, 100_000);
   Check (B /= 0 and then B /= A and then Holds (B, 100, 9) and then Heap.Usable_Size (A) = 0,
          "realloc grows into a new block, contents kept, old freed");
   Check (Heap.Reallocate (B, 50) = B, "a smaller realloc stays");
   C := Heap.Reallocate (B, 5_000_000);
   Check (C /= 0 and then Holds (C, 100, 9), "realloc into a huge block");
   Heap.Free (C);
   A := Heap.Allocate (64, 16); Fill (A, 64, 16#FF#); Heap.Free (A);
   B := Heap.Allocate_Zeroed (64, 16);
   Check (B = A and then Holds (B, 64, 0), "a reused block comes back zeroed");
   Heap.Free (B);

   --  Sized frees (Rust's dealloc): small, medium, a medium block shrunk
   --  in place and freed with its small size, and a huge block shrunk to a
   --  small size, which moves into an arena (regression: it stayed huge, and
   --  a sized free of the new size read an arena header that is not there).
   declare
      S1 : constant Unsigned_64 := Heap.Allocate (40, 16);
      M1 : constant Unsigned_64 := Heap.Allocate (20_000, 16);
      M2 : constant Unsigned_64 := Heap.Allocate (30_000, 16);
      H1 : constant Unsigned_64 := Heap.Allocate (3_000_000, 16);
      Shrunk, Moved : Unsigned_64;
   begin
      Heap.Free_Sized (S1, 40, 16);
      Heap.Free_Sized (M1, 20_000, 16);
      Check (Heap.Usable_Size (S1) = 0 and then Heap.Usable_Size (M1) = 0, "sized frees release small and medium");
      Shrunk := Heap.Reallocate (M2, 100);
      Check (Shrunk = M2, "a medium block shrunk to 100 bytes stays");
      Heap.Free_Sized (Shrunk, 100, 16);
      Check (Heap.Usable_Size (M2) = 0, "and its sized free (as small) finds it");
      Fill (H1, 100, 4);
      Moved := Heap.Reallocate (H1, 100);
      Check (Moved /= H1 and then Moved /= 0 and then Holds (Moved, 100, 4) and then Heap.Usable_Size (H1) = 0,
             "a huge block shrunk to a small size moves into an arena, contents kept");
      Heap.Free_Sized (Moved, 100, 16);
      Check (Heap.Usable_Size (Moved) = 0, "and its sized free releases it");
   end;

   --  Invalid frees are ignored.
   Heap.Free (12345); Heap.Free (A + 8); Heap.Free (0);
   Check (Heap.Usable_Size (12345) = 0, "an unknown address holds nothing");

   --  Running out: refusals leave live blocks alone.
   declare
      Kept : constant Unsigned_64 := Heap.Allocate (512, 16);
   begin
      Fill (Kept, 512, 5);
      Linux_Provider.Set_Quota (Linux_Provider.Committed);
      Check (Heap.Allocate (8_000_000, 16) = 0, "a huge block beyond the quota is refused");
      declare
         Item : Unsigned_64;
         Refused : Boolean := False;
      begin
         for I in 1 .. 100_000 loop
            Item := Heap.Allocate (4000, 16);
            if Item = 0 then Refused := True; exit; end if;
         end loop;
         Check (Refused, "small blocks run out at the quota");
      end;
      Check (Holds (Kept, 512, 5), "a live block survives the refusals");
      Linux_Provider.Set_Quota (Unsigned_64'Last);
      Check (Heap.Allocate (8_000_000, 16) /= 0, "with quota again, it allocates");
   end;

   --  A refused release keeps the huge block's reservation in the heap's
   --  accounts (it is still the process's), and freeing it again retries.
   declare
      Item : constant Unsigned_64 := Heap.Allocate (3_000_000, 4096);
      Reserved, Committed : Unsigned_64;
   begin
      Reserved := Heap.Reserved_Bytes;
      Committed := Heap.Committed_Bytes;
      Linux_Provider.Refuse_Release := True;
      Heap.Free (Item);
      Check (Heap.Reserved_Bytes = Reserved and then Heap.Committed_Bytes = Committed,
             "a refused release keeps the reservation counted");
      Check (Heap.Usable_Size (Item) = 0, "the refused block is no longer live");
      Linux_Provider.Refuse_Release := False;
      Heap.Free (Item);
      Check (Heap.Reserved_Bytes < Reserved, "freeing it again gives it back");
   end;

   --  The huge directory grows past its first backing step (26,214
   --  records), and every block still routes to its own reservation.
   declare
      COUNT : constant := 27_000;
      Items : array (1 .. COUNT) of Unsigned_64;
      Arenas : constant Natural := Heap.Arena_Count;
      Ok : Boolean := True;
   begin
      for I in Items'Range loop
         Items (I) := Heap.Allocate (HUGE_BYTES, 16);
         Ok := Ok and then Items (I) /= 0;
      end loop;
      Check (Ok and then Heap.Arena_Count = Arenas + COUNT, "27,000 huge blocks, each its own reservation");
      for I in Items'Range loop
         Ok := Ok and then Heap.Usable_Size (Items (I)) = HUGE_USABLE
           and then Heap.Usable_Size (Items (I) + 4096) = 0;
      end loop;
      Check (Ok, "each routes to its own arena; an interior address is no block");
      for I in Items'Range loop
         Heap.Free (Items (I));
      end loop;
      Check (Heap.Arena_Count = Arenas, "all given back");
   end;

   --  Random stress against a shadow model.
   declare
      LIVE : constant := 2_000;
      type Slot is record
         Item, Size : Unsigned_64 := 0;
         Value : Storage_Element := 0;
      end record;
      Slots : array (1 .. LIVE) of Slot;
      subtype Pick is Positive range 1 .. LIVE;
      package Picks is new Ada.Numerics.Discrete_Random (Pick);
      subtype Sizes is Positive range 1 .. 3_000_000;
      package Size_Picks is new Ada.Numerics.Discrete_Random (Sizes);
      G : Picks.Generator;
      Z : Size_Picks.Generator;
      Corrupt, Overlap, Unaligned : Boolean := False;
   begin
      Picks.Reset (G, 11); Size_Picks.Reset (Z, 13);
      for Step in 1 .. 300_000 loop
         declare
            K : constant Pick := Picks.Random (G);
            R : constant Unsigned_64 := Unsigned_64 (Size_Picks.Random (Z));
         begin
            if Slots (K).Item /= 0 then
               Corrupt := Corrupt or else not Holds (Slots (K).Item, Slots (K).Size, Slots (K).Value);
               Heap.Free (Slots (K).Item);
               Slots (K).Item := 0;
            else
               Slots (K).Size :=
                 (if Step mod 997 = 0 then R elsif Step mod 31 = 0 then R mod 200_000 + 1 else R mod 2_000 + 1);
               Slots (K).Item := Heap.Allocate (Slots (K).Size, 16);
               Slots (K).Value := Storage_Element (Step mod 251);
               if Slots (K).Item /= 0 then
                  Unaligned := Unaligned or else Slots (K).Item mod 16 /= 0;
                  Fill (Slots (K).Item, Slots (K).Size, Slots (K).Value);
               end if;
            end if;
         end;
      end loop;
      for I in Slots'Range loop
         if Slots (I).Item /= 0 then
            Corrupt := Corrupt or else not Holds (Slots (I).Item, Slots (I).Size, Slots (I).Value);
            for J in I + 1 .. Slots'Last loop
               if Slots (J).Item /= 0 then
                  Overlap := Overlap or else
                    (Slots (I).Item < Slots (J).Item + Slots (J).Size and then
                     Slots (J).Item < Slots (I).Item + Slots (I).Size);
               end if;
            end loop;
         end if;
      end loop;
      Check (not Corrupt, "300,000 random steps: no block corrupted");
      Check (not Overlap, "no two live blocks overlap");
      Check (not Unaligned, "every block 16-byte aligned");
   end;
   --  A thread's exit leaves its heap; the same thread takes one again
   --  (adopting its own) and its blocks are still its own.
   declare
      Kept : constant Unsigned_64 := Heap.Allocate (200, 16);
      Again : Unsigned_64;
   begin
      Fill (Kept, 200, 7);
      Heap.Thread_Exit;
      Heap.Thread_Exit;
      Again := Heap.Allocate (200, 16);
      Check (Again /= 0 and then Again /= Kept and then Holds (Kept, 200, 7)
             and then Heap.Usable_Size (Kept) = 256, "after Thread_Exit the thread takes a heap again");
      Heap.Free (Again);
      Heap.Free (Kept);
      Check (Heap.Allocate (200, 16) = Kept, "its blocks are freed locally again");
   end;

   Check (Remote_Twice.Run, "a remote free that fills its batch is sent once (no block handed out twice)");

   --  Threads: cross-thread frees, exits, adoption.
   declare
      Outcome : Thread_Stress.Outcome;
   begin
      Thread_Stress.Run (Workers => 8, Generations => 12, Rounds => 20_000, Result => Outcome);
      Put_Line ("threads: 8 workers x 12 generations;" & Outcome.Blocks'Image & " blocks freed by another thread;"
                & " arenas before" & Outcome.Before_Arenas'Image
                & ", after the first generation" & Outcome.First_Arenas'Image & ", after the last"
                & Outcome.Last_Arenas'Image);
      Check (not Outcome.Corrupt, "threads: every block held its pattern until freed");
      Check (not Outcome.Unaligned and then not Outcome.Refused, "threads: every allocation aligned and served");
      Check (Outcome.Blocks > 100_000, "threads: blocks moved between threads" & Outcome.Blocks'Image);
      --  Without adoption each generation would add heaps and arenas.
      Check (Outcome.First_Arenas > Outcome.Before_Arenas and then Outcome.Last_Arenas <= Outcome.First_Arenas + 8,
             "threads: later generations adopt the heaps (arenas" & Outcome.First_Arenas'Image & " ->"
             & Outcome.Last_Arenas'Image & ")");
   end;
   Check (Linux_Provider.Contract_Violations = 0, "the provider's contract is never broken");

   Put_Line ("cualloc:" & Checks'Image & " checks," & Failures'Image & " failures; arenas"
             & Heap.Arena_Count'Image & ", reserved" & Heap.Reserved_Bytes'Image
             & ", committed" & Heap.Committed_Bytes'Image);
   if Failures > 0 then Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure); end if;
end Main;
