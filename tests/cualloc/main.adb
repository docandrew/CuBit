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
with Parked_Twice;

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

   type Unsigned_64_Array is array (Positive range <>) of Unsigned_64;
   --  A request of each small class's size.
   Classes : constant Unsigned_64_Array := [16, 32, 48, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 384, 448, 512, 640, 768, 896, 1_024, 1_280, 1_536, 1_792, 2_048, 2_560, 3_072, 3_584, 4_096];
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
   --  Size classes (ALLOC-001 step 12): four per power of two above 64
   --  bytes, so a request is rounded by at most a quarter; medium sizes
   --  likewise in pages above 8 pages.
   declare
      type Pair is record
         Bytes, Usable : Unsigned_64;
      end record;
      type Pairs is array (Positive range <>) of Pair;
      Cases : constant Pairs :=
        [(1, 16), (16, 16), (17, 32), (33, 48), (49, 64), (65, 80), (97, 112), (129, 160), (200, 224),
         (257, 320), (1_025, 1_280), (1_537, 1_792), (2_049, 2_560), (3_585, 4_096), (4_096, 4_096),
         (4_097, 8_192), (32_769, 40_960), (36_865, 40_960), (200_000, 229_376), (1_048_576, 1_048_576)];
      Item : Unsigned_64;
      Rounded : Boolean := True;
   begin
      for C of Cases loop
         Item := Heap.Allocate (C.Bytes, 16);
         Rounded := Rounded and then Item /= 0 and then Item mod 16 = 0 and then Heap.Usable_Size (Item) = C.Usable;
         if Item /= 0 then
            Fill (Item, C.Bytes, 16#3C#);
            Rounded := Rounded and then Holds (Item, C.Bytes, 16#3C#);
            Heap.Free (Item);
         end if;
      end loop;
      Check (Rounded, "small and medium requests take the smallest class that holds them");
   end;
   --  A freed medium block is parked for the class's next request (still
   --  allocated in its arena's core, marked parked): it reports no usable
   --  bytes, a second free of it is ignored (parking twice fails), so it
   --  is handed out once; an interior address is ignored too.
   declare
      M1 : constant Unsigned_64 := Heap.Allocate (20_000, 16);
      M2, M3 : Unsigned_64;
   begin
      Fill (M1, 20_000, 16#4D#);
      Heap.Free (M1 + 4_096);
      Check (Heap.Usable_Size (M1) = 20_480, "an interior address of a medium block is not freed");
      Heap.Free (M1);
      Check (Heap.Usable_Size (M1) = 0, "a parked medium block reports no usable bytes");
      Heap.Free (M1);
      M2 := Heap.Allocate (20_000, 16);
      M3 := Heap.Allocate (20_000, 16);
      Check (M2 = M1 and then M3 /= 0 and then M3 /= M1 and then Heap.Usable_Size (M2) = 20_480,
             "a medium block freed twice is parked once and handed out once");
      Heap.Free_Sized (M2, 20_000, 16);
      Heap.Free (M3);
      Check (Heap.Usable_Size (M2) = 0 and then Heap.Usable_Size (M3) = 0, "both parked or released");
   end;
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
   --  Small requests aligned beyond 16 bytes, several live at once (a
   --  regression: with classes of non-power-of-two strides (step 12) only
   --  a slab's first block was aligned; a single allocate-free loop always
   --  reused it and passed).
   declare
      Aligned : Boolean := True;
      Items : array (1 .. 5) of Unsigned_64;
   begin
      for Shift in 5 .. 12 loop
         for Size of Unsigned_64_Array'(1, 2 ** Shift / 2 + 1, 2 ** Shift - 1, 100, 1_000, 3_000) loop
            if Size <= 4_096 then
               for K in Items'Range loop
                  Items (K) := Heap.Allocate (Size, 2 ** Shift);
                  Aligned := Aligned and then Items (K) /= 0 and then Items (K) mod 2 ** Shift = 0
                    and then Heap.Usable_Size (Items (K)) >= Size;
               end loop;
               for K in Items'Range loop
                  Heap.Free (Items (K));
               end loop;
            end if;
         end loop;
      end loop;
      Check (Aligned, "small blocks aligned to 32 .. 4096, five live at once");
   end;

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
   --  Medium calloc (step 13: a medium block never handed out before is
   --  not cleared, its backing is still the provider's zero). More than the
   --  heap parks (8 a class): most of the first size's blocks are released,
   --  and the second size (another class, nothing parked) reuses their
   --  dirty pages; fresh, parked and reused blocks all read zero.
   declare
      Blocks : array (1 .. 40) of Unsigned_64;
      Zeroed : Boolean := True;
      procedure Round (Bytes : Unsigned_64; Value : Storage_Element) is
      begin
         for K in Blocks'Range loop
            Blocks (K) := Heap.Allocate_Zeroed (Bytes, 16);
            Zeroed := Zeroed and then Blocks (K) /= 0 and then Holds (Blocks (K), Bytes, 0);
            Fill (Blocks (K), Bytes, Value);
         end loop;
         for K in reverse Blocks'Range loop
            Heap.Free (Blocks (K));
         end loop;
      end Round;
   begin
      for R in 1 .. 2 loop
         Round (64 * 4_096, 16#A5#);
         Round (40 * 4_096 - 100, 16#5A#);
         Round (20 * 4_096, 16#3C#);
      end loop;
      Check (Zeroed, "medium calloc reads zero: fresh, parked, released and reused blocks");
   end;
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

   --  A sized free with the wrong size never corrupts (ALLOC-001 item 11):
   --  the size is not trusted. Huge blocks of every placement (plain, page,
   --  64 KiB, 32 MiB and 64 MiB alignment) freed with a small size are
   --  found through the radix map (their arena-header probe reads their own
   --  zero header page, or is not made at all); a block whose first word
   --  is a forged copy of a live small arena's identity is not taken for
   --  that arena; small blocks freed with another small size, or a huge
   --  one, are released from their slab; nothing else changes.
   declare
      type Alignments is array (Positive range <>) of Unsigned_64;
      Aligns : constant Alignments := [16, 4_096, 65_536, 33_554_432, 67_108_864];
      Canary : constant Unsigned_64 := Heap.Allocate (48, 16);
      Identity : Unsigned_64 with Import, Address => Addr (Canary - Canary mod 33_554_432);
      Forged : Unsigned_64;
      Reserved : Unsigned_64;
      Small, Other : Unsigned_64;
   begin
      Fill (Canary, 48, 16#5C#);
      Forged := Identity;
      Check (Forged /= 0, "a small arena carries its identity word");
      for Align of Aligns loop
         declare
            H : constant Unsigned_64 := Heap.Allocate (3_000_000, Align);
            First : Unsigned_64 with Import, Address => Addr (H);
         begin
            Check (H /= 0 and then H mod Align = 0, "huge block aligned" & Align'Image);
            First := Forged;
            Reserved := Heap.Reserved_Bytes;
            Heap.Free_Sized (H, 40, 16);
            Check (Heap.Usable_Size (H) = 0 and then Heap.Reserved_Bytes < Reserved,
                   "a huge block freed with a small size is released, alignment" & Align'Image);
         end;
      end loop;
      Check (Holds (Canary, 48, 16#5C#) and then Heap.Usable_Size (Canary) = 48,
             "the arena named by the forged identity is untouched");
      Small := Heap.Allocate (40, 16);
      Other := Heap.Allocate (100, 16);
      Heap.Free_Sized (Small, 2_000, 16);
      Heap.Free_Sized (Other, 5_000_000, 16);
      Check (Heap.Usable_Size (Small) = 0 and then Heap.Usable_Size (Other) = 0,
             "small blocks freed with another small size, or a huge size, are released");
      Check (Holds (Canary, 48, 16#5C#), "and the canary is still intact");
      Heap.Free_Sized (Canary, 48, 16);
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
             and then Heap.Usable_Size (Kept) = 224, "after Thread_Exit the thread takes a heap again");
      Heap.Free (Again);
      Heap.Free (Kept);
      --  Both come back to this heap's next two allocations of the class
      --  (the first freed is parked and taken first, step 13).
      declare
         First : constant Unsigned_64 := Heap.Allocate (200, 16);
         Second : constant Unsigned_64 := Heap.Allocate (200, 16);
      begin
         Check ((First = Kept and then Second = Again) or else (First = Again and then Second = Kept),
                "its blocks are freed locally again");
         Heap.Free (First);
         Heap.Free (Second);
      end;
   end;

   Check (Remote_Twice.Run, "a remote free that fills its batch is sent once (no block handed out twice)");

   --  Small double frees. A freed small block is parked when its heap has
   --  none (step 13: the heap's spare, live in its core, free to its heap,
   --  taken by its class's next allocation); others go back to the core.
   declare
      Parked, Plain, A, B, C, D, E : Unsigned_64;
      --  One block of every class, held through the checks: the heap's
      --  spare block (one per heap, any class) comes back to one of them,
      --  so the next free here is parked.
      Drained : Unsigned_64_Array (Classes'Range);
   begin
      for K in Classes'Range loop
         Drained (K) := Heap.Allocate (Classes (K), 16);
      end loop;
      Parked := Heap.Allocate (40, 16);
      Plain := Heap.Allocate (40, 16);
      Heap.Free (Parked);
      Check (Heap.Usable_Size (Parked) = 0 and then Heap.Reallocate (Parked, 10) = 0,
             "a parked small block reports no usable bytes and is not reallocated");
      Heap.Free (Parked);
      Heap.Free_Sized (Parked, 40, 16);
      Heap.Free (Plain);
      Heap.Free (Plain);
      Heap.Free_Sized (Plain, 40, 16);
      A := Heap.Allocate (40, 16);
      B := Heap.Allocate (40, 16);
      C := Heap.Allocate (40, 16);
      D := Heap.Allocate (40, 16);
      Check (A = Parked and then B = Plain, "the parked block, then the core's latest");
      Check (A /= 0 and then B /= 0 and then C /= 0 and then D /= 0 and then C /= A and then C /= B
             and then D /= A and then D /= B and then C /= D,
             "small blocks freed twice (parked or not, plain or sized) are handed out once");
      --  An interior address of a parked block, and of a live one: ignored.
      Heap.Free (A);
      Heap.Free (A + 16);
      Heap.Free (C + 16);
      E := Heap.Allocate (40, 16);
      Check (E = A and then Heap.Usable_Size (C) = 48, "interior addresses are not freed");
      --  A block the core released, freed again while nothing is parked:
      --  not live, so not parked.
      Heap.Free (E);
      Heap.Free (B);
      E := Heap.Allocate (40, 16);
      Heap.Free (B);
      B := Heap.Allocate (40, 16);
      A := Heap.Allocate (40, 16);
      Check (B /= A and then A /= C and then A /= D and then A /= E and then B /= 0 and then A /= 0,
             "a released block freed again is not parked");
      Heap.Free (A);
      Heap.Free (B);
      Heap.Free (C);
      Heap.Free (D);
      Heap.Free (E);
      for Item of Drained loop
         Heap.Free (Item);
      end loop;
   end;
   Check (Parked_Twice.Run, "a parked block freed again by another thread is handed out once");
   --  A slab whose last live block but one is freed while that one is
   --  parked: both go back to the core (the slab empties), and every block
   --  is handed out once.
   declare
      Items : array (1 .. 3 * 21) of Unsigned_64;
      Drained : Unsigned_64_Array (Classes'Range);
      Distinct : Boolean := True;
   begin
      --  Each round's first free is parked (the spare, whatever its class,
      --  comes back to one of these).
      for Round in 1 .. 2 loop
         for K in Classes'Range loop
            Drained (K) := Heap.Allocate (Classes (K), 16);
         end loop;
         for K in Items'Range loop
            Items (K) := Heap.Allocate (3_000, 16);
         end loop;
         for K in Items'Range loop
            for J in K + 1 .. Items'Last loop
               Distinct := Distinct and then Items (K) /= 0 and then Items (K) /= Items (J);
            end loop;
         end loop;
         --  Every block handed out is live (a stale parked block, free in
         --  its core, reports no usable bytes).
         for K in Items'Range loop
            Distinct := Distinct and then Heap.Usable_Size (Items (K)) = 3_072;
         end loop;
         for D in Drained'Range loop
            Distinct := Distinct and then Heap.Usable_Size (Drained (D)) = Classes (D);
         end loop;
         --  No item overlaps a block of Drained (a stale parked block
         --  handed out again may lie under a block of another class).
         for K in Items'Range loop
            for D in Drained'Range loop
               Distinct := Distinct and then Drained (D) /= 0
                 and then (Items (K) + 3_000 <= Drained (D) or else Drained (D) + Classes (D) <= Items (K));
            end loop;
         end loop;
         for D in Drained'Range loop
            for E in D + 1 .. Drained'Last loop
               Distinct := Distinct and then
                 (Drained (D) + Classes (D) <= Drained (E) or else Drained (E) + Classes (E) <= Drained (D));
            end loop;
         end loop;
         --  A block from the middle first (parked: its slab is full of
         --  Items), then the rest.
         Heap.Free (Items (Items'Last / 2));
         for K in Items'Range loop
            if K /= Items'Last / 2 then
               Heap.Free (Items (K));
            end if;
         end loop;
         for Item of Drained loop
            Heap.Free (Item);
         end loop;
      end loop;
      Check (Distinct, "a slab left with only its parked block empties; no block handed out twice");
   end;

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
