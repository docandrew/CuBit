with Ada.Text_IO;
with Heap_Bitmap; use Heap_Bitmap;
with Heap_Classes;
with Slab_Model;
with Heap_Slab_Instance;
with Interfaces; use Interfaces;
procedure Slab_Tests is
   State : Pool;
   Item : Slot;
   OK : Boolean;
   Seen : array (Slot) of Boolean := [others => False];
   procedure Check (Condition : Boolean) is
   begin
      if not Condition then raise Program_Error with "slab regression"; end if;
   end Check;
begin
   for Size in Heap_Classes.Request_Size loop
      declare
         Expected : Positive := 16;
      begin
         while Expected < Size loop Expected := Expected * 2; end loop;
         Check (Heap_Classes.Stride (Heap_Classes.Class_For (Size)) = Expected);
      end;
   end loop;
   for Class in Heap_Classes.Size_Class loop
      for Offset in 0 .. Slab_Model.Slab_Bytes loop
         Check (Heap_Classes.Quotient (Offset, Class) = Offset / Heap_Classes.Stride (Class));
         Check (Heap_Classes.Aligned (Offset, Class) = (Offset mod Heap_Classes.Stride (Class) = 0));
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("PASS classes: all request sizes and slab-local quotient/alignment boundaries");
   for Position in Heap_Slab_Instance.Offset loop
      Check (Heap_Slab_Instance.Page_Of (Position) = Position / Heap_Slab_Instance.Slab_Bytes + 1);
      Check (Heap_Slab_Instance.Local_Of (Position) = Position mod Heap_Slab_Instance.Slab_Bytes);
   end loop;
   Ada.Text_IO.Put_Line ("PASS geometry: every offset in the 16 MiB arena matches integer division/remainder");
   Initialize (State, Max_Slots);
   for I in 1 .. Max_Slots loop
      Allocate (State, Item, OK);
      Check (OK and then not Seen (Item)); Seen (Item) := True;
   end loop;
   Allocate (State, Item, OK);
   Check (not OK);
   Reconfigure (State, 1, OK);
   Check (not OK and then Capacity (State) = Max_Slots and then Used (State) = Max_Slots);
   for I in reverse Slot loop
      Release (State, I, OK);
      Check (OK);
      Release (State, I, OK);
      Check (not OK);
   end loop;
   Reconfigure (State, 1, OK);
   Check (OK and then Capacity (State) = 1);
   Release (State, Max_Slots, OK);
   Check (not OK);
   Allocate (State, Item, OK);
   Check (OK and then Item = 1);
   Allocate (State, Item, OK);
   Check (not OK);
   Ada.Text_IO.Put_Line ("PASS bitmap: exhaustion, duplicate release, live reconfiguration denied, empty reuse");
   declare
      type Limits is array (Positive range <>) of Slot;
      Boundaries : constant Limits := [1, 2, 63, 64, 65, 127, 128, 129, 4_095, 4_096];
   begin
      for Limit of Boundaries loop
         Initialize (State, Limit);
         Seen := [others => False];
         for I in 1 .. Limit loop
            Allocate (State, Item, OK);
            Check (OK and then Item <= Limit and then not Seen (Item)); Seen (Item) := True;
         end loop;
         for Hole in 2 .. Limit loop
            -- Recycle two slots in both release orders, across every bitmap
            -- word boundary and at partial-tail capacities.
            Release (State, Hole, OK); Check (OK);
            Release (State, 1, OK); Check (OK);
            Allocate (State, Item, OK); Check (OK and then Item = 1);
            Allocate (State, Item, OK); Check (OK and then Item = Hole);
            Check (Used (State) = Limit);
            Release (State, 1, OK); Check (OK);
            Release (State, Hole, OK); Check (OK);
            Allocate (State, Item, OK); Check (OK and then Item = Hole);
            Allocate (State, Item, OK); Check (OK and then Item = 1);
            Check (Used (State) = Limit);
         end loop;
         Allocate (State, Item, OK); Check (not OK);
         Check (for all I in Slot => Live (State, I) = (I <= Limit));
      end loop;
   end;
   Ada.Text_IO.Put_Line ("PASS slots: every recycled position, both LIFO orders, word boundaries and tail capacities");
   declare
      Order : array (Slot) of Slot;
      Seed : Unsigned_32 := 16#A824_F075#;
      J, Saved : Slot;
   begin
      Initialize (State, Max_Slots);
      for I in Slot loop
         Allocate (State, Item, OK); Check (OK);
         Order (I) := Item;
      end loop;
      for I in reverse 2 .. Max_Slots loop
         Seed := Seed * 1_664_525 + 1_013_904_223;
         J := Slot (Seed mod Unsigned_32 (I) + 1);
         Saved := Order (I); Order (I) := Order (J); Order (J) := Saved;
      end loop;
      for I in Slot loop
         Release (State, Order (I), OK); Check (OK);
      end loop;
      Check (Used (State) = 0);
      Seen := [others => False];
      for I in reverse Slot loop
         Allocate (State, Item, OK); Check (OK and then not Seen (Item)); Seen (Item) := True;
      end loop;
      Check (Used (State) = Max_Slots);
      Allocate (State, Item, OK); Check (not OK);
   end;
   Ada.Text_IO.Put_Line ("PASS cache: full-capacity shuffled release and complete reuse without duplicates");
   -- One more free slot than the cache can hold, spread across a mostly live
   -- slab. The evicted entry must remain recoverable, and unrelated live slots
   -- must never be returned. Also reject duplicate releases while full.
   Initialize (State, Max_Slots);
   for I in Slot loop Allocate (State, Item, OK); Check (OK); end loop;
   for I in 1 .. 65 loop
      Release (State, I * 61, OK); Check (OK);
      Release (State, I * 61, OK); Check (not OK);
   end loop;
   Check (Used (State) = Max_Slots - 65);
   Seen := [others => False];
   for I in 1 .. 65 loop
      Allocate (State, Item, OK);
      Check (OK and then Item mod 61 = 0 and then Item <= 65 * 61
             and then not Seen (Item));
      Seen (Item) := True;
      if I = 1 then Check (Item = 65 * 61); end if;
   end loop;
   Check (Used (State) = Max_Slots and then (for all I in Slot => Live (State, I)));
   Allocate (State, Item, OK); Check (not OK);
   Ada.Text_IO.Put_Line ("PASS cache: overflow eviction, sparse refill and duplicate release preserve all capacity");
   declare
      Occupied : array (Slot) of Boolean := [others => False];
      Counted : Natural := 0;
      Seed : Unsigned_32 := 16#7182_4567#;
   begin
      Initialize (State, 257);
      for Iteration in 1 .. 20_000 loop
         Seed := Seed * 1_664_525 + 1_013_904_223;
         if (Seed and 256) = 0 then
            Allocate (State, Item, OK);
            Check (OK = (Counted < 257));
            if OK then Check (not Occupied (Item)); Occupied (Item) := True; Counted := Counted + 1; end if;
         else
            Item := Slot (Shift_Right (Seed, 16) mod 300 + 1);
            Release (State, Item, OK);
            Check (OK = Occupied (Item));
            if OK then Occupied (Item) := False; Counted := Counted - 1; end if;
         end if;
         Check (Used (State) = Counted);
         Check (for all I in Slot => Live (State, I) = Occupied (I));
      end loop;
   end;
   Ada.Text_IO.Put_Line ("PASS bitmap: 20000 independent model operations, non-power-of-two capacity");

   declare
      package M renames Slab_Model;
      use type M.Release_Status;
      use type M.State;
      Heap, Before : M.State;
      Value : M.Allocation;
      Status : M.Release_Status;
      Saved : array (1 .. 64) of M.Offset;
      Small : array (1 .. 16_384) of M.Offset;
   begin
      M.Initialize (Heap);
      for I in Saved'Range loop
         M.Allocate (Heap, 4_096, Value);
         Check (Value.Value.Success);
         Saved (I) := Value.Value.Position;
      end loop;
      Before := Heap;
      M.Allocate (Heap, 16, Value);
      Check (not Value.Value.Success and then Heap = Before);
      M.Release (Heap, Saved (1), Status);
      Check (Status = M.Released);
      Before := Heap;
      M.Allocate (Heap, 16, Value);
      Check (not Value.Value.Success and then Heap = Before); -- Partial is not empty.
      for I in 2 .. 16 loop M.Release (Heap, Saved (I), Status); Check (Status = M.Released); end loop;
      M.Allocate (Heap, 16, Value);
      Check (Value.Value.Success and then M.Page_Of (Value.Value.Position) = 1);
      for I in 17 .. 64 loop Check (M.Live (Heap, Saved (I))); end loop;
      M.Release (Heap, Value.Value.Position + 1, Status);
      Check (Status = M.Invalid_Offset);
      M.Release (Heap, Value.Value.Position, Status);
      Check (Status = M.Released);
      for I in 17 .. 64 loop M.Release (Heap, Saved (I), Status); Check (Status = M.Released); end loop;
      -- Every backing slab can now serve the smallest class, not a fixed quota.
      for I in Small'Range loop
         M.Allocate (Heap, 16, Value);
         Check (Value.Value.Success);
         Small (I) := Value.Value.Position;
      end loop;
      Before := Heap;
      M.Allocate (Heap, 4_096, Value);
      Check (not Value.Value.Success and then Heap = Before);
      for Position of Small loop
         M.Release (Heap, Position, Status); Check (Status = M.Released);
         Before := Heap;
         M.Release (Heap, Position, Status); Check (Status = M.Not_Allocated and then Heap = Before);
      end loop;
      Check (for all P in M.Slab_Id => M.Used (Heap, P) = 0);
   end;
   Ada.Text_IO.Put_Line ("PASS slabs: partial-page retyping denied, empty-page reassignment, 16384 small objects, reuse");
   declare
      package M renames Slab_Model;
      use type M.Release_Status;
      use type M.State;
      Heap, Before : M.State;
      Value : M.Allocation;
      Status : M.Release_Status;
      Saved : array (1 .. 64) of M.Offset;
   begin
      M.Initialize (Heap);
      for I in Saved'Range loop
         M.Allocate (Heap, 4_096, Value); Check (Value.Value.Success);
         Saved (I) := Value.Value.Position;
      end loop;
      -- Finish a release/reallocate at each end of the slab range while a
      -- different slab still has one hole. The next allocation must find that
      -- hole, including fallback from the final slab back to the first.
      for Reverse_Order in Boolean loop
         declare
            Low : constant M.Offset := Saved (1);
            High : constant M.Offset := Saved (64);
            First : constant M.Offset := (if Reverse_Order then High else Low);
            Last : constant M.Offset := (if Reverse_Order then Low else High);
         begin
            Check (M.Page_Of (Low) = M.Slab_Id'First and then M.Page_Of (High) = M.Slab_Id'Last);
            M.Release (Heap, First, Status); Check (Status = M.Released);
            M.Release (Heap, Last, Status); Check (Status = M.Released);
            M.Allocate (Heap, 4_096, Value);
            Check (Value.Value.Success and then Value.Value.Position = Last);
            M.Allocate (Heap, 4_096, Value);
            Check (Value.Value.Success and then Value.Value.Position = First);
            Before := Heap;
            M.Allocate (Heap, 4_096, Value);
            Check (not Value.Value.Success and then Heap = Before);
            Check (for all Position of Saved => M.Live (Heap, Position));
         end;
      end loop;
   end;
   Ada.Text_IO.Put_Line ("PASS slab scan: nearby hit, last-to-first fallback and unchanged exhaustion");
   declare
      package M renames Heap_Slab_Instance;
      use type M.Release_Status;
      Heap : M.State;
      Value : M.Allocation;
      Status : M.Release_Status;
      Saved : array (1 .. 256) of M.Offset;
   begin
      M.Initialize (Heap);
      for I in Saved'Range loop
         M.Allocate (Heap, 4_096, Value); Check (Value.Value.Success);
         Saved (I) := Value.Value.Position;
      end loop;
      Check (M.Page_Of (Saved (1)) = 1 and then M.Page_Of (Saved (256)) = 16);
      M.Release (Heap, Saved (256), Status); Check (Status = M.Released);
      M.Release (Heap, Saved (1), Status); Check (Status = M.Released);
      M.Allocate (Heap, 4_096, Value);
      Check (Value.Value.Success and then Value.Value.Position = Saved (1));
      -- The remaining hole is beyond lookahead. Prefer it to retyping any of
      -- the many unused slabs, and preserve all existing allocations.
      M.Allocate (Heap, 4_096, Value);
      Check (Value.Value.Success and then Value.Value.Position = Saved (256));
      Check (for all Position of Saved => M.Live (Heap, Position));
   end;
   Ada.Text_IO.Put_Line ("PASS slab scan: distant same-class capacity before empty-slab reassignment");
   declare
      package M renames Slab_Model;
      use type M.Release_Status;
      use type M.State;
      type Block is record
         Active : Boolean := False;
         Position : M.Offset := 0;
         Bytes : Positive := 16;
      end record;
      Blocks : array (1 .. 128) of Block;
      Heap, Before : M.State;
      Value : M.Allocation;
      Status : M.Release_Status;
      Seed : Unsigned_32 := 16#7623_1195#;
      Index, Size, Rounded : Positive;
      Counts : array (M.Slab_Id) of Natural;
   begin
      M.Initialize (Heap);
      for Step in 1 .. 30_000 loop
         Seed := Seed * 1_664_525 + 1_013_904_223;
         Index := Integer (Shift_Right (Seed, 16) mod 128) + 1;
         if Blocks (Index).Active then
            M.Release (Heap, Blocks (Index).Position, Status);
            Check (Status = M.Released);
            Blocks (Index).Active := False;
         end if;
         Seed := Seed * 1_664_525 + 1_013_904_223;
         Size := Integer (Shift_Right (Seed, 16) mod 4_096) + 1;
         Rounded := 16;
         while Rounded < Size loop Rounded := Rounded * 2; end loop;
         Before := Heap;
         M.Allocate (Heap, Size, Value);
         if Value.Value.Success then
            for B of Blocks loop
               Check (not B.Active or else
                 B.Position + B.Bytes <= Value.Value.Position or else
                 Value.Value.Position + Rounded <= B.Position);
            end loop;
            Blocks (Index) := (True, Value.Value.Position, Rounded);
         else
            Check (Heap = Before);
         end if;
         Counts := [others => 0];
         for B of Blocks loop
            if B.Active then
               Check (M.Live (Heap, B.Position));
               Check (Heap_Classes.Stride (M.Class_Of (Heap, M.Page_Of (B.Position))) = B.Bytes);
               Counts (M.Page_Of (B.Position)) := Counts (M.Page_Of (B.Position)) + 1;
            end if;
         end loop;
         Check (for all P in M.Slab_Id => M.Used (Heap, P) = Counts (P));
      end loop;
      for B of Blocks loop
         if B.Active then M.Release (Heap, B.Position, Status); Check (Status = M.Released); end if;
      end loop;
      Check (for all P in M.Slab_Id => M.Used (Heap, P) = 0);
   end;
   Ada.Text_IO.Put_Line ("PASS slabs: 30000 mixed-lifetime operations, independent live intervals and counts");
end Slab_Tests;
