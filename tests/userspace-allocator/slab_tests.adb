with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Heap_Bitmap; use Heap_Bitmap;
with Heap_Classes; use type Heap_Classes.Size_Class;
with Slab_Model;
with Heap_Slab_Instance;
with Interfaces; use Interfaces;
procedure Slab_Tests is
   State : Pool;
   Position : Local;
   OK : Boolean;
   Seen : array (Slot) of Boolean := [others => False];
   procedure Check (Condition : Boolean) is
   begin
      if not Condition then raise Program_Error with "slab regression"; end if;
   end Check;
begin
   --  Classes: 16-byte steps to 64, then four per power of two; each size
   --  takes the smallest class that holds it.
   for Size in Heap_Classes.Request_Size loop
      declare
         --  The next class above Expected: +16 up to 64, then a quarter of
         --  the power of two below.
         function Next (Expected : Positive) return Positive is
            Power : Positive := 64;
         begin
            if Expected < 64 then return Expected + 16; end if;
            while Power * 2 <= Expected loop Power := Power * 2; end loop;
            return Expected + Power / 4;
         end Next;
         Expected : Positive := 16;
      begin
         while Expected < Size loop Expected := Next (Expected); end loop;
         Check (Heap_Classes.Stride (Heap_Classes.Class_For (Size)) = Expected);
         Check (Heap_Classes.Stride (Heap_Classes.Class_For (Size)) * 4 <= Size * 5 + 60);
      end;
   end loop;
   for Class in Heap_Classes.Size_Class loop
      Check (Heap_Classes.Slots (Class) = Slab_Model.Slab_Bytes / Heap_Classes.Stride (Class));
      Check (Heap_Classes.Stride (Class) mod 16 = 0);
      for Offset in 0 .. Slab_Model.Slab_Bytes loop
         declare
            Q : Natural;
            Exact : Boolean;
         begin
            Heap_Classes.Divide (Offset, Class, Q, Exact);
            Check (Q = Offset / Heap_Classes.Stride (Class));
            Check (Exact = (Offset mod Heap_Classes.Stride (Class) = 0));
         end;
      end loop;
      for Item in 0 .. Heap_Classes.Slots (Class) - 1 loop
         Check (Heap_Classes.Scale (Item, Class) = Item * Heap_Classes.Stride (Class));
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("PASS classes: all request sizes, every slab offset's reciprocal quotient and exactness");
   for Position in Heap_Slab_Instance.Offset loop
      Check (Heap_Slab_Instance.Page_Of (Position) = Position / Heap_Slab_Instance.Slab_Bytes + 1);
      Check (Heap_Slab_Instance.Local_Of (Position) = Position mod Heap_Slab_Instance.Slab_Bytes);
   end loop;
   Ada.Text_IO.Put_Line ("PASS geometry: every offset in the 16 MiB arena matches integer division/remainder");
   --  CuAlloc never initializes a small arena's core: fresh backing reads
   --  as zero, which must be exactly the initial state.
   declare
      type Bytes is array (1 .. Heap_Slab_Instance.State'Size / 8) of Unsigned_8;
      function To_Bytes is new Ada.Unchecked_Conversion (Heap_Slab_Instance.State, Bytes);
      type Pool_Bytes is array (1 .. Pool'Size / 8) of Unsigned_8;
      function Pool_To_Bytes is new Ada.Unchecked_Conversion (Pool, Pool_Bytes);
      Heap : Heap_Slab_Instance.State;
      Fresh : Pool;
   begin
      Heap_Slab_Instance.Initialize (Heap);
      Check (for all B of To_Bytes (Heap) => B = 0);
      Check (Heap_Slab_Instance.State'Size = 256 * Heap_Slab_Instance.Pool_Bytes * 8);
      Initialize (Fresh, Heap_Classes.Size_Class'First);
      Check (for all B of Pool_To_Bytes (Fresh) => B = 0);
   end;
   Ada.Text_IO.Put_Line ("PASS initial state: all zero bytes (fresh backing needs no initialization)");
   --  The pool: blocks are byte offsets; a live bit per 16-byte granule.
   Initialize (State, Heap_Classes.Bytes_16);
   for I in 1 .. Max_Slots loop
      Allocate (State, Position, OK);
      Check (OK and then Position mod 16 = 0 and then not Seen (Position / 16 + 1));
      Seen (Position / 16 + 1) := True;
   end loop;
   Allocate (State, Position, OK);
   Check (not OK);
   Reconfigure (State, Heap_Classes.Bytes_32, OK);
   Check (not OK and then Capacity (State) = Max_Slots and then Used (State) = Max_Slots);
   for I in reverse Slot loop
      Release (State, (I - 1) * 16, OK);
      Check (OK);
      Release (State, (I - 1) * 16, OK);
      Check (not OK);
   end loop;
   Reconfigure (State, Heap_Classes.Bytes_4096, OK);
   Check (OK and then Capacity (State) = 16 and then Class (State) = Heap_Classes.Bytes_4096);
   Release (State, 0, OK);
   Check (not OK);
   Allocate (State, Position, OK);
   Check (OK and then Position = 0);
   --  Inside the block, unaligned, or past its start: refused.
   for Probe in 1 .. 4_095 loop
      Release (State, Probe, OK);
      Check (not OK and then Used (State) = 1);
   end loop;
   Check (Is_Live (State, 0) and then not Is_Live (State, 16) and then not Is_Live (State, 4_096));
   Ada.Text_IO.Put_Line ("PASS pool: exhaustion, duplicate release, live reconfiguration denied, interior offsets refused");
   declare
      type Kinds is array (Positive range <>) of Heap_Classes.Size_Class;
      Classes : constant Kinds :=
        [Heap_Classes.Bytes_16, Heap_Classes.Bytes_48, Heap_Classes.Bytes_64, Heap_Classes.Bytes_112,
         Heap_Classes.Bytes_1280, Heap_Classes.Bytes_3584, Heap_Classes.Bytes_4096];
      Limit, Stride : Positive;
      First : Local;
   begin
      for C of Classes loop
         Initialize (State, C);
         Limit := Heap_Classes.Slots (C);
         Stride := Heap_Classes.Stride (C);
         Check (Capacity (State) = Limit);
         Seen := [others => False];
         for I in 1 .. Limit loop
            Allocate (State, Position, OK);
            Check (OK and then Position mod Stride = 0 and then Position + Stride <= 65_536
                   and then not Seen (Position / Stride + 1));
            Seen (Position / Stride + 1) := True;
         end loop;
         for Hole in 2 .. Limit loop
            -- Recycle two blocks in both release orders, across every bitmap
            -- word boundary and at the slab's last block. (With two blocks
            -- the pool empties: it then hands out its blocks in order again.)
            Release (State, (Hole - 1) * Stride, OK); Check (OK);
            Release (State, 0, OK); Check (OK);
            Allocate (State, First, OK); Check (OK and then First = 0);
            Allocate (State, Position, OK); Check (OK and then Position = (Hole - 1) * Stride);
            Check (Used (State) = Limit);
            Release (State, 0, OK); Check (OK);
            Release (State, (Hole - 1) * Stride, OK); Check (OK);
            Allocate (State, Position, OK); Check (OK and then Position = (Hole - 1) * Stride);
            Allocate (State, Position, OK); Check (OK and then Position = 0);
            Check (Used (State) = Limit);
         end loop;
         Allocate (State, Position, OK); Check (not OK);
         Check (for all I in Slot => Live (State, I) = (I <= Limit));
      end loop;
   end;
   Ada.Text_IO.Put_Line ("PASS blocks: every recycled position, both LIFO orders, word boundaries, seven classes");
   declare
      Order : array (Slot) of Local;
      Seed : Unsigned_32 := 16#A824_F075#;
      J : Slot;
      Saved : Local;
   begin
      Initialize (State, Heap_Classes.Bytes_16);
      for I in Slot loop
         Allocate (State, Position, OK); Check (OK);
         Order (I) := Position;
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
         Allocate (State, Position, OK); Check (OK and then not Seen (Position / 16 + 1));
         Seen (Position / 16 + 1) := True;
      end loop;
      Check (Used (State) = Max_Slots);
      Allocate (State, Position, OK); Check (not OK);
   end;
   Ada.Text_IO.Put_Line ("PASS cache: full-capacity shuffled release and complete reuse without duplicates");
   -- One more free block than the cache can hold, spread across a mostly
   -- live slab. The evicted entry must remain recoverable, and unrelated
   -- live blocks must never be returned. Also reject duplicate releases.
   Initialize (State, Heap_Classes.Bytes_16);
   for I in Slot loop Allocate (State, Position, OK); Check (OK); end loop;
   for I in 1 .. 249 loop
      Release (State, (I * 16 - 1) * 16, OK); Check (OK);
      Release (State, (I * 16 - 1) * 16, OK); Check (not OK);
   end loop;
   Check (Used (State) = Max_Slots - 249);
   Seen := [others => False];
   for I in 1 .. 249 loop
      Allocate (State, Position, OK);
      Check (OK and then (Position / 16 + 1) mod 16 = 0 and then Position / 16 + 1 <= 249 * 16
             and then not Seen (Position / 16 + 1));
      Seen (Position / 16 + 1) := True;
      if I = 1 then Check (Position = (249 * 16 - 1) * 16); end if;
   end loop;
   Check (Used (State) = Max_Slots and then (for all I in Slot => Live (State, I)));
   Allocate (State, Position, OK); Check (not OK);
   Ada.Text_IO.Put_Line ("PASS cache: overflow eviction, sparse refill and duplicate release preserve all capacity");
   declare
      Occupied : array (0 .. 65_535) of Boolean := [others => False];
      Counted : Natural := 0;
      Seed : Unsigned_32 := 16#7182_4567#;
      Probe : Local;
   begin
      --  Blocks of 48 bytes: 1365 of them, the slab's last 16 bytes unused.
      Initialize (State, Heap_Classes.Bytes_48);
      for Iteration in 1 .. 20_000 loop
         Seed := Seed * 1_664_525 + 1_013_904_223;
         if (Seed and 256) = 0 then
            Allocate (State, Position, OK);
            Check (OK = (Counted < 1_365));
            if OK then
               Check (Position mod 48 = 0 and then not Occupied (Position));
               Occupied (Position) := True; Counted := Counted + 1;
            end if;
         else
            --  Any offset: a block's start, its inside, unaligned, the tail.
            Probe := Local (Shift_Right (Seed, 16) mod 65_536);
            if (Seed and 512) = 0 then Probe := Probe - Probe mod 48; end if;
            Check (Is_Live (State, Probe) = Occupied (Probe));
            Release (State, Probe, OK);
            Check (OK = Occupied (Probe));
            if OK then Occupied (Probe) := False; Counted := Counted - 1; end if;
         end if;
         Check (Used (State) = Counted);
      end loop;
      Check (for all I in 1 .. 1_365 => Live (State, I) = Occupied ((I - 1) * 48));
   end;
   Ada.Text_IO.Put_Line ("PASS pool: 20000 independent model operations at any offset, non-power-of-two stride");

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
         M.Allocate (Heap, 4_096, 1, 1, Value);
         Check (Value.Value.Success);
         Saved (I) := Value.Value.Position;
      end loop;
      Before := Heap;
      M.Allocate (Heap, 16, 1, 1, Value);
      Check (not Value.Value.Success and then Heap = Before);
      M.Release (Heap, Saved (1), Status);
      Check (Status = M.Released);
      Before := Heap;
      M.Allocate (Heap, 16, 1, 1, Value);
      Check (not Value.Value.Success and then Heap = Before); -- Partial is not empty.
      for I in 2 .. 16 loop M.Release (Heap, Saved (I), Status); Check (Status = M.Released); end loop;
      M.Allocate (Heap, 16, 1, 1, Value);
      Check (Value.Value.Success and then M.Page_Of (Value.Value.Position) = 1);
      for I in 17 .. 64 loop Check (M.Live (Heap, Saved (I))); end loop;
      M.Release (Heap, Value.Value.Position + 1, Status);
      Check (Status = M.Not_Allocated);
      M.Release (Heap, Value.Value.Position + 16, Status);
      Check (Status = M.Not_Allocated);
      M.Release (Heap, Value.Value.Position, Status);
      Check (Status = M.Released);
      for I in 17 .. 64 loop M.Release (Heap, Saved (I), Status); Check (Status = M.Released); end loop;
      -- Every backing slab can now serve the smallest class, not a fixed quota.
      for I in Small'Range loop
         M.Allocate (Heap, 16, 1, 1, Value);
         Check (Value.Value.Success);
         Small (I) := Value.Value.Position;
      end loop;
      Before := Heap;
      M.Allocate (Heap, 4_096, 1, 1, Value);
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
         M.Allocate (Heap, 4_096, 1, 1, Value); Check (Value.Value.Success);
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
            --  The hint (the caller's last slab) first, then the scan.
            M.Allocate (Heap, 4_096, M.Page_Of (Last), M.Page_Of (Last), Value);
            Check (Value.Value.Success and then Value.Value.Position = Last);
            M.Allocate (Heap, 4_096, M.Page_Of (Last), M.Page_Of (Last), Value);
            Check (Value.Value.Success and then Value.Value.Position = First);
            Before := Heap;
            M.Allocate (Heap, 4_096, 1, 1, Value);
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
         M.Allocate (Heap, 4_096, 1, 1, Value); Check (Value.Value.Success);
         Saved (I) := Value.Value.Position;
      end loop;
      Check (M.Page_Of (Saved (1)) = 1 and then M.Page_Of (Saved (256)) = 16);
      M.Release (Heap, Saved (256), Status); Check (Status = M.Released);
      M.Release (Heap, Saved (1), Status); Check (Status = M.Released);
      M.Allocate (Heap, 4_096, M.Page_Of (Saved (1)), M.Page_Of (Saved (1)), Value);
      Check (Value.Value.Success and then Value.Value.Position = Saved (1));
      -- The remaining hole is beyond lookahead. Prefer it to retyping any of
      -- the many unused slabs, and preserve all existing allocations.
      M.Allocate (Heap, 4_096, 1, 1, Value);
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
         Rounded := Heap_Classes.Stride (Heap_Classes.Class_For (Size));
         Before := Heap;
         M.Allocate (Heap, Size, 1, 1, Value);
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
         --  The fast free's computed liveness (one multiplication) agrees
         --  with the specification's, at every block and at random offsets.
         for B of Blocks loop
            if B.Active then
               Check (M.Is_Live (Heap, B.Position) and then M.Is_Live (Heap, B.Position) = M.Live (Heap, B.Position));
            end if;
         end loop;
         for K in 1 .. 8 loop
            Seed := Seed * 1_664_525 + 1_013_904_223;
            declare
               Probe : constant M.Offset := M.Offset (Seed mod Unsigned_32 (M.Offset'Last + 1));
            begin
               Check (M.Is_Live (Heap, Probe) = M.Live (Heap, Probe));
            end;
         end loop;
      end loop;
      for B of Blocks loop
         if B.Active then M.Release (Heap, B.Position, Status); Check (Status = M.Released); end if;
      end loop;
      Check (for all P in M.Slab_Id => M.Used (Heap, P) = 0);
   end;
   Ada.Text_IO.Put_Line ("PASS slabs: 30000 mixed-lifetime operations, independent live intervals and counts, computed liveness");
end Slab_Tests;
