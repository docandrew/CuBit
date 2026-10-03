with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Physical_Extents;
with Intel_GPU_Extent_Allocator;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Extent_Replies;
with Intel_GPU_Extent_Directory;
with System.Storage_Elements; use System.Storage_Elements;
procedure Extent_Allocator_Tests is
   package E renames Intel_GPU_Physical_Extents;
   package D renames Intel_GPU_Extent_Directory;
   Base : constant Unsigned_64 := 16#7000_0000_0000#;
   Calls : Natural := 0;
   Fail_At, Lose_At : Natural := 17;
   Ready : Boolean := True;
   Alias : Boolean := False;
   function Owner_Ready return Boolean is (Ready);
   function Allocate (CPU : Unsigned_64) return Unsigned_64 is
   begin
      pragma Assert (CPU = Base + Unsigned_64 (Calls) * E.Block_Bytes);
      Calls := Calls + 1;
      if Calls = Lose_At then Ready := False; end if;
      if Calls = Fail_At then return Unsigned_64'Last; end if;
      if Alias and Calls = 2 then return 2 ** 32 - 2 * E.Block_Bytes; end if;
      return 2 ** 32 - Unsigned_64 (2 * Calls) * E.Block_Bytes;
   end Allocate;
   package Allocator is new Intel_GPU_Extent_Allocator (Owner_Ready, Allocate);
   Backing : D.Borrowed_View;
   OK : Boolean;
begin
   declare
      Object : Allocator.Pool;
      View : Intel_GPU_Buffer_Reply.Extent_View;
      More : Boolean;
   begin
      for Turn in 1 .. 8 loop
         Allocator.Step_Buffer (Object, 7, 1, 4096, 1, View, OK, More);
         pragma Assert (Calls = Turn and More = (Turn < 8) and OK = (Turn = 8));
         pragma Assert (Intel_GPU_Buffer_Reply.Valid (View) = (Turn = 8));
         pragma Assert (Allocator.Memory_Budget (Object).Committed = Unsigned_64 (Turn) * E.Block_Bytes);
         pragma Assert (Allocator.Memory_Budget (Object).Retained =
           (if Turn < 8 then 0 else E.Capacity / 2));
      end loop;
      Allocator.Step_Buffer (Object, 7, 1, 4096, 1, View, OK, More);
      pragma Assert (OK and not More and Calls = 8);
   end;
   Calls := 0;
   declare
      package V renames Intel_GPU_Buffer_Reply;
      Object : Allocator.Pool;
      First, Second, Again : V.Extent_View;
      Usage : Allocator.Budget;
   begin
      pragma Assert (not Allocator.Memory_Budget (Object).Known);
      Allocator.Acquire_Buffer (Object, 0, 1, 4096, 1, First, OK);
      pragma Assert (not OK and Calls = 0);
      Allocator.Acquire_Buffer (Object, 7, 1, 4096, 1, First, OK);
      pragma Assert (OK and Calls = 8 and V.CPU_Address (First) = Base);
      pragma Assert (Allocator.Memory_Budget (Object).Committed = E.Capacity / 2);
      Usage := Allocator.Memory_Budget (Object);
      pragma Assert (Usage.Known and Usage.Capacity = E.Capacity and
        Usage.Retained = E.Capacity / 2 and Usage.Available = E.Capacity / 2
        and Usage.Unassigned_Slots = 15);
      Allocator.Acquire_Buffer (Object, 7, 2, 4096, 1, Second, OK);
      pragma Assert (OK and Calls = 16 and V.Same_Arena (First, Second));
      pragma Assert (V.CPU_Address (Second) = Base + E.Capacity / 2);
      pragma Assert (V.Page_Address (Second, 0) = 2 ** 32 - 18 * E.Block_Bytes);
      Usage := Allocator.Memory_Budget (Object);
      pragma Assert (Usage.Known and Usage.Retained = E.Capacity and
        Usage.Available = 0 and Usage.Unassigned_Slots = 14);
      Allocator.Acquire_Buffer (Object, 7, 1, 4096, 1, Again, OK);
      pragma Assert (OK and V.CPU_Address (Again) = V.CPU_Address (First));
      Allocator.Acquire_Buffer (Object, 7, 1, 1, 1, Again, OK);
      pragma Assert (not OK and not V.Valid (Again));
      Allocator.Acquire_Buffer (Object, 8, 1, 4096, 1, Again, OK);
      pragma Assert (not OK and not V.Valid (Again));
      Allocator.Acquire_Buffer (Object, 7, 3, 1, 1, Again, OK);
      pragma Assert (not OK and not V.Valid (Again) and Calls = 16);
      pragma Assert (Allocator.Memory_Budget (Object).Available = 0);
      Ready := False;
      pragma Assert (not Allocator.Memory_Budget (Object).Known);
      Allocator.Acquire_Buffer (Object, 7, 1, 4096, 1, Again, OK);
      Ready := True;
      Allocator.Acquire_Buffer (Object, 7, 1, 4096, 1, Again, OK);
      pragma Assert (not OK and not V.Valid (Again) and Calls = 16);
      pragma Assert (not Allocator.Memory_Budget (Object).Known);
   end;
   Calls := 0;
   declare
      package V renames Intel_GPU_Buffer_Reply;
      Object : Allocator.Pool;
      First, Second, Again : V.Extent_View;
   begin
      Allocator.Acquire_Buffer (Object, 7, 1, 4096, 2, Again, OK);
      pragma Assert (not OK and Calls = 0); -- no speculative arena on bad generation
      Allocator.Acquire_Buffer (Object, 7, 1, 4096, 1, First, OK);
      pragma Assert (OK);
      Allocator.Acquire_Buffer (Object, 7, 2, 4096, 1, Second, OK);
      pragma Assert (OK and Calls = 16);
      for Generation in Unsigned_32 range 1 .. 128 loop
         Allocator.Retire_Buffer (Object, 7, 1, Generation, False, OK);
         pragma Assert (not OK and Allocator.Memory_Budget (Object).Available = 0);
         Allocator.Retire_Buffer (Object, 8, 1, Generation, True, OK);
         pragma Assert (not OK);
         Allocator.Retire_Buffer (Object, 7, 1, Generation + 1, True, OK);
         pragma Assert (not OK);
         Allocator.Retire_Buffer (Object, 7, 1, Generation, True, OK);
         pragma Assert (OK and Allocator.Memory_Budget (Object).Available = E.Capacity / 2);
         pragma Assert (Allocator.Memory_Budget (Object).Unassigned_Slots = 15);
         Allocator.Retire_Buffer (Object, 7, 1, Generation, True, OK);
         pragma Assert (not OK); -- no duplicate refund
         Allocator.Acquire_Buffer (Object, 7, 1, 4096, Generation, Again, OK);
         pragma Assert (not OK and not V.Valid (Again));
         Allocator.Acquire_Buffer (Object, 7, 1, 4096, Generation + 2, Again, OK);
         pragma Assert (not OK);
         Allocator.Acquire_Buffer (Object, 7, 1, 4096, Generation + 1, Again, OK);
         pragma Assert (OK and V.CPU_Address (Again) = V.CPU_Address (First));
         pragma Assert (V.Page_Address (Again, 0) = V.Page_Address (First, 0));
         Allocator.Retire_Buffer (Object, 7, 1, Generation, True, OK);
         pragma Assert (not OK and Allocator.Memory_Budget (Object).Available = 0);
         Allocator.Acquire_Buffer (Object, 7, 2, 4096, 1, Again, OK);
         pragma Assert (OK and V.CPU_Address (Again) = V.CPU_Address (Second));
         pragma Assert (Calls = 16); -- physical arena retained, never allocated again
      end loop;
      Ready := False;
      Allocator.Retire_Buffer (Object, 7, 1, 129, True, OK);
      pragma Assert (not OK);
      Ready := True;
      pragma Assert (not Allocator.Memory_Budget (Object).Known);
   end;
   Calls := 0;
   declare
      package V renames Intel_GPU_Buffer_Reply;
      Object : Allocator.Pool;
      View : V.Extent_View;
      Before : Allocator.Budget;
   begin
      -- Fill every ticket with a 2 MiB slice, then create separated holes.
      for Slot in 1 .. V.Layout.Bootstrap_Slots loop
         Allocator.Acquire_Buffer (Object, 7, Slot, 512, 1, View, OK);
         pragma Assert (OK and V.CPU_Address (View) =
           Base + Unsigned_64 (Slot - V.Layout.Slot'First) * E.Block_Bytes);
      end loop;
      for Slot in 1 .. V.Layout.Bootstrap_Slots loop
         if Slot mod 2 = 1 then
            Allocator.Retire_Buffer (Object, 7, Slot, 1, True, OK);
            pragma Assert (OK);
         end if;
      end loop;
      Before := Allocator.Memory_Budget (Object);
      pragma Assert (Before.Available = E.Capacity / 2 and
        Before.Unassigned_Slots = 8);
      -- Enough aggregate space is not enough contiguous space. Rejection
      -- must consume neither the generation nor the allocation budget.
      Allocator.Acquire_Buffer (Object, 7, 1, 1024, 2, View, OK);
      pragma Assert (not OK and not V.Valid (View));
      pragma Assert (Allocator.Memory_Budget (Object).Available = Before.Available);
      pragma Assert (Allocator.Memory_Budget (Object).Unassigned_Slots = 8);
      Allocator.Retire_Buffer (Object, 7, 2, 1, True, OK);
      pragma Assert (OK);
      Allocator.Acquire_Buffer (Object, 7, 1, 1536, 2, View, OK);
      pragma Assert (OK and V.CPU_Address (View) = Base);
      -- Slot order is no longer address order: the search must still avoid
      -- every retained neighbor when refilling holes.
      Allocator.Acquire_Buffer (Object, 7, 2, 512, 2, View, OK);
      pragma Assert (OK and V.CPU_Address (View) = Base + 4 * E.Block_Bytes);
      for Slot in 1 .. V.Layout.Bootstrap_Slots loop
         if Slot mod 2 = 0 and Slot /= 2 then
            Allocator.Acquire_Buffer (Object, 7, Slot, 512, 1, View, OK);
            pragma Assert (OK and V.CPU_Address (View) =
              Base + Unsigned_64 (Slot - V.Layout.Slot'First) * E.Block_Bytes);
         end if;
      end loop;
      pragma Assert (Calls = 16);
   end;
   Calls := 0;
   for Mask in Unsigned_32 range 0 .. 255 loop
      declare
         package V renames Intel_GPU_Buffer_Reply;
         Object : Allocator.Pool;
         View : V.Extent_View;
         Occupied : array (Natural range 0 .. 7) of Boolean := [others => True];
         Hole : Natural;
      begin
         Calls := 0;
         -- Descending tickets create an address order unrelated to ticket order.
         for Index in reverse 1 .. 8 loop
            Allocator.Acquire_Buffer (Object, 7, Index, 1, 1, View, OK);
            pragma Assert (OK and V.CPU_Address (View) = Base + Unsigned_64 (8 - Index) * 4096);
         end loop;
         for Index in 1 .. 8 loop
            if (Mask and Shift_Left (1, Index - 1)) /= 0 then
               Allocator.Retire_Buffer (Object, 7, Index, 1, True, OK);
               pragma Assert (OK);
               Occupied (8 - Index) := False;
            end if;
         end loop;
         for Index in 1 .. 8 loop
            if (Mask and Shift_Left (1, Index - 1)) /= 0 then
               Hole := 0;
               while Occupied (Hole) loop Hole := Hole + 1; end loop;
               Allocator.Acquire_Buffer (Object, 7, Index, 1, 2, View, OK);
               pragma Assert (OK and V.CPU_Address (View) = Base + Unsigned_64 (Hole) * 4096);
               Occupied (Hole) := True;
            end if;
         end loop;
         pragma Assert (Allocator.Memory_Budget (Object).Retained = 8 * 4096);
         pragma Assert (Calls = 1);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Ordered extents PASS:256 removal masks, reversed ticket order, first-fit bitmap oracle");
   Calls := 0;
   declare Object : Allocator.Pool; begin
      Allocator.Acquire (Object, Base + 1, Backing, OK);
      pragma Assert (not OK and Calls = 0);
      Allocator.Acquire (Object, Base, Backing, OK);
      pragma Assert (OK and D.Valid (Backing) and Calls = 16);
      Allocator.Acquire (Object, Base, Backing, OK);
      pragma Assert (OK and Calls = 16);
      Allocator.Acquire (Object, Base + E.Capacity, Backing, OK);
      pragma Assert (not OK and not D.Valid (Backing) and Calls = 16);
      Ready := False;
      Allocator.Acquire (Object, Base, Backing, OK);
      Ready := True;
      Allocator.Acquire (Object, Base, Backing, OK);
      pragma Assert (not OK and not D.Valid (Backing) and Calls = 16);
   end;
   -- Fail every incremental physical-growth boundary after a usable prefix.
   -- Borrowed prefixes invalidate on quarantine, without freeing physical
   -- backing. The failed owner must never retry allocation callbacks.
   for Lost in Boolean loop
      for Boundary in 2 .. 16 loop
         declare
            Object : Allocator.Pool;
            Prior, Failed_Map : D.Borrowed_View;
            Prefix_Bytes : constant Unsigned_64 := Unsigned_64 (Boundary - 1) * E.Block_Bytes;
         begin
            Calls := 0; Ready := True; Alias := False; Fail_At := 17; Lose_At := 17;
            Allocator.Acquire (Object, Base, Prior, OK, Prefix_Bytes);
            pragma Assert (OK and Calls = Boundary - 1);
            pragma Assert (Allocator.Memory_Budget (Object).Committed = Prefix_Bytes);
            for Query in 1 .. 32 loop
               pragma Assert (D.Byte_Count (Allocator.Snapshot (Object)) = Prefix_Bytes);
               pragma Assert (Calls = Boundary - 1);
            end loop;
            Fail_At := (if Lost then 17 else Boundary);
            Lose_At := (if Lost then Boundary else 17);
            Allocator.Acquire (Object, Base, Failed_Map, OK, Prefix_Bytes + 1);
            pragma Assert (not OK and Calls = Boundary and not D.Valid (Failed_Map));
            pragma Assert (not Allocator.Memory_Budget (Object).Known);
            pragma Assert (not D.Valid (Allocator.Snapshot (Object)));
            pragma Assert (not D.Resolve (Prior, Prefix_Bytes - 4096, 4096).Valid);
            Ready := True; Fail_At := 17; Lose_At := 17;
            Allocator.Acquire (Object, Base, Failed_Map, OK, Prefix_Bytes + 1);
            pragma Assert (not OK and Calls = Boundary and not D.Valid (Failed_Map));
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Demand failure boundaries PASS:30 late allocation/owner-loss boundaries; snapshots read-only, prior backing retained, failed pool budget unavailable, no replay");
   for Lost in Boolean loop
      for N in 1 .. 16 loop
         declare
            Object : Allocator.Pool;
            View : Intel_GPU_Buffer_Reply.Extent_View;
         begin
            Calls := 0; Ready := True;
            Fail_At := (if Lost then 17 else N);
            Lose_At := (if Lost then N else 17);
            Allocator.Acquire (Object, Base, Backing, OK);
            pragma Assert (not OK and not D.Valid (Backing) and Calls = N);
            pragma Assert (not Allocator.Memory_Budget (Object).Known);
            Allocator.Acquire_Buffer (Object, 1, 1, 1, 1, View, OK);
            pragma Assert (not OK and not Intel_GPU_Buffer_Reply.Valid (View) and Calls = N);
            Ready := True; Fail_At := 17; Lose_At := 17;
            Allocator.Acquire (Object, Base, Backing, OK);
            pragma Assert (not OK and not D.Valid (Backing) and Calls = N);
         end;
      end loop;
   end loop;
   declare
      package V renames Intel_GPU_Buffer_Reply;
      type RAM is array (Natural range <>) of Unsigned_64;
      Metadata : RAM (0 .. 5 * 512 - 1) := [others => 16#CAFE#] with Alignment => 4096;
      Metadata_Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Metadata'Address));
      Object : Allocator.Pool;
      View : V.Extent_View;
      Previous : Natural := 0;
      Capacity : Positive;
   begin
      Calls := 0; Alias := False; Ready := True; Fail_At := 17; Lose_At := 17;
      for Round in 0 .. 4 loop
         if Round > 0 then
            Allocator.Extend_Records (Object, Metadata_Base, Unsigned_64 (Round) * 4096, OK);
            pragma Assert (OK);
            pragma Assert (Allocator.Memory_Budget (Object).Unassigned_Slots =
              Allocator.Record_Capacity (Object) - Previous);
            Allocator.Extend_Records (Object, Metadata_Base, Unsigned_64 (Round) * 4096, OK);
            pragma Assert (not OK); -- duplicate growth must not credit records
            pragma Assert (Allocator.Memory_Budget (Object).Unassigned_Slots =
              Allocator.Record_Capacity (Object) - Previous);
         end if;
         Capacity := Allocator.Record_Capacity (Object);
         pragma Assert (Capacity > Previous);
         Allocator.Acquire_Buffer (Object, 7, Capacity + 1, 1, 1, View, OK);
         pragma Assert (not OK);
         for Index in Previous + 1 .. Capacity loop
            Allocator.Acquire_Buffer (Object, 7, Index, 1, 1, View, OK);
            pragma Assert (OK and V.CPU_Address (View) = Base + Unsigned_64 (Index - 1) * 4096);
            pragma Assert (Allocator.Memory_Budget (Object).Unassigned_Slots = Capacity - Index);
         end loop;
         for Index in 1 .. Capacity loop
            Allocator.Acquire_Buffer (Object, 7, Index, 1, 1, View, OK);
            pragma Assert (OK and V.CPU_Address (View) = Base + Unsigned_64 (Index - 1) * 4096);
         end loop;
         pragma Assert (Allocator.Memory_Budget (Object).Retained = Unsigned_64 (Capacity) * 4096);
         pragma Assert (Allocator.Memory_Budget (Object).Unassigned_Slots = 0 and
           Calls = (Capacity + 511) / 512);
         for Word in Round * 512 .. Metadata'Last loop
            pragma Assert (Metadata (Word) = 16#CAFE#);
         end loop;
         Previous := Capacity;
      end loop;
      Allocator.Retire_Buffer (Object, 7, 200, 1, True, OK);
      pragma Assert (OK and Allocator.Memory_Budget (Object).Unassigned_Slots = 1);
      Allocator.Retire_Buffer (Object, 7, 200, 1, True, OK);
      pragma Assert (not OK and Allocator.Memory_Budget (Object).Unassigned_Slots = 1);
      Allocator.Acquire_Buffer (Object, 7, 200, 1, 2, View, OK);
      pragma Assert (OK and V.CPU_Address (View) = Base + 199 * 4096);
      pragma Assert (Allocator.Memory_Budget (Object).Unassigned_Slots = 0);
      Ready := False;
      Allocator.Extend_Records (Object, Metadata_Base, 5 * 4096, OK);
      pragma Assert (not OK and Allocator.Record_Capacity (Object) = Previous);
      Ready := True;
   end;
   Ada.Text_IO.Put_Line ("Extent record growth PASS: four boundaries, hundreds of live slices, old views stable, generation reuse, uncommitted indices and revoked growth rejected");
   for Pattern in 1 .. 3 loop
      declare
         Object : Allocator.Pool;
         View : Intel_GPU_Buffer_Reply.Extent_View;
         type RAM is array (1 .. 4096) of Unsigned_8 with Alignment => 4096;
         Metadata : RAM := [others => 0];
         Slot : Positive;
      begin
         Calls := 0;
         Allocator.Extend_Records (Object,
           Unsigned_64 (To_Integer (Metadata'Address)), 4096, OK);
         pragma Assert (OK);
         for Generation in Unsigned_32 range 1 .. 3 loop
            for Index in 1 .. 128 loop
               Allocator.Acquire_Buffer (Object, 7, Index, 1, Generation, View, OK);
               pragma Assert (OK);
               pragma Assert (Intel_GPU_Buffer_Reply.CPU_Address (View) =
                 Base + Unsigned_64 (Index - 1) * 4096);
            end loop;
            for I in 0 .. 127 loop
               Slot := (case Pattern is
                 when 1 => I + 1, when 2 => 128 - I,
                 when others => (I * 65) mod 128 + 1);
               Allocator.Retire_Buffer (Object, 7, Slot, Generation, True, OK);
               pragma Assert (OK);
               pragma Assert (Allocator.Memory_Budget (Object).Retained =
                 Unsigned_64 (127 - I) * 4096);
               Allocator.Retire_Buffer (Object, 7, Slot, Generation, True, OK);
               pragma Assert (not OK);
            end loop;
            pragma Assert (Allocator.Memory_Budget (Object).Unassigned_Slots =
              Allocator.Record_Capacity (Object));
         end loop;
         pragma Assert (Calls = 1); -- retirement/reuse does not reclaim backing
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Neighbor retirement PASS: 128 BOs, ascending/descending/permuted removal, three generations, exact budgets, retained backing");
   for Lost in Boolean loop
      declare
         Object : Allocator.Pool;
         View : Intel_GPU_Buffer_Reply.Extent_View;
         Old : D.Borrowed_View;
      begin
         Calls := 0; Ready := True; Alias := False; Fail_At := 17; Lose_At := 17;
         Allocator.Acquire_Buffer (Object, 7, 1, 1, 1, View, OK);
         pragma Assert (OK and Calls = 1);
         Old := Allocator.Snapshot (Object);
         pragma Assert (D.Byte_Count (Old) = E.Block_Bytes and Calls = 1);
         -- A later growth failure retains the old physical pages but cannot
         -- publish the new slice or report the failed pool as usable.
         Fail_At := (if Lost then 17 else 2);
         Lose_At := (if Lost then 2 else 17);
         Allocator.Acquire_Buffer (Object, 7, 2, 512, 1, View, OK);
         pragma Assert (not OK and Calls = 2);
         pragma Assert (not D.Valid (Allocator.Snapshot (Object)));
         pragma Assert (not D.Resolve (Old, 0, 4096).Valid);
         Ready := True; Fail_At := 17; Lose_At := 17;
         Allocator.Acquire_Buffer (Object, 7, 2, 512, 1, View, OK);
         pragma Assert (not OK and Calls = 2);
      end;
   end loop;
   declare
      package R renames Intel_GPU_Extent_Replies;
      package V renames Intel_GPU_Buffer_Reply;
      Object : Allocator.Pool;
      Decoder : R.Assembly;
      Views : array (1 .. 16) of V.Extent_View;
      Granted : V.Extent_View;
      Part : E.Span;
   begin
      Calls := 0; Ready := True; Alias := False; Fail_At := 17; Lose_At := 17;
      for Count in 1 .. 16 loop
         Allocator.Acquire_Buffer (Object, 7, Count, 512, 1, Granted, OK);
         pragma Assert (OK and Calls = Count);
         if Count = 1 then R.Start (Decoder, Base, 7, OK, Count);
         else R.Extend (Decoder, Count, OK); end if;
         pragma Assert (OK and not Intel_GPU_Extent_Directory.Valid (R.Result (Decoder)));
         Part := D.Resolve (Allocator.Snapshot (Object),
           Unsigned_64 (Count - 1) * E.Block_Bytes, E.Block_Bytes);
         pragma Assert (Part.Valid and Calls = Count); -- query is read-only
         R.Accept_Reply (Decoder,
           [Unsigned_64 (Count - 1), Part.Address,
            Base + Unsigned_64 (Count - 1) * E.Block_Bytes, 7], OK);
         pragma Assert (OK);
         Views (Count) := V.From_Extents (R.Result (Decoder), 7,
           V.CPU_Address (Granted) - Base, V.Byte_Count (Granted));
         pragma Assert (V.Valid (Views (Count)));
         for Prior in 1 .. Count loop
            pragma Assert (V.Same_Arena (Views (Prior), Views (Count)));
            for Page in 0 .. 511 loop
               pragma Assert (V.Page_Address (Views (Prior), Unsigned_64 (Page) * 4096) =
                 2 ** 32 - Unsigned_64 (2 * Prior) * E.Block_Bytes + Unsigned_64 (Page) * 4096);
            end loop;
         end loop;
      end loop;
      for Index in 1 .. 16 loop
         Allocator.Retire_Buffer (Object, 7, Index, 1, True, OK);
         pragma Assert (OK);
      end loop;
      pragma Assert (Allocator.Memory_Budget (Object).Retained = 0 and
        Allocator.Memory_Budget (Object).Committed = E.Capacity and Calls = 16);
   end;
   for Lost_Reply in 1 .. 8 loop
      declare
         package R renames Intel_GPU_Extent_Replies;
         Object : Allocator.Pool;
         Decoder : R.Assembly;
         Granted : Intel_GPU_Buffer_Reply.Extent_View;
         Before : Allocator.Budget;
         Part : E.Span;
      begin
         Calls := 0; Ready := True; Alias := False;
         Allocator.Acquire_Buffer (Object, 7, 1, 512, 1, Granted, OK);
         pragma Assert (OK and Calls = 1);
         R.Start (Decoder, Base, 7, OK, 1); pragma Assert (OK);
         Part := D.Resolve (Allocator.Snapshot (Object), 0, E.Block_Bytes);
         R.Accept_Reply (Decoder, [0, Part.Address, Base, 7], OK); pragma Assert (OK);
         Allocator.Acquire_Buffer (Object, 7, 2, 4096, 1, Granted, OK);
         pragma Assert (OK and Calls = 9);
         Before := Allocator.Memory_Budget (Object);
         R.Extend (Decoder, 9, OK); pragma Assert (OK);
         for Index in 1 .. Lost_Reply loop
            Part := D.Resolve (Allocator.Snapshot (Object), Unsigned_64 (Index) * E.Block_Bytes, E.Block_Bytes);
            if Index = Lost_Reply then R.Cancel (Decoder); end if;
            R.Accept_Reply (Decoder,
              [Unsigned_64 (Index), Part.Address, Base + Unsigned_64 (Index) * E.Block_Bytes, 7], OK);
            pragma Assert (OK = (Index /= Lost_Reply));
         end loop;
         pragma Assert (not Intel_GPU_Extent_Directory.Valid (R.Result (Decoder)) and Calls = 9);
         pragma Assert (Allocator.Memory_Budget (Object).Retained = Before.Retained and
           Allocator.Memory_Budget (Object).Committed = Before.Committed);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Demand transport PASS:16 growth steps, retained page views, read-only queries,8 lost suffix boundaries retain backing (hosted, no IPC/HW)");
   declare Object : Allocator.Pool; begin
      Calls := 0; Alias := True;
      Allocator.Acquire (Object, Base, Backing, OK);
      pragma Assert (not OK and not D.Valid (Backing) and Calls = 2);
   end;
   Ada.Text_IO.Put_Line ("Extent allocator PASS:16 blocks, 128 generation reuse cycles, fragmented holes/coalescing, disjoint/idempotent views, exhaustion, all failure/ownership boundaries, no retry, alias rejection (mock allocation)");
end Extent_Allocator_Tests;
