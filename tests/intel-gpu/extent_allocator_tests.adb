with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Physical_Extents;
with Intel_GPU_Extent_Allocator;
with Intel_GPU_Buffer_Reply;
procedure Extent_Allocator_Tests is
   package E renames Intel_GPU_Physical_Extents;
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
   Backing : E.Map;
   OK : Boolean;
begin
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
      pragma Assert (OK and Calls = 16 and V.CPU_Address (First) = Base);
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
      for Slot in V.Layout.Slot loop
         Allocator.Acquire_Buffer (Object, 7, Slot, 512, 1, View, OK);
         pragma Assert (OK and V.CPU_Address (View) =
           Base + Unsigned_64 (Slot - V.Layout.Slot'First) * E.Block_Bytes);
      end loop;
      for Slot in V.Layout.Slot loop
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
      for Slot in V.Layout.Slot loop
         if Slot mod 2 = 0 and Slot /= 2 then
            Allocator.Acquire_Buffer (Object, 7, Slot, 512, 1, View, OK);
            pragma Assert (OK and V.CPU_Address (View) =
              Base + Unsigned_64 (Slot - V.Layout.Slot'First) * E.Block_Bytes);
         end if;
      end loop;
      pragma Assert (Calls = 16);
   end;
   Calls := 0;
   declare Object : Allocator.Pool; begin
      Allocator.Acquire (Object, Base + 1, Backing, OK);
      pragma Assert (not OK and Calls = 0);
      Allocator.Acquire (Object, Base, Backing, OK);
      pragma Assert (OK and E.Ready (Backing) and Calls = 16);
      Allocator.Acquire (Object, Base, Backing, OK);
      pragma Assert (OK and Calls = 16);
      Allocator.Acquire (Object, Base + E.Capacity, Backing, OK);
      pragma Assert (not OK and not E.Ready (Backing) and Calls = 16);
      Ready := False;
      Allocator.Acquire (Object, Base, Backing, OK);
      Ready := True;
      Allocator.Acquire (Object, Base, Backing, OK);
      pragma Assert (not OK and not E.Ready (Backing) and Calls = 16);
   end;
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
            pragma Assert (not OK and not E.Ready (Backing) and Calls = N);
            pragma Assert (not Allocator.Memory_Budget (Object).Known);
            Allocator.Acquire_Buffer (Object, 1, 1, 1, 1, View, OK);
            pragma Assert (not OK and not Intel_GPU_Buffer_Reply.Valid (View) and Calls = N);
            Ready := True; Fail_At := 17; Lose_At := 17;
            Allocator.Acquire (Object, Base, Backing, OK);
            pragma Assert (not OK and not E.Ready (Backing) and Calls = N);
         end;
      end loop;
   end loop;
   declare Object : Allocator.Pool; begin
      Calls := 0; Alias := True;
      Allocator.Acquire (Object, Base, Backing, OK);
      pragma Assert (not OK and not E.Ready (Backing) and Calls = 2);
   end;
   Ada.Text_IO.Put_Line ("Extent allocator PASS:16 blocks, 128 generation reuse cycles, fragmented holes/coalescing, disjoint/idempotent views, exhaustion, all failure/ownership boundaries, no retry, alias rejection (mock allocation)");
end Extent_Allocator_Tests;
