pragma Ada_2022;
with BuddyAllocator;
with Kernel_Window;
with Mem_mgr;
with Process_Frames;
with TLB_Context;
with TLB_Shootdown;

package body Process_Pool is
   use type Virtmem.PhysAddress;

   Region_Frames : constant := 512;
   Max_Batch : constant := 64;

   Pool_Regions : Natural := 0 with Atomic;

   function Regions return Natural is (Pool_Regions);

   procedure Round is
   begin
      -- A batch's entries are cleared; every CPU drops what it cached.
      if TLB_Shootdown.This_CPU_Online then
         TLB_Shootdown.Invalidate_All;
      else
         -- Boot, one CPU (the others start with empty TLBs).
         TLB_Context.Flush_All;
      end if;
   end Round;

   -- Take up to an eighth of the pool's size (at least one region) from
   -- the kernel's lists; Grew when any joined.
   procedure Grow (Grew : out Boolean; Ceiling : Unsigned_64 := 0) is
      Wanted : constant Positive :=
        Positive'Min (Max_Batch, Positive'Max (1, Pool_Regions / 8));
      Bases : array (1 .. Max_Batch) of Virtmem.PhysAddress := [others => 0];
      Saved : array (1 .. Max_Batch) of Virtmem.PageTableEntry := [others => Virtmem.EMPTY_PTE];
      Count : Natural := 0;
   begin
      for I in 1 .. Wanted loop
         BuddyAllocator.takeRegion (Bases (I), Ceiling);
         exit when Bases (I) = 0;
         Mem_mgr.detachLinearRegion (Bases (I), Saved (I));
         Count := I;
      end loop;
      Grew := Count > 0;
      if not Grew then
         return;
      end if;
      Round;
      for I in 1 .. Count loop
         BuddyAllocator.joinPool (Bases (I), Saved (I));
      end loop;
      Pool_Regions := Pool_Regions + Count;
   end Grow;

   -- Zero up to Launder_Batch free dirty frames through this CPU's window,
   -- then one TLB round, then they go back clean: no CPU can still
   -- translate one through its window slot when a process gets it
   -- (Kernel_Map_Ownership: Window_Zero, Window_Unmap, Complete_Round,
   -- Mark_Clean). Laundered when any were.
   Launder_Batch : constant := 64;

   procedure Launder (Laundered : out Boolean) is
      Frames : BuddyAllocator.Frame_List (1 .. Launder_Batch);
      Count : Natural;
   begin
      BuddyAllocator.takeDirty (Frames, Count);
      Laundered := Count > 0;
      if not Laundered then
         return;
      end if;
      for I in 1 .. Count loop
         Kernel_Window.Zero (Process_Frames.Make (Unsigned_64 (Frames (I))));
      end loop;
      Round;
      for I in 1 .. Count loop
         BuddyAllocator.returnClean (Frames (I));
      end loop;
   end Launder;

   procedure Alloc_Run
     (Order : Natural; Ceiling : Unsigned_64; Base : out Virtmem.PhysAddress)
   is
      Clean, More : Boolean;
   begin
      -- Clean frames first; else launder dirty ones; else grow. A run of
      -- several frames may need a fresh region (laundering goes frame by
      -- frame), so each step is tried once more after the one before.
      for Attempt in 1 .. 3 loop
         BuddyAllocator.allocFromPool
           (BuddyAllocator.Order (Order), Ceiling, True, Base, Clean);
         if Base /= 0 then
            return;
         end if;
         exit when Attempt = 3;
         if Attempt = 1 then
            Launder (More);
            if not More then
               Grow (More, Ceiling);
            end if;
         else
            Grow (More, Ceiling);
         end if;
         exit when not More;
      end loop;
      Base := 0;
   end Alloc_Run;

   procedure Alloc_Frame (Frame : out Virtmem.PhysAddress) is
   begin
      Alloc_Run (0, 0, Frame);
   end Alloc_Frame;

   -- Wholly free regions back to the kernel: dirty frames zeroed through
   -- the window first, so the kernel's map never shows process data.
   procedure Reclaim (Released : out Boolean) is
      Base : Virtmem.PhysAddress;
      Saved : Virtmem.PageTableEntry;
      Found : Boolean;
   begin
      Released := False;
      loop
         BuddyAllocator.leavePool (Base, Saved, Found);
         exit when not Found;
         for I in 0 .. Region_Frames - 1 loop
            declare
               Frame : constant Virtmem.PhysAddress :=
                 Base + Virtmem.PhysAddress (I) * Virtmem.PhysAddress (Virtmem.FRAME_SIZE);
            begin
               if not BuddyAllocator.poolFrameClean (Frame) then
                  Kernel_Window.Zero (Process_Frames.Make (Unsigned_64 (Frame)));
               end if;
            end;
         end loop;
         Mem_mgr.attachLinearRegion (Base, Saved);
         BuddyAllocator.free (9, Virtmem.P2Va (Base));
         Pool_Regions := Pool_Regions - 1;
         Released := True;
      end loop;
   end Reclaim;

   procedure Setup is
   begin
      BuddyAllocator.installPoolReclaimHandler (Reclaim'Access);
   end Setup;

end Process_Pool;
