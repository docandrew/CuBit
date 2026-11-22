--  KERN-014 model (docs/kernel-memory-map.md): the kernel never maps a frame
--  that holds process data, except through a per-CPU window.
--
--  A hosted SPARK model, not kernel code. It states the ownership ledger
--  the design proposes and proves that every operation preserves
--  Isolated. The kernel adapter (page-table writes, INVLPG, TLB rounds) is
--  outside the model; the assumptions it must meet are listed below.
--
--  State:
--   * each 2 MiB region of RAM is Kernel_Mapped (its linear-map entry is
--     present), Stale (the entry is cleared, but a CPU may still translate
--     it until a TLB round that began after the clear completes) or
--     Unmapped (cleared and flushed);
--   * each frame has an owner (Free, Kernel, Process, Device) and what it
--     holds (Zero, Kernel_Data, Process_Data), a pin, and (Residue) the
--     round count at its last window unmap;
--   * each CPU has one window slot, mapping at most one frame;
--   * Rounds counts completed all-CPU TLB rounds (TLB_Shootdown.Invalidate_All).
--
--  Proved (gnatprove level 2, run.sh --prove): every operation keeps
--  Isolated, and Unreachable follows from it: a frame holding process data
--  that no window maps is not reachable through the kernel's map.
--
--  Assumed (the kernel adapter):
--   * Begin_Unmap is called after the region's linear-map entry is
--     cleared, and Complete_Round only after an Invalidate_All that began
--     after every clear recorded so far has returned;
--   * Window_Unmap is called after the slot's entry is cleared and INVLPG
--     ran on this CPU, with no CR3 change between Window_Map and
--     Window_Unmap (the window section runs with interrupts disabled);
--   * Device frames are freed only after IOMMU_Domains.Release (the IOMMU
--     ledger proves the device side; this model proves the CPU side);
--   * View_Unmap is called after the viewer's entry for the frame is
--     cleared and a TLB round that began after the clear has completed
--     (Debug_Sessions.Remove_Batch).
--
--  Debug views (DBG-001, Debug_Sessions): a debugger maps a target's own
--  frame read-only into its own user mapping, holding one view pin each.
--  A view is never the kernel's map, so it changes nothing about Isolated's
--  kernel side. What the model adds: a viewed frame is always its owner's
--  (Process or Device) or Retired (its owner freed it while viewed), a
--  Retired frame is still viewed, and only the last view's removal frees
--  it. So a viewed frame never reaches another owner (Viewed_Owned:
--  Alloc_User needs Free), and a freed frame keeps holding its old owner's
--  data only while its viewer, who could already read it, still maps it.
pragma Ada_2022;
with Interfaces; use Interfaces;

package Kernel_Map_Ownership with SPARK_Mode is
   Region_Count      : constant := 8;    -- model size only
   Frames_Per_Region : constant := 512;  -- 4 KiB frames per 2 MiB region
   CPU_Count         : constant := 4;

   subtype Region_Index is Natural range 0 .. Region_Count - 1;
   subtype Frame_Index is Natural range 0 .. Region_Count * Frames_Per_Region - 1;
   subtype CPU_Index is Natural range 0 .. CPU_Count - 1;
   subtype Round_Count is Unsigned_64 range 0 .. Unsigned_64'Last - 1;

   type Map_State is (Kernel_Mapped, Stale, Unmapped);
   --  Retired: its owner freed it while a debug view still maps it.
   type Owner_Kind is (Free, Kernel, Process, Device, Retired);
   subtype Process_Owner is Owner_Kind range Process .. Device;
   type Content_Kind is (Zero, Kernel_Data, Process_Data);

   type Region_Record is record
      State : Map_State := Kernel_Mapped;
      Stamp : Round_Count := 0;
   end record;


   type Window_Record is record
      Mapped : Boolean := False;
      Frame  : Frame_Index := 0;
   end record;

   type Region_Table is array (Region_Index) of Region_Record;
   --  Per frame: owner, what it holds, pinned (one array per field, so an
   --  update to one leaves the others' facts untouched for the prover).
   type Owner_Table is array (Frame_Index) of Owner_Kind;
   type Content_Table is array (Frame_Index) of Content_Kind;
   type Pin_Table is array (Frame_Index) of Boolean;
   type Window_Table is array (CPU_Index) of Window_Record;
   --  The round count at each frame's last window unmap (Residue).
   type Residue_Table is array (Frame_Index) of Round_Count;
   --  Debug views mapping each frame (each holds one view pin).
   Max_Views : constant := 126;
   subtype View_Count is Natural range 0 .. Max_Views;
   type View_Table is array (Frame_Index) of View_Count;

   Regions : Region_Table := [others => <>];
   Owners   : Owner_Table := [others => Free];
   Contents : Content_Table := [others => Zero];
   Pins     : Pin_Table := [others => False];
   Windows : Window_Table := [others => <>];
   Residue : Residue_Table := [others => 0];
   Viewers : View_Table := [others => 0];
   --  Clean (KERN-014 slice 4): the pool may hand the frame out as it is.
   type Clean_Table is array (Frame_Index) of Boolean;
   Clean   : Clean_Table := [others => False];
   Rounds  : Round_Count := 1;

   function Region_Of (F : Frame_Index) return Region_Index is
     (F / Frames_Per_Region);

   function In_Window (F : Frame_Index) return Boolean is
     (for some C in CPU_Index => Windows (C).Mapped and then Windows (C).Frame = F)
     with Global => Windows;

   --  What the kernel's own map can reach: its linear map (until a round
   --  completes after the entry was cleared), or a window slot.
   function Kernel_Reachable (F : Frame_Index) return Boolean is
     (Regions (Region_Of (F)).State /= Unmapped or else In_Window (F))
     with Global => (Regions, Windows);

   function Frame_Ok (F : Frame_Index) return Boolean is
     ((if Owners (F) = Kernel then
         Regions (Region_Of (F)).State = Kernel_Mapped)
      and then (if Owners (F) in Process_Owner then
         Regions (Region_Of (F)).State = Unmapped)
      and then (if Contents (F) = Process_Data then
         Regions (Region_Of (F)).State = Unmapped)
      and then (if Regions (Region_Of (F)).State = Stale then
         Owners (F) = Free)
      and then (if Owners (F) = Retired then
         Regions (Region_Of (F)).State = Unmapped and then Viewers (F) > 0)
      and then (if Viewers (F) > 0 then
         Owners (F) in Process_Owner | Retired))
     with Global => (Regions, Owners, Contents, Viewers);

   function Window_Fits (W : Window_Record) return Boolean is
     (if W.Mapped then
        Regions (Region_Of (W.Frame)).State = Unmapped
        and then (Owners (W.Frame) = Free
                  or else Pins (W.Frame)))
     with Global => (Regions, Owners, Pins);

   function Window_Ok (C : CPU_Index) return Boolean is
     (Window_Fits (Windows (C)))
     with Global => (Regions, Owners, Pins, Windows);

   function Isolated return Boolean is
     ((for all F in Frame_Index => Frame_Ok (F))
      and then (for all C in CPU_Index => Window_Ok (C)))
     with Global => (Regions, Owners, Contents, Pins, Windows, Viewers);

   --  A clean frame is free, zero, and a round has completed since it was
   --  last in a window: no CPU can still translate it through a slot. The
   --  process pool hands out only clean frames (Process_Pool's laundering:
   --  zero through the window, then one round, then clean).
   function Clean_Ok (F : Frame_Index) return Boolean is
     (if Clean (F) then
        Owners (F) = Free and then Contents (F) = Zero and then Rounds > Residue (F))
     with Global => (Clean, Owners, Contents, Rounds, Residue);

   function Laundered return Boolean is
     (for all F in Frame_Index => Clean_Ok (F))
     with Global => (Clean, Owners, Contents, Rounds, Residue);

   --  The property the design exists for.
   procedure Unreachable (F : Frame_Index) with
     Ghost,
     Pre  => Isolated and then Laundered and then Contents (F) = Process_Data
             and then not In_Window (F),
     Post => not Kernel_Reachable (F);

   -- Kernel pool ---------------------------------------------------------

   procedure Alloc_Kernel (F : Frame_Index) with
     Pre  => Isolated and then Laundered and then Owners (F) = Free
             and then Regions (Region_Of (F)).State = Kernel_Mapped,
     Post => Isolated and then Laundered and then Owners (F) = Kernel
             and then Contents (F) = Kernel_Data;

   procedure Free_Kernel (F : Frame_Index) with
     Pre  => Isolated and then Laundered and then Owners (F) = Kernel,
     Post => Isolated and then Laundered and then Owners (F) = Free;

   --  Zero a free frame through the linear map (a region about to leave).
   procedure Kernel_Zero (F : Frame_Index) with
     Pre  => Isolated and then Laundered and then Owners (F) = Free
             and then Regions (Region_Of (F)).State = Kernel_Mapped,
     Post => Isolated and then Laundered and then Contents (F) = Zero;

   -- Process pool --------------------------------------------------------

   --  Only zeroed frames, and only once a round has passed since the frame
   --  was last in a window (no remote CPU can still translate that slot).
   procedure Alloc_User (F : Frame_Index; Kind : Process_Owner) with
     Pre  => Isolated and then Laundered and then Owners (F) = Free
             and then Regions (Region_Of (F)).State = Unmapped
             and then Clean (F)
             and then not In_Window (F),
     Post => Isolated and then Laundered and then Owners (F) = Kind
             --  What it gets: zeros, and no CPU can still reach it through a
             --  window slot (a round has completed since its last use).
             and then Contents (F) = Zero and then Rounds > Residue (F);

   --  A process (or its device) stores into its own frame.
   procedure Process_Writes (F : Frame_Index) with
     Pre  => Isolated and then Laundered and then Owners (F) in Process_Owner,
     Post => Isolated and then Laundered and then Contents (F) = Process_Data;

   --  After the process's mapping is gone and a round completed (and, for
   --  a Device frame, IOMMU_Domains.Release). The frame stays dirty.
   procedure Free_User (F : Frame_Index) with
     Pre  => Isolated and then Laundered and then Owners (F) in Process_Owner
             and then not Pins (F)
             and then Viewers (F) = 0,
     Post => Isolated and then Laundered and then Owners (F) = Free;

   procedure Pin (F : Frame_Index) with
     Pre  => Isolated and then Laundered and then Owners (F) in Process_Owner
             and then not Pins (F),
     Post => Isolated and then Laundered and then Pins (F);

   procedure Unpin (F : Frame_Index) with
     Pre  => Isolated and then Laundered and then Pins (F) and then not In_Window (F),
     Post => Isolated and then Laundered and then not Pins (F);

   -- Debug views (DBG-001) -----------------------------------------------------

   --  Map a view of a frame the target owns (BuddyAllocator.pinOwnedFrame).
   procedure View_Map (F : Frame_Index) with
     Pre  => Isolated and then Laundered and then Owners (F) in Process_Owner
             and then Viewers (F) < Max_Views,
     Post => Isolated and then Laundered and then Viewers (F) = Viewers'Old (F) + 1;

   --  The owner frees a frame a view still maps: the free is deferred
   --  (Frame_Pins.Request_Free answers Keep_Frame).
   procedure Free_Viewed (F : Frame_Index) with
     Pre  => Isolated and then Laundered and then Owners (F) in Process_Owner
             and then not Pins (F)
             and then Viewers (F) > 0,
     Post => Isolated and then Laundered and then Owners (F) = Retired;

   --  A view goes (after its entry is cleared and a round completed); the
   --  last view of a Retired frame frees it (Frame_Pins.Unpin answers
   --  Reclaim_Frame), dirty.
   procedure View_Unmap (F : Frame_Index) with
     Pre  => Isolated and then Laundered and then Viewers (F) > 0,
     Post => Isolated and then Laundered and then Viewers (F) = Viewers'Old (F) - 1
             and then (if Owners'Old (F) = Retired and then Viewers'Old (F) = 1
                       then Owners (F) = Free
                       else Owners (F) = Owners'Old (F));

   --  What the views exist for: a viewed frame is never free, so never
   --  allocated to another owner, and is never the kernel's.
   procedure Viewed_Owned (F : Frame_Index) with
     Ghost,
     Pre  => Isolated and then Laundered and then Viewers (F) > 0,
     Post => Owners (F) /= Free and then Owners (F) /= Kernel
             and then Regions (Region_Of (F)).State = Unmapped;

   -- The window -------------------------------------------------------------

   procedure Window_Map (C : CPU_Index; F : Frame_Index) with
     Pre  => Isolated and then Laundered and then not Windows (C).Mapped
             and then Regions (Region_Of (F)).State = Unmapped
             and then (Owners (F) = Free or else Pins (F)),
     Post => Isolated and then Laundered and then Windows (C).Mapped and then Windows (C).Frame = F;

   --  Zero a free, dirty frame through the window.
   procedure Window_Zero (C : CPU_Index) with
     Pre  => Isolated and then Laundered and then Windows (C).Mapped
             and then Owners (Windows (C).Frame) = Free,
     Post => Isolated and then Laundered and then Contents (Windows (C).Frame) = Zero;

   --  Copy into a process's frame (a child's image at spawn).
   procedure Window_Copy (C : CPU_Index) with
     Pre  => Isolated and then Laundered and then Windows (C).Mapped
             and then Owners (Windows (C).Frame) in Process_Owner,
     Post => Isolated and then Laundered and then Contents (Windows (C).Frame) = Process_Data;

   procedure Window_Unmap (C : CPU_Index) with
     Pre  => Isolated and then Laundered and then Windows (C).Mapped,
     Post => Isolated and then Laundered and then not Windows (C).Mapped
             and then Residue (Windows'Old (C).Frame) = Rounds;

   -- Pool transitions (2 MiB, no fixed split) ---------------------------------

   procedure Complete_Round with
     Pre  => Laundered and then Rounds < Round_Count'Last,
     Post => Laundered and then Rounds = Rounds'Old + 1;

   --  A laundered frame: zeroed, a round since its last window use.
   procedure Mark_Clean (F : Frame_Index) with
     Pre  => Isolated and then Laundered and then Owners (F) = Free
             and then Contents (F) = Zero and then Rounds > Residue (F),
     Post => Isolated and then Laundered and then Clean (F);

   --  Kernel pool -> process pool, step 1: the entry is cleared.
   procedure Begin_Unmap (R : Region_Index) with
     Pre  => Isolated and then Laundered and then Regions (R).State = Kernel_Mapped
             and then (for all F in Frame_Index =>
                         (if Region_Of (F) = R then Owners (F) = Free)),
     Post => Isolated and then Laundered and then Regions (R).State = Stale
             and then Regions (R).Stamp = Rounds;

   --  Step 2: a round completed after the clear.
   procedure Finish_Unmap (R : Region_Index) with
     Pre  => Isolated and then Laundered and then Regions (R).State = Stale
             and then Rounds > Regions (R).Stamp,
     Post => Isolated and then Laundered and then Regions (R).State = Unmapped;

   --  Process pool -> kernel pool: only free frames that hold no process
   --  data, and no window on any of them.
   procedure Map_Back (R : Region_Index) with
     Pre  => Isolated and then Laundered and then Regions (R).State = Unmapped
             and then (for all F in Frame_Index =>
                         (if Region_Of (F) = R then
                            Owners (F) = Free
                            and then Contents (F) /= Process_Data))
             and then (for all C in CPU_Index =>
                         (if Windows (C).Mapped then
                            Region_Of (Windows (C).Frame) /= R)),
     Post => Isolated and then Laundered and then Regions (R).State = Kernel_Mapped;
end Kernel_Map_Ownership;
