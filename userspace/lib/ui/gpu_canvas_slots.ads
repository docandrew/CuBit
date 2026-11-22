------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The GPU canvas widget's frame bookkeeping (CuBit.UI.GPU_Canvas): which
--  render target the application may draw into, which one is being read
--  back, and which one holds the newest finished frame. Pure decisions over
--  plain state; the widget performs the Vulkan work each transition names.
--  Proved (tests/gpu-canvas/run.sh --prove).
--
--  A slot's life:  Empty -> Free -> Acquired -> Copying -> Front -> Free.
--  Empty: no storage. Free: storage, no frame. Acquired: lent to the
--  application to render into. Copying: the widget's readback was submitted.
--  Front: the newest finished frame, the one Paint shows. At most one slot
--  is Front; a newer finished frame replaces it and the old one is Free.
------------------------------------------------------------------------------
package GPU_Canvas_Slots with SPARK_Mode, Pure is

   --  Two targets: the application renders frame N+1 while frame N is read
   --  back and shown.
   Slot_Count : constant := 2;
   type Slot_Index is range 1 .. Slot_Count;

   type Slot_Phase is (Empty, Free, Acquired, Copying, Front);
   type Phase_Array is array (Slot_Index) of Slot_Phase;

   --  Render target edge in physical pixels (a 4K window at density 2 is
   --  7680 wide; Vulkan guarantees 4096 and Intel offers 16384).
   Maximum_Extent : constant := 16_384;
   subtype Extent is Natural range 0 .. Maximum_Extent;
   type Size is record
      Width, Height : Extent := 0;
   end record;
   function Is_Empty (S : Size) return Boolean is (S.Width = 0 or else S.Height = 0);

   --  Bumped on every reallocation, so the application can tell its
   --  per-target objects (framebuffers, depth images) are stale.
   type Generation_Count is mod 2 ** 32;

   type State is record
      Phases : Phase_Array := [others => Empty];
      Allocated : Size;                --  storage size of every non-Empty slot
      Wanted : Size;                   --  what the layout asks for
      Visible : Boolean := True;       --  application's say
      Front_Painted : Boolean := True; --  no Front, or it was painted
      Generation : Generation_Count := 0;
   end record;

   --  Slots 1 .. Last in Phase.
   function Count_Through (S : State; Phase : Slot_Phase; Last : Slot_Index) return Natural is
     ((if S.Phases (Last) = Phase then 1 else 0) +
      (if Last = Slot_Index'First then 0 else Count_Through (S, Phase, Last - 1)))
     with Subprogram_Variant => (Decreases => Last),
          Post => Count_Through'Result <= Natural (Last);

   function Count (S : State; Phase : Slot_Phase) return Natural is
     (Count_Through (S, Phase, Slot_Index'Last))
     with Post => Count'Result <= Slot_Count;

   --  Every slot has storage or none has; at most one Front.
   function Valid (S : State) return Boolean is
     ((Count (S, Empty) = 0 or else Count (S, Empty) = Slot_Count) and then
      Count (S, Front) <= 1 and then
      (if Count (S, Empty) = 0 then not Is_Empty (S.Allocated)));

   function In_Flight (S : State) return Boolean is
     (Count (S, Acquired) > 0 or else Count (S, Copying) > 0);

   type Decision is
     (Grant,         --  hand Slot to the application
      Hidden,        --  invisible or zero-sized: draw nothing
      Busy,          --  wait: no free slot, frame unpainted, or work in flight
      Reallocate);   --  nothing in flight and the size changed: rebuild

   type Choice is record
      Kind : Decision := Busy;
      Slot : Slot_Index := Slot_Index'First;
   end record;

   function Free_Slot (S : State) return Slot_Index
     with Pre => Count (S, Free) > 0,
          Post => S.Phases (Free_Slot'Result) = Free;

   --  What Acquire should do now.
   function Choose (S : State) return Choice
     with Pre => Valid (S),
          Post =>
            (case Choose'Result.Kind is
               when Grant => S.Phases (Choose'Result.Slot) = Free and then
                             S.Visible and then not Is_Empty (S.Wanted) and then
                             S.Wanted = S.Allocated and then S.Front_Painted,
               when Hidden => not S.Visible or else Is_Empty (S.Wanted),
               when Reallocate => S.Visible and then not In_Flight (S) and then
                                  not Is_Empty (S.Wanted) and then S.Wanted /= S.Allocated,
               when Busy => True);

   procedure Acquire (S : in out State; Slot : Slot_Index)
     with Pre => Valid (S) and then S.Phases (Slot) = Free,
          Post => Valid (S) and then S.Phases (Slot) = Acquired and then
                  (for all I in Slot_Index => (if I /= Slot then S.Phases (I) = S.Phases'Old (I)));

   procedure Present (S : in out State; Slot : Slot_Index)
     with Pre => Valid (S) and then S.Phases (Slot) = Acquired,
          Post => Valid (S) and then S.Phases (Slot) = Copying and then
                  (for all I in Slot_Index => (if I /= Slot then S.Phases (I) = S.Phases'Old (I)));

   --  The readback finished: Slot is the newest frame.
   procedure Complete (S : in out State; Slot : Slot_Index)
     with Pre => Valid (S) and then S.Phases (Slot) = Copying,
          Post => Valid (S) and then S.Phases (Slot) = Front and then
                  Count (S, Front) = 1 and then not S.Front_Painted;

   function Front_Slot (S : State) return Slot_Index
     with Pre => Count (S, Front) > 0,
          Post => S.Phases (Front_Slot'Result) = Front;

   procedure Painted (S : in out State)
     with Pre => Valid (S), Post => Valid (S) and then S.Front_Painted and then
                                    S.Phases = S.Phases'Old;

   --  Storage was (re)built at Wanted for every slot.
   procedure Reallocated (S : in out State)
     with Pre => not Is_Empty (S.Wanted) and then not In_Flight (S),
          Post => Valid (S) and then Count (S, Free) = Slot_Count and then
                  S.Allocated = S.Wanted and then S.Front_Painted and then
                  S.Generation = S.Generation'Old + 1;

   --  All storage was released.
   procedure Released (S : in out State)
     with Post => Valid (S) and then Count (S, Empty) = Slot_Count and then
                  S.Allocated = (0, 0) and then S.Front_Painted;

   --  A lent slot came back without a present (its storage was rebuilt).
   procedure Cancel (S : in out State; Slot : Slot_Index)
     with Pre => Valid (S) and then S.Phases (Slot) = Acquired,
          Post => Valid (S) and then S.Phases (Slot) = Free;

end GPU_Canvas_Slots;
