with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Present_Timing;
with CuBit.Timing_Histograms;
with Compositor_Latency; use Compositor_Latency;
--  Hosted tests of the LAT-002 ledger: one key press followed hop by hop
--  to its latch, refreshes late counted on a hardware grid and on the
--  virtual grid, dropped frames retried, unshown inputs expired, the
--  ledger bounded.
procedure Latency_Tests is
   package PT renames CuBit.Present_Timing;
   package TH renames CuBit.Timing_Histograms;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Ada.Text_IO.Put_Line ("FAIL " & Name);
         raise Program_Error with Name;
      end if;
      Checks := Checks + 1;
   end Check;

   Period : constant := 16_667;
   Surface : constant := 7;

   --  One press: driver at Origin, then fixed hop costs, then the backend
   --  arms at Ready and latches at Latch.
   procedure One (L : in out Ledger; S : in out Stats; Origin : Time_Us; Serial, Token : Unsigned_64;
                  Timing : PT.Timing) is
      Item : Slot;
   begin
      Intake (L, Origin, Origin + 20, Serial, Item);
      Route (L, Item, Surface, Serial);
      Delivered (L, Surface, Serial, Origin + 40);
      Published (L, Surface, Serial, Origin + 140);
      Compose (L, 0, CPU, 1_000, 2, Origin + 150);
      Frame_Ready (L, 0, Origin + 190);
      Submitted (L, 0, Token, Origin + 200);
      Completed (L, S, Token, Timing, Timing.Observed_Us + 5);
   end One;

   procedure Hop_Cases is
      L : Ledger;
      S : Stats;
      Vblank : constant Time_Us := 1_000_000;
   begin
      --  Input 1 ms before a vblank; armed 220 us later: made it (0 late).
      One (L, S, Vblank - 1_000, 1, 11,
           (PT.Hardware_Latch, Vblank - 780, Vblank, Vblank + 300, Period));
      Check (S.Shown = 1 and then S.Late (0) = 1, "made the next refresh");
      Check (TH.Count (S.Stages (Input_To_Ready)) = 1 and then
             TH.Maximum (S.Stages (Input_To_Ready)) = 220, "input to ready");
      Check (TH.Maximum (S.Stages (Driver_To_Intake)) = 20 and then
             TH.Maximum (S.Stages (Intake_To_Delivery)) = 20 and then
             TH.Maximum (S.Stages (Delivery_To_Publish)) = 100 and then
             TH.Maximum (S.Stages (Publish_To_Compose)) = 10 and then
             TH.Maximum (S.Stages (Compose_To_Submit)) = 50 and then
             TH.Maximum (S.Stages (Submit_To_Ready)) = 20 and then
             TH.Maximum (S.Stages (Ready_To_Latch)) = 780 and then
             TH.Maximum (S.Stages (Latch_To_Observed)) = 300, "every hop");
      Check (S.By_Phase (7) (0) = 1, "phase: the last eighth before the vblank");
      Check (S.Grid = Hardware_Grid and then S.Period = Period and then S.Timed_Frames = 1,
             "hardware grid");
      Check (S.Paths (CPU).Frames = 1 and then S.Paths (CPU).Pixels = 1_000 and then
             S.Paths (CPU).Rectangles = 2 and then TH.Maximum (S.Paths (CPU).Render) = 40,
             "frame path stats");
      Check (In_Flight (L) = 0, "shown input forgotten");
      --  LAT-002: frames that carried an input, and their damage.
      Compose (L, 0, CPU, 50_000, 1, Vblank + 500);   --  an animation frame: no input
      Take_Counts (L, S);
      Check (S.Input_Frames = 1 and then S.Input_Frame_Pixels = 1_000 and then
             S.Input_Frame_Max_Pixels = 1_000, "only frames carrying an input count as input frames");
      --  Input 100 us before a vblank: latched one refresh later (1 late).
      One (L, S, Vblank + Period - 100, 2, 12,
           (PT.Hardware_Latch, Vblank + Period + 120, Vblank + 2 * Period, Vblank + 2 * Period + 10,
            Period));
      Check (S.Late (1) = 1, "missed one refresh");
      --  Three refreshes late, and the bucket caps at four or more.
      Check (Refreshes_Late (0, 4 * Period, Period) = 3 and then
             Refreshes_Late (0, Period, Period) = 0 and then
             Refreshes_Late (Period, Period, Period) = 0, "refreshes late arithmetic");
      Check (Bucket (9) = Late_Bucket'Last and then Bucket (2) = 2, "buckets");
   end Hop_Cases;

   procedure Virtual_Cases is
      L : Ledger;
      S : Stats;
      G : constant Time_Us := 100 * Virtual_Period_Us;
   begin
      Check (Grid_At_Or_After (G + 1, 0, Virtual_Period_Us) = G + Virtual_Period_Us and then
             Grid_At_Or_After (G, 0, Virtual_Period_Us) = G, "virtual grid");
      --  A host fence: ready when the fence completes, latched at the next
      --  virtual vblank.
      One (L, S, G + 100, 1, 21, (PT.Host_Fence, G + 300, G + 400, G + 400, 0));
      Check (S.Grid = Virtual_Grid and then S.Late (0) = 1 and then
             TH.Maximum (S.Stages (Input_To_Ready)) = 300 and then
             TH.Maximum (S.Stages (Ready_To_Latch)) = Virtual_Period_Us - 400 and then
             TH.Count (S.Stages (Latch_To_Observed)) = 0, "fence on the virtual grid");
   end Virtual_Cases;

   procedure Lifecycle_Cases is
      L : Ledger;
      S : Stats;
      Item : Slot;
   begin
      --  Unrouted presses are not followed.
      Intake (L, 10, 20, 1, Item);
      Forget_Unrouted (L, Item);
      Check (In_Flight (L) = 0, "unrouted forgotten");
      --  A publication with a lower watermark does not cover the event.
      Intake (L, 10, 20, 2, Item);
      Route (L, Item, Surface, 5);
      Delivered (L, Surface, 5, 30);
      Published (L, Surface, 4, 40);
      Published (L, Surface + 1, 9, 40);
      Compose (L, 0, GPU, 10, 1, 50);
      Submitted (L, 0, 31, 60);
      Completed (L, S, 31, (PT.Hardware_Latch, 70, 80, 90, Period), 100);
      Check (S.Shown = 0 and then In_Flight (L) = 1, "uncovered event waits");
      Check (S.Paths (GPU).Frames = 1, "the frame still counts for its path");
      --  Covered, composed, dropped by display, then shown by the redraw.
      Published (L, Surface, 5, 200);
      Compose (L, 0, GPU, 10, 1, 210);
      Submitted (L, 0, 32, 220);
      Dropped (L, S, 32);
      Check (S.Dropped_Frames = 1 and then In_Flight (L) = 1, "dropped frame keeps its inputs");
      Compose (L, 0, GPU, 10, 1, 300);
      Submitted (L, 0, 33, 310);
      Completed (L, S, 33, PT.No_Timing, 400);
      Check (S.Shown = 1 and then S.Untimed = 1 and then In_Flight (L) = 0, "untimed shown");
      --  Expiry.
      Intake (L, 1_000, 1_000, 3, Item);
      Route (L, Item, Surface, 6);
      Expire (L, S, 1_000 + Expiry_Us + 1);
      Check (S.Unshown = 1 and then In_Flight (L) = 0, "expired as unshown");
      --  Bounded: Capacity + 5 presses evict five.
      for I in 1 .. Capacity + 5 loop
         Intake (L, Time_Us (I), Time_Us (I), Unsigned_64 (I), Item);
         Route (L, Item, Surface, Unsigned_64 (100 + I));
      end loop;
      Take_Counts (L, S);
      Check (In_Flight (L) = Capacity and then S.Evicted = 5 and then
             S.Inputs = Unsigned_64 (Capacity + 5 + 3 + 0), "bounded with evictions");
      Reset (S, 5);
      Check (S.Started = 5 and then S.Inputs = 0 and then S.Shown = 0 and then
             S.Paths (GPU).Frames = 0 and then TH.Count (S.Stages (Input_To_Ready)) = 0 and then
             S.Late = [0, 0, 0, 0, 0], "reset in place");
   end Lifecycle_Cases;
   --  MET-002: every sample the ledger takes is queued as an observation,
   --  each stage's value equal to its histogram's; past the queue's
   --  capacity, observations are counted as lost with their time range.
   procedure Observation_Cases is
      L : Ledger;
      S : Stats;
      Vblank : constant Time_Us := 1_000_000;
      type Stage_Values is array (Stage) of Unsigned_64;
      Seen : Stage_Values := [others => 0];
      Kinds : array (Observation_Kind) of Natural := [others => 0];
      Lost_Count : Unsigned_64 := 0;
      Lost_First, Lost_Last : Unsigned_64 := 0;
      procedure Each (Item : Observation) is
      begin
         Kinds (Item.Kind) := Kinds (Item.Kind) + 1;
         if Item.Kind = Stage_Sample then
            Seen (Item.Of_Stage) := Item.Value;
         end if;
      end Each;
      procedure Lost (Kind : Observation_Kind; Of_Stage : Stage; Path : Render_Path;
                      Count, First_Us, Last_Us : Unsigned_64) is
         pragma Unreferenced (Of_Stage, Path);
      begin
         if Kind = Input_Taken then
            Lost_Count := Lost_Count + Count;
            Lost_First := First_Us;
            Lost_Last := Last_Us;
         end if;
      end Lost;
      procedure Drain is new Drain_Observations (Each, Lost);
      Item : Slot;
   begin
      One (L, S, Vblank - 1_000, 1, 11,
           (PT.Hardware_Latch, Vblank - 780, Vblank, Vblank + 300, Period));
      Drain (L);
      Check (Seen (Input_To_Ready) = 220 and then Seen (Ready_To_Latch) = 780 and then
             Seen (Latch_To_Observed) = 300 and then Seen (Driver_To_Intake) = 20,
             "observations carry each stage's time");
      Check (Kinds (Stage_Sample) = 10 and then Kinds (Late_Sample) = 1 and then Kinds (Input_Taken) = 1 and then
             Kinds (Input_Shown) = 1 and then Kinds (Damage_Pixels) = 1 and then Kinds (Render_Cost) = 1,
             "one observation per sample");
      Kinds := [others => 0];
      Drain (L);
      Check (Kinds = [Observation_Kind => 0], "a drain empties the queue");
      --  The next timed frame, one refresh later: one vblank per frame.
      One (L, S, Vblank + 1_000, 2, 12,
           (PT.Hardware_Latch, Vblank + Period - 500, Vblank + Period, Vblank + Period + 10, Period));
      Drain (L);
      Check (Kinds (Frame_Vblanks) = 1, "frames against vblanks");
      --  Past the queue's capacity: counted, never dropped silently.
      for I in 1 .. Observation_Capacity + 7 loop
         Intake (L, Time_Us (I), Time_Us (I), Unsigned_64 (I), Item);
         Forget_Unrouted (L, Item);
      end loop;
      Kinds := [others => 0];
      Drain (L);
      Check (Kinds (Input_Taken) = Observation_Capacity and then Lost_Count = 7 and then
             Lost_First = Observation_Capacity + 1 and then Lost_Last = Observation_Capacity + 7,
             "overflow counted with its time range");
   end Observation_Cases;
begin
   Hop_Cases;
   Virtual_Cases;
   Lifecycle_Cases;
   Observation_Cases;
   Ada.Text_IO.Put_Line ("compositor-latency: PASS checks=" & Checks'Image);
end Latency_Tests;
