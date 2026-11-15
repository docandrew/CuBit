pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Present_Timing;
with CuBit.Timing_Histograms;

--  Input-to-photon accounting (LAT-002): every key press from a driver is
--  followed through Desktop to the refresh that showed it, and the time of
--  each hop is kept in bounded histograms. No allocation, no IPC, no clock
--  of its own: the caller passes microseconds of the published monotonic
--  clock (CuBit.Monotonic.Microseconds) to every call.
--
--  One input's identity is its driver's (source sequence and acquisition
--  time), then the app's event serial on the channel it was queued to
--  (Target, Serial); the app's publication watermark (Input_After) ties
--  the serial to a surface publication, Desktop's next composition to an
--  output frame, the frame's completion token to the backend's present
--  timing (CuBit.Present_Timing).
--
--  The hops of an input, in order (Stage):
--    driver acquisition -> Desktop intake -> app takes the event -> app
--    publishes a frame covering it -> Desktop starts composing an output
--    frame with it -> the frame is submitted to display.svc -> the
--    backend armed it (ready for the latch) -> the latch -> the backend saw
--    the latch.
--  Refreshes late: how many vblanks passed between the first one after the
--  driver acquired the input and the one that latched the frame showing
--  it; zero means it made the next refresh. A backend with no refresh
--  (Host_Fence: virtio-gpu, a firmware framebuffer) is measured against a
--  virtual Virtual_Period_Us grid, and its fence stands for the arm.
package Compositor_Latency with SPARK_Mode is
   subtype Time_Us is Unsigned_64;
   Unavailable : constant Time_Us := Unsigned_64'Last;

   --  Inputs followed at once; a press beyond it evicts the oldest.
   Capacity : constant := 64;
   type Slot is range 0 .. Capacity;
   No_Slot : constant Slot := 0;
   subtype Live_Slot is Slot range 1 .. Capacity;

   Maximum_Outputs : constant := 2;
   type Output_Number is range 0 .. Maximum_Outputs - 1;

   --  An input that no frame showed within this is counted as unshown (a
   --  key with no visible effect) and forgotten.
   Expiry_Us : constant := 2_000_000;
   --  The virtual refresh for backends without one (60 Hz).
   Virtual_Period_Us : constant := 16_667;

   type Render_Path is (CPU, GPU);

   type Stage is
     (Driver_To_Intake,     --  driver acquisition to Desktop's intake
      Intake_To_Delivery,   --  queued until the app took the event
      Delivery_To_Publish,  --  the app's handling, drawing and publication
      Publish_To_Compose,   --  waiting for Desktop to start a frame
      Compose_To_Submit,    --  Desktop's composition, raster/render, readback
      Submit_To_Ready,      --  display.svc's copy, IPC, the backend's arm
      Ready_To_Latch,       --  slack: the frame waits for its vblank
      Latch_To_Observed,    --  the backend's polling lag (release, not photon)
      Input_To_Ready,       --  the headline budget: driver to frame ready
      Input_To_Latch);      --  driver to the latch (photon start)

   ---------------------------------------------------------------------------
   --  Budgets (printed, not enforced): input to frame-ready-for-latch at
   --  most 250 us at p99, split over the hops before the latch. The hops
   --  after it are not CuBit's work (the vblank wait) or not on the photon
   --  path (polling lag); they have none.
   ---------------------------------------------------------------------------
   Input_To_Ready_Budget_Us : constant := 250;
   Driver_To_Intake_Budget_Us : constant := 25;
   Intake_To_Delivery_Budget_Us : constant := 25;
   Delivery_To_Publish_Budget_Us : constant := 100;
   Publish_To_Compose_Budget_Us : constant := 25;
   Compose_To_Submit_Budget_Us : constant := 50;
   Submit_To_Ready_Budget_Us : constant := 25;
   pragma Compile_Time_Error
     (Driver_To_Intake_Budget_Us + Intake_To_Delivery_Budget_Us + Delivery_To_Publish_Budget_Us +
      Publish_To_Compose_Budget_Us + Compose_To_Submit_Budget_Us + Submit_To_Ready_Budget_Us /=
      Input_To_Ready_Budget_Us, "stage budgets must add up to the input-to-ready budget");
   No_Budget : constant := 0;
   function Budget_Us (Item : Stage) return Unsigned_64 is
     (case Item is
        when Driver_To_Intake => Driver_To_Intake_Budget_Us,
        when Intake_To_Delivery => Intake_To_Delivery_Budget_Us,
        when Delivery_To_Publish => Delivery_To_Publish_Budget_Us,
        when Publish_To_Compose => Publish_To_Compose_Budget_Us,
        when Compose_To_Submit => Compose_To_Submit_Budget_Us,
        when Submit_To_Ready => Submit_To_Ready_Budget_Us,
        when Input_To_Ready => Input_To_Ready_Budget_Us,
        when Ready_To_Latch | Latch_To_Observed | Input_To_Latch => No_Budget);
   function Stage_Name (Item : Stage) return String is
     (case Item is
        when Driver_To_Intake => "driver_to_intake",
        when Intake_To_Delivery => "intake_to_delivery",
        when Delivery_To_Publish => "delivery_to_publish",
        when Publish_To_Compose => "publish_to_compose",
        when Compose_To_Submit => "compose_to_submit",
        when Submit_To_Ready => "submit_to_ready",
        when Ready_To_Latch => "ready_to_latch_slack",
        when Latch_To_Observed => "latch_to_observed",
        when Input_To_Ready => "input_to_ready",
        when Input_To_Latch => "input_to_latch");

   --  Refreshes late, 0 .. Late_Buckets - 1, the last meaning "or more".
   Late_Buckets : constant := 5;
   type Late_Bucket is range 0 .. Late_Buckets - 1;
   --  Where in the refresh interval the input arrived: eighths of the
   --  period after a vblank.
   Phase_Count : constant := 8;
   type Phase is range 0 .. Phase_Count - 1;
   type Late_Counts is array (Late_Bucket) of Unsigned_64;
   type Phase_Late_Counts is array (Phase) of Late_Counts;
   type Stage_Histograms is array (Stage) of CuBit.Timing_Histograms.Histogram;

   --  The grid the refreshes were counted on.
   type Grid_Kind is (No_Grid, Hardware_Grid, Virtual_Grid);

   type Path_Stats is record
      Frames : Unsigned_64 := 0;
      Pixels, Rectangles : Unsigned_64 := 0;
      --  Composition start to frame ready to submit (Desktop's drawing since
      --  the previous frame included), and submit to the backend's arm.
      Render : CuBit.Timing_Histograms.Histogram := CuBit.Timing_Histograms.Empty;
      Submit_To_Ready : CuBit.Timing_Histograms.Histogram := CuBit.Timing_Histograms.Empty;
   end record;
   type Path_Table is array (Render_Path) of Path_Stats;

   --  One report interval's statistics.
   type Stats is record
      Started : Time_Us := Unavailable;
      Inputs, Shown, Unshown, Evicted, Untimed, Dropped_Frames : Unsigned_64 := 0;
      Late : Late_Counts := [others => 0];
      By_Phase : Phase_Late_Counts := [others => [others => 0]];
      Stages : Stage_Histograms := [others => CuBit.Timing_Histograms.Empty];
      Paths : Path_Table;
      Grid : Grid_Kind := No_Grid;
      Period : Unsigned_64 := 0;
      --  Frames whose present timing arrived, and the refreshes in the
      --  interval at the last period (frames presented vs vblanks).
      Timed_Frames : Unsigned_64 := 0;
   end record;

   --  Start a new interval at Now, in place (Stats is tens of kilobytes:
   --  no temporary aggregate on the caller's stack).
   procedure Reset (S : in out Stats; Now : Time_Us);

   type Ledger is private;

   ---------------------------------------------------------------------------
   --  Hops. Each finds its records by identity; unknown ones are ignored.
   ---------------------------------------------------------------------------
   --  A key press from a driver: Origin its acquisition (Unavailable when
   --  the driver did not stamp it), Now Desktop's intake. The new record is
   --  Routing until Route or Forget.
   procedure Intake (L : in out Ledger; Origin, Now : Time_Us; Source_Sequence : Unsigned_64;
                     Item : out Slot);
   --  Desktop queued it to an app channel as event Serial.
   procedure Route (L : in out Ledger; Item : Slot; Target, Serial : Unsigned_64);
   --  Not routed to an app (Desktop consumed it, or it was swallowed): not
   --  followed (Desktop's own redraws have no watermark to tie them).
   procedure Forget_Unrouted (L : in out Ledger; Item : Slot);
   --  The app took event Serial from channel Target.
   procedure Delivered (L : in out Ledger; Target, Serial : Unsigned_64; Now : Time_Us);
   --  The app published a frame of surface Target covering events up to
   --  Watermark (zero: unknown, ignored).
   procedure Published (L : in out Ledger; Target, Watermark : Unsigned_64; Now : Time_Us);
   --  Desktop drew into its scene (the CPU path rasterizes here): the time
   --  is charged to the next frame's render cost.
   procedure Drew (L : in out Ledger; Started, Finished : Time_Us);
   --  Desktop captured Output's damage for a new frame: every published
   --  input is in it. Pixels and Rectangles: the frame's damage.
   procedure Compose (L : in out Ledger; Output : Output_Number; Path : Render_Path;
                      Pixels, Rectangles : Unsigned_64; Now : Time_Us);
   --  Output's frame is rendered and ready to submit.
   procedure Frame_Ready (L : in out Ledger; Output : Output_Number; Now : Time_Us);
   --  Output's frame went to display.svc with completion Token.
   procedure Submitted (L : in out Ledger; Output : Output_Number; Token : Unsigned_64;
                        Now : Time_Us);
   --  Display returned the frame unshown: its inputs wait for the redraw.
   procedure Dropped (L : in out Ledger; S : in out Stats; Token : Unsigned_64);
   --  The frame with Token was shown; Timing is the backend's (No_Timing
   --  when it reported none: the inputs count as Untimed).
   procedure Completed (L : in out Ledger; S : in out Stats; Token : Unsigned_64;
                        Timing : CuBit.Present_Timing.Timing; Now : Time_Us);
   --  Count as unshown and forget inputs taken more than Expiry_Us ago.
   procedure Expire (L : in out Ledger; S : in out Stats; Now : Time_Us);
   --  Inputs taken since the last Take, for Stats.Inputs and Evicted.
   procedure Take_Counts (L : in out Ledger; S : in out Stats);

   function In_Flight (L : Ledger) return Natural;

   ---------------------------------------------------------------------------
   --  Pure arithmetic, exposed for tests.
   ---------------------------------------------------------------------------
   --  The first vblank at or after Time on a grid of Period through Anchor.
   function Grid_At_Or_After (Time, Anchor, Period : Time_Us) return Time_Us
     with Pre => Period > 0 and then Time <= Unavailable / 2 and then Anchor <= Unavailable / 2;
   --  Vblanks strictly after Origin and strictly before Latch (Latch on
   --  the grid): zero when Latch is the first vblank after Origin.
   function Refreshes_Late (Origin, Latch, Period : Time_Us) return Unsigned_64 is
     (if Latch <= Origin then 0 else (Latch - Origin - 1) / Period)
     with Pre => Period > 0;
   function Bucket (Late : Unsigned_64) return Late_Bucket is
     (if Late >= Late_Buckets - 1 then Late_Bucket'Last else Late_Bucket (Late));
   --  The input's phase: eighths of a period since the vblank before it.
   function Phase_Of (Origin, Latch, Period : Time_Us) return Phase
     with Pre => Period > 0;
private
   type Input_State is (Free, Routing, Queued, Taken, Ready, Composed, Sent);
   type Input_Record is record
      State : Input_State := Free;
      Source_Sequence : Unsigned_64 := 0;
      Target, Serial : Unsigned_64 := 0;
      Output : Output_Number := 0;
      Token : Unsigned_64 := 0;
      Origin, Intake, Delivered_At, Published_At, Composed_At, Submitted_At : Time_Us := Unavailable;
   end record;
   type Input_Table is array (Live_Slot) of Input_Record;
   type Frame_Record is record
      Active : Boolean := False;
      Path : Render_Path := CPU;
      Started, Ready_At, Submitted_At : Time_Us := Unavailable;
      Draw_Us : Unsigned_64 := 0;
      Pixels, Rectangles : Unsigned_64 := 0;
      Token : Unsigned_64 := 0;
   end record;
   type Frame_Table is array (Output_Number) of Frame_Record;
   --  Submitted frames awaiting completion: one per output in flight.
   type Sent_Frame is record
      Used : Boolean := False;
      Token : Unsigned_64 := 0;
      Frame : Frame_Record;
   end record;
   type Sent_Table is array (Output_Number) of Sent_Frame;
   type Ledger is record
      Inputs : Input_Table;
      Frames : Frame_Table;
      Sent : Sent_Table;
      Pending_Draw_Us : Unsigned_64 := 0;
      Taken, Evicted : Unsigned_64 := 0;
   end record;
end Compositor_Latency;
