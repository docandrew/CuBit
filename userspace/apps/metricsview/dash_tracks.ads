------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  One drawn series of one panel (MET-002's app): its column ring (the
--  level of detail at the panel's width), which columns are final, and the
--  fetches that fill it through a Dash_Feeds.Feed.
--
--  @description
--  A column is final once it is older than Settle_Margin_Us (the store
--  drains outlets every 100 ms and producers publish a few frames after an
--  input): it is fetched once. Newer columns are provisional and fetched
--  again each refresh, so a sample shows on the next frame and a late one
--  is still taken. Final columns are held as one run Lo .. Hi; a refresh
--  extends it to the right (the live edge, one call) and then to the left
--  in chunks while the frame's fetch budget lasts (first load, pan, zoom),
--  so interaction never waits for the store. Columns narrower than the
--  store's finest tier are made from raw samples (exact), wider ones from
--  tier windows. Only columns whose summary changed are marked dirty.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Charts_Buckets;
with Charts_Columns;
with Charts_Quantiles;
with Charts_Time_Axis;
with Dash_Feeds;
with Dash_Model;

package Dash_Tracks is
   package Axis renames Charts_Time_Axis;
   subtype Column_Number is Axis.Column_Number;

   --  The store's finest tier (DESIGN-metrics-store.md: 1 s, 10 s, 1 min,
   --  10 min): narrower columns are made from raw samples.
   Finest_Tier_Us : constant := Axis.Microseconds_Per_Second;
   --  When a column stops changing: the store's drain interval (100 ms),
   --  producers' publication lag, and slack.
   Settle_Margin_Us : constant := 500 * Axis.Microseconds_Per_Millisecond;
   --  Columns fetched per call to the left of what is held.
   Chunk_Columns : constant := 64;
   --  How often an unresolved series is looked for again (a series appears
   --  with its first sample).
   Resolve_Interval_Us : constant := Axis.Microseconds_Per_Second;
   --  The most raw samples one fetch gathers (a dense chunk is refetched
   --  in smaller pieces): 8 MiB of values and times.
   Maximum_Batch_Samples : constant := 524_288;

   type Track_State is (Unresolved, Resolved, Missing, Refused, Unavailable);

   type Column_Ring_Access is access Charts_Columns.Ring;
   type Sample_Access is access Charts_Quantiles.Sample_Array;
   --  The most pushed samples a track keeps for its push-made columns
   --  (16 MiB of times and values); past it the oldest half is let go and
   --  those columns stay as made.
   Maximum_Recent_Samples : constant := 1_048_576;
   type Bucket_Ring_Access is access Charts_Buckets.Ring;

   --  Shared by the UI thread and the data thread (by reference, never
   --  copied): its fields change only under Dash_Sync's lock.
   type Track is limited record
      Panel : Dash_Model.Panel_Index := 1;
      --  Which of the panel's series specs, and which match of it (a
      --  prefix spec has several tracks).
      Spec_Index : Dash_Model.Series_Index := 1;
      Match_Index : Positive := 1;
      Spec : Dash_Model.Series_Spec;
      Info : Dash_Feeds.Series_Info;
      State : Track_State := Unresolved;
      Next_Resolve_Us : Unsigned_64 := 0;
      Columns : Column_Ring_Access;
      --  A ring as large, kept for the next zoom's resample (no allocation
      --  on the UI thread's path).
      Spare : Column_Ring_Access;
      --  Bumped by every viewport change (Follow): an answer the store gave
      --  for an older viewport is dropped, not folded.
      Generation : Unsigned_64 := 0;
      Buckets : Bucket_Ring_Access;
      Layout : Charts_Buckets.Layout;
      Width_Us : Axis.Column_Width := 1;
      --  The final columns held: Lo .. Next - 1 (none when Lo = Next).
      Lo, Next : Column_Number := 0;
      --  Columns changed since Take_Dirty.
      Dirty : Boolean := False;
      Dirty_First, Dirty_Last : Column_Number := 0;
      --  Stat panels: the store's merged window over the viewport.
      Stats : Dash_Feeds.Window_Stats;
      Stats_Valid, Stats_Stale : Boolean := False;
      --  The window before the viewport (as long), for the change shown
      --  beside a stat; read again every Previous_Interval_Us.
      Previous : Dash_Feeds.Window_Stats;
      Previous_Valid : Boolean := False;
      Previous_Due_Us : Unsigned_64 := 0;
      Stats_Due_Us : Unsigned_64 := 0;
      --  Set by the data thread when Stats or Previous changed; the UI
      --  thread redraws the panel and clears it.
      Stats_Changed : Boolean := False;
      --  Columns from tier windows (quantiles merged): the panel says so.
      From_Windows : Boolean := False;
      --  Push (DESIGN-metrics-push.md): the samples since Push_From_Us arrive
      --  through Accept_Sample, with no call; the columns after the one
      --  holding Push_From_Us are made from them (Recent), the ones up to it
      --  through the query path once they settle.
      Pushed : Boolean := False;
      Push_From_Us : Unsigned_64 := 0;
      Recent_Times, Recent_Values : Sample_Access;
      Recent_Used : Natural := 0;
      --  The time span whose push-made columns must be made again.
      Push_Dirty : Boolean := False;
      Push_Dirty_From_Us, Push_Dirty_To_Us : Unsigned_64 := 0;
      --  The store's count of samples published (the last probe's, and the
      --  one this track's columns were last fetched at).
      Latest_Published, Seen_Published : Unsigned_64 := 0;
      --  The newest column the last fetch reached (columns after it have
      --  not been asked for, published or not).
      Fetched_To : Column_Number := 0;
   end record;

   --  Make T's rings for a plot Columns wide at width W ending at V.Last
   --  (Buckets: a histogram panel's layout, else no bucket ring).
   procedure Configure (T : in out Track; V : Axis.Viewport; Histogram : Boolean; Layout : Charts_Buckets.Layout);

   --  The data thread's operations (Resolve, Refresh, Refresh_Stats, the
   --  push intake) are called with Dash_Sync's lock held and let it go
   --  around each call to the store; the UI thread's (Configure, Follow,
   --  Take_Dirty) and its drawing also hold it.

   --  Look the series up (when due); a prefix track takes its Match_Index-th match.
   procedure Resolve (T : in out Track; F : in out Dash_Feeds.Feed'Class; Now_Us : Unsigned_64);

   --  The viewport changed: same width (pan, live edge) keeps what is still
   --  in view; another width (zoom, resize) resamples the held columns into a
   --  preview and refetches everything.
   procedure Follow (T : in out Track; V : Axis.Viewport);

   --  Refetch the provisional columns and extend the final run, while
   --  F's clock is before Budget_End_Us (the live edge is always fetched).
   --  With nothing published since the last fetch (Latest_Published =
   --  Seen_Published) the provisional columns are already right: they
   --  become final with no call.
   procedure Refresh
     (T : in out Track; F : in out Dash_Feeds.Feed'Class; V : Axis.Viewport; Now_Us, Budget_End_Us : Unsigned_64);

   --  Start taking pushed samples (stamped From_Us and later).
   procedure Begin_Push (T : in out Track; From_Us : Unsigned_64);
   --  One pushed sample of T's series.
   procedure Accept_Sample (T : in out Track; Time_Us, Value : Unsigned_64);
   --  The push lost samples in First_Us .. Last_Us (the subscription shed
   --  them): those columns are made again through the query path.
   procedure Push_Lost (T : in out Track; First_Us, Last_Us : Unsigned_64);

   --  Every column in view is loaded (final or provisional).
   function Complete (T : Track; V : Axis.Viewport) return Boolean;

   --  Stat panels: the merged window over the viewport, when stale, and
   --  the window before it, when due.
   Previous_Interval_Us : constant := Axis.Microseconds_Per_Second;
   --  A stat's window is asked for at most this often (a merged sketch is a
   --  store call; a number changing faster than this is not read anyway).
   Stats_Interval_Us : constant := 250 * Axis.Microseconds_Per_Millisecond;
   procedure Refresh_Stats (T : in out Track; F : in out Dash_Feeds.Feed'Class; V : Axis.Viewport; Now_Us : Unsigned_64);

   procedure Take_Dirty (T : in out Track; Any : out Boolean; First, Last : out Column_Number);

   --  Calls to the feed and samples folded so far (the app's own metrics).
   function Fetches return Unsigned_64;
   function Samples_Folded return Unsigned_64;
end Dash_Tracks;
