pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channels;
with CuBit.Control_Events;
with CuBit.Messages;
with CuBit.Metric_Protocol;
with Metric_Scopes;
with Metric_Series;
private with Heap_Arrays;

--  metrics.svc's push subscriptions (MET-002, DESIGN-metrics-push.md,
--  CuBit.Metric_Updates). A reader opens a consuming channel through its
--  metrics-observer endpoint (Open) and subscribes it to a selector
--  (Subscribe). Every sample the store then takes for a series that the
--  selector names and the reader's read scopes allow (Metric_Scopes, as for
--  every read) is put into that reader's ring (Offer_Sample), with the
--  store's gaps (Offer_Gap). The ring is bounded and sheds: what does not
--  fit is counted and sent as one Subscriber_Shed gap once there is room.
--  After each pass the readers that got anything are woken (Flush): one
--  channel wake per reader per pass, never one per sample.
--
--  While anyone subscribes, metrics.svc takes the publishers' rings every
--  Live_Drain_Interval_Ms instead of every Metric_Outlets.Drain_Interval_Ms,
--  so a sample reaches a live reader within a few milliseconds; publishers
--  still make no system call per sample (DESIGN-metrics-push.md: a
--  per-put kick from Channels.Put would cost one per sample).
package Metric_Subscriptions is
   Live_Drain_Interval_Ms : constant := 4;
   --  Subscriptions at once (each one channel of CuBit.Metric_Updates.Ring_Pages).
   Maximum_Subscriptions : constant := 1_024;
   --  Channel numbers of subscriptions start here, apart from outlets' and
   --  launchers' (OP_CLOSE carries the number).
   Number_Base : constant := 2 ** 32;

   type Table is limited private;

   --  A reader's consuming open (From's observer badge, Authority).
   procedure Open
     (T : in out Table; From : CuBit.Messages.Process_ID; Authority : Unsigned_64;
      Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message);
   function Owns (Number : Unsigned_64) return Boolean is (Number > Number_Base);
   procedure Close (T : in out Table; From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message);
   procedure Ended (T : in out Table; Event : CuBit.Control_Events.Event);

   --  Subscribe the channel Number that From opened to Sel. Out_Of_Scope:
   --  the reader has no read scope at all, or names an exact series outside
   --  them (a broader selector simply leaves out what it may not see).
   procedure Subscribe
     (T : in out Table; Scopes : Metric_Scopes.Table; From : CuBit.Messages.Process_ID; Number : Unsigned_64;
      Sel : Metric_Series.Selector; Result : out CuBit.Metric_Protocol.Status);

   --  Anyone subscribed (metrics.svc then drains every Live_Drain_Interval_Ms).
   function Active (T : Table) return Boolean;

   procedure Offer_Sample
     (T : in out Table; Store : Metric_Series.Store; Scopes : Metric_Scopes.Table;
      Index : Metric_Series.Series_Index; Time_Us, Value : Unsigned_64);
   procedure Offer_Gap
     (T : in out Table; Store : Metric_Series.Store; Scopes : Metric_Scopes.Table;
      Index : Metric_Series.Series_Index; Count, First_Us, Last_Us : Unsigned_64);

   --  After a pass: pending shed reports, and a wake for each reader given
   --  anything.
   procedure Flush (T : in out Table);

   type Totals is record
      Subscriptions, Put, Shed : Unsigned_64 := 0;
   end record;
   function Accounting (T : Table) return Totals;
private
   --  Per series, whether a subscription takes it: not yet decided, or
   --  decided (the series' selector and scope do not change).
   type Decision is (Undecided, Taken, Passed);
   package Decision_Blocks is new Heap_Arrays (Decision);

   type Subscription is record
      Owner : CuBit.Messages.Process_ID := CuBit.Messages.No_Process;
      Reader : Unsigned_64 := 0;
      Link : CuBit.Channels.Channel;
      Subscribed : Boolean := False;
      Sel : Metric_Series.Selector;
      Decisions : Decision_Blocks.Block := Decision_Blocks.Empty;
      --  Put since the last wake.
      Fresh : Boolean := False;
      --  Shed and not yet reported.
      Shed, Shed_First_Us, Shed_Last_Us : Unsigned_64 := 0;
   end record;
   package Subscription_Blocks is new Heap_Arrays (Subscription);
   type Table is limited record
      Items : Subscription_Blocks.Block := Subscription_Blocks.Empty;
      Used : Natural := 0;
      Live : Natural := 0;
      Sums : Totals;
   end record;
end Metric_Subscriptions;
