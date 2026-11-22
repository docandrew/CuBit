------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The metrics viewer's feed on CuBit today: metrics.svc's query path
--  (CuBit.Metric_Queries) through the program's metrics-observer endpoint,
--  within the read scopes its manifest declares (IPC-008). One lent page
--  and one call per read; each call is timed into metricsview.query_us.
--
--  @description
--  A push subscription (DESIGN-metrics-push.md) replaces this feed; the
--  tracks and the view stay as they are.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Charts_Columns;
with Charts_Quantiles;
with CuBit.Channels;
with CuBit.Metric_Queries;
with Dash_Feeds;
with Dash_Model;

package Dash_Query_Feeds is
   type Query_Feed is new Dash_Feeds.Feed with private;

   --  Bound to the program's "series" startup entry (absent: every read
   --  answers Unavailable).
   procedure Bind (F : in out Query_Feed);
   function Bound (F : Query_Feed) return Boolean;

   overriding procedure Find
     (F : in out Query_Feed; Spec : Dash_Model.Series_Spec; Found : out Dash_Feeds.Series_Infos;
      Listed, Total : out Natural; Result : out Dash_Feeds.Status);
   overriding procedure List_All
     (F : in out Query_Feed; Skip : Natural; Found : out Dash_Feeds.Series_Infos; Listed, Total : out Natural;
      Result : out Dash_Feeds.Status);
   overriding procedure Read_Raw
     (F : in out Query_Feed; Id : Dash_Feeds.Series_Id; From_Us, To_Us, Skip : Unsigned_64;
      Times, Values : out Charts_Quantiles.Sample_Array; Rows, Total : out Natural; Result : out Dash_Feeds.Status);
   overriding procedure Read_Windows
     (F : in out Query_Feed; Id : Dash_Feeds.Series_Id; From_Us, To_Us, Skip, Step_Us : Unsigned_64;
      Windows : out Charts_Columns.Window_Rows; Rows, Total : out Natural; Resolution_Us : out Unsigned_64;
      Result : out Dash_Feeds.Status);
   overriding procedure Read_Stats
     (F : in out Query_Feed; Id : Dash_Feeds.Series_Id; From_Us, To_Us : Unsigned_64;
      Stats : out Dash_Feeds.Window_Stats; Result : out Dash_Feeds.Status);
   overriding procedure Start_Push (F : in out Query_Feed; Started : out Boolean);
   overriding function Pushing (F : Query_Feed) return Boolean;
   overriding procedure Take_Updates (F : in out Query_Feed; Into : out Dash_Feeds.Update_Array; Count : out Natural);
   overriding function Now_Us (F : Query_Feed) return Unsigned_64;
   overriding function Calls (F : Query_Feed) return Unsigned_64;

   --  Time spent in calls so far (microseconds), and the slowest one.
   function Call_Time (F : Query_Feed) return Unsigned_64;
   function Slowest_Call (F : Query_Feed) return Unsigned_64;
private
   type Reader_Access is access CuBit.Metric_Queries.Reader;
   type Query_Feed is new Dash_Feeds.Feed with record
      Reader : Reader_Access;
      Call_Count, Time_Us, Slowest_Us : Unsigned_64 := 0;
      --  The push subscription's ring (CuBit.Metric_Updates).
      Link : CuBit.Channels.Channel;
      Push : Boolean := False;
      --  Elements that did not decode (counted, never used).
      Malformed : Unsigned_64 := 0;
   end record;
end Dash_Query_Feeds;
