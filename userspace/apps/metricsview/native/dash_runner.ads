------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The metrics viewer's two threads on CuBit: the program's main thread is
--  the UI thread (main.adb: the window, input, drawing; no I/O), and the
--  data thread started here runs Dash_View.Data_Step every Data_Poll_Ms,
--  or at once when the UI thread asks (Wake: a zoom, a pan, a new range),
--  reading the store's pushed ring and calling the store. The two share the
--  tracks under Dash_Sync's lock, which neither holds across I/O.
--
--  @description
--  CuBit.Metrics is not thread-safe: both threads publish under the same
--  lock (Observe).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Metrics;
with Dash_Query_Feeds;
with Dash_View;

package Dash_Runner is
   Feed : aliased Dash_Query_Feeds.Query_Feed;
   View : Dash_View.View;

   --  How often the data thread looks at the pushed ring when nothing
   --  wakes it: well inside a frame, and a ring read costs no call.
   Data_Poll_Ms : constant := 4;
   --  The data thread's stack.
   Data_Stack_Bytes : constant := 1_048_576;

   --  Start the data thread. False: it could not be made (the UI thread
   --  then runs Data_Step itself, once per frame).
   procedure Start (Started : out Boolean);
   function Running return Boolean;
   --  Ask the data thread for a step now (the viewport moved).
   procedure Wake;
   --  Ask it to end (the window closed).
   procedure Stop;

   --  Write the dashboard if an edit made it due (the data thread after each
   --  step; the UI thread when there is no data thread).
   procedure Save_Step;

   --  One sample of Item from either thread (serialized under Dash_Sync).
   procedure Observe (Item : CuBit.Metrics.Series; Value : Unsigned_64);

   --  The data thread's steps so far, and its last and slowest step.
   function Steps return Unsigned_64;
   function Last_Step_Us return Unsigned_64;
   function Slowest_Step_Us return Unsigned_64;
end Dash_Runner;
