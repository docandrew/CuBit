------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Everyday metrics for any program (MET-002): series published through
--  the program's com.cubit-os.stdmetric outlet (CuBit.Metric_Outlet). The
--  manifest declares the outlet, and nothing else is registered or bound:
--
--     outlets => [(Outlet "com.cubit-os.stdmetric" Element.Metric_Records)]
--
--     Frames : constant CuBit.Metrics.Series :=
--       CuBit.Metrics.Series_Named ("myapp.frames", Counter, Count);
--     Render : constant CuBit.Metrics.Series :=
--       CuBit.Metrics.Series_Named ("myapp.render_us", Distribution, Microseconds);
--     ...
--     CuBit.Metrics.Observe (Frames, 1);
--     CuBit.Metrics.Observe (Render, Elapsed_Us);
--
--  @description
--  A sample is a copy into the outlet's ring, stamped with the published
--  monotonic clock (CuBit.Monotonic, no system call): no IPC, no waiting,
--  no completion, no event loop. metrics.svc drains the ring on its own
--  schedule and never asks to be woken, so publishing takes no system call
--  either (CuBit.Channels kicks only a consumer that armed its wait). The
--  ring is opened, with one bounded call to metrics.svc, by the first
--  sample: a program that publishes nothing pays nothing.
--
--  No sample is lost silently. A full ring sheds the sample and counts it
--  against its series; the count and the shed samples' time range go out
--  as one Loss element ahead of the series' next sample (or at Flush), and
--  metrics.svc stores it as a typed gap. A program without the outlet (its
--  manifest does not declare it, metrics.svc was not running at launch, or
--  metrics.svc ended) counts every sample as lost (Lost) and keeps going.
--
--  One task per program: calls are serialized by the program's own loop,
--  as for CuBit.Log. The series table grows on the process heap (CuAlloc)
--  up to CuBit.Metric_Outlet.Maximum_Series_Per_Source; a declaration past
--  it, past memory, or with a bad name gives No_Series (Refused counts).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Metric_Outlet;

package CuBit.Metrics is
   package Outlet renames CuBit.Metric_Outlet;
   subtype Series_Kind is Outlet.Series_Kind;
   subtype Unit is Outlet.Unit;
   Counter : constant Series_Kind := Outlet.Counter;
   Gauge : constant Series_Kind := Outlet.Gauge;
   Distribution : constant Series_Kind := Outlet.Distribution;
   Count : constant Unit := Outlet.Count;
   Microseconds : constant Unit := Outlet.Microseconds;
   Nanoseconds : constant Unit := Outlet.Nanoseconds;
   Bytes : constant Unit := Outlet.Bytes;
   Pixels : constant Unit := Outlet.Pixels;

   type Series is private;
   No_Series : constant Series;
   function Valid (Item : Series) return Boolean;

   --  The series Name (CuBit.Metric_Outlet.Valid_Name) of Kind in Measure.
   --  Declaring the same name again gives the same series when its kind and
   --  unit agree, No_Series (refused) when they do not. Nothing is sent
   --  until its first sample.
   function Series_Named (Name : String; Kind : Series_Kind; Measure : Unit) return Series;

   --  One sample now: a Counter's increment, a Gauge's value, a
   --  Distribution's observation. No_Series is ignored.
   procedure Observe (Item : Series; Value : Unsigned_64);
   --  The same at a time the caller stamped (CuBit.Monotonic.Microseconds).
   procedure Observe_At (Item : Series; Value : Unsigned_64; Time_Us : Unsigned_64);

   --  Count samples of Item the program itself could not take (its own
   --  queue was full), the first at First_Us and the last at Last_Us: they
   --  are reported with the ring's own sheds, as a gap of the series.
   procedure Note_Lost (Item : Series; Count, First_Us, Last_Us : Unsigned_64)
   with Pre => First_Us <= Last_Us;

   --  Samples of Item not delivered to the ring so far (shed, or no
   --  outlet). Those shed while wired are reported to metrics.svc as gaps.
   function Lost (Item : Series) return Unsigned_64;
   --  The same for the whole program.
   function Lost return Unsigned_64;
   --  Declarations given No_Series.
   function Refused return Unsigned_64;

   type Outlet_State is
     (Not_Opened,   --  no sample yet: nothing opened
      Open,         --  publishing into the ring
      Unwired);     --  no metrics.svc to publish to (counted as lost)
   function State return Outlet_State;

   --  Put every pending loss report now (a loss otherwise goes out ahead
   --  of its series' next sample): no waiting, no system call.
   procedure Report_Losses;

   --  For exit paths and checks: put every pending loss report, then ask
   --  metrics.svc to drain and wait, at most Wait_Ms, until it has taken
   --  everything. Drained False on timeout or without an outlet.
   Default_Flush_Wait_Ms : constant := 200;
   procedure Flush (Drained : out Boolean; Wait_Ms : Natural := Default_Flush_Wait_Ms);
private
   type Series is record
      Key : Outlet.Series_Count := 0;
   end record;
   No_Series : constant Series := (Key => 0);
   function Valid (Item : Series) return Boolean is (Item.Key /= 0);
end CuBit.Metrics;
