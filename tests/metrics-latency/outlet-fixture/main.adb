--  The outlet fixture of the metrics-latency test (MET-002), built twice
--  from this one source: metrics-declared.app, whose manifest declares the
--  com.cubit-os.stdmetric outlet, and metrics-undeclared.app, whose manifest
--  does not. Each publishes the same samples through CuBit.Metrics and
--  reports what it was given. The declared one publishes into metrics.svc's
--  store; the undeclared one has no outlet entry, so every sample is counted
--  lost locally and none reaches the store (tests/metrics-latency/
--  metrics_session.py checks both from the store's side).
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Handles.Startup;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Metric_Outlet;
with CuBit.Metrics;
with CuBit.Startup_State;
with CuBit.Startup_Tables;

procedure Main is
   package ST renames CuBit.Startup_Tables;
   --  The series both variants publish, and how many samples.
   Series_Name : constant String := "fixture.outlet.samples";
   Samples : constant := 32;
   --  Time for Flush to see metrics.svc take the ring.
   Flush_Wait_Ms : constant := 2_000;
   Samples_Series : constant CuBit.Metrics.Series :=
     CuBit.Metrics.Series_Named (Series_Name, CuBit.Metrics.Distribution, CuBit.Metrics.Microseconds);
   Has_Entry : constant Boolean :=
     ST.Find (CuBit.Startup_State.Current, ST.To_Name (CuBit.Metric_Outlet.Startup_Binding)) /= 0;
   Has_Endpoint : constant Boolean :=
     CuBit.Handles.Startup.Optional_Endpoint_Named (CuBit.Metric_Outlet.Startup_Binding).Present;
   Drained : Boolean;
   Ignore : Unsigned_64;
begin
   for Sample in 1 .. Samples loop
      CuBit.Metrics.Observe (Samples_Series, Unsigned_64 (Sample));
   end loop;
   CuBit.Metrics.Flush (Drained, Flush_Wait_Ms);
   debugPrint ("metrics-fixture: entry=" & (if Has_Entry then "present" else "absent") &
               " endpoint=" & (if Has_Endpoint then "present" else "absent") &
               " state=" & CuBit.Metrics.Outlet_State'Image (CuBit.Metrics.State) &
               " drained=" & (if Drained then "yes" else "no") &
               " lost" & Unsigned_64'Image (CuBit.Metrics.Lost) & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT, 0);
end Main;
