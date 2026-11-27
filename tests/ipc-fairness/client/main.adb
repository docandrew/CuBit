--  fairness-client (tests/ipc-fairness, headless bench-fairness-*): one
--  client process of the endpoint fairness benchmark (IPC revision 2, S0).
--  It starts its threads, each calling bench-ipc-server's endpoint in a loop
--  until the window ends, then prints one summary line:
--
--    FAIRNESS: client label=L threads=M calls=C p50_us=.. p99_us=..
--      max_us=.. worst_p99_us=.. starved=.. ticks_per_ms=..
--
--  With one thread the quantiles are that thread's. With more, p50/p99 are
--  the medians over its threads and max/worst_p99 the largest; starved
--  counts threads that made no call in the window. Quantiles
--  are histogram bucket upper bounds (CuBit.Timing_Histograms).
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Benchmark_Clock;
with CuBit.Console_Route;
with CuBit.Messages;
with CuBit.Monotonic;
with CuBit.Timing_Histograms;
with Fair_Threads; use Fair_Threads;
with Parameters;

procedure Main is
   package Timing renames CuBit.Timing_Histograms;
   P : constant Parameters.Fair_Parameters_Value := Parameters.Value;
   Label : constant Unsigned_64 := Unsigned_64 (P.label);
   Count : constant Thread_Index := Thread_Index (P.threads);
   Rate : Unsigned_64;
   Started : Natural := 0;
   Total : Unsigned_64 := 0;
   Any_Failed : Boolean := False;
   Starved : Unsigned_64 := 0;

   type Values is array (Thread_Index range <>) of Unsigned_64;
   P50s, P99s : Values (1 .. Count);
   Max, Worst : Unsigned_64 := 0;

   function Image (V : Unsigned_64) return String is
      S : constant String := Unsigned_64'Image (V);
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   function Micros (Ticks : Unsigned_64) return String is
     (if Rate >= 1_000 then Image (Ticks / (Rate / 1_000)) else "?");

   function Median (V : in out Values) return Unsigned_64 is
      Swap : Unsigned_64;
   begin
      for I in V'First + 1 .. V'Last loop
         for J in reverse V'First + 1 .. I loop
            exit when V (J - 1) <= V (J);
            Swap := V (J); V (J) := V (J - 1); V (J - 1) := Swap;
         end loop;
      end loop;
      return V ((V'First + V'Last) / 2);
   end Median;

   procedure Say (Text : String) is
   begin
      CuBit.Messages.debugPrint ("FAIRNESS: " & Text & ASCII.LF);
   end Say;
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   CuBit.Benchmark_Clock.Calibrate (Rate);
   Window_Start := Unsigned_64 (P.start);
   Window_End := Unsigned_64 (P.start) + Unsigned_64 (P.window);
   if CuBit.Monotonic.Milliseconds >= Window_Start then
      Say ("FAIL label=" & Image (Label) & " started after its window opened");
      return;
   end if;
   for I in 1 .. Count loop
      exit when not Start (I);
      Started := Started + 1;
   end loop;
   for I in 1 .. Started loop
      Join (I);
   end loop;
   if Started < Count then
      Say ("FAIL label=" & Image (Label) & " started" & Natural'Image (Started) &
           " of" & Thread_Index'Image (Count) & " threads");
      return;
   end if;
   for I in 1 .. Count loop
      Total := Total + Calls (I);
      Any_Failed := Any_Failed or else Failed (I);
      if Calls (I) = 0 then
         Starved := Starved + 1;
      end if;
      P50s (I) := Timing.Quantile_Upper (Samples (I), 50);
      P99s (I) := Timing.Quantile_Upper (Samples (I), 99);
      Worst := Unsigned_64'Max (Worst, P99s (I));
      Max := Unsigned_64'Max (Max, Timing.Maximum (Samples (I)));
   end loop;
   if Any_Failed then
      Say ("FAIL label=" & Image (Label) & " a wrong reply");
      return;
   end if;
   Say ("client label=" & Image (Label) & " threads=" & Image (Unsigned_64 (Count)) &
        " calls=" & Image (Total) &
        " p50_us=" & Micros (Median (P50s)) & " p99_us=" & Micros (Median (P99s)) &
        " max_us=" & Micros (Max) & " worst_p99_us=" & Micros (Worst) &
        " starved=" & Image (Starved) & " ticks_per_ms=" & Image (Rate));
end Main;
