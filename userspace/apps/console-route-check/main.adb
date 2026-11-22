------------------------------------------------------------------------------
--  console-route-check (ABI-002): debugPrint without the kernel console.
--
--  Routes its debug text into its logstore ring (CuBit.Console_Route) and
--  prints, through debugPrint only: a marker, a line written in pieces, a
--  line with control characters, a line longer than a record (it
--  continues on a second record), and a CuBit.Log record with its echo
--  (one record, marked as console text, not a second debugPrint copy).
--  tests/headless/run.sh (console-route) finds them on the serial console,
--  each once, under this program's name: only logstore's mirror puts them
--  there. The last marker says whether anything was lost.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Console_Route;
with CuBit.Log;
with CuBit.Logging;
with CuBit.Log_Records;
with CuBit.Monotonic;
with CuBit.Waits;
with Startup;

procedure Main is
   Long : constant String (1 .. 600) := [others => 'L'];
   --  How long logstore takes to take (and, in test builds, mirror) one
   --  line after debugPrint: Flush returns once it has. Rounds of one line
   --  each, 10 ms apart so logstore is asleep each time (the usual case).
   Rounds : constant := 21;
   type Samples is array (1 .. Rounds) of Unsigned_64;
   Took : Samples;
   procedure Sort (S : in out Samples) is
      T : Unsigned_64;
   begin
      for I in S'Range loop
         for J in I + 1 .. S'Last loop
            if S (J) < S (I) then T := S (I); S (I) := S (J); S (J) := T; end if;
         end loop;
      end loop;
   end Sort;
begin
   CuBit.Console_Route.Route_To (Startup.Logstore);
   debugPrint ("CONSOLE-ROUTE: routed" & ASCII.LF);
   debugPrint ("CONSOLE-ROUTE: in ");
   debugPrint ("three ");
   debugPrint ("pieces" & ASCII.LF);
   debugPrint ("CONSOLE-ROUTE: bell" & ASCII.BEL & "tab" & ASCII.HT & "end" & ASCII.CR & ASCII.LF);
   debugPrint ("CONSOLE-ROUTE: long " & Long & ASCII.LF);
   CuBit.Log.Info ("CONSOLE-ROUTE: logged once");
   --  An announcement (CuBit.Logging) once routed: it must not take the
   --  program's channel from the route (logstore keeps one channel per
   --  publisher; a second open replaces the first, and every line after
   --  it was lost: devices, files, calculator, logs, ABI-002).
   declare
      Announced : Boolean;
   begin
      CuBit.Logging.Announce ("CONSOLE-ROUTE: announced", Announced, Startup.Logstore);
      debugPrint ("CONSOLE-ROUTE: after the announcement, announced=" &
                  (if Announced then "true" else "false") & ASCII.LF);
   end;
   --  The same for a publisher of its own (files' timing records).
   declare
      Own : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);
      Made : constant CuBit.Log_Records.Decoded :=
        CuBit.Log_Records.Make ("CONSOLE-ROUTE: its own publisher", CuBit.Log_Records.Information);
      Accepted : Boolean := False;
   begin
      if Made.Success then
         CuBit.Logging.Emit (Own, Made.Value, Accepted);
      end if;
      debugPrint ("CONSOLE-ROUTE: after its own publisher, accepted=" &
                  (if Accepted then "true" else "false") & ASCII.LF);
   end;
   for R in Took'Range loop
      declare
         Start : constant Unsigned_64 := CuBit.Monotonic.Microseconds;
         Ignore : Unsigned_64;
      begin
         debugPrint ("CONSOLE-ROUTE: round" & R'Image & ASCII.LF);
         CuBit.Console_Route.Flush;
         Took (R) := CuBit.Monotonic.Microseconds - Start;
         Ignore := CuBit.Waits.Sleep_For (10);
      end;
   end loop;
   Sort (Took);
   debugPrint ("CONSOLE-ROUTE: taken after us min=" & Took (1)'Image & " median=" &
               Took ((Rounds + 1) / 2)'Image & " max=" & Took (Rounds)'Image & ASCII.LF);
   CuBit.Console_Route.Flush;
   debugPrint ("CONSOLE-ROUTE: lost=" &
               Unsigned_64'Image (CuBit.Console_Route.Lost + CuBit.Log.Lost) & ASCII.LF);
   CuBit.Console_Route.Flush;
end Main;
