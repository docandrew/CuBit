------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Sleep test app
--
--  Long-lived process that reports on its status connector and sleeps in a loop.
--  Useful for testing `streams` shell command and process lifecycle.
------------------------------------------------------------------------------
with CuBit.Console_Route;
with CuBit.Waits;
with Interfaces; use Interfaces;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Streams;

procedure main is
   use ASCII;
   count : Unsigned_32 := 0;
   ignore : Unsigned_64;
   handled : Boolean;
   --  The connector this program reports on (its manifest declares it).
   Status : CuBit.Streams.StreamId;
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   debugPrint ("sleep: starting" & LF);

   Status := CuBit.Streams.Open_Outlet ("com.cubit.sleep.status");
   CuBit.Streams.streamPrint (
      Status, "sleep: running..." & LF);

   loop
      --  Handle pending stream IPC (OP_STREAM_LIST, subscribe, etc.)
      handled := CuBit.Streams.streamHandleSubscription;

      count := count + 1;
      if count mod 10 = 0 then
         debugPrint ("sleep: tick" & LF);
      end if;

      ignore := CuBit.Waits.Sleep_For (1000);
   end loop;
end main;
