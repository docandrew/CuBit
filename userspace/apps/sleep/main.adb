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

      ignore := syscall (SYSCALL_SLEEP, 1000);
   end loop;
end main;
