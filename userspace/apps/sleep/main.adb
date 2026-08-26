------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Sleep test app
--
--  Long-lived process that creates a stdout stream and sleeps in a loop.
--  Useful for testing `streams` shell command and process lifecycle.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Protocols;
with CuBit.Streams;

procedure main is
   use ASCII;
   count : Unsigned_32 := 0;
   ignore : Unsigned_64;
   handled : Boolean;
begin
   debugPrint ("sleep: starting" & LF);

   CuBit.Streams.streamCreateTyped
     (CuBit.Streams.STREAM_STDOUT, 4, CuBit.Streams.TYPE_TEXT_LINE,
      CuBit.Protocols.TEXT_LINE_CONTRACT);

   CuBit.Streams.streamPrint (
      CuBit.Streams.STREAM_STDOUT, "sleep: running..." & LF);

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
