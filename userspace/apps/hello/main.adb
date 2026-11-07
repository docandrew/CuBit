pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Control_Events;
with CuBit.Log_Records;
with CuBit.Logging;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Process_Events;
with Startup;

--  The device-build test's new app (tests/device-builds): its sources and
--  its manifest are built on CuBit, every step there. It says hello once
--  in the log and on the console, waits until it is asked to stop, says
--  so and exits 0.
procedure Main is
   use type CuBit.Control_Events.Event_Kind;
   use type CuBit.Control_Events.Control_Kind;

   Writer : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);
   Poll_Ms : constant := 100;
   Flush_Wait_Ms : constant := 2_000;
   Exit_Success : constant := 0;

   procedure Say (Text : String) is
      Made : constant CuBit.Log_Records.Decoded := CuBit.Log_Records.Make (Text);
      Accepted, Drained : Boolean := False;
   begin
      debugPrint (Text & ASCII.LF);
      if Made.Success then
         CuBit.Logging.Emit (Writer, Made.Value, Accepted);
         CuBit.Logging.Flush (Writer, Drained, Wait_Ms => Flush_Wait_Ms);
      end if;
   end Say;

   Event : CuBit.Control_Events.Event;
   Found : Boolean;
   Ignore : Unsigned_64;
begin
   Say ("hello: hello from an app built on CuBit");
   loop
      CuBit.Process_Events.Next (Event, Found);
      exit when Found and then Event.Kind = CuBit.Control_Events.Control
        and then Event.Control = CuBit.Control_Events.Stop;
      if not Found then
         Ignore := syscall (SYSCALL_SLEEP, Poll_Ms);
      end if;
   end loop;
   Say ("hello: stopping");
   Ignore := syscall (SYSCALL_EXIT, Exit_Success);
end Main;
