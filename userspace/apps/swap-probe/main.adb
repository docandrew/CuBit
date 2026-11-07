pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Control_Events;
with CuBit.Log_Records;
with CuBit.Logging;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Process_Events;
with Startup;

--  The device-build test's app (tests/device-builds): it says which build
--  it is (Build, below: the test edits it on the host, builds it on CuBit
--  and replaces the running copy), once on the console and once in the
--  log, then waits until it is asked to stop and exits 0.
procedure Main is
   use type CuBit.Control_Events.Event_Kind;
   use type CuBit.Control_Events.Control_Kind;

   Build : constant String := "1";

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
   Say ("swap-probe: build " & Build & " running");
   loop
      CuBit.Process_Events.Next (Event, Found);
      exit when Found and then Event.Kind = CuBit.Control_Events.Control
        and then Event.Control = CuBit.Control_Events.Stop;
      if not Found then
         Ignore := syscall (SYSCALL_SLEEP, Poll_Ms);
      end if;
   end loop;
   Say ("swap-probe: build " & Build & " stopping");
   Ignore := syscall (SYSCALL_EXIT, Exit_Success);
end Main;
