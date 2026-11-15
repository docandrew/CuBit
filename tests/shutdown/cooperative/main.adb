--  shutdown-cooperative (tests/shutdown, headless shutdown): obeys Stop
--  within its quiesce time and exits 0 (STOPPED_CLEAN).
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Control_Events;
with CuBit.Process_Events;
with Parameters;
with Startup;
pragma Unreferenced (Startup);

procedure Main is
   use type CuBit.Control_Events.Event_Kind, CuBit.Control_Events.Control_Kind;
   P : constant Parameters.Cooperative_Parameters_Value := Parameters.Value;
   Poll_Ms : constant := 50;
   Event : CuBit.Control_Events.Event;
   Found : Boolean;
   Activity : Activity_Result;
   Ignore : Unsigned_64;
begin
   debugPrint ("shutdown-cooperative: running" & ASCII.LF);
   loop
      Activity := Wait_For_Activity_Until (Deadline_After (Poll_Ms));
      exit when Activity = Unavailable;
      loop
         CuBit.Process_Events.Next (Event, Found);
         exit when not Found;
         if Event.Kind = CuBit.Control_Events.Control and then Event.Control = CuBit.Control_Events.Stop then
            debugPrint ("shutdown-cooperative: Stop received; quiescing" & ASCII.LF);
            Ignore := syscall (SYSCALL_SLEEP, Unsigned_64 (P.quiesce_ms));
            debugPrint ("shutdown-cooperative: quiesced; exiting 0" & ASCII.LF);
            return;
         end if;
      end loop;
   end loop;
end Main;
