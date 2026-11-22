--  shutdown-stubborn (tests/shutdown, headless shutdown): ignores Stop, so
--  procmgr ends it after its grace (FORCED).
with CuBit.Console_Route;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Control_Events;
with CuBit.Process_Events;
with Startup;
pragma Unreferenced (Startup);

procedure Main is
   use type CuBit.Control_Events.Event_Kind, CuBit.Control_Events.Control_Kind;
   Poll_Ms : constant := 50;
   Event : CuBit.Control_Events.Event;
   Found : Boolean;
   Activity : Activity_Result;
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   debugPrint ("shutdown-stubborn: running" & ASCII.LF);
   loop
      Activity := Wait_For_Activity_Until (Deadline_After (Poll_Ms));
      exit when Activity = Unavailable;
      loop
         CuBit.Process_Events.Next (Event, Found);
         exit when not Found;
         if Event.Kind = CuBit.Control_Events.Control and then Event.Control = CuBit.Control_Events.Stop then
            debugPrint ("shutdown-stubborn: Stop received; ignoring it" & ASCII.LF);
         end if;
      end loop;
   end loop;
end Main;
