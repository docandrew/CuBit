--  launch-gate (CAP-012): requires clock control. It runs only if a launch
--  gave it control the user session's envelope withholds.
with CuBit.Messages; use CuBit.Messages;
with Startup;
pragma Unreferenced (Startup);

procedure Main is
begin
   debugPrint ("launch-gate-greedy: started with clock control FAIL" & ASCII.LF);
end Main;
