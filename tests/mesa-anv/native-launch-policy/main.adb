with CuBit.Console_Route;
with CuBit.Messages; use CuBit.Messages;

procedure Main is
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   -- Neither launch in this fixture has an admitted rendering session.
   -- Reaching even the first application instruction is a policy failure.
   debugPrint ("TEST: FAIL render-launch-policy child resumed" & ASCII.LF);
end Main;
