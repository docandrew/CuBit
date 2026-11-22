with CuBit.Console_Route;
with CCL_Agent_App;

procedure Agent_Main is
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   CCL_Agent_App.Run;
end Agent_Main;
