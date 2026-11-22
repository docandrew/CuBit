--  ccl-console on CuBit: its debug text into its logstore ring, never the
--  kernel console (ABI-002), then the console (shared with the Linux-hosted
--  preview's console_main.adb, which has no logstore).
with CuBit.Console_Route;
with CCL_Console;

procedure CCL_Console_Main is
begin
   CuBit.Console_Route.Route_Named ("logstore");
   CCL_Console.Run;
end CCL_Console_Main;
