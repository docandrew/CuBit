--  ccl-workbench on CuBit: its debug text into its logstore ring, never the
--  kernel console (ABI-002), then the workbench (shared with the
--  Linux-hosted preview's main.adb, which has no logstore).
with CuBit.Console_Route;
with CCL_Workbench;

procedure CCL_Workbench_Main is
begin
   CuBit.Console_Route.Route_Named ("logstore");
   CCL_Workbench.Run;
end CCL_Workbench_Main;
