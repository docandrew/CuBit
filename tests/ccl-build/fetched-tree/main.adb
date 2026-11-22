--  ccl-build's fetched-tree fixture (tests/ccl-build/run.sh): a program
--  using a library whose sources are a fetched, pinned tree.
with CuBit.Console_Route;
with CuBit.Messages; use CuBit.Messages;
with Fetched_Greeting;

procedure main is
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   debugPrint (Fetched_Greeting.Text & ASCII.LF);
end main;
