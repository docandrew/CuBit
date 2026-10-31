--  ccl-build's fetched-tree fixture (tests/ccl-build/run.sh): a program
--  using a library whose sources are a fetched, pinned tree.
with CuBit.Messages; use CuBit.Messages;
with Fetched_Greeting;

procedure main is
begin
   debugPrint (Fetched_Greeting.Text & ASCII.LF);
end main;
