with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Logging;
with CuBit.Log_Records;
with Intel_GPU_Diagnostic_Capture;
with Intel_GPU_Diagnostics;
procedure Diagnostics_Tests is
   package D renames Intel_GPU_Diagnostics;
   R : aliased CompletionEntry;
   Reply : CompletionEntry;
   package Logs renames CuBit.Log_Records;
   use type Logs.Severity;
   package Queue_Test is new Intel_GPU_Diagnostic_Capture (4, 32);
   Q : Queue_Test.Queue;
   Value, Loan : Queue_Test.Captured_Record;
   Found : Boolean;
begin
   Queue_Test.Append (Q, "held", Logs.Error);
   Queue_Test.Take (Q, Loan, Found);
   pragma Assert (Found and Loan.Level = Logs.Error);
   for I in 1 .. 1000 loop
      Queue_Test.Append (Q, Natural'Image (I), Logs.Severity'Val (I mod 6));
   end loop;
   pragma Assert (Queue_Test.Lost (Q) = 996 and Loan.Level = Logs.Error);
   for I in 997 .. 1000 loop
      Queue_Test.Take (Q, Value, Found);
      pragma Assert (Found and Value.Level = Logs.Severity'Val (I mod 6));
      pragma Assert (Value.Text (1 .. Value.Length) = Natural'Image (I));
   end loop;
   D.Capture ("first", Logs.Debug); D.Capture ("second", Logs.Warning);
   pragma Assert (Submissions = 0 and CuBit.Logging.Emissions = 0);
   D.Tick;
   pragma Assert (Submissions = 1);
   Reply.token := 77; Reply.msg.words := [1, 2, 3, 4]; Inject (Reply);
   pragma Assert (D.Poll_Driver (R'Address) = 1 and R = Reply);
   Reply := (16#49470001#, COMPLETION_OK, ((16#F000#, 0, 0, 0), [0, 0, 0, 0]));
   Inject (Reply);
   Reply.token := 88; Inject (Reply);
   pragma Assert (D.Poll_Driver (R'Address) = 1 and R = Reply);
   --  Ready: a tick publishes what is buffered, a copy each into the ring,
   --  then the capture summary (no loss: Debug).
   Now := 100; D.Tick;
   pragma Assert (CuBit.Logging.Emissions = 3);
   pragma Assert (CuBit.Logging.Last_Value.Level = Logs.Debug);
   --  A flood: at most a batch (64) per tick, then the overflow summary.
   for I in 1 .. 1000 loop D.Capture ("later"); end loop;
   Now := 200; D.Tick;
   pragma Assert (CuBit.Logging.Emissions = 3 + 64);
   for Ticks in 1 .. 20 loop
      Now := Now + 100; D.Tick;
   end loop;
   pragma Assert (CuBit.Logging.Emissions = 3 + 512 + 1);
   pragma Assert (CuBit.Logging.Last_Value.Level = Logs.Warning);
   pragma Assert (CuBit.Logging.Last_Value.Text (1 .. 37) = "intel-gpu: diagnostic capture overflo");
   --  Publications never complete on the queue: a reply with the old publish
   --  token is just another completion, returned to the caller.
   Reply := (16#49470002#, COMPLETION_OK, ((16#F000#, 4, 0, 0), [0, 0, 0, 0]));
   Inject (Reply);
   pragma Assert (D.Poll_Driver (R'Address) = 1 and R = Reply);
   Ada.Text_IO.Put_Line ("diagnostics PASS: nonblocking capture, interleaved routing, batched ring publication, overflow summary (fixture)");
end Diagnostics_Tests;
