with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Logging;
with Intel_GPU_Diagnostics;
procedure Diagnostics_Tests is
   package D renames Intel_GPU_Diagnostics;
   R : aliased CompletionEntry;
   Reply : CompletionEntry;
begin
   D.Capture ("first"); D.Capture ("second");
   pragma Assert (Submissions = 0 and CuBit.Logging.Emissions = 0);
   D.Tick;
   pragma Assert (Submissions = 1);
   Reply.token := 77; Reply.msg.words := [1, 2, 3, 4]; Inject (Reply);
   pragma Assert (D.Poll_Driver (R'Address) = 1 and R = Reply);
   Reply := (16#49470001#, COMPLETION_OK, ((16#F000#, 0, 0, 0), [0, 0, 0, 0]));
   Inject (Reply);
   Reply.token := 88; Inject (Reply);
   pragma Assert (D.Poll_Driver (R'Address) = 1 and R = Reply);
   Now := 100; D.Tick;
   pragma Assert (CuBit.Logging.Emissions = 1);
   for I in 1 .. 1000 loop D.Capture ("later"); end loop;
   Now := 1000; D.Tick;
   pragma Assert (CuBit.Logging.Emissions = 1);
   pragma Assert (CuBit.Logging.Last_Value.Text (1 .. 5) = "first");
   Reply := (16#49470002#, COMPLETION_OK, ((16#F000#, 4, 0, 0), [0, 0, 0, 0]));
   Inject (Reply);
   pragma Assert (D.Poll_Driver (R'Address) = 0);
   D.Tick;
   pragma Assert (CuBit.Logging.Emissions = 2);
   -- Malformed completion must permanently stop reuse, even if the fixture
   -- publisher reports no pending loan. The real publisher is stricter.
   Reply.msg.tag.flags := 1; Inject (Reply);
   pragma Assert (D.Poll_Driver (R'Address) = 0);
   Now := 10000; D.Tick;
   pragma Assert (CuBit.Logging.Emissions = 2);
   Reply.token := 99; Inject (Reply);
   pragma Assert (D.Poll_Driver (R'Address) = 1 and R = Reply);
   Reply.token := 16#49470002#;
   for I in 1 .. 9 loop Inject (Reply); end loop;
   Reply.token := 100; Inject (Reply);
   pragma Assert (D.Poll_Driver (R'Address) = 0);
   pragma Assert (D.Poll_Driver (R'Address) = 1 and R = Reply);
   Ada.Text_IO.Put_Line ("diagnostics PASS: nonblocking capture, interleaved routing, immutable pending copy, malformed reply stops reuse (fixture)");
end Diagnostics_Tests;
