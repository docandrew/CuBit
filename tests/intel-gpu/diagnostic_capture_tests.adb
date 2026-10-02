with Ada.Text_IO;
with Interfaces;
with Intel_GPU_Diagnostic_Capture;
procedure Diagnostic_Capture_Tests is
   use type Interfaces.Unsigned_64;
   package Capture is new Intel_GPU_Diagnostic_Capture (4, 32);
   use Capture;
   Q : Queue;
   E, In_Flight : Captured_Record;
   Found : Boolean;
begin
   Take (Q, E, Found);
   pragma Assert (not Found and E.Length = 0);
   Append (Q, "in-flight private copy");
   Take (Q, In_Flight, Found);
   pragma Assert (Found);
   -- Repeated overflow/wrap must not mutate the copy held for publication.
   for I in 1 .. 1000 loop
      Append (Q, Natural'Image (I));
   end loop;
   pragma Assert (Count (Q) = 4 and Lost (Q) = 996);
   pragma Assert (In_Flight.Text (1 .. In_Flight.Length) = "in-flight private copy");
   for I in 997 .. 1000 loop
      Take (Q, E, Found);
      pragma Assert (Found and E.Text (1 .. E.Length) = Natural'Image (I));
   end loop;
   pragma Assert (Count (Q) = 0);
   E := Latest (Q);
   pragma Assert (E.Text (1 .. E.Length) = " 1000");
   for I in 1 .. 100 loop
      Append (Q, "final status");
      Take (Q, E, Found);
      pragma Assert (Found and E.Text (1 .. E.Length) = "final status");
   end loop;
   pragma Assert (Lost (Q) = 996);
   Ada.Text_IO.Put_Line ("diagnostic capture PASS: FIFO, wrap, overflow, retained latest, private-copy lifetime");
end Diagnostic_Capture_Tests;
