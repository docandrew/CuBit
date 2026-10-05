with Ada.Text_IO;
with Compositor_Input_Acknowledgment;
procedure Input_Acknowledgment_Tests is
   package A renames Compositor_Input_Acknowledgment;
   package IQ renames A.IQ;
   use type IQ.Word;
   use type IQ.Event;
   use type IQ.Queue;
   Q, Before : IQ.Queue;
   Close : IQ.Word;
begin
   for After in IQ.Word range 0 .. 33 loop
      for Pending_Close in IQ.Word range 0 .. 33 loop
         for I in IQ.Index loop
            Q (I) := (I mod 3 /= 0, IQ.Word ((I * 13) mod 32 + 1), 6, 42, IQ.Word (I), 0);
         end loop;
         Before := Q; Close := Pending_Close;
         A.Apply (Q, Close, After);
         pragma Assert (Close = (if Pending_Close <= After then 0 else Pending_Close));
         for I in IQ.Index loop
            pragma Assert (Q (I) =
              (if Before (I).Valid and Before (I).Serial <= After
               then IQ.Cleared (Before (I)) else Before (I)));
         end loop;
         Before := Q;
         A.Apply (Q, Close, After);
         pragma Assert (Q = Before); -- retry is idempotent
      end loop;
   end loop;
   Q (0) := (True, IQ.Word'Last, 10, 42, 0, 0); Close := IQ.Word'Last;
   A.Apply (Q, Close, IQ.Word'Last - 1);
   pragma Assert (Q (0).Valid and Close = IQ.Word'Last);
   A.Apply (Q, Close, IQ.Word'Last);
   pragma Assert (not Q (0).Valid and Close = 0);
   Ada.Text_IO.Put_Line ("INPUT ACKNOWLEDGMENT: PASS 1156 queue/close combinations plus serial exhaustion");
end Input_Acknowledgment_Tests;
