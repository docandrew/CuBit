with Ada.Text_IO;
with Compositor_Input_Batches;
procedure Input_Batches_Tests is
   package B renames Compositor_Input_Batches;
   package Q renames B.IQ;
   use type Q.Word;
   use type Q.Event;
   use type Q.Queue;
   use type B.Batch;
   Queue : Q.Queue;
   Close : Q.Event;
   Cases : Natural := 0;
   -- Independent oracle: collect candidates, then insertion-sort by serial.
   procedure Check (After : Q.Word; Maximum : B.Limit) is
      Original : constant Q.Queue := Queue;
      Result : constant B.Batch := B.Snapshot (Queue, Close, After, Maximum);
      Candidates : array (1 .. Q.Capacity + 1) of Q.Event;
      N : Natural := 0;
      procedure Add (E : Q.Event) is
         J : Natural;
      begin
         if not E.Valid or else E.Serial <= After then return; end if;
         N := N + 1; J := N;
         while J > 1 and then Candidates (J - 1).Serial > E.Serial loop
            Candidates (J) := Candidates (J - 1); J := J - 1;
         end loop;
         Candidates (J) := E;
      end Add;
   begin
      for E of Queue loop Add (E); end loop;
      Add (Close);
      pragma Assert (Queue = Original);
      pragma Assert (Result.Length = Natural'Min (N, Maximum));
      pragma Assert (Result.More = (N > Maximum));
      pragma Assert (Result.Through =
        (if N = 0 then After else Candidates (Result.Length).Serial));
      for I in B.Limit loop
         pragma Assert (Result.Items (I) =
           (if I <= Result.Length then Candidates (I)
            else Q.Event'(others => <>)));
      end loop;
      -- Retry before acknowledgment reproduces exactly the same snapshot.
      pragma Assert (Result = B.Snapshot (Queue, Close, After, Maximum));
      Cases := Cases + 1;
   end Check;
begin
   for Occupancy in 0 .. Q.Capacity loop
      for Rotation in 0 .. 2 loop
         Queue := [others => (others => <>)];
         for I in 1 .. Occupancy loop
            Queue ((I * 13 + Rotation * 7) mod Q.Capacity) :=
              (True, Q.Word (I * 2), Q.Word (I mod 9 + 1),
               42, Q.Word (I * 101), Q.Word'Last - Q.Word (I));
         end loop;
         for Close_Position in 0 .. 3 loop
            Close := (Close_Position /= 0, Q.Word (Close_Position * 22 + 1),
                      10, 42, 0, 0);
            for A in 0 .. 34 loop
               for Maximum in B.Limit loop Check (Q.Word (A * 2), Maximum); end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   Queue := [others => (others => <>)];
   Queue (31) := (True, Q.Word'Last - 1, 9, 42, 12, 34);
   Close := (True, Q.Word'Last, 10, 42, 0, 0);
   for Maximum in B.Limit loop
      Check (Q.Word'Last - 2, Maximum);
      Check (Q.Word'Last - 1, Maximum);
      Check (Q.Word'Last, Maximum);
   end loop;
   Ada.Text_IO.Put_Line ("INPUT BATCHES: PASS" & Cases'Image &
     " ordered payload, close, continuation, retry and exhaustion cases");
end Input_Batches_Tests;
