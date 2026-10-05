with Servo_Input_Admission;
with Client_Input_Budget;
with Ada.Text_IO;
procedure Admission_Tests is
   package B renames Client_Input_Budget;
   package A renames Servo_Input_Admission;
   use type B.Tick;
   Batch : B.Batch;
   Cached : Natural;
   Seen : Natural := 0;
begin
   -- A slow fetch returns eight events. After its first event consumed the
   -- wall-time slice, all seven cached events must drain before another fetch.
   Batch := B.Open (100);
   pragma Assert (A.Can_Take (Batch, 100, 0, False));
   B.Charge (Batch);
   Cached := 7;
   while A.Can_Take (Batch, 600, Cached, False) loop
      pragma Assert (Cached > 0);
      B.Charge (Batch); Cached := Cached - 1;
   end loop;
   pragma Assert (Cached = 0 and B.Used (Batch) = 8);
   -- Cache occupancy never overrides the stale hit-map barrier.
   for N in 0 .. 32 loop
      pragma Assert (not A.Can_Take (Batch, 100, N, True));
   end loop;
   -- Even a permanently nonempty cache cannot starve the render opportunity.
   Batch := B.Open (100);
   while A.Can_Take (Batch, 600, 8, False) loop
      B.Charge (Batch); Seen := Seen + 1;
   end loop;
   pragma Assert (Seen = 32 and B.Used (Batch) = B.Poll_Limit);
   -- An empty cache must follow the existing fetch deadline, including a
   -- backwards clock, without advancing state on denied admission.
   for Start in B.Tick'(0) .. B.Tick'(2) loop
      Batch := B.Open (Start); B.Charge (Batch);
      for Now in B.Tick'(0) .. B.Tick'(4) loop
         pragma Assert (A.Can_Take (Batch, Now, 0, False) = B.Can_Poll (Batch, Now));
         pragma Assert (B.Used (Batch) = 1);
      end loop;
   end loop;
   Batch := B.Open (B.Tick'Last); B.Charge (Batch);
   pragma Assert (not A.Can_Take (Batch, 0, 0, False));
   pragma Assert (A.Can_Take (Batch, 0, 1, False));
   Ada.Text_IO.Put_Line ("PASS input admission: slow-fetch drain, stale-controls barrier, 32-event ceiling, fetch deadline and clock boundaries");
end Admission_Tests;
