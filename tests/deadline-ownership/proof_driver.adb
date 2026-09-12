package body Proof_Driver with SPARK_Mode is
   procedure Check is
      use Deadlines;
      S : State;
      Old_Ticket, New_Ticket : Ticket;
      Accepted : Boolean;
      R : Outcome;
      Late : Tick;
   begin
      Arm (S, (5, 1), 100, Old_Ticket, Accepted);
      pragma Assert (Accepted);
      Arm (S, (5, 2), 200, New_Ticket, Accepted);
      pragma Assert (Accepted and New_Ticket /= Old_Ticket);
      Cancel (S, (5, 1), Old_Ticket, Accepted);
      pragma Assert (not Accepted and View (S).Active);
      Poll (S, 100, (5, 2), R, Late);
      pragma Assert (R = Nothing_Due and View (S).Active);
      Poll (S, 200, (5, 2), R, Late);
      pragma Assert (R = Expired and Late = 0 and not View (S).Active);
      Poll (S, 201, (5, 2), R, Late);
      pragma Assert (R = Nothing_Due);
   end Check;
end Proof_Driver;
