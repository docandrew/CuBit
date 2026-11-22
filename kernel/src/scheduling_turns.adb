package body Scheduling_Turns with SPARK_Mode is
   procedure Count (Totals : in out Counters; Event : Event_Kind) is
   begin
      if Totals (Event) < Unsigned_64'Last then
         Totals (Event) := Totals (Event) + 1;
      end if;
   end Count;
   function Remaining (S : State) return Unsigned_64 is (S.Credit);
   function Fresh (Ticks : Unsigned_64) return State is ((Credit => Ticks));
   procedure Charge (S : in out State; Ticks : Unsigned_64) is
   begin
      if Ticks >= S.Credit then S.Credit := 0;
      else S.Credit := S.Credit - Ticks;
      end if;
   end Charge;
   procedure Move (Source : in out State; Target : out State) is
   begin
      Target := Source;
      Source := Empty;
   end Move;
   procedure Prove_Split (Initial, First, Second : Unsigned_64) is
      Whole : State := Fresh (Initial);
      Split : State := Whole;
      Saved : State;
   begin
      Charge (Whole, First + Second);
      Charge (Split, First);
      Move (Split, Saved);
      pragma Assert (Remaining (Split) = 0);
      Move (Saved, Split);
      pragma Assert (Remaining (Saved) = 0);
      Charge (Split, Second);
      pragma Assert (Remaining (Whole) = Remaining (Split));
   end Prove_Split;
end Scheduling_Turns;
