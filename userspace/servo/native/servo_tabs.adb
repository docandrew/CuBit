package body Servo_Tabs with SPARK_Mode is
   function Count (S : State) return Natural is
      N : Natural := 0;
   begin
      for I in Slot loop
         pragma Loop_Invariant (N <= I - 1);
         if S.Items (I) = Live then N := N + 1; end if;
      end loop;
      return N;
   end Count;
   procedure Open_Tab (S : in out State; Added : out Selection) is
   begin
      Added := 0;
      for I in Slot loop
         if S.Items (I) = Available then
            S.Items (I) := Live; S.Active := I; Added := I; return;
         end if;
      end loop;
   end Open_Tab;
   procedure Select_Tab (S : in out State; I : Slot) is
   begin
      if S.Items (I) = Live then S.Active := I; end if;
   end Select_Tab;
   procedure Close_Tab (S : in out State; I : Slot) is
   begin
      if S.Items (I) /= Live then return; end if;
      S.Items (I) := Parking;
      if S.Active = I then
         S.Active := 0;
         for J in Slot loop
            pragma Loop_Invariant (S.Active = 0);
            pragma Loop_Invariant (for all K in Slot'First .. J - 1 => S.Items (K) /= Live);
            if S.Items (J) = Live then S.Active := J; return; end if;
         end loop;
      end if;
   end Close_Tab;
   procedure Parked (S : in out State; I : Slot) is
   begin
      if S.Items (I) = Parking then S.Items (I) := Available; end if;
   end Parked;
   procedure Cycle (S : in out State; Backward : Boolean) is
      I : Selection := S.Active;
   begin
      for N in Slot loop
         if Backward then I := (if I <= 1 then Capacity else I - 1);
         else I := (if I = Capacity then 1 else I + 1); end if;
         if S.Items (I) = Live then S.Active := I; return; end if;
      end loop;
   end Cycle;
end Servo_Tabs;
