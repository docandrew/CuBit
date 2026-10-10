package body Client_Tooltip_Policy with SPARK_Mode is
   procedure Pointer_At (S : in out Tooltip_State; Over : Target; X, Y : Natural; Now_Ms : Unsigned_64) is
      Moved : constant Boolean := X /= S.X or else Y /= S.Y;
   begin
      if Over = NO_TARGET then
         S := (others => <>);
         return;
      end if;
      if Over = S.Dismissed then
         --  Still on the dismissed target: nothing until another.
         S.Current := Idle;
         S.On := Over;
      elsif S.Current = Shown and then Over /= S.On then
         --  Sliding: the next tip shows at once.
         S.On := Over;
         S.Dismissed := NO_TARGET;
      elsif S.Current = Shown then
         null;
      elsif Over /= S.On or else S.Current = Idle or else Moved then
         --  The pointer must rest: each move restarts the delay.
         S.Current := Pending;
         S.On := Over;
         S.Since := Now_Ms;
         S.Dismissed := NO_TARGET;
      end if;
      S.X := X;
      S.Y := Y;
   end Pointer_At;

   procedure Dismiss (S : in out Tooltip_State) is
   begin
      S.Dismissed := S.On;
      S.Current := Idle;
   end Dismiss;

   procedure Tick (S : in out Tooltip_State; Now_Ms : Unsigned_64; Changed : out Boolean) is
   begin
      Changed := False;
      if S.Current = Pending and then Now_Ms >= S.Since and then Now_Ms - S.Since >= SHOW_DELAY_MS then
         S.Current := Shown;
         Changed := True;
      end if;
   end Tick;
end Client_Tooltip_Policy;
