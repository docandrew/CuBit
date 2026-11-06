pragma Ada_2022;
package body Agent_Loop with SPARK_Mode is

   --  A provider's count added, saturating at the quantity's ceiling.
   function Plus (A : Token_Count; B : Unsigned_64) return Token_Count is
     (if B >= Maximum_Tokens then Maximum_Tokens else Quantity'Min (Maximum_Tokens, A + Quantity (B)));

   function Plus_Cost (A : Nano_Usd; B : Unsigned_64) return Nano_Usd is
     (if B >= Maximum_Cost then Maximum_Cost else Quantity'Min (Maximum_Cost, A + Quantity (B)));

   procedure Finish (S : in out State; Why : Stop)
   with Post => S.Current = Finished and then S.Used = S.Used'Old and then S.Limits = S.Limits'Old
                and then (if S.Current'Old /= Finished then S.Ended_As = Why)
   is
   begin
      if S.Current /= Finished then
         S.Current := Finished;
         S.Ended_As := Why;
      end if;
      S.Pending := 0;
   end Finish;

   procedure Clock (S : in out State; Now_Ms : Unsigned_64)
   with Post => S.Used.Tool_Calls = S.Used'Old.Tool_Calls and then S.Limits = S.Limits'Old
                and then S.Current = S.Current'Old and then S.Pending = S.Pending'Old
   is
   begin
      S.Used.Wall_Ms :=
        (if Now_Ms <= S.Started_Ms then 0
         elsif Now_Ms - S.Started_Ms >= Maximum_Wall_Ms then Maximum_Wall_Ms
         else Quantity (Now_Ms - S.Started_Ms));
   end Clock;

   --  The budget that has run out, if one has (tool calls are checked
   --  against each turn's calls instead).
   procedure Exhausted (S : State; Why : out Stop; Out_Of_Budget : out Boolean) is
   begin
      Why := Completed;
      if Tokens_Of (S.Used) >= S.Limits.Tokens then
         Why := Token_Budget;
      elsif S.Used.Cost >= S.Limits.Cost then
         Why := Cost_Budget;
      elsif S.Used.Wall_Ms >= S.Limits.Wall_Ms then
         Why := Time_Budget;
      end if;
      Out_Of_Budget := Why /= Completed;
   end Exhausted;

   function Start (B : Budget; Now_Ms : Unsigned_64) return State is
     ((Current => Ready, Ended_As => Completed, Limits => B, Used => (others => <>),
       Started_Ms => Now_Ms, Pending => 0));

   procedure Begin_Request
     (S : in out State; Now_Ms : Unsigned_64; Limits : out Request_Limits; Allowed : out Boolean)
   is
      Remaining : Quantity;
      Spent_Tokens : Quantity;
   begin
      Limits := (others => <>);
      Allowed := False;
      Clock (S, Now_Ms);
      Spent_Tokens := Tokens_Of (S.Used);
      if Spent_Tokens >= S.Limits.Tokens then
         Finish (S, Token_Budget);
         return;
      elsif S.Used.Cost >= S.Limits.Cost then
         Finish (S, Cost_Budget);
         return;
      elsif S.Used.Wall_Ms >= S.Limits.Wall_Ms then
         Finish (S, Time_Budget);
         return;
      end if;
      pragma Assert (Spent_Tokens < S.Limits.Tokens);
      Remaining := S.Limits.Tokens - Spent_Tokens;
      pragma Assert (Remaining >= 1 and then Spent_Tokens + Remaining = S.Limits.Tokens);
      Limits.Max_Tokens := Quantity'Min (S.Limits.Request_Max_Tokens, Remaining);
      pragma Assert (Limits.Max_Tokens <= Remaining);
      pragma Assert (Tokens_Of (S.Used) + Limits.Max_Tokens <= S.Limits.Tokens);
      Limits.Ceiling := S.Limits.Cost - S.Used.Cost;
      Limits.Deadline_Ms :=
        Quantity'Min (S.Limits.Request_Deadline_Ms, S.Limits.Wall_Ms - S.Used.Wall_Ms);
      S.Current := Asking;
      Allowed := True;
   end Begin_Request;

   procedure Model_Ended
     (S : in out State; Now_Ms : Unsigned_64; Outcome : Model_Outcome; Usage : Exchange_Usage;
      Tool_Calls : Natural)
   is
      Why : Stop := Completed;
      Out_Of_Budget : Boolean;
   begin
      Clock (S, Now_Ms);
      S.Used.Input_Tokens := Plus (S.Used.Input_Tokens, Usage.Input_Tokens);
      S.Used.Output_Tokens := Plus (S.Used.Output_Tokens, Usage.Output_Tokens);
      S.Used.Cache_Read_Tokens := Plus (S.Used.Cache_Read_Tokens, Usage.Cache_Read_Tokens);
      S.Used.Cache_Write_Tokens := Plus (S.Used.Cache_Write_Tokens, Usage.Cache_Write_Tokens);
      S.Used.Cost := Plus_Cost (S.Used.Cost, Usage.Cost);
      if S.Used.Turns < Natural'Last then
         S.Used.Turns := S.Used.Turns + 1;
      end if;
      Exhausted (S, Why, Out_Of_Budget);
      case Outcome is
         when End_Turn =>
            Finish (S, Completed);
         when Cancelled_Stop =>
            Finish (S, Cancelled);
         when Cost_Stop =>
            Finish (S, Cost_Budget);
         when Deadline_Stop =>
            Finish (S, (if Out_Of_Budget and then Why = Time_Budget then Time_Budget else Model_Failed));
         when Max_Tokens =>
            Finish (S, (if Out_Of_Budget and then Why = Token_Budget then Token_Budget else Model_Max_Tokens));
         when Refusal =>
            Finish (S, Model_Refused);
         when Failed =>
            Finish (S, Model_Failed);
         when Pause_Turn =>
            if Out_Of_Budget then
               Finish (S, Why);
            else
               S.Current := Ready;
            end if;
         when Tool_Use =>
            if Tool_Calls = 0 then
               Finish (S, Model_Failed);
            elsif Tool_Calls > S.Limits.Tool_Calls - S.Used.Tool_Calls then
               Finish (S, Tool_Call_Budget);
            elsif Out_Of_Budget then
               --  Their results could not be sent: no request remains.
               Finish (S, Why);
            else
               S.Current := Running_Tools;
               S.Pending := Tool_Calls;
            end if;
      end case;
   end Model_Ended;

   procedure Tool_Done (S : in out State; Refused : Boolean) is
   begin
      S.Used.Tool_Calls := S.Used.Tool_Calls + 1;
      if Refused and then S.Used.Refused_Calls < Maximum_Tool_Calls then
         S.Used.Refused_Calls := S.Used.Refused_Calls + 1;
      end if;
      S.Pending := S.Pending - 1;
      if S.Pending = 0 then
         S.Current := Ready;
      end if;
   end Tool_Done;

   procedure Cancel (S : in out State) is
   begin
      Finish (S, Cancelled);
   end Cancel;

   procedure Context_Full (S : in out State) is
   begin
      Finish (S, Context_Full);
   end Context_Full;

   procedure Tick (S : in out State; Now_Ms : Unsigned_64) is
   begin
      if S.Current in Ready | Running_Tools then
         Clock (S, Now_Ms);
         if S.Used.Wall_Ms >= S.Limits.Wall_Ms then
            Finish (S, Time_Budget);
         end if;
      end if;
   end Tick;
   procedure Retry_Request (S : in out State) is
   begin
      S.Current := Ready;
   end Retry_Request;
end Agent_Loop;
