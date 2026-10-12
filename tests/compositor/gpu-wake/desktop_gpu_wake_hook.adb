package body Desktop_GPU_Wake_Hook is
   use type Interfaces.Unsigned_64;
   function Arm (Token : Interfaces.Unsigned_64) return Arm_Result is
   begin
      Arms := Arms + 1;
      if Token <= Last_Token then
         Tokens_Increasing := False;
      end if;
      Last_Token := Token;
      if Next_Answer = Armed then
         Armed_Token := Token;
      end if;
      return Next_Answer;
   end Arm;
   function Collect (Receipt : CuBit.Messages.CompletionEntry) return Boolean is
      Mine : constant Boolean := Armed_Token /= 0 and then Receipt.token = Armed_Token;
   begin
      if Mine then
         Armed_Token := 0;
      end if;
      return Mine;
   end Collect;
   procedure Blocking_Report (Text : out String; Last : out Natural) is
   begin
      Reports := Reports + 1;
      Text := [others => ' '];
      Text (Text'First .. Text'First + Report_Last - 1) := Report_Text (1 .. Report_Last);
      Last := Text'First + Report_Last - 1;
      Report_Last := 0;
   end Blocking_Report;
   procedure Job_Report (Text : out String; Last : out Natural) is
   begin
      Text := [others => ' '];
      Last := Text'First - 1;
   end Job_Report;
   function Boost_Hint return Boolean is
   begin
      Hints := Hints + 1;
      return Hints mod 2 = 1;   --  every other hint coalesced
   end Boost_Hint;
   function Boost_Policy (On : Boolean) return Boolean is
   begin
      Policies := Policies + 1;
      Last_Policy := On;
      return True;
   end Boost_Policy;
end Desktop_GPU_Wake_Hook;
