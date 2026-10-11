with Interfaces;
with CuBit.Messages;
--  Hosted stand-in for the Mesa glue's wake hook: scripted answers, and a
--  record of every token it was given.
package Desktop_GPU_Wake_Hook is
   type Arm_Result is (Armed, Idle, Unavailable);
   function Arm (Token : Interfaces.Unsigned_64) return Arm_Result;
   function Collect (Receipt : CuBit.Messages.CompletionEntry) return Boolean;
   function Boost_Hint return Boolean;
   procedure Blocking_Report (Text : out String; Last : out Natural);
   Hints : Natural := 0;
   procedure Job_Report (Text : out String; Last : out Natural);

   Next_Answer : Arm_Result := Armed;
   Arms : Natural := 0;
   Last_Token, Armed_Token : Interfaces.Unsigned_64 := 0;
   Tokens_Increasing : Boolean := True;
   Report_Text : String (1 .. 32) := "create=1/9us/9us" & [17 .. 32 => ' '];
   Report_Last : Natural := 16;
end Desktop_GPU_Wake_Hook;
