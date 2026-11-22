pragma Ada_2022;
package body Agent_Reports with SPARK_Mode is

   Digits_Of : constant String (1 .. 10) := "0123456789";
   Maximum_Digits : constant := 20;

   function Image (Value : Unsigned_64) return String
   with Post => Image'Result'Length <= Maximum_Digits
   is
      Result : String (1 .. Maximum_Digits) := [others => '0'];
      First : Positive range 1 .. Maximum_Digits := Maximum_Digits;
      V : Unsigned_64 := Value;
   begin
      for K in reverse 1 .. Maximum_Digits loop
         pragma Loop_Invariant (First in K .. Maximum_Digits);
         Result (K) := Digits_Of (Natural (V mod 10) + 1);
         First := K;
         V := V / 10;
         exit when V = 0;
      end loop;
      return Result (First .. Maximum_Digits);
   end Image;

   function Identifier (Text : String) return String is
     (if Text'Length in 1 .. Maximum_Identifier and then
         (for all C of Text => C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.')
      then Text else "(invalid)");

   function Stop_Name (Value : Agent_Loop.Stop) return String is
     (case Value is
         when Agent_Loop.Completed => "completed",
         when Agent_Loop.Cancelled => "cancelled",
         when Agent_Loop.Token_Budget => "token_budget",
         when Agent_Loop.Cost_Budget => "cost_budget",
         when Agent_Loop.Time_Budget => "time_budget",
         when Agent_Loop.Tool_Call_Budget => "tool_call_budget",
         when Agent_Loop.Context_Full => "context_full",
         when Agent_Loop.Model_Max_Tokens => "model_max_tokens",
         when Agent_Loop.Model_Refused => "model_refused",
         when Agent_Loop.Model_Failed => "model_failed");

   function Outcome_Name (Value : Tool_Outcome) return String is
     (case Value is
         when Ran => "ok",
         when Failed => "failed",
         when Not_Granted => "not_granted",
         when Malformed => "malformed",
         when Outside_Places => "outside_places",
         when Not_Run => "not_run");

   function Cut (Text : String) return String is
     (Text (Text'First .. Text'First - 1 + Natural'Min (Text'Length, Maximum_Line)))
   with Post => Cut'Result'Length <= Maximum_Line;

   function Request_Line
     (Session : Unsigned_64; Mission : Agent_Missions.Profile; Turn : Natural; Model : String;
      History_Bytes, Tools : Natural; Max_Tokens, Ceiling_Nano_Usd, Deadline_Ms : Unsigned_64)
      return String
   is
     (Cut ("agent: session " & Image (Session) & " mission " & Agent_Missions.Profile_Name (Mission) &
           " request turn " & Image (Unsigned_64 (Turn)) & " model " & Identifier (Model) &
           " history_bytes " & Image (Unsigned_64 (History_Bytes)) & " tools " & Image (Unsigned_64 (Tools)) &
           " max_tokens " & Image (Max_Tokens) & " ceiling_nano_usd " & Image (Ceiling_Nano_Usd) &
           " deadline_ms " & Image (Deadline_Ms) & " cache on"));

   function Tool_Line
     (Session, Exchange : Unsigned_64; Mission : Agent_Missions.Profile; Tool_Name, Call_Id : String;
      Operation : String; Outcome : Tool_Outcome; Input_Bytes, Result_Bytes : Natural) return String
   is
     (Cut ("agent: session " & Image (Session) & " mission " & Agent_Missions.Profile_Name (Mission) &
           " tool " & Identifier (Tool_Name) & " grant " &
           (if Operation'Length = 0 then "none" else Identifier (Operation)) &
           " outcome " & Outcome_Name (Outcome) & " input_bytes " & Image (Unsigned_64 (Input_Bytes)) &
           " result_bytes " & Image (Unsigned_64 (Result_Bytes)) & " provenance exchange " &
           Image (Exchange) & " call " & Identifier (Call_Id)));

   function End_Line
     (Session : Unsigned_64; Mission : Agent_Missions.Profile; Stop : Agent_Loop.Stop;
      Spent : Agent_Loop.Spent) return String
   is
     (Cut ("agent: session " & Image (Session) & " mission " & Agent_Missions.Profile_Name (Mission) &
           " stop " & Stop_Name (Stop) & " turns " & Image (Unsigned_64 (Spent.Turns)) &
           " tool_calls " & Image (Unsigned_64 (Spent.Tool_Calls)) &
           " refused " & Image (Unsigned_64 (Spent.Refused_Calls)) &
           " input " & Image (Unsigned_64 (Spent.Input_Tokens)) & " output " & Image (Unsigned_64 (Spent.Output_Tokens)) &
           " cache_read " & Image (Unsigned_64 (Spent.Cache_Read_Tokens)) & " cache_write " & Image (Unsigned_64 (Spent.Cache_Write_Tokens)) &
           " cost_nano_usd " & Image (Unsigned_64 (Spent.Cost)) & " wall_ms " & Image (Unsigned_64 (Spent.Wall_Ms))));
end Agent_Reports;
