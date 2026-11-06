------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What the agent loop logs (AGENT-002, agent-security.md "Explainable
--  agent action"): every request and every tool call, with the mission,
--  the grant it ran under and its provenance (the exchange and the model's
--  call id), and how each mission ended.
--
--  @description
--  Lines are built from metadata only: counts, typed outcomes, the tool's
--  name and CCL operation, ids. No prompt, response, tool input or tool
--  result text is ever an argument here, so none can reach the log; the
--  API key never reaches the agent at all (llm.svc holds its use). Names
--  and ids that came from the model are shown only when they are plain
--  identifiers ([A-Za-z0-9_-], bounded), else as "(invalid)".
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Agent_Loop;
with Agent_Missions;

package Agent_Reports with SPARK_Mode is

   Maximum_Line : constant := 512;
   Maximum_Identifier : constant := 64;

   --  Text from the model shown as an identifier, or "(invalid)".
   function Identifier (Text : String) return String
   with Post => Identifier'Result'Length <= Maximum_Identifier;

   function Stop_Name (Value : Agent_Loop.Stop) return String
   with Post => Stop_Name'Result'Length <= 32;

   function Request_Line
     (Session : Unsigned_64; Mission : Agent_Missions.Profile; Turn : Natural; Model : String;
      History_Bytes, Tools : Natural; Max_Tokens, Ceiling_Nano_Usd, Deadline_Ms : Unsigned_64)
      return String
   with Pre => Model'Length <= Maximum_Identifier,
        Post => Request_Line'Result'Length <= Maximum_Line;

   type Tool_Outcome is (Ran, Failed, Not_Granted, Malformed, Outside_Places, Not_Run);
   function Outcome_Name (Value : Tool_Outcome) return String
   with Post => Outcome_Name'Result'Length <= 32;

   function Tool_Line
     (Session, Exchange : Unsigned_64; Mission : Agent_Missions.Profile; Tool_Name, Call_Id : String;
      Operation : String; Outcome : Tool_Outcome; Input_Bytes, Result_Bytes : Natural) return String
   with Pre => Tool_Name'Length <= 4096 and then Call_Id'Length <= 4096 and then Operation'Length <= 64,
        Post => Tool_Line'Result'Length <= Maximum_Line;

   function End_Line
     (Session : Unsigned_64; Mission : Agent_Missions.Profile; Stop : Agent_Loop.Stop;
      Spent : Agent_Loop.Spent) return String
   with Post => End_Line'Result'Length <= Maximum_Line;
end Agent_Reports;
