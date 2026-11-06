with Interfaces;
with CCL.Binding_Blocks;
with CCL.Catalog;
with CCL.Objects;

--  The agent loop in CCL (AGENT-002 step 2, docs/agent-harness.md), on the
--  agent core (userspace/lib/agent) and llm.svc:
--    (agent.ask (Agent_Request mission => Mission.Diagnose prompt => "..."
--       model => Agent_Model.Profile
--       budget => (Agent_Budget tokens => 200000 cost_nano_usd => 500000000
--                  wall_ms => 600000 tool_calls => 20 request_max_tokens => 4096
--                  request_deadline_ms => 120000)
--       places => ["@mem:0/work"] session => "diag-1"))
--                      Agent_Session: the mission, started at once. Every
--                      budget is spelled out (no defaults); the model is the
--                      profile's (Agent_Model.Profile; Diagnose asks
--                      claude-sonnet-5-5) or a named one; places are the
--                      read-only roots its file tools may name; session names
--                      the record of the conversation in the console's
--                      workspace (asking again under the same name continues
--                      it).
--    (agent.transcript s)  Stream<String>: the model's text as it streams and
--                      a card for every tool call (tool, input and result
--                      summaries, outcome).
--    (agent.reply s)   Task<Agent_Reply>: how the mission ended (a typed
--                      Agent_Stop), usage, cost, tool calls, once it has.
--    (agent.cancel s)  stops it (Agent_Stop.Cancelled).
--  The tools are typed CCL operations the mission grants; nothing else
--  exists for the model. The types are CCL source; the digest and keys are
--  SHA-256 of it (then "#" and the type's name), and tests/ccl-console
--  checks them.
package CCL.Interfaces.Agent with SPARK_Mode is

   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 3;
   PUBLISHED_TYPES : constant := 15;
   use Standard.Interfaces;

   TYPE_SOURCE : constant String :=
     "(type Mission (enum Diagnose)) " &
     "(type Agent_Model (variant (Profile) (Named String))) " &
     "(type Agent_Tokens (range 1 100000000)) (type Agent_Cost (range 1 1000000000000)) " &
     "(type Agent_Milliseconds (range 1 86400000)) (type Agent_Calls (range 0 1000)) " &
     "(type Agent_Request_Tokens (range 1 128000)) (type Agent_Request_Ms (range 1 600000)) " &
     "(type Agent_Budget (record (tokens Agent_Tokens) (cost_nano_usd Agent_Cost) " &
     "(wall_ms Agent_Milliseconds) (tool_calls Agent_Calls) (request_max_tokens Agent_Request_Tokens) " &
     "(request_deadline_ms Agent_Request_Ms))) " &
     "(type Agent_Request (record (mission Mission) (prompt String) (model Agent_Model) " &
     "(budget Agent_Budget) (places (List String)) (session String))) " &
     "(type Agent_Session (record (id Integer))) " &
     "(type Agent_Stop (enum Completed Cancelled Token_Budget Cost_Budget Time_Budget " &
     "Tool_Call_Budget Context_Full Model_Max_Tokens Model_Refused Model_Failed)) " &
     "(type Agent_Reply (record (stop Agent_Stop) (turns Integer) (tool_calls Integer) " &
     "(refused_calls Integer) (input_tokens Integer) (output_tokens Integer) " &
     "(cache_read_tokens Integer) (cache_write_tokens Integer) (cost_nano_usd Integer) " &
     "(duration_ms Integer) (session String) (detail String)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#145B_E16E_3475_D5B0#,
      16#5C99_21F2_F1AD_C90D#,
      16#4194_36F9_65E6_9841#,
      16#7323_54A1_51DE_E584#];
   REQUEST_KEY : constant CCL.Objects.Schema_Key :=
     [16#4066_C3C7_526A_0CC8#,
      16#DB87_BE02_BF12_6F0F#,
      16#94B6_9A69_F891_B219#,
      16#C047_6CA0_E0A3_7E7D#];
   SESSION_KEY : constant CCL.Objects.Schema_Key :=
     [16#6E50_FEE9_C985_0FD2#,
      16#0233_5FF5_7304_F084#,
      16#5851_1DD0_D2EA_0600#,
      16#8E5F_A7B9_B792_E7C7#];
   REPLY_KEY : constant CCL.Objects.Schema_Key :=
     [16#1C86_D7E9_4EF6_A801#,
      16#2359_3F5D_0473_DBAA#,
      16#C367_F89C_7C11_A3C3#,
      16#DD03_75E1_A795_6B32#];

   --  Fields, in declaration order (1-based, as CCL.Objects.Views numbers them).
   REQUEST_MISSION : constant := 1;
   REQUEST_PROMPT : constant := 2;
   REQUEST_MODEL : constant := 3;
   REQUEST_BUDGET : constant := 4;
   REQUEST_PLACES : constant := 5;
   REQUEST_SESSION : constant := 6;
   BUDGET_TOKENS : constant := 1;
   BUDGET_COST : constant := 2;
   BUDGET_WALL : constant := 3;
   BUDGET_TOOL_CALLS : constant := 4;
   BUDGET_REQUEST_TOKENS : constant := 5;
   BUDGET_REQUEST_DEADLINE : constant := 6;
   SESSION_FIELDS : constant := 1;
   REPLY_FIELDS : constant := 12;
   --  Alternatives, numbered from 1.
   MODEL_PROFILE : constant := 1;
   MODEL_NAMED : constant := 2;

   type Operation is (Ask, Transcript, Reply, Cancel);
   function Name (Item : Operation) return String is
     (case Item is
         when Ask => "ask", when Transcript => "transcript", when Reply => "reply",
         when Cancel => "cancel");
   FIRST_BINDING : constant Unsigned_32 := CCL.Binding_Blocks.First (CCL.Binding_Blocks.Agent);
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));

   type Contracts is record
      Request, Session, Reply : CCL.Objects.Binding;
   end record;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Agent;
