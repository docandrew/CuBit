------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The agent loop's state machine and a mission's budgets (AGENT-002 step 2,
--  docs/agent-harness.md "The loop"): ask the model, run the tool calls it
--  makes, ask again, until it ends its turn, a budget is reached or the
--  person cancels. Pure: the caller does the asking and the running.
--
--  @description
--  Budgets are explicit for every mission (no defaults): tokens (input and
--  output, as the provider reports them), cost (nano-USD), wall time and
--  tool calls. Each request is limited by what remains: its output tokens
--  by the tokens left, its cost ceiling by the cost left (llm.svc refuses
--  to let an exchange run past its ceiling), its deadline by the time
--  left. The model asking for more tool calls than remain stops the loop
--  before any of them runs. Exhaustion is a typed stop, never a silent cut.
--  Tool calls the mission refuses (an unknown tool, a malformed input)
--  count against the budget like the calls that run.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Agent_Loop with Pure, SPARK_Mode is

   ---------------------------------------------------------------------------
   --  Budgets: named ceilings on what one mission may be given.
   ---------------------------------------------------------------------------
   --  Budget quantities: signed (proof stays in linear arithmetic), far
   --  below their type's bound.
   type Quantity is range 0 .. 2**62;
   Maximum_Tokens : constant := 100_000_000;
   subtype Token_Count is Quantity range 0 .. Maximum_Tokens;
   --  USD 1,000 in nano-USD.
   Maximum_Cost : constant := 1_000_000_000_000;
   subtype Nano_Usd is Quantity range 0 .. Maximum_Cost;
   --  A day.
   Maximum_Wall_Ms : constant := 86_400_000;
   subtype Milliseconds is Quantity range 0 .. Maximum_Wall_Ms;
   Maximum_Tool_Calls : constant := 1_000;
   subtype Call_Count is Natural range 0 .. Maximum_Tool_Calls;

   --  One request's limits (CuBit.Llm_Protocol's Output_Tokens and
   --  Deadline_Ms ranges).
   Maximum_Request_Tokens : constant := 128_000;
   subtype Request_Tokens is Quantity range 1 .. Maximum_Request_Tokens;
   Maximum_Request_Ms : constant := 600_000;
   subtype Request_Ms is Quantity range 1 .. Maximum_Request_Ms;

   type Budget is record
      Tokens : Token_Count := 0;
      Cost : Nano_Usd := 0;
      Wall_Ms : Milliseconds := 0;
      Tool_Calls : Call_Count := 0;
      --  Each request's own limits, below what remains of the mission's.
      Request_Max_Tokens : Request_Tokens := 1;
      Request_Deadline_Ms : Request_Ms := 1;
   end record;

   type Spent is record
      Input_Tokens, Output_Tokens, Cache_Read_Tokens, Cache_Write_Tokens : Token_Count := 0;
      Cost : Nano_Usd := 0;
      Wall_Ms : Milliseconds := 0;
      Tool_Calls : Call_Count := 0;
      Refused_Calls : Call_Count := 0;
      Turns : Natural := 0;
   end record;

   --  The tokens a mission's token budget counts.
   function Tokens_Of (S : Spent) return Quantity is
     (S.Input_Tokens + S.Output_Tokens + S.Cache_Read_Tokens + S.Cache_Write_Tokens);

   ---------------------------------------------------------------------------
   --  The loop.
   ---------------------------------------------------------------------------
   type Phase is (Ready, Asking, Running_Tools, Finished);

   --  Why a mission ended.
   type Stop is
     (Completed, Cancelled, Token_Budget, Cost_Budget, Time_Budget, Tool_Call_Budget,
      Context_Full, Model_Max_Tokens, Model_Refused, Model_Failed);

   --  How one exchange with the model ended (from its typed stop).
   type Model_Outcome is
     (End_Turn, Tool_Use, Pause_Turn, Max_Tokens, Refusal, Cost_Stop, Deadline_Stop,
      Cancelled_Stop, Failed);

   type State is private;

   function Phase_Of (S : State) return Phase;
   function Stop_Of (S : State) return Stop;
   function Budget_Of (S : State) return Budget;
   function Spent_Of (S : State) return Spent;
   --  Tool calls of the current turn still to run.
   function Pending_Of (S : State) return Call_Count;

   function Within_Budget (S : State) return Boolean is
     (Spent_Of (S).Tool_Calls <= Budget_Of (S).Tool_Calls);

   function Start (B : Budget; Now_Ms : Unsigned_64) return State
   with Post => Phase_Of (Start'Result) = Ready and then Budget_Of (Start'Result) = B
                and then Within_Budget (Start'Result);

   type Request_Limits is record
      Max_Tokens : Request_Tokens := 1;
      Ceiling : Nano_Usd := 0;
      Deadline_Ms : Request_Ms := 1;
   end record;

   --  The next request, if the budgets allow one: its limits are within
   --  what remains. Otherwise the loop finishes with the budget's stop.
   procedure Begin_Request
     (S : in out State; Now_Ms : Unsigned_64; Limits : out Request_Limits; Allowed : out Boolean)
   with Pre => Phase_Of (S) = Ready and then Within_Budget (S),
        Post => Within_Budget (S) and then
                (if Allowed then
                   Phase_Of (S) = Asking and then
                   Limits.Max_Tokens <= Budget_Of (S).Request_Max_Tokens and then
                   Tokens_Of (Spent_Of (S)) + Limits.Max_Tokens <= Budget_Of (S).Tokens and then
                   Spent_Of (S).Cost + Limits.Ceiling = Budget_Of (S).Cost and then
                   Limits.Deadline_Ms <= Budget_Of (S).Request_Deadline_Ms and then
                   Spent_Of (S).Wall_Ms + Limits.Deadline_Ms <= Budget_Of (S).Wall_Ms
                 else Phase_Of (S) = Finished);

   type Exchange_Usage is record
      Input_Tokens, Output_Tokens, Cache_Read_Tokens, Cache_Write_Tokens : Unsigned_64 := 0;
      Cost : Unsigned_64 := 0;
   end record;

   --  The exchange ended as Outcome, with Usage, asking for Tool_Calls
   --  tool calls (Tool_Use).
   procedure Model_Ended
     (S : in out State; Now_Ms : Unsigned_64; Outcome : Model_Outcome; Usage : Exchange_Usage;
      Tool_Calls : Natural)
   with Pre => Phase_Of (S) = Asking and then Within_Budget (S),
        Post => Within_Budget (S) and then Phase_Of (S) in Ready | Running_Tools | Finished and then
                (if Phase_Of (S) = Running_Tools then
                   Pending_Of (S) = Tool_Calls and then Tool_Calls > 0 and then
                   Spent_Of (S).Tool_Calls + Pending_Of (S) <= Budget_Of (S).Tool_Calls);

   --  One tool call of the turn ran (or was refused: Refused).
   procedure Tool_Done (S : in out State; Refused : Boolean)
   with Pre => Phase_Of (S) = Running_Tools and then Pending_Of (S) > 0 and then
               Spent_Of (S).Tool_Calls + Pending_Of (S) <= Budget_Of (S).Tool_Calls,
        Post => Within_Budget (S) and then
                Spent_Of (S).Tool_Calls = Spent_Of (S'Old).Tool_Calls + 1 and then
                (if Pending_Of (S) = 0 then Phase_Of (S) = Ready
                 else Phase_Of (S) = Running_Tools and then
                      Spent_Of (S).Tool_Calls + Pending_Of (S) <= Budget_Of (S).Tool_Calls);

   --  The person cancelled ((agent.cancel s)).
   procedure Cancel (S : in out State)
   with Post => Phase_Of (S) = Finished and then
                (if Phase_Of (S'Old) /= Finished then Stop_Of (S) = Cancelled);

   --  The conversation no longer fits one request.
   procedure Context_Full (S : in out State)
   with Post => Phase_Of (S) = Finished;

   --  The request was not sent (the conversation was compacted to fit a
   --  ceiling first): ask again, nothing spent.
   procedure Retry_Request (S : in out State)
   with Pre => Phase_Of (S) = Asking and then Within_Budget (S),
        Post => Phase_Of (S) = Ready and then Within_Budget (S) and then
                Spent_Of (S) = Spent_Of (S'Old) and then Budget_Of (S) = Budget_Of (S'Old);

   --  Time passed: outside an exchange (whose own deadline llm.svc keeps),
   --  the wall-time budget may run out.
   procedure Tick (S : in out State; Now_Ms : Unsigned_64)
   with Pre => Within_Budget (S), Post => Within_Budget (S);

private
   type State is record
      Current : Phase := Ready;
      Ended_As : Stop := Completed;
      Limits : Budget;
      Used : Spent;
      Started_Ms : Unsigned_64 := 0;
      Pending : Call_Count := 0;
   end record;

   function Phase_Of (S : State) return Phase is (S.Current);
   function Stop_Of (S : State) return Stop is (S.Ended_As);
   function Budget_Of (S : State) return Budget is (S.Limits);
   function Spent_Of (S : State) return Spent is (S.Used);
   function Pending_Of (S : State) return Call_Count is (S.Pending);
end Agent_Loop;
