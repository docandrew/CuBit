--  agent.app (AGENT-002 step 3, docs/agent-harness.md "The Agent app"): the
--  in-CuBit agent's own window over the agent core the console's
--  (agent.ask ...) uses. One input line asks a Diagnose mission (continuing
--  one recorded session) or takes the Anthropic API key into secrets.svc;
--  the transcript shows the model's text and a card for every tool call;
--  the side panel shows the mission's authority (tools, place, no
--  approvals), its budgets as meters, and its state. Esc stops a mission.
package CCL_Agent_App is
   procedure Run;
end CCL_Agent_App;
