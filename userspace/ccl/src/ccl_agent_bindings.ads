with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
with CCL_Stream_Table;
with Agent_Loop;
with Agent_Missions;

--  agent.* (CCL.Interfaces.Agent) over the agent core (userspace/lib/agent)
--  and the platform's CCL_Llm: the agent loop of AGENT-002 step 2.
--
--  agent.ask starts a mission at once: its transcript stream and reply task
--  open in the session's table, its granted-binding view is built (only the
--  operations its profile grants, as the host itself is granted them), and
--  its tool list is generated from their CCL signatures. Pump then drives
--  the loop: it asks the model through llm.svc, streams the text into the
--  transcript, runs each tool call the model makes (decoded into the
--  operation's typed parameter, checked against the mission, run through
--  the view), records every block in the session's file, and completes the
--  reply when the loop stops. Every request and tool call is logged with
--  its mission, grant and provenance; no prompt, response or tool text is.
package CCL_Agent_Bindings is
   --  After every other interface is installed: the tools are resolved in
   --  Catalog and granted from Grants.
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean;
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Streams : in out CCL_Stream_Table.Table; Now_Ms : Interfaces.Unsigned_64;
      Reply : out CCL.Host_Values.Call_Result);

   --  Drive every running mission. Run is the host's operation dispatch
   --  (the tools run there, through the mission's view).
   generic
      with procedure Run
        (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
         Reply : out CCL.Host_Values.Call_Result);
   procedure Pump
     (Streams : in out CCL_Stream_Table.Table; Now_Ms : Interfaces.Unsigned_64; Arrived : out Boolean);

   --  A mission is running: a front end that sleeps wakes within Poll_Ms.
   Poll_Ms : constant := 50;
   function Running return Boolean;

   --  For a front end that draws a mission itself (the agent app): its
   --  state, budgets and spending, and its transcript's text from an
   --  offset (the same text agent.transcript streams).
   MAXIMUM_SHORT : constant := 160;
   type Short_Text is record
      Data : String (1 .. MAXIMUM_SHORT) := [others => ' '];
      Length : Natural range 0 .. MAXIMUM_SHORT := 0;
   end record;
   function Image (Item : Short_Text) return String is (Item.Data (1 .. Item.Length));
   type Mission_View is record
      Found, Running, Asking, Recorded : Boolean := False;
      Phase : Agent_Loop.Phase := Agent_Loop.Ready;
      Stop : Agent_Loop.Stop := Agent_Loop.Completed;
      Budget : Agent_Loop.Budget;
      Spent : Agent_Loop.Spent;
      Mission : Agent_Missions.Profile := Agent_Missions.Diagnose;
      Started_Ms : Interfaces.Unsigned_64 := 0;
      Tool_Count, Compactions, History_Bytes, Place_Count : Natural := 0;
      Prompt_Tokens : Interfaces.Unsigned_64 := 0;
      Model, Session_Name, Detail, Last_Tool, First_Place : Short_Text;
   end record;
   function View_Of (Id : Interfaces.Unsigned_64) return Mission_View;
   --  The names of the tools the mission grants, space separated.
   function Granted_Tools (Id : Interfaces.Unsigned_64) return String;
   procedure Transcript_Text
     (Id : Interfaces.Unsigned_64; From : Natural; Into : out String; Last : out Natural);

   --  The missions agent.ask started since the last call, for a front end
   --  that gives each live cards (the console).
   MAXIMUM_STARTED : constant := 4;
   type Started_Array is array (1 .. MAXIMUM_STARTED) of Interfaces.Unsigned_64;
   procedure Take_Started (Items : out Started_Array; Count : out Natural);
end CCL_Agent_Bindings;
