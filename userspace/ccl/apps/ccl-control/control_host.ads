with Interfaces;
with CCL.Completions;
with CCL.Language;
with CCL.Periodic_Programs;

-- Native trusted binding adapter, not part of the parser/compiler or wire
-- codec. No application-supplied PID, binding number, or endpoint slot.
package Control_Host is
   procedure Initialize (Success : out Boolean);
   procedure Read_Clock (Available : out Boolean; Value : out Interfaces.Unsigned_64);
   --  Session: a browser tab's session (Control_Wire.Request.Session). Its
   --  definitions, kept values and streams persist across its requests; 0
   --  is a fresh session discarded afterwards. It separates tabs and grants
   --  nothing: every tab has this host's same grants.
   procedure Evaluate
     (Session : Interfaces.Unsigned_64; Source : String;
      Result : out CCL.Language.Interpretation_Result);
   --  What completes Before (the text up to a caret): this host's catalog
   --  and the session's own definitions.
   procedure Complete
     (Session : Interfaces.Unsigned_64; Before : String; Result : out CCL.Completions.Result);
   --  The live monitor runs Source in the session's environment.
   procedure Start_Monitor
     (Session : Interfaces.Unsigned_64; Source : String; Accepted : out Boolean);
   procedure Stop_Monitor (Identity : Interfaces.Unsigned_64; Accepted : out Boolean);
   procedure Pump;
   function Monitor return CCL.Periodic_Programs.Program;
   function Next_Deadline return Interfaces.Unsigned_64;
end Control_Host;
