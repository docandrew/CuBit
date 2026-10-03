with CCL.Language;
with CCL.Sessions;

--  The REPL's workspace commands, shared by every CCL desktop front end.
--  The front end owns the workspace (CCL_Workspace), so these live outside
--  the session engine:
--    :files        the .ccl files in the workspace
--    :save NAME    the session's definitions as a new file (never replaces)
--    :load NAME    a file's definitions into the session
--  Anything else goes to Submit, the front end's live evaluation.
generic
   with procedure Submit
     (Item : in out CCL.Sessions.Session; Source : String;
      Fuel : CCL.Sessions.Fuel_Budget;
      Outcome : out CCL.Language.Interpretation_Result;
      Shown : String := "");
procedure CCL_REPL_Commands
  (Item : in out CCL.Sessions.Session; Source : String;
   Fuel : CCL.Sessions.Fuel_Budget; Outcome : out CCL.Language.Interpretation_Result);
