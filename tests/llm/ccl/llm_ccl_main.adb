--  Linux-hosted tests of llm.* and secrets.* in CCL (CCL.Interfaces.Llm,
--  CCL_Llm_Bindings, CCL.Interfaces.Secrets) over the whole host
--  environment the console uses, with a scripted CCL_Llm in place of
--  llm.svc (fake_ccl_llm.adb) and the preview's stand-in CCL_Secrets. Not
--  CuBit: the headless llm-mock test runs the real path.
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
with CCL.Language;
with CCL.Sessions;
with CCL.Streams;
with CCL_Host_Environment;
with CCL_Llm_Bindings;
with Llm_Script;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Llm_Ccl_Main is
   use type Interfaces.Unsigned_64;
   Failures, Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;
   function Contains (Text, Pattern : String) return Boolean is
     (Ada.Strings.Fixed.Index (Text, Pattern) > 0);

   Clock : Interfaces.Unsigned_64 := 1_000;
   function Now (Available : out Boolean) return Interfaces.Unsigned_64 is
   begin
      Available := True;
      Clock := Clock + 10;
      return Clock;
   end Now;
   package Host is new CCL_Host_Environment (Now);

   Catalog : CCL.Catalog.Interface_Catalog;
   Grants : CCL.Catalog.Granted_Bindings;
   type No_Context is null record;
   Context : No_Context;
   procedure Invoke
     (C : in out No_Context; Binding : Interfaces.Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (C);
   begin
      Host.Invoke (Binding, Argument, Reply);
   end Invoke;
   procedure Read
     (C : in out No_Context; Request : CCL.Streams.View_Request; Reply : in out CCL.Streams.View_Reply)
   is
      pragma Unreferenced (C);
   begin
      Host.Read_Stream (Request, Reply);
   end Read;
   procedure Submit is new CCL.Sessions.Submit_With_Values (No_Context, Invoke, Read_Stream => Read);
   Session : CCL.Sessions.Session;

   function Run (Source : String) return String is
      Outcome : CCL.Language.Interpretation_Result;
   begin
      Submit (Session, Source, CCL.Sessions.Default_Fuel, Grants, Context, Outcome);
      return CCL.Sessions.Result_Image (Outcome);
   end Run;

   procedure Pump is
      Arrived : Boolean;
   begin
      Host.Pump_Streams (Arrived);
   end Pump;

   Installed : Boolean;
   Ask : constant String :=
     "(define r (llm.ask (Llm_Request prompt => ""hello"" model => Llm_Model.Default " &
     "max_tokens => 1024 deadline_ms => 60000 cost_ceiling => (Llm_Ceiling.Nano_Usd 100000000) " &
     "system => ""Be brief."" mission => ""hosted"")))";
begin
   CCL.Catalog.Initialize (Catalog);
   CCL.Catalog.Initialize (Grants);
   Host.Install (Catalog, Grants, Installed);
   Check (Installed, "the host environment installs llm.* and secrets.*");
   CCL.Sessions.Initialize (Session, Catalog);

   declare
      Asked : constant String := Run (Ask);
   begin
      Check (Llm_Script.Asked and then Length (Llm_Script.Last_Conversation) = 16 + 5 and then not Llm_Script.Last_Cache and then Llm_Script.Last_Model_Length = 0
             and then Llm_Script.Last_Max_Tokens = 1024 and then Llm_Script.Last_Deadline = 60_000
             and then Llm_Script.Last_Bounded and then Llm_Script.Last_Ceiling = 100_000_000
             and then Llm_Script.Last_Mission_Length = 6 and then Llm_Script.Last_System_Length = 9,
             "llm.ask passes every limit through, Default as no model name: " & Asked);
      Check (Contains (Asked, "Llm_Exchange"), "llm.ask answers the exchange: " & Asked);
   end;
   Check (Contains (Run ("(llm.text (Llm_Exchange 7))"), "Stream<String>"),
          "llm.text answers a Stream<String> (a text stream's handle)");
   declare
      Text_Before : constant String := Run ("(window 64 (llm.text r))");
   begin
      Check (Contains (Text_Before, "[]"), "no text before any arrived: " & Text_Before);
   end;
   Llm_Script.Released := 1;
   Pump;
   declare
      Partial : constant String := Run ("(window 64 (llm.text r))");
   begin
      Check (Contains (Partial, """Hello from""") and then not Contains (Partial, "END"),
             "the first piece streams in: " & Partial);
   end;
   Llm_Script.Released := 2;
   Pump;
   declare
      Lines : constant String := Run ("(window 64 (llm.text r))");
   begin
      Check (Contains (Lines, """Hello from the script."", ""Second """),
             "a line feed ends a line, the next grows: " & Lines);
   end;
   Check (Contains (Run ("(wait (llm.reply r))"), "Waiting"), "the reply waits while the exchange runs");
   Llm_Script.Released := 3;
   Llm_Script.Ended := True;
   Pump;
   declare
      Reply : constant String := Run ("(wait (llm.reply r))");
      Whole : constant String := Run ("(window 64 (llm.text r))");
   begin
      Check (Contains (Reply, "Llm_Stop.End_Turn") and then Contains (Reply, "(Llm_Cost.Priced 340000)"),
             "the reply is typed: " & Reply);
      Check (Contains (Whole, """Second line, END."""), "the whole text: " & Whole);
   end;
   declare
      Refused : constant String := Run
        ("(llm.ask (Llm_Request prompt => ""x"" model => Llm_Model.Default max_tokens => 0 " &
         "deadline_ms => 1 cost_ceiling => Llm_Ceiling.Unlimited system => """" mission => """"))");
   begin
      Check (not Contains (Refused, "Llm_Exchange"), "max_tokens 0 is refused by type: " & Refused);
   end;
   declare
      Untyped : constant String := Run ("(llm.ask ""hello"")");
   begin
      Check (not Contains (Untyped, "Llm_Exchange"), "a bare prompt carries no limits and is refused: " & Untyped);
   end;
   declare
      Stand_In : constant String := Run ("(secrets.set (Secret ""anthropic-api-key""))");
   begin
      Check (Contains (Stand_In, "secrets"), "secrets.set without secrets.svc says why: " & Stand_In);
   end;
   if Failures = 0 then
      Put_Line ("PASS: llm.* and secrets.* in CCL" & Checks'Image & " checks");
   else
      Put_Line ("FAIL:" & Failures'Image & " of" & Checks'Image);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Llm_Ccl_Main;
