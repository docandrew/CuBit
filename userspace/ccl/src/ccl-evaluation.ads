with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
with CCL.Language;
with CCL.Objects;
with CCL.Streams;
with CCL.VM;

--  Running CCL source. Every program is analysed, compiled to CCLB, linked
--  against the host's granted bindings, verified and run on the VM: there is
--  no other engine (user decision, 2026-10-05). Compiling and verifying an
--  entry costs tens of microseconds, so a REPL entry pays nothing for it.
--
--  Results use CCL.Language.Interpretation_Result, so a front end shows a
--  value the same way whichever program produced it.
package CCL.Evaluation is

   --  Evaluate Source against the host: Invoke answers each granted
   --  operation the program calls, Read_Stream each stream view and wait.
   --  A wait on a task still pending stops the run with Waiting_On_Task
   --  (Waited_On names the task); the caller runs the entry again once it
   --  completes (CCL.Sessions.Resume_With_Values).
   generic
      type Host_Context is limited private;
      with procedure Invoke
        (Context : in out Host_Context; Binding : Interfaces.Unsigned_32;
         Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result);
      --  Answers stream views (docs/ccl-streams.md). Reply arrives as
      --  No_Such_Stream; a host without streams leaves it so.
      with procedure Read_Stream
        (Context : in out Host_Context; Request : CCL.Streams.View_Request;
         Reply : in out CCL.Streams.View_Reply) is null;
   procedure Evaluate_With_Values
     (Source : String; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Result : out CCL.Language.Interpretation_Result)
     with Post => Result.Fuel_Remaining <= Fuel;

   --  As Evaluate_With_Values, for a program already analysed (a retained
   --  handler: CCL.Language.Handlers). Its root is what runs.
   generic
      type Host_Context is limited private;
      with procedure Invoke
        (Context : in out Host_Context; Binding : Interfaces.Unsigned_32;
         Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result);
      with procedure Read_Stream
        (Context : in out Host_Context; Request : CCL.Streams.View_Request;
         Reply : in out CCL.Streams.View_Reply) is null;
   procedure Evaluate_Analysis_With_Values
     (Analysis : CCL.Language.Analysis_Result; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Result : out CCL.Language.Interpretation_Result)
     with Post => Result.Fuel_Remaining <= Fuel;

   --  A host that answers with scalars only (Integer, Boolean).
   generic
      type Host_Context is limited private;
      with procedure Invoke
        (Context : in out Host_Context; Binding : Interfaces.Unsigned_32;
         Argument : CCL.VM.Value; Value : out CCL.VM.Value; Success : out Boolean);
   procedure Evaluate_With_Host
     (Source : String; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Result : out CCL.Language.Interpretation_Result)
     with Post => Result.Fuel_Remaining <= Fuel;

   --  Pure evaluation: no host operations.
   procedure Evaluate
     (Source : String; Fuel : Natural; Result : out CCL.Language.Interpretation_Result)
     with Post => Result.Fuel_Remaining <= Fuel;
   procedure Evaluate
     (Source : String; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Result : out CCL.Language.Interpretation_Result)
     with Post => Result.Fuel_Remaining <= Fuel;

   --  As Evaluate_With_Values, delivering the result as a native image of
   --  Expected (independently approved metadata, never a grant). A program
   --  whose result type does not match Expected is refused before it runs.
   generic
      type Host_Context is limited private;
      with procedure Invoke
        (Context : in out Host_Context; Binding : Interfaces.Unsigned_32;
         Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result);
   procedure Evaluate_Object_With_Values
     (Source : String; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Expected : CCL.Objects.Binding;
      Result : out CCL.Language.Object_Interpretation_Result)
     with Post => Result.Fuel_Remaining <= Fuel;

   procedure Evaluate_Object
     (Source : String; Fuel : Natural; Expected : CCL.Objects.Binding;
      Result : out CCL.Language.Object_Interpretation_Result)
     with Post => Result.Fuel_Remaining <= Fuel;
end CCL.Evaluation;
