with Interfaces;
with CCL.Sessions;
with CCL.Language;
with CCL.Periodic_Programs;
with CCL.Completions;

--  Development control protocol v1. No transport, GUI, or native IPC here.
--  A host supplies observations from its explicitly authorized adapters.
package CCL.Control with SPARK_Mode is
   --  Present_Expression evaluates like Evaluate_Expression and answers with
   --  the result as CCL.Presentations describes it (what the native console
   --  shows); Read_Image_Rows answers with a band of a stored image's pixels;
   --  Present_Monitor answers with the periodic program's last result,
   --  presented (a live cell observed from outside); Complete_Expression
   --  answers what completes the source (the text before a caret).
   type Operation is (Inspect_Bindings, Evaluate_Expression, Read_Clock,
                      Start_Monitor, Stop_Monitor, Inspect_Monitor,
                      Present_Expression, Read_Image_Rows, Present_Monitor,
                      Complete_Expression);
   for Operation use (Inspect_Bindings => 1, Evaluate_Expression => 2, Read_Clock => 3,
                      Start_Monitor => 4, Stop_Monitor => 5, Inspect_Monitor => 6,
                      Present_Expression => 7, Read_Image_Rows => 8, Present_Monitor => 9,
                      Complete_Expression => 10);
   for Operation'Size use 8;
   type Observation is record
      Process_Id, Network_Process, Clock_Process : Interfaces.Unsigned_64 := 0;
      Clock_Available : Boolean := False;
      Monotonic_Ms : Interfaces.Unsigned_64 := 0;
   end record;
   type Response is record
      Observed : Observation;
      Outcome : CCL.Language.Interpretation_Result;
      Monitor : CCL.Periodic_Programs.Program;
      Accepted : Boolean := False;
      Completion : CCL.Completions.Result;
   end record;
   procedure Execute
     (Session : in out CCL.Sessions.Session; Op : Operation;
      Source : String; Host : Observation; Result : out Response);
end CCL.Control;
