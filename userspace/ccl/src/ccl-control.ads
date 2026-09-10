with Interfaces;
with CCL.Sessions;
with CCL.Language;
with CCL.Periodic_Programs;

--  Development control protocol v1. No transport, GUI, or native IPC here.
--  A host supplies observations from its explicitly authorized adapters.
package CCL.Control with SPARK_Mode is
   type Operation is (Inspect_Bindings, Evaluate_Expression, Read_Clock,
                      Start_Monitor, Stop_Monitor, Inspect_Monitor);
   for Operation use (Inspect_Bindings => 1, Evaluate_Expression => 2, Read_Clock => 3,
                      Start_Monitor => 4, Stop_Monitor => 5, Inspect_Monitor => 6);
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
   end record;
   procedure Execute
     (Session : in out CCL.Sessions.Session; Op : Operation;
      Source : String; Host : Observation; Result : out Response);
end CCL.Control;
