with Interfaces;
with CCL.Sessions;

--  Development control protocol v1. No transport, GUI, or native IPC here.
--  A host supplies observations from its explicitly authorized adapters.
package CCL.Control with SPARK_Mode is
   type Operation is (Inspect_Bindings, Evaluate_Expression, Read_Clock);
   for Operation use (Inspect_Bindings => 1, Evaluate_Expression => 2, Read_Clock => 3);
   for Operation'Size use 8;
   Max_Response : constant := 8_192;
   type Response is record
      Data : String (1 .. Max_Response) := [others => ' '];
      Length : Natural range 0 .. Max_Response := 0;
   end record;
   type Observation is record
      Process_Id, Network_Process, Clock_Process : Interfaces.Unsigned_64 := 0;
      Clock_Available : Boolean := False;
      Monotonic_Ms : Interfaces.Unsigned_64 := 0;
   end record;
   procedure Execute
     (Session : in out CCL.Sessions.Session; Op : Operation;
      Source : String; Host : Observation; Result : out Response);
end CCL.Control;
