pragma Ada_2022;
package body Delay_Proof_Model with SPARK_Mode is
   procedure Run_One (Item : AML_Delays.Request; Deadline : Interfaces.Unsigned_64;
      Budget : Waits.Observation_Budget; Result : out Waits.Report) is
   begin Waits.Run (Item, Deadline, Budget, Result); end Run_One;
end Delay_Proof_Model;
