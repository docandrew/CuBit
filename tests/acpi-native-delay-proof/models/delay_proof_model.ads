pragma Ada_2022;
with ACPI_Delay_Waits;
with AML_Delays;
with Interfaces;
with Proof_Inputs;
package Delay_Proof_Model with SPARK_Mode is
   package Waits is new ACPI_Delay_Waits (Proof_Inputs.Read_Raw, Proof_Inputs.Sleep_Raw, 1_000);
   procedure Run_One (Item : AML_Delays.Request; Deadline : Interfaces.Unsigned_64;
      Budget : Waits.Observation_Budget; Result : out Waits.Report);
end Delay_Proof_Model;
