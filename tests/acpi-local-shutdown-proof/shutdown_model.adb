pragma Ada_2022;
pragma SPARK_Mode (On);
package body Shutdown_Model is
   procedure Close_Consumers (Accepted : out Boolean) is
   begin Accepted := Configuration.Consumers_Accept; end Close_Consumers;
   procedure Close_Local is null;
   procedure Begin_Stop is null;
   procedure Maintain (Deadline : out Word; May_Continue : out Boolean) is
   begin Deadline := Configuration.Hardware_Deadline; May_Continue := Configuration.Hardware_Continues; end Maintain;
   procedure Cleanup (Completed : out Boolean) is
   begin Completed := Configuration.Fields_Done; end Cleanup;
   function May_Exit return Boolean is (Configuration.Hardware_Idle);
   procedure View (Quiescent : out Boolean; Deadline : out Word) is
   begin Quiescent := Configuration.Others_Idle; Deadline := Configuration.Other_Deadline; end View;
end Shutdown_Model;
