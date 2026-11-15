pragma Ada_2022;
pragma SPARK_Mode (On);
with Interfaces;
package Shutdown_Model is
   subtype Word is Interfaces.Unsigned_64;
   type Inputs is record
      Consumers_Accept, Fields_Done, Hardware_Idle, Others_Idle : Boolean := False;
      Hardware_Continues : Boolean := False;
      Hardware_Deadline, Other_Deadline : Word := Word'Last;
   end record;
   Configuration : Inputs;
   procedure Close_Consumers (Accepted : out Boolean)
     with Global => (Input => Configuration), Post => Accepted = Configuration.Consumers_Accept;
   procedure Close_Local with Global => null;
   procedure Begin_Stop with Global => null;
   procedure Maintain (Deadline : out Word; May_Continue : out Boolean)
     with Global => (Input => Configuration),
       Post => Interfaces."=" (Deadline, Configuration.Hardware_Deadline)
         and then May_Continue = Configuration.Hardware_Continues;
   procedure Cleanup (Completed : out Boolean)
     with Global => (Input => Configuration), Post => Completed = Configuration.Fields_Done;
   function May_Exit return Boolean
     with Global => (Input => Configuration), Post => May_Exit'Result = Configuration.Hardware_Idle;
   procedure View (Quiescent : out Boolean; Deadline : out Word)
     with Global => (Input => Configuration), Post => Quiescent = Configuration.Others_Idle
       and then Interfaces."=" (Deadline, Configuration.Other_Deadline);
end Shutdown_Model;
