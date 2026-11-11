pragma Ada_2022;
with AML_Profile_Events;
with AML_Dispatch_Profile;
-- Diagnostic syntax grouping only, never opcode acceptance or permissions.
package AML_Profile_Classifier with Pure, SPARK_Mode is
   function Classify (Item : AML_Profile_Events.Event)
      return AML_Dispatch_Profile.Instruction_Class with Global => null;
end AML_Profile_Classifier;
