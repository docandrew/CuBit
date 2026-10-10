pragma Ada_2022;
package AML_Objects.Test_Faults with SPARK_Mode => Off is
   -- Deliberate representation corruption, used only by the hosted contract tests.
   procedure Alias_Backing (Store : in out State; Target, Source : Object_ID);
end AML_Objects.Test_Faults;
