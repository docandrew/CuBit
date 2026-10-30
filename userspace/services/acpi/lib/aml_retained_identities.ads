with Interfaces;
package AML_Retained_Identities with SPARK_Mode, Pure is
   type Incarnation is new Interfaces.Unsigned_64;
   subtype Incarnation_Budget is Incarnation range 1 .. Incarnation'Last;
end AML_Retained_Identities;
