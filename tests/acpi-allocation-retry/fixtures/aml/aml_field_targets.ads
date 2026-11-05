pragma Ada_2022;
with AML_Field_Access;
with AML_Field_Plans;
package AML_Field_Targets with SPARK_Mode, Pure is
   type Vector is array (AML_Field_Plans.Datum_Position) of AML_Field_Access.Target;
end AML_Field_Targets;
