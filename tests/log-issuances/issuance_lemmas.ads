--  Proof obligation: every publisher in the driver budget has its own tag.
pragma Ada_2022;
with Driver_Log_Issuances; use Driver_Log_Issuances;
package Issuance_Lemmas with SPARK_Mode is
   pragma Assert (Fixed_Unique);
end Issuance_Lemmas;
