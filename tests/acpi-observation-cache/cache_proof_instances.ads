with ACPI_Sample_Cache;
with ACPI_Observation_Values;
package Cache_Proof_Instances with SPARK_Mode is
   package Lid is new ACPI_Sample_Cache
     (ACPI_Observation_Values.Lid_Sample, ACPI_Observation_Values.Empty_Lid_Sample);
   package Power is new ACPI_Sample_Cache
     (ACPI_Observation_Values.Power_Source_Sample, ACPI_Observation_Values.Empty_Power_Source_Sample);
end Cache_Proof_Instances;
