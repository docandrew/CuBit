with Interfaces;
-- Owned descriptive values only: no interpreter descriptors, handles or authority.
package ACPI_Observation_Values with SPARK_Mode, Pure is
   type Lid_State is (Unknown, Closed, Open);
   type Power_Source_State is (Unknown, Offline, Online);
   type Lid_Sample is record
      State : Lid_State := Unknown;
      Raw : Interfaces.Unsigned_64 := 0;
   end record;
   type Power_Source_Sample is record
      State : Power_Source_State := Unknown;
      Raw : Interfaces.Unsigned_64 := 0;
   end record;
   Empty_Lid_Sample : constant Lid_Sample := (others => <>);
   Empty_Power_Source_Sample : constant Power_Source_Sample := (others => <>);
end ACPI_Observation_Values;
