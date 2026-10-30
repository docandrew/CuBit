pragma Ada_2022;
with Interfaces;
package ACPI_Device_Policy with Pure, SPARK_Mode is
   use type Interfaces.Unsigned_64;
   subtype Status_Flags is Interfaces.Unsigned_64;
   Present_Mask : constant Status_Flags := 16#01#;
   Functioning_Mask : constant Status_Flags := 16#08#;
   Missing_Status_Default : constant Status_Flags := 16#FFFF_FFFF#;
   type Status_Decision is (Initialize_And_Descend, Descend_Only, Prune_Subtree);
   function Decide (Flags : Interfaces.Unsigned_64) return Status_Decision
     with Global => null,
       Contract_Cases =>
         ((Flags and Present_Mask) /= 0 => Decide'Result = Initialize_And_Descend,
          (Flags and Present_Mask) = 0 and (Flags and Functioning_Mask) /= 0
            => Decide'Result = Descend_Only,
          (Flags and Present_Mask) = 0 and (Flags and Functioning_Mask) = 0
            => Decide'Result = Prune_Subtree);
   -- These are outcomes supplied by a trusted region coordinator, not authority.
   type Region_Outcome is (Regions_Connected, No_Regions_Required,
      Regions_Unavailable, Region_Initialization_Failed);
end ACPI_Device_Policy;
