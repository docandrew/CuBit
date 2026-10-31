package body ACPI_Device_Policy with SPARK_Mode is
   function Decide (Flags : Interfaces.Unsigned_64) return Status_Decision is
     (if (Flags and Present_Mask) /= 0 then Initialize_And_Descend
      elsif (Flags and Functioning_Mask) /= 0 then Descend_Only else Prune_Subtree);
end ACPI_Device_Policy;
