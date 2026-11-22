pragma Ada_2022;
package ACPI_Capacity_Growth with SPARK_Mode is
   -- Representation ceiling only. The process allocator enforces the actual
   -- memory budget; Seed is an allocation hint, never an admission limit.
   subtype Count is Natural;
   function Next_Capacity
     (Current, Demand : Count; Seed : Positive) return Count
     with Post => Next_Capacity'Result >= Current
       and then Next_Capacity'Result >= Demand
       and then (if Demand <= Current then Next_Capacity'Result = Current);
end ACPI_Capacity_Growth;
