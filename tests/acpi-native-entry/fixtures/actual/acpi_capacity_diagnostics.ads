pragma Ada_2022;
with Interfaces;
package ACPI_Capacity_Diagnostics with SPARK_Mode is
   type Cache_Kind is (No_Cache, NVS_Targets, Combined_Targets);
   type Failure_Kind is (No_Failure, Process_Heap, Entry_Representation, Byte_Representation);
   type Entry_Count is new Interfaces.Unsigned_64;
   type Byte_Count is new Interfaces.Unsigned_64;
   type Report is record
      Cache : Cache_Kind := No_Cache;
      Failure : Failure_Kind := No_Failure;
      Demand, Capacity, Entry_Limit : Entry_Count := 0;
      Requested_Bytes, Byte_Limit : Byte_Count := 0;
   end record;
   No_Report : constant Report := (others => <>);
   function Present (Item : Report) return Boolean is
     (Item.Cache /= No_Cache and then Item.Failure /= No_Failure);
   -- Pure formatting of a copied diagnostic. An allocator refusal does not
   -- reveal which other allocation consumed the process heap budget.
   function Format (Item : Report) return String;
end ACPI_Capacity_Diagnostics;
