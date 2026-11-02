pragma Ada_2022;
with Interfaces;
package ACPI_Allocation_Diagnostics with SPARK_Mode, Pure is
   type Component_Kind is (No_Component, Snapshot_Catalog, Copied_Input, Table_Copy, AML_Owner);
   type Limit_Kind is (No_Limit, Entry_Representation, Byte_Representation, Process_Storage);
   type Demand_Unit is (Entries, Payload_Bytes, Owner_Graph_Bytes, Owner_Block_Bytes);
   type Count is new Interfaces.Unsigned_64;
   type Report is record
      Component : Component_Kind := No_Component;
      Limit : Limit_Kind := No_Limit;
      Demand : Count := 0;
      Unit : Demand_Unit := Payload_Bytes;
      Bound : Count := 0;
   end record;
   function Present (Item : Report) return Boolean is
     (Item.Component /= No_Component and then Item.Limit /= No_Limit);
   -- Requested owner-graph bytes are Ada object representations, excluding
   -- allocator bookkeeping and temporary/secondary-stack construction space.
   function Format (Item : Report) return String;
   type Value_Sample is record
      Available : Boolean := False;
      Objects_Used, Objects_Capacity : Count := 0;
      Bytes_Used, Bytes_Capacity : Count := 0;
      Elements_Used, Elements_Capacity : Count := 0;
   end record;
   -- Exit-state observation after Complete_Startup returns. A rolled-back
   -- module can erase intermediate usage; this is not a peak/refusal demand.
   function Format (Item : Value_Sample) return String;
end ACPI_Allocation_Diagnostics;
