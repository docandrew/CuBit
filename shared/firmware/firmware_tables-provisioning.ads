pragma Ada_2022;
with Firmware_Tables.Catalog;

-- Storage requirements, not permission to read physical memory. Only sealed
-- catalogs contribute requirements; no partial inventory may be provisioned.
package Firmware_Tables.Provisioning with SPARK_Mode, Pure is
   subtype Byte_Count is Long_Long_Integer range
     0 .. Long_Long_Integer (Catalog.Max_Tables) * Long_Long_Integer (Positive'Last);
   type Requirements is record
      Tables : Natural range 0 .. Catalog.Max_Tables := 0;
      Bytes : Byte_Count := 0;
      Largest : Natural := 0;
   end record;

   function Prefix_Bytes (S : Catalog.State; N : Natural) return Byte_Count
     with Ghost, Pre => N <= Catalog.Count (S),
       Subprogram_Variant => (Decreases => N),
       Post => Prefix_Bytes'Result >= Byte_Count (N) * Table_Header_Size
         and then Prefix_Bytes'Result <= Byte_Count (N) * Byte_Count (Positive'Last);

   function Measure (S : Catalog.State) return Requirements
     with Post => Measure'Result.Tables = Catalog.Count (S)
       and then Measure'Result.Bytes = Prefix_Bytes (S, Catalog.Count (S))
       and then Measure'Result.Bytes >=
         Byte_Count (Measure'Result.Tables) * Table_Header_Size
       and then Measure'Result.Bytes <=
         Byte_Count (Measure'Result.Tables) * Byte_Count (Positive'Last)
       and then (if Catalog.Count (S) = 0 then Measure'Result.Largest = 0)
       and then (if Catalog.Count (S) > 0 then
         (for some I in 1 .. Catalog.Count (S) =>
           Catalog.Item (S, I).Extent = Measure'Result.Largest))
       and then (for all I in 1 .. Catalog.Count (S) =>
         Catalog.Item (S, I).Extent <= Measure'Result.Largest);

   type Decision is (Inventory_Unavailable, Table_Quota, Byte_Quota,
                     Individual_Quota, Approved);
   type Plan is record
      Needed : Requirements;
      Status : Decision := Inventory_Unavailable;
   end record;
   -- Quotas are local resource policy, not limits imposed by ACPI. Needed is
   -- retained on rejection, even if the total cannot fit an Ada array bound.
   function Select_Capacity
     (S : Catalog.State; Table_Limit, Byte_Limit, Individual_Limit : Positive)
      return Plan
     with Post => Select_Capacity'Result.Needed = Measure (S)
       and then
         (Select_Capacity'Result.Status = Approved) =
           (Catalog.Count (S) > 0
            and then Measure (S).Tables <= Table_Limit
            and then Measure (S).Bytes <= Byte_Count (Byte_Limit)
            and then Measure (S).Largest <= Individual_Limit);
end Firmware_Tables.Provisioning;
