pragma Ada_2022;
package body Firmware_Tables.Provisioning with SPARK_Mode is
   function Prefix_Bytes (S : Catalog.State; N : Natural) return Byte_Count is
     (if N = 0 then 0
      else Prefix_Bytes (S, N - 1) + Byte_Count (Catalog.Item (S, N).Extent));

   function Measure (S : Catalog.State) return Requirements is
      R : Requirements;
   begin
      for I in 1 .. Catalog.Count (S) loop
         R.Bytes := R.Bytes + Byte_Count (Catalog.Item (S, I).Extent);
         R.Largest := Natural'Max (R.Largest, Catalog.Item (S, I).Extent);
         R.Tables := I;
         pragma Loop_Invariant (R.Tables = I);
         pragma Loop_Invariant (R.Bytes = Prefix_Bytes (S, I));
         pragma Loop_Invariant
           (R.Bytes >= Byte_Count (I) * Table_Header_Size
            and then R.Bytes <= Byte_Count (I) * Byte_Count (Positive'Last));
         pragma Loop_Invariant
           (for all J in 1 .. I => Catalog.Item (S, J).Extent <= R.Largest);
         pragma Loop_Invariant
           (for some J in 1 .. I => Catalog.Item (S, J).Extent = R.Largest);
      end loop;
      return R;
   end Measure;

   function Select_Capacity
     (S : Catalog.State; Table_Limit, Byte_Limit, Individual_Limit : Positive)
      return Plan is
      R : constant Requirements := Measure (S);
   begin
      return (Needed => R,
              Status => (if R.Tables = 0 then Inventory_Unavailable
                         elsif R.Tables > Table_Limit then Table_Quota
                         elsif R.Bytes > Byte_Count (Byte_Limit) then Byte_Quota
                         elsif R.Largest > Individual_Limit then Individual_Quota
                         else Approved));
   end Select_Capacity;
end Firmware_Tables.Provisioning;
