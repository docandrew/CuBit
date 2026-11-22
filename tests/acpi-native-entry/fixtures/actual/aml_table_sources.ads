pragma Ada_2022;
-- Local source identity only. No address, hardware authority, DDB handle,
-- definition identity or registry reservation is represented by these values.
package AML_Table_Sources with SPARK_Mode, Pure is
   type Boot_Table_ID is new Positive;
   type Copied_Table_ID is new Positive;
   type Source_Kind is (No_Source, Boot_Source, Copied_Source);
   type Source (Kind : Source_Kind := No_Source) is record
      case Kind is
         when No_Source => null;
         when Boot_Source => Boot : Boot_Table_ID;
         when Copied_Source => Copied : Copied_Table_ID;
      end case;
   end record;
   Absent_Source : constant Source := (Kind => No_Source);
   type Table_Counts is record
      Boot, Copied : Natural := 0;
   end record;
   subtype Quotas is Table_Counts;
   type Registry_Slot is new Natural;
   No_Slot : constant Registry_Slot := 0;
   function Fits (Limit : Quotas) return Boolean is
     (Limit.Copied <= Natural'Last - Limit.Boot);
   function Capacity (Limit : Quotas) return Natural
     with Pre => Fits (Limit),
       Post => Capacity'Result = Limit.Boot + Limit.Copied;
   -- Limits are fixed for an owner lifetime. Counts may grow within them.
   -- An invalid count/overflowing configuration or absent source fails closed.
   -- Unused boot quota slots stay holes: copied slots start after BOOT QUOTA,
   -- never after current count, so growing either inventory cannot move a slot.
   function Present (Limit : Quotas; Count : Table_Counts; Item : Source)
     return Boolean
     with Post => Present'Result =
       (Fits (Limit) and then Count.Boot <= Limit.Boot
        and then Count.Copied <= Limit.Copied
        and then (case Item.Kind is
          when No_Source => False,
          when Boot_Source => Natural (Item.Boot) <= Count.Boot,
          when Copied_Source => Natural (Item.Copied) <= Count.Copied));
   function Slot_For (Limit : Quotas; Count : Table_Counts; Item : Source)
     return Registry_Slot
     with Post => (Slot_For'Result /= No_Slot) = Present (Limit, Count, Item)
       and then Slot_For'Result =
         (if not Present (Limit, Count, Item) then No_Slot else
           (case Item.Kind is
              when No_Source => No_Slot,
              when Boot_Source => Registry_Slot (Item.Boot),
              when Copied_Source => Registry_Slot (Limit.Boot + Natural (Item.Copied))));
   function Source_For (Limit : Quotas; Count : Table_Counts; Slot : Registry_Slot)
     return Source
     with Post => (if Source_For'Result.Kind /= No_Source then
       Present (Limit, Count, Source_For'Result)
       and then Slot_For (Limit, Count, Source_For'Result) = Slot);
end AML_Table_Sources;
