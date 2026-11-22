pragma Ada_2022;
package body AML_Table_Sources with SPARK_Mode is
   function Capacity (Limit : Quotas) return Natural is
     (Limit.Boot + Limit.Copied);
   function Counts_Fit (Limit : Quotas; Count : Table_Counts) return Boolean is
     (Fits (Limit) and then Count.Boot <= Limit.Boot
       and then Count.Copied <= Limit.Copied);
   function Present (Limit : Quotas; Count : Table_Counts; Item : Source)
     return Boolean is
   begin
      if not Counts_Fit (Limit, Count) then return False; end if;
      case Item.Kind is
         when No_Source => return False;
         when Boot_Source => return Natural (Item.Boot) <= Count.Boot;
         when Copied_Source => return Natural (Item.Copied) <= Count.Copied;
      end case;
   end Present;
   function Slot_For (Limit : Quotas; Count : Table_Counts; Item : Source)
     return Registry_Slot is
   begin
      if not Present (Limit, Count, Item) then return No_Slot; end if;
      case Item.Kind is
         when No_Source => return No_Slot;
         when Boot_Source => return Registry_Slot (Item.Boot);
         when Copied_Source =>
            return Registry_Slot (Limit.Boot + Natural (Item.Copied));
      end case;
   end Slot_For;
   function Source_For (Limit : Quotas; Count : Table_Counts; Slot : Registry_Slot)
     return Source is
      Index : constant Natural := Natural (Slot);
   begin
      if not Counts_Fit (Limit, Count) or else Slot = No_Slot
        or else Index > Capacity (Limit)
      then return Absent_Source; end if;
      if Index <= Limit.Boot then
         if Index > Count.Boot then return Absent_Source; end if;
         return (Boot_Source, Boot_Table_ID (Index));
      end if;
      if Index - Limit.Boot > Count.Copied then return Absent_Source; end if;
      return (Copied_Source, Copied_Table_ID (Index - Limit.Boot));
   end Source_For;
end AML_Table_Sources;
