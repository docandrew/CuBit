pragma Ada_2022;
package body ACPI_Snapshot_Manifest with SPARK_Mode is
   use type Firmware_Tables.Snapshots.Table_ID;

   function Valid (Items : Catalog; Expected_Count : Natural) return Boolean is
   begin
      if Expected_Count = 0 or else Items'Length /= Expected_Count then
         return False;
      end if;
      for I in Items'Range loop
         if Items (I).ID /= Firmware_Tables.Snapshots.Table_ID
             (I - Items'First + 1)
           or else Items (I).Name = "FACS"
           or else (if I = Items'First then Items (I).Name /= "DSDT"
                    else Items (I).Name = "DSDT")
         then
            return False;
         end if;
      end loop;
      return True;
   end Valid;

   function Analyze
     (Items : Catalog; Expected_Count : Natural; Limits : Quotas) return Plan
   is
      Result : Plan;
   begin
      if Expected_Count = 0 or else Items'Length /= Expected_Count then
         return Result;
      end if;
      if not Valid (Items, Expected_Count) then
         Result.Status := Malformed;
         return Result;
      end if;
      for Item of Items loop
         Result.Needed.Tables := Result.Needed.Tables + 1;
         Result.Needed.Bytes := Result.Needed.Bytes + Byte_Count (Item.Extent);
         Result.Needed.Largest := Natural'Max
           (Result.Needed.Largest, Item.Extent);
      end loop;
      Result.Status :=
        (if Result.Needed.Tables > Limits.Tables then Table_Quota
         elsif Result.Needed.Bytes > Byte_Count (Limits.Bytes) then Byte_Quota
         elsif Result.Needed.Largest > Limits.Individual then Individual_Quota
         else Approved);
      return Result;
   end Analyze;
end ACPI_Snapshot_Manifest;
