pragma Ada_2022;
package body ACPI_Region_Stop_Policy with SPARK_Mode is
   function First_Missing (Facts : Evidence) return Refusal is
   begin
      for Item in Requirement loop
         if not Facts (Item) then return (True, Item); end if;
         pragma Loop_Invariant (for all Prior in Requirement'First .. Item => Facts (Prior));
      end loop;
      return (Missing => False);
   end First_Missing;
end ACPI_Region_Stop_Policy;
