pragma Ada_2022;
package body Root_Export_Admission with SPARK_Mode is
   function Choose (Device, Firmware : Root_Demand) return Selection is
   begin
      if Fits (Device, Firmware) then
         return (Admitted,
                 Launch_Configuration.Root_Count (Device),
                 Launch_Configuration.Root_Count (Firmware));
      end if;
      return (Inventory_Capacity, 0, 0);
   end Choose;
end Root_Export_Admission;
