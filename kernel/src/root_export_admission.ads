pragma Ada_2022;
with Interfaces;
with Launch_Configuration;

--  A launch inventory is indivisible: admission never selects a prefix.
package Root_Export_Admission with Pure, SPARK_Mode is
   use type Launch_Configuration.Root_Count;
   type Root_Demand is new Interfaces.Unsigned_64;
   type Outcome is (Admitted, Inventory_Capacity);
   Capacity_Reason : constant String := "ROOT_INVENTORY_CAPACITY";

   type Selection is record
      Status : Outcome;
      Device, Firmware : Launch_Configuration.Root_Count;
   end record;

   function Fits (Device, Firmware : Root_Demand) return Boolean is
     (Device <= Root_Demand (Launch_Configuration.Maximum_Roots)
      and then Firmware <=
        Root_Demand (Launch_Configuration.Maximum_Roots) - Device);

   function Choose (Device, Firmware : Root_Demand) return Selection
     with Post =>
       (if Fits (Device, Firmware) then
          Choose'Result.Status = Admitted
          and then Root_Demand (Choose'Result.Device) = Device
          and then Root_Demand (Choose'Result.Firmware) = Firmware
        else
          Choose'Result.Status = Inventory_Capacity
          and then Choose'Result.Device = 0
          and then Choose'Result.Firmware = 0);
end Root_Export_Admission;
