pragma Ada_2022;
with Interfaces;
with ACPI_Backend_Binding_Types;
with ACPI_Firmware_Data_Layouts;
with ACPI_PCI_Config_Reads;
-- Pure policy for partitioning the sealed catalog's unused device VA tail.
-- These ranges are metadata; only the trusted native owner may hand them off.
package ACPI_Backend_Device_Arenas with Pure, SPARK_Mode is
   subtype Word is Interfaces.Unsigned_64;
   use type Word;
   Page_Bytes : constant Word := ACPI_Firmware_Data_Layouts.Page_Bytes;
   Minimum_PCI_Pages : constant Word := ACPI_PCI_Config_Reads.Bus_Bytes / Page_Bytes;
   type Window is record
      First, Limit : Word := 0;
   end record;
   function Valid (Item : Window) return Boolean is
     (Item.First /= 0 and then Item.First < Item.Limit
      and then Item.Limit <= ACPI_Backend_Binding_Types.User_Virtual_Limit
      and then Item.First mod Page_Bytes = 0 and then Item.Limit mod Page_Bytes = 0);
   function Inside (Item, Parent : Window) return Boolean is
     (Valid (Item) and then Valid (Parent)
      and then Item.First >= Parent.First and then Item.Limit <= Parent.Limit);
   type Outcome is (Invalid_Arena, Ready);
   type Selection is record
      Status : Outcome := Invalid_Arena;
      Has_PCI, Has_Data : Boolean := False;
      PCI, Data : Window;
   end record;
   -- Data keeps the whole range when an optional PCI partition is too small.
   -- A single consumer receives the whole range; two consumers split on a
   -- page boundary, with PCI admitted only when it can hold a complete bus.
   function Plan (Available : Window; Want_PCI, Want_Data : Boolean) return Selection
     with Post =>
       (if Plan'Result.Status = Ready then Valid (Available)
        and then (if Plan'Result.Has_PCI then Want_PCI
          and then Inside (Plan'Result.PCI, Available)
          and then (Plan'Result.PCI.Limit - Plan'Result.PCI.First) / Page_Bytes >= Minimum_PCI_Pages)
        and then (if Plan'Result.Has_Data then Want_Data
          and then Inside (Plan'Result.Data, Available))
        and then (if Plan'Result.Has_PCI and Plan'Result.Has_Data then
          Plan'Result.PCI.Limit = Plan'Result.Data.First)
        and then Plan'Result.Has_Data = Want_Data);
end ACPI_Backend_Device_Arenas;
