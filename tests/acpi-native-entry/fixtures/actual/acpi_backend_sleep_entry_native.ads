pragma Ada_2022;
with Interfaces;
with CuBit.Handles;
with ACPI_Backend_Catalog;
with ACPI_Backend_Sleep_Entry;
with ACPI_Backend_Windows;
with ACPI_Sleep_Entry;
generic
   with package Catalog is new ACPI_Backend_Catalog
     (Memory_Authority => CuBit.Handles.Device_Memory,
      No_Memory => CuBit.Handles.No_Device_Memory,
      Port_Authority => CuBit.Handles.IO_Ports,
      No_Ports => CuBit.Handles.No_IO_Ports, others => <>);
-- Backend-internal binding only. No public endpoint dispatches these callback
-- entries. Every transfer view must be the immediate result of Owner's exact
-- Select_Access call in the serialized, retained hardware owner.
package ACPI_Backend_Sleep_Entry_Native is
   function View_Allowed (View : Catalog.Bindings.Access_View) return Boolean;
   procedure Transfer_View (View : Catalog.Bindings.Access_View;
      Width : ACPI_Backend_Windows.T.Access_Width; For_Write : Boolean;
      Input : Interfaces.Unsigned_64; Output : out Interfaces.Unsigned_64;
      Status : out ACPI_Sleep_Entry.Completion);
   procedure Read_Clock (Now : out Interfaces.Unsigned_64; Available : out Boolean);
   package Owner is new ACPI_Backend_Sleep_Entry
     (Catalog.Context, Catalog.Bindings.Access_View, View_Allowed,
      Catalog.Select_Access, Transfer_View, Read_Clock);
end ACPI_Backend_Sleep_Entry_Native;
