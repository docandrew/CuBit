pragma Ada_2022;
with Interfaces;
with CuBit.Handles;
with ACPI_Backend_Bindings;
with ACPI_Backend_Global_Signal;
with ACPI_PM1_Global_Release;
generic
   with package Bindings is new ACPI_Backend_Bindings
     (Memory_Authority => CuBit.Handles.Device_Memory, No_Memory => <>,
      Port_Authority => CuBit.Handles.IO_Ports, No_Ports => <>, others => <>);
   type Hardware_State is limited private;
   with function Select_Access
     (Hardware : Hardware_State; ID : Bindings.W.Window_ID; Space : Bindings.P.Address_Space;
      Address : Interfaces.Unsigned_64; Width : Bindings.T.Access_Width; For_Write : Boolean)
      return Bindings.Access_View;
package ACPI_Backend_Global_Signal_Native is
   -- Trusted callback only, never a public scalar/address write API.
   procedure Transfer_Word
     (View : Bindings.Access_View; For_Write : Boolean; Input : Interfaces.Unsigned_16;
      Output : out Interfaces.Unsigned_16; Status : out ACPI_PM1_Global_Release.Completion);
   package Owner is new ACPI_Backend_Global_Signal (Bindings, Hardware_State, Select_Access, Transfer_Word);
end ACPI_Backend_Global_Signal_Native;
