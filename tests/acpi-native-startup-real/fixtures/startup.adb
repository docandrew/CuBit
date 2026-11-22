package body Startup is
   function Hardware return CuBit.ACPI_Windows.Grant is
      Item:CuBit.ACPI_Windows.Grant;
   begin
      Item.IO_Used:=1;
      Item.IO(1):=(CuBit.Handles.Testing.Ports,CuBit.Handles.Testing.Port_First,CuBit.Handles.Testing.Port_Count);
      return Item;
   end Hardware;
end Startup;
