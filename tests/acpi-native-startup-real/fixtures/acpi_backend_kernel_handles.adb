pragma Ada_2022;
with CuBit.Handles.Testing;with CuBit.Monotonic;with Owner_Test_Control;
package body ACPI_Backend_Kernel_Handles is
   package B renames ACPI_Backend_Binding_Types;
   package T renames CuBit.Handles.Testing;
   use type CuBit.Handles.IO_Ports;
   procedure Inspect_Memory(Authority:CuBit.Handles.Device_Memory;Item:out B.Description) is
      pragma Unreferenced(Authority);
   begin Item:=(others=><>);end Inspect_Memory;
   procedure Inspect_Ports(Authority:CuBit.Handles.IO_Ports;Item:out B.Description) is
   begin
      Owner_Test_Control.Port_Inspections:=Owner_Test_Control.Port_Inspections+1;
      Item:=(others=><>);
      if Authority=T.Ports then Item:=(True,B.Port_Resource,Interfaces.Unsigned_64(T.Port_First),
        Interfaces.Unsigned_64(T.Port_Count),True,True,False);end if;
   end Inspect_Ports;
   procedure Map_Memory(Authority:CuBit.Handles.Device_Memory;Physical,Virtual,Pages:Interfaces.Unsigned_64;
      Read_Only:Boolean;Mapped:out Boolean) is
      pragma Unreferenced(Authority,Physical,Virtual,Pages,Read_Only,Mapped);
   begin raise Program_Error with "fixture must not map hardware memory";end Map_Memory;
   procedure Read_Clock(Now:out B.Tick;Available:out Boolean) is
      Sample:constant CuBit.Monotonic.Reading:=CuBit.Monotonic.Read;
   begin Available:=Sample.Available;Now:=0;if Available then Now:=B.Tick(Sample.Microseconds);end if;end Read_Clock;
end ACPI_Backend_Kernel_Handles;
