pragma Ada_2022;
with CuBit.Handles.IPC;
with CuBit.Handles.Devices;
with CuBit.Monotonic;
with ACPI_Backend_Firmware_Native;
package body ACPI_Backend_Kernel_Handles is
   package B renames ACPI_Backend_Binding_Types;
   use type CuBit.Handles.ABI.Kind;
   use type Interfaces.Unsigned_64;
   function Decode (Item : CuBit.Handles.IPC.Description; Expected : CuBit.Handles.ABI.Kind)
     return B.Description is
   begin
      if not Item.Current then return (others => <>); end if;
      return (Current => True,
        Kind => (if Item.Kind /= Expected then B.No_Resource
          elsif Expected = CuBit.Handles.ABI.Device_Memory then B.Memory_Resource else B.Port_Resource),
        Base => Item.Object, Length => Item.Parameter,
        Readable => Item.Held.Read, Writable => Item.Held.Write, Delegable => Item.Held.Grant);
   end Decode;
   procedure Inspect_Memory
     (Authority : CuBit.Handles.Device_Memory; Item : out B.Description) is
      package F renames ACPI_Backend_Firmware_Native;
      Actual : constant F.Policy.Report := F.Inspect (Authority);
      use type F.Policy.Outcome;
   begin
      Item := (others => <>);
      if Actual.Status /= F.Policy.Admitted then return; end if;
      Item := (True, B.Memory_Resource,
        Interfaces.Unsigned_64 (Actual.Memory.First_Page) * F.Policy.M.Page_Bytes,
        Interfaces.Unsigned_64 (Actual.Memory.Pages) * F.Policy.M.Page_Bytes,
        True, F.Policy.M.Writable_Of (Actual.Memory.Kind), False);
   end Inspect_Memory;
   procedure Inspect_Ports
     (Authority : CuBit.Handles.IO_Ports; Item : out B.Description) is
   begin Item := Decode (CuBit.Handles.IPC.Describe (Authority), CuBit.Handles.ABI.IO_Ports); end Inspect_Ports;
   procedure Map_Memory
     (Authority : CuBit.Handles.Device_Memory; Physical, Virtual, Pages : Interfaces.Unsigned_64;
      Read_Only : Boolean; Mapped : out Boolean) is
   begin Mapped := CuBit.Handles.Devices.Map_Device (Authority, Physical, Virtual, Pages, Read_Only); end Map_Memory;
   procedure Read_Clock (Now : out B.Tick; Available : out Boolean) is
      Sample : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
   begin
      Now := 0; Available := Sample.Available;
      if Sample.Available then Now := B.Tick (Sample.Microseconds); end if;
   end Read_Clock;
end ACPI_Backend_Kernel_Handles;
