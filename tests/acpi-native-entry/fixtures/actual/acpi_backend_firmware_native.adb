pragma Ada_2022;
with CuBit.Handles.IPC;
package body ACPI_Backend_Firmware_Native is
   use type CuBit.Handles.ABI.Kind;
   function Inspect (Held : CuBit.Handles.Device_Memory) return Policy.Report is
      Actual : constant CuBit.Handles.IPC.Description := CuBit.Handles.IPC.Describe (Held);
   begin
      if not Actual.Current then return (Status => Policy.Not_Current); end if;
      return Policy.Decode
        ((Current => True, Memory_Handle => Actual.Kind = CuBit.Handles.ABI.Device_Memory,
          Rights => (Actual.Held.Read, Actual.Held.Write, Actual.Held.Execute,
            Actual.Held.Grant, Actual.Held.Revoke),
          Tag => Actual.Authority_Tag, First => Actual.Object, Bytes => Actual.Parameter));
   end Inspect;
   function Inspect_Claim
     (Held : CuBit.Handles.Device_Memory; First_Page, Pages : Interfaces.Unsigned_64;
      Kind : Policy.M.Memory_Kind; Cache : Policy.M.Cache_Mode) return Policy.Report is
     (Policy.Check_Claim (Inspect (Held), First_Page, Pages, Kind, Cache));
end ACPI_Backend_Firmware_Native;
