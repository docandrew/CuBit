pragma Ada_2022;
with Interfaces;
with CuBit.Handles;
with CuBit.Process_IDs;
with ACPI_PCI_Protocol;
package ACPI_Native_PCI_Windows is
   subtype Word is Interfaces.Unsigned_64;
   type Phase is (Fresh, Serving, Failed);
   type Statistics is record
      Stage : Phase := Fresh;
      Working : Boolean := False;
      Rows, Mapped, Refused, Cleanup_Pending : Natural := 0;
   end record;
   function Observe return Statistics;
   -- Trusted one-shot ownership of an exclusive, unused device arena.
   -- Scope is the frontend's independently provisioned kernel badge; Epoch
   -- identifies this backend lifetime. No request supplies virtual addresses.
   procedure Initialize
     (Endpoint : CuBit.Handles.Endpoint; Server : CuBit.Process_IDs.Process_ID;
      Scope, Epoch, First, Limit : Word; Accepted : out Boolean);
   procedure Dispatch
     (Scope : Word; Item : ACPI_PCI_Protocol.Packet;
      Result : out ACPI_PCI_Protocol.Packet; Allow_Setup : Boolean := True);
   -- Allow_Setup is trusted local admission, never a wire flag. False keeps
   -- retained metadata/reads usable but forbids acquiring another bus.
   -- Rows, mappings and successfully installed handles last until process
   -- exit. Failed mapping tickets are quarantined and are never reused.
end ACPI_Native_PCI_Windows;
