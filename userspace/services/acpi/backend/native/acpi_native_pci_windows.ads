pragma Ada_2022;
with Interfaces;
with CuBit.Handles;
with CuBit.Process_IDs;
with ACPI_PCI_Protocol;
with ACPI_PCI_Bus_Geometry;
with ACPI_Local_PCI_Results;
package ACPI_Native_PCI_Windows is
   subtype Word is Interfaces.Unsigned_64;
   type Phase is (Fresh, Serving, Failed);
   type Statistics is record
      Stage : Phase := Fresh;
      Working : Boolean := False;
      Data_Stopped : Boolean := False;
      Rows, Mapped, Refused, Cleanup_Pending : Natural := 0;
   end record;
   function Observe return Statistics;
   function Pending return Boolean; -- O(1), no row/profile scan.
   -- Trusted one-shot ownership of an exclusive, unused device arena.
   -- Scope is the frontend's independently provisioned kernel badge; Epoch
   -- identifies this backend lifetime. No request supplies virtual addresses.
   procedure Initialize
     (Endpoint : CuBit.Handles.Endpoint; Server : CuBit.Process_IDs.Process_ID;
      Scope, Epoch, First, Limit : Word; Accepted : out Boolean);
   subtype Local_Window is ACPI_Local_PCI_Results.Window_Description;
   procedure Prepare_Local (Scope : Word; Key : ACPI_PCI_Protocol.P.Bus_Key;
      Setup_Deadline_Milliseconds : ACPI_PCI_Protocol.Wire.Time_Instant;
      Allow_Setup : Boolean; Result : out Local_Window);
   procedure Describe_Local (Scope,Epoch,ID : Word; Result : out Local_Window);
   procedure Read_Local (Scope,Epoch,ID : Word; Region : ACPI_PCI_Protocol.P.Region;
      Offset : Word; Width : ACPI_PCI_Protocol.P.T.Access_Width;
      Deadline_Microseconds : Word; Guard : not null access function return Boolean;
      Result : out ACPI_Local_PCI_Results.Result);
   -- Local typed reads never replace caller segment/bus. The sole owner keeps
   -- the original handle/mapping. Setup may call only downward to its broker;
   -- callers must refuse new setup while any Field lease is held/pending.
   procedure Dispatch
     (Scope : Word; Item : ACPI_PCI_Protocol.Packet;
      Result : out ACPI_PCI_Protocol.Packet; Allow_Setup : Boolean := True);
   -- Allow_Setup is trusted local admission, never a wire flag. False keeps
   -- retained metadata/reads usable but forbids acquiring another bus.
   -- Rows, mappings and successfully installed handles last until process
   -- exit. Failed mapping tickets are quarantined and are never reused.
end ACPI_Native_PCI_Windows;
