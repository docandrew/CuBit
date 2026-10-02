with Interfaces;
with ACPI_Endpoint;
with ACPI_Requests;
with ACPI_Service;
with CuBit.Messages;
with CuBit.Memory_Grants;
-- Native trusted boundary, serialized by the service loop. The supervisor must
-- bind Provider_Slot and Config.Provider_Tag to the same snapshot provider.
-- The provider must keep the grant bytes immutable until import completes.
-- Acquisition pins lifetime, not contents. This package is not SPARK-proved.
package ACPI_Native_Blocks is
   type State is limited private;
   type Import_Status is (Rejected, Acquisition_Failed, Processed, Cleanup_Pending);
   Import_Table_Grant : constant Interfaces.Unsigned_32 := 8;
   -- Four-word request: [revision, generation*2**32+slot, table ID,
   -- kind*2**32+byte length]. Kind: 0=DSDT, 1=SSDT, 2=Description.
   -- No unused reference bits, addresses or offsets are accepted. Config and
   -- Provider_Slot come from trusted startup; Stamp comes from kernel IPC.
   procedure Dispatch
     (Adapter : in out State; Server : in out ACPI_Requests.State;
      Config : ACPI_Endpoint.Configuration; Stamp : Interfaces.Unsigned_64;
      Provider_Slot : CuBit.Messages.CapabilitySlot;
      Request : ACPI_Requests.Packet; Reply : out ACPI_Requests.Packet);
   function Pending (Adapter : State) return Boolean;
   -- Retry a previous return before accepting another grant. Never discard
   -- Adapter while Pending, except during process teardown handled by kernel.
   procedure Retry_Return (Adapter : in out State);
   -- Stamp comes from a kernel-received message, not its payload. Remaining
   -- arguments are decoded metadata; Reference is identity, never authority.
   -- Stale tokens, exhausted revisions and invalid phases are rejected before
   -- acquisition. The serialized service core rechecks admission after mapping.
   -- Reply carries the service result for Rejected/Processed/Cleanup_Pending;
   -- ignore Reply on Acquisition_Failed. Cleanup_Pending means import already
   -- ran: retry only Return, never repeat the import under a new revision.
   procedure Import_Grant
     (Adapter : in out State; Server : in out ACPI_Requests.State;
      Config : ACPI_Endpoint.Configuration; Stamp : Interfaces.Unsigned_64;
      Provider_Slot : CuBit.Messages.CapabilitySlot;
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Token : Interfaces.Unsigned_64; ID : Positive;
      Kind : ACPI_Service.Table_Kind; Length : Natural;
      Reply : out ACPI_Requests.Packet; Status : out Import_Status);
private
   type State is limited record
      Held : Boolean := False;
      Reference : CuBit.Memory_Grants.Grant_Reference;
   end record;
end ACPI_Native_Blocks;
