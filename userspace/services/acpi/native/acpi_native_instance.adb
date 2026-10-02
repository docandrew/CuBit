with ACPI_Endpoint;
with ACPI_Requests;
with ACPI_Service;
with ACPI_Native_Blocks;
with ACPI_Native_Server;
with CuBit.Messages;
package body ACPI_Native_Instance is
   -- Keep the current startup instance in static storage. An unconstrained
   -- library-level object initialized by Fresh would request an implicit heap
   -- allocation, which this freestanding runtime does not provide. Discovery-
   -- sized allocation must be explicit in the future bootstrap adapter.
   Server : ACPI_Requests.State
     (ACPI_Service.Max_Tables, ACPI_Service.Max_Total_Bytes, ACPI_Service.Max_Table_Bytes)
     := ACPI_Requests.Fresh;
   Adapter : ACPI_Native_Blocks.State;
   Started : Boolean := False;
   procedure Start is
      Config : ACPI_Endpoint.Configuration;
      Provider_Slot : CuBit.Messages.CapabilitySlot;
      Accepted : Boolean;
      Reason : ACPI_Native_Server.Stop_Reason;
   begin
      if Started then return; end if;
      Started := True;
      ACPI_Native_Server.Await_Configuration (Config, Provider_Slot, Accepted);
      if not Accepted then
         CuBit.Messages.debugPrint ("acpi: bootstrap unavailable" & ASCII.LF);
         return;
      end if;
      ACPI_Native_Server.Run (Server, Adapter, Config, Provider_Slot, Reason);
      CuBit.Messages.debugPrint ("acpi: stopped: " & Reason'Image & ASCII.LF);
   end Start;
end ACPI_Native_Instance;
