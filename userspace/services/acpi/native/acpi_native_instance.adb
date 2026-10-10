with ACPI_Endpoint;
with ACPI_Requests;
with ACPI_Service;
with ACPI_Native_Blocks;
with ACPI_Native_Server;
with CuBit.Messages;
package body ACPI_Native_Instance is
   -- Fixed, process-lifetime limited storage creates one owned arena in place.
   -- Discovery-sized allocation remains future bootstrap work; construction
   -- never copies an initialized arena or requests implicit heap storage.
   Server : ACPI_Requests.State
     (Table_Capacity => ACPI_Service.Max_Tables,
      Byte_Capacity => ACPI_Service.Max_Total_Bytes,
      Table_Byte_Limit => ACPI_Service.Max_Table_Bytes, Initial_Revision => 0);
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
