with ACPI_Requests;
with ACPI_Serving_State;
with Interfaces;
with CuBit.Messages;
-- Request and From must come from the actual kernel receive. From supplies
-- the expected grant owner and refusal identity. Observer authority comes only
-- from the kernel-stamped authorityTag, never a PID or request word.
package ACPI_Native_Endpoint is
   -- Native-only page: [revision, mode, reason, runtime-admitted].
   Metrics_Serving_State_Page : constant Interfaces.Unsigned_64 := 19;
   procedure Select_Runtime(Runtime_Admitted:Boolean;Accepted:out Boolean);
   procedure Select_Observer(Facts:ACPI_Serving_State.Eligibility;
      Physical_Retirement_Confirmed:Boolean;Accepted:out Boolean);
   function Serving_State return ACPI_Serving_State.Snapshot;
   function Reply_Now return Boolean;
   procedure Dispatch
     (Server : in out ACPI_Requests.State;
      From : CuBit.Messages.Process_ID;
      Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message);
end ACPI_Native_Endpoint;
