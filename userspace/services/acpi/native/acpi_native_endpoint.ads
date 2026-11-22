with ACPI_Requests;
with ACPI_Native_Cached_Power;
with ACPI_Native_Cached_Battery;
with ACPI_Serving_State;
with Interfaces;
with CuBit.Messages;
-- Request and From must come from the actual kernel receive. From supplies
-- the expected grant owner and refusal identity. Observer authority comes only
-- from the kernel-stamped authorityTag, never a PID or request word.
package ACPI_Native_Endpoint is
   -- Native-only page: [revision, mode, reason, runtime-admitted].
   Metrics_Serving_State_Page : constant Interfaces.Unsigned_64 := 19;
   -- Cached lid/AC: [page,max-age,subpage,expected-cache-generation].
   Metrics_Lid_Page : constant Interfaces.Unsigned_64 := ACPI_Native_Cached_Power.Metrics_Lid_Page;
   Metrics_Source_Page : constant Interfaces.Unsigned_64 := ACPI_Native_Cached_Power.Metrics_Source_Page;
   -- [page,max-age,slot/subpage selector,expected-inventory-generation].
   Metrics_Battery_Page : constant Interfaces.Unsigned_64 := ACPI_Native_Cached_Battery.Metrics_Battery_Page;
   procedure Select_Runtime(Runtime_Admitted:Boolean;Accepted:out Boolean);
   procedure Select_Observer(Facts:ACPI_Serving_State.Eligibility;
      Physical_Retirement_Confirmed:Boolean;Accepted:out Boolean);
   function Serving_State return ACPI_Serving_State.Snapshot;
   function Reply_Now return Boolean;
   procedure Dispatch
     (Server : in out ACPI_Requests.State;
      From : CuBit.Messages.Process_ID;
      Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message);
   -- Trusted local scheduler gate. Observer reads remain available; no new
   -- power preparation/entry or passive loan starts while a metadata/query
   -- owner excludes other work. Cached_Button_Watches_Open separately permits
   -- copied button facts and nonblocking kernel reply operations in selected
   -- runtime mode; it never opens AML, hardware or peer calls. Drain leaves it
   -- closed. Neither permission can be opened by a wire word.
   procedure Dispatch_With_Admission
     (Server : in out ACPI_Requests.State; From : CuBit.Messages.Process_ID;
      Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message;
      Admission_Open : Boolean; Cached_Button_Watches_Open : Boolean := False;
      Cached_Shutdown_Open : Boolean := False);
end ACPI_Native_Endpoint;
