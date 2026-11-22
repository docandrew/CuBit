pragma Ada_2022;
with ACPI_Requests;
with ACPI_Host_Bridge_Owners;
-- Trusted process-local callbacks for the one existing native runtime. They
-- create no namespace, Field provider, delay domain, endpoint or event loop.
package ACPI_Native_Host_Bridge_Windows is
   package O renames ACPI_Host_Bridge_Owners;
   function Metadata_Ready (Server : ACPI_Requests.State) return Boolean;
   function Query_Ready (Server : ACPI_Requests.State) return Boolean;
   function Current return Boolean;
   procedure Clock (Microseconds, Milliseconds : out O.W.Word; Available : out Boolean);
   procedure Begin_Window (Server : in out ACPI_Requests.State;
      Deadline_Microseconds : O.W.Word; Result : out O.Window_Result);
   procedure End_Window (Server : in out ACPI_Requests.State;
      Healthy : Boolean; Closed : out Boolean; Now_Milliseconds : out O.W.Word);
   -- The serving loop must still enforce the manager's Blocks_AML across
   -- capture/query, and keep Server at its original stable address. No caller
   -- may independently open/close this window or replace its delay domain.
end ACPI_Native_Host_Bridge_Windows;
