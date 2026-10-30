pragma Ada_2022;
with Interfaces;
with ACPI_Requests;
with ACPI_Protocol;
with ACPI_Endpoint;
-- Stamp is exclusively the kernel-stamped received-message authorityTag.
-- This adapter has NO runtime role configuration or snapshot upload role.
package ACPI_Observer_Endpoint with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   use type ACPI_Requests.State_Model;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_16;
   use type ACPI_Requests.Words;
   procedure Dispatch
     (Server : in out ACPI_Requests.State; Stamp : ACPI_Protocol.Role_Tag;
      Request : ACPI_Requests.Packet; Reply : out ACPI_Requests.Packet)
   with Pre => ACPI_Requests.Valid (Server),
     Post => ACPI_Requests.Valid (Server)
       and then ACPI_Requests.Model (Server) = ACPI_Requests.Model (Server)'Old
       and then Reply.Length = 4 and then Reply.Flags = 0 and then Reply.Reserved = 0
       and then Reply.Label in ACPI_Endpoint.Reply_OK | ACPI_Endpoint.Reply_Error
       and then (if not ACPI_Protocol.Is_Observer (Stamp) then
         Reply.Label = ACPI_Endpoint.Reply_Error and then
         Reply.Data = ACPI_Requests.Words'
           [Interfaces.Unsigned_64 (ACPI_Requests.Outcome'Pos (ACPI_Requests.Denied)), 0, 0, 0]);
end ACPI_Observer_Endpoint;
