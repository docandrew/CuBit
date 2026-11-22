pragma Ada_2022;
with Interfaces;
with ACPI_Global_Lock_Owner;
with ACPI_Global_Lock_Scoped_Sessions;
with ACPI_Global_Lock_Requests;
with ACPI_Global_Lock_Protocol;
generic
   type Principal is private;
   No_Principal : Principal;
   type Scope_Tag is private;
   No_Scope : Scope_Tag;
   with package Owner is new ACPI_Global_Lock_Owner
     (Principal => Scope_Tag, No_Principal => No_Scope, others => <>);
   with package Sessions is new ACPI_Global_Lock_Scoped_Sessions
     (Principal, No_Principal, Scope_Tag, No_Scope, Owner);
   with package Requests is new ACPI_Global_Lock_Requests
     (Principal, No_Principal, Scope_Tag, No_Scope, Owner, Sessions, others => <>);
   with procedure Clock (Now : out Interfaces.Unsigned_64; Available : out Boolean);
package ACPI_Global_Lock_Dispatch with SPARK_Mode is
   -- Scope and Caller are authenticated kernel receive metadata, never
   -- payload fields. Serialize through the backend's existing sole loop.
   procedure Handle
     (State : in out Requests.Context; Scope : Scope_Tag; Caller : Principal;
      Request : ACPI_Global_Lock_Protocol.Packet;
      Response : out ACPI_Global_Lock_Protocol.Packet; Handled : out Boolean);
end ACPI_Global_Lock_Dispatch;
