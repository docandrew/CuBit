pragma Ada_2022;
with ACPI_Cached_Power_Codec;
with ACPI_Cached_Power_Values;
with ACPI_Protocol;
-- Transport-neutral owner adapter. Caller supplies the actual received stamp
-- and serializes this read with the same service owner; no peer call occurs.
generic
   type Owner_State (<>) is limited private;
   with package Codec is new ACPI_Cached_Power_Codec (<>);
   with procedure Read_Power (Server : Owner_State;
      Maximum_Age : ACPI_Cached_Power_Values.Microseconds;
      Value : out ACPI_Cached_Power_Values.Snapshot);
package ACPI_Cached_Power_Dispatch is
   type Outcome is (Not_Handled, Denied, Malformed, Stale, OK);
   type Reply is record
      Status : Outcome := Not_Handled;
      Data : Codec.Reply_Words := Codec.Empty_Reply;
   end record;
   procedure Dispatch (Server : Owner_State; Stamp : ACPI_Protocol.Role_Tag;
      Request : Codec.Packet; Result : out Reply; Read_Allowed : Boolean);
end ACPI_Cached_Power_Dispatch;
