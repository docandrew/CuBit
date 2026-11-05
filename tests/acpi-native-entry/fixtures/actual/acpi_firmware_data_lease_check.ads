pragma Ada_2022;
with ACPI_Firmware_Data_Frames;
with ACPI_Global_Lock_Owner;
with ACPI_Global_Lock_Scoped_Sessions;
with ACPI_Global_Lock_Requests;
generic
   type Principal is private;
   No_Principal : Principal;
   -- ABI conversion of the complete authenticated receive identity. Native
   -- actual is CuBit.Process_IDs.From_Word; never a slot or narrowed integer.
   with function Principal_From_Word
      (Value : ACPI_Firmware_Data_Frames.Word) return Principal;
   type Scope_Tag is private;
   No_Scope : Scope_Tag;
   with package Owner is new ACPI_Global_Lock_Owner
      (Principal => Scope_Tag, No_Principal => No_Scope, others => <>);
   with package Sessions is new ACPI_Global_Lock_Scoped_Sessions
      (Principal, No_Principal, Scope_Tag, No_Scope, Owner);
   with package Requests is new ACPI_Global_Lock_Requests
      (Principal, No_Principal, Scope_Tag, No_Scope, Owner, Sessions, others => <>);
   Lock_State : in out Requests.Context;
   Authorized_Scope : Scope_Tag;
-- Adapter over one existing retained lock request owner. Owns no physical
-- lock, configuration, acquisition, release, maintenance or ticket state.
package ACPI_Firmware_Data_Lease_Check with SPARK_Mode is
   procedure Check (Caller, Domain, Sequence : ACPI_Firmware_Data_Frames.Word;
      Accepted : out Boolean);
end ACPI_Firmware_Data_Lease_Check;
