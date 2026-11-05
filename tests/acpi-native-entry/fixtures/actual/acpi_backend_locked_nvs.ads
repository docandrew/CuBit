pragma Ada_2022;
with Interfaces;
with ACPI_Backend_NVS_Records;
with ACPI_Global_Lock_Owner;
with ACPI_Global_Lock_Scoped_Sessions;
with ACPI_Global_Lock_Requests;
with ACPI_Locked_NVS;
with ACPI_Locked_NVS_Protocol;
generic
   with package Records is new ACPI_Backend_NVS_Records (<>);
   Record_State : in out Records.Context;
   type Principal is private;
   No_Principal : Principal;
   type Scope_Tag is new Interfaces.Unsigned_64;
   No_Scope : Scope_Tag;
   with package Owner is new ACPI_Global_Lock_Owner
     (Principal => Scope_Tag, No_Principal => No_Scope, others => <>);
   with package Sessions is new ACPI_Global_Lock_Scoped_Sessions
     (Principal, No_Principal, Scope_Tag, No_Scope, Owner);
   with package Requests is new ACPI_Global_Lock_Requests
     (Principal, No_Principal, Scope_Tag, No_Scope, Owner, Sessions, others => <>);
package ACPI_Backend_Locked_NVS with SPARK_Mode is
   -- These trusted callbacks close over the same retained named-record owner
   -- used by the ordinary NVS metadata endpoint; no new mapping or data owner.
   package Callbacks is
      procedure Describe_Datum
        (Scope : Scope_Tag; ID : ACPI_Locked_NVS_Protocol.Record_ID;
         Datum : ACPI_Locked_NVS_Protocol.Geometry;
         Epoch : out Interfaces.Unsigned_64; Accepted : out Boolean);
      procedure Read_Datum
        (Scope : Scope_Tag; Epoch : Interfaces.Unsigned_64;
         ID : ACPI_Locked_NVS_Protocol.Record_ID; Datum : ACPI_Locked_NVS_Protocol.Geometry;
         Value : out Interfaces.Unsigned_64; Completed : out Boolean);
   end Callbacks;
   package Service is new ACPI_Locked_NVS
     (Principal, No_Principal, Scope_Tag, No_Scope, Owner, Sessions, Requests,
      Callbacks.Describe_Datum, Callbacks.Read_Datum);
end ACPI_Backend_Locked_NVS;
