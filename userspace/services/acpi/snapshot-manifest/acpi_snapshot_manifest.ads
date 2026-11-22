pragma Ada_2022;
with Firmware_Tables.Snapshots;

-- Address-free preflight before constructing provider and service storage.
-- Metadata is trusted only after receipt on the authenticated snapshot export
-- interface. Neither an ordinal nor this capacity decision grants authority.
package ACPI_Snapshot_Manifest with SPARK_Mode, Pure is
   type Catalog is array (Positive range <>) of
     Firmware_Tables.Snapshots.Public_Descriptor;
   subtype Byte_Count is Long_Long_Integer range
     0 .. Long_Long_Integer (Natural'Last) * Long_Long_Integer (Positive'Last);
   type Requirements is record
      Tables : Natural := 0;
      Bytes : Byte_Count := 0;
      Largest : Natural := 0;
   end record;
   type Quotas is record
      Tables : Positive;
      Bytes : Positive;
      Individual : Positive;
   end record;
   type Outcome is
     (Incomplete, Malformed, Table_Quota, Byte_Quota, Individual_Quota, Approved);
   type Plan is record
      Status : Outcome := Incomplete;
      Needed : Requirements;
   end record;

   -- Expected_Count comes from the complete immutable export manifest, never
   -- from a prefix received so far. Array bounds need not begin at one; IDs do.
   -- DSDT must be first and unique; other entries retain their received order.
   function Valid (Items : Catalog; Expected_Count : Natural) return Boolean;
   function Analyze
     (Items : Catalog; Expected_Count : Natural; Limits : Quotas) return Plan
     with Post =>
       (if Analyze'Result.Status = Approved then
          Valid (Items, Expected_Count)
          and then Analyze'Result.Needed.Tables = Expected_Count
          and then Analyze'Result.Needed.Tables <= Limits.Tables
          and then Analyze'Result.Needed.Bytes <= Byte_Count (Limits.Bytes)
          and then Analyze'Result.Needed.Largest <= Limits.Individual);
end ACPI_Snapshot_Manifest;
