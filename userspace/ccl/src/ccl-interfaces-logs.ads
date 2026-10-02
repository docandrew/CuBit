with Interfaces;
with CCL.Catalog;
with CCL.Objects;
with CCL.Types;

--  The logs interface: (logs.recent "service") returns the recent records of
--  one service as a typed List<LogEntry> (userspace/ccl/interfaces/
--  logs.schema). A host (the Workbench) answers it from logstore with an
--  observer's authority; this package holds only the types, the catalog
--  publication and the image a host builds.
package CCL.Interfaces.Logs with SPARK_Mode is
   use Standard.Interfaces;

   --  SHA-256 of logs.schema: its types' schema key and the interface's
   --  digest.
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#FFFB_3B9C_5ACC_9568#, 16#2851_E401_4433_082B#,
      16#535F_A49B_2220_1931#, 16#DD23_1FA9_A3AE_D99E#];
   SCHEMA_KEY : constant CCL.Objects.Schema_Key :=
     [16#FFFB_3B9C_5ACC_9568#, 16#2851_E401_4433_082B#,
      16#535F_A49B_2220_1931#, 16#DD23_1FA9_A3AE_D99E#];

   --  Severity's members, in order (CuBit.Log_Records.Severity).
   type Severity is (Trace, Debug, Information, Warning, Error, Critical);

   --  A service name in (logs.recent "name").
   MAX_SERVICE_NAME : constant := 64;

   --  Cells one entry takes in an image: the record, its time, its severity
   --  (member and unit payload), its source and its message.
   ENTRY_CELLS : constant := 6;
   --  The entries one result can carry: the list's count cell, then entries.
   MAX_ENTRIES : constant := (CCL.Objects.Maximum_Cells - 1) / ENTRY_CELLS;
   subtype Entry_Count is Natural range 0 .. MAX_ENTRIES;

   --  Severity, LogEntry and List-LogEntry in Types, and the binding of the
   --  list type to SCHEMA_KEY.
   procedure Define_Types
     (Types : in out CCL.Types.Registry; Entries : out CCL.Types.Type_Reference;
      Contract : out CCL.Objects.Binding; Accepted : out Boolean);

   --  The schema and the logs interface (recent), for discovery only: a host
   --  separately installs a binding for the operations it answers.
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);

   --  Building a result: Start an empty list, Add entries while Added, the
   --  image is then complete and valid for the contract.
   procedure Start (Contract : CCL.Objects.Binding; Image : out CCL.Objects.Image);
   procedure Add
     (Image : in out CCL.Objects.Image; Time : Unsigned_64; Level : Severity;
      Source : Unsigned_64; Message : String; Added : out Boolean);
end CCL.Interfaces.Logs;
