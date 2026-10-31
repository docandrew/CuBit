pragma Ada_2022;
with Interfaces;
with ACPI_FADT;
with ACPI_Snapshot_Reader;
generic
   with procedure Clock (Now : out ACPI_Snapshot_Reader.Tick; Available : out Boolean);
   Chunk_Max : Positive;
package ACPI_Backend_FADT_Source is
   package R renames ACPI_Snapshot_Reader;
   package F renames R.F;
   subtype Count is Interfaces.Unsigned_64;
   type Inventory is record
      Ready : Boolean := False;
      Tables, Total_Bytes, Largest_Table : Count := 0;
   end record;
   type Description is record
      Name : F.Signature := "    ";
      Revision : F.Byte := 0;
      Extent : Count := 0;
   end record;
   type Outcome is (Not_Attempted, Metadata_Ready, FADT_Ready, Already_Consumed,
      Wrong_Order, Source_Refused, Inventory_Not_Ready, Invalid_Inventory,
      Inspection_Budget_Exceeded, Invalid_Description, Missing_FADT, Duplicate_FADT,
      Representation_Limit, Inventory_Mismatch, Wrong_Capacity,
      Clock_Unavailable, Clock_Reversed, Deadline_Expired, Table_Refused, FADT_Refused);
   type Report is record
      Status : Outcome := Not_Attempted;
      Descriptions : Natural := 0;
      Selected : Boolean := False;
      ID : Positive := 1;
      Required_Bytes : Count := 0;
      Source_Status : R.Copy_Status := R.Copy_OK;
      Copy : R.Report;
   end record;
   type Context is limited private;
   function Observe (Run : Context) return Report;
   generic
      with procedure Info (Item : out Inventory; Status : out R.Copy_Status);
      with procedure Describe (ID : Positive; Item : out Description; Status : out R.Copy_Status);
      with procedure Read_Part
        (ID : R.S.Table_ID; Offset : Natural; Into : out F.Bytes;
         Copied : out R.Byte_Count; Status : out R.Copy_Status);
   package Using_Source is
      -- Immutable, sealed, same authority throughout this context. Callbacks
      -- are bounded, serialized and never retain caller buffers or reenter.
      -- Budget limits inventory-scan descriptions, not table bytes. Read also
      -- rechecks the selected description once. Unrelated large SDTs
      -- need not fit local buffer indexing; only the selected FADT must fit.
      procedure Inspect
        (Run : in out Context; Deadline : R.Tick; Budget : Natural; Result : out Report);
      -- Caller allocates exactly Required_Bytes. One copy attempt only, using
      -- the same overall deadline and existing Snapshot_Reader validation.
      -- All failures zero Data and Description; no partial FADT is published.
      procedure Read
        (Run : in out Context; Data : out F.Bytes;
         Item : out ACPI_FADT.Description; Result : out Report);
   end Using_Source;
private
   type Phase is (Fresh, Inspecting, Awaiting_Data, Reading, Finished);
   type Context is limited record
      Stage : Phase := Fresh;
      Info : Report;
      Catalog : Inventory;
      Selected : Description;
      Deadline, Last : R.Tick := 0;
      Seen_Clock : Boolean := False;
   end record;
end ACPI_Backend_FADT_Source;
