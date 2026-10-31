pragma Ada_2022;
with Interfaces;
with ACPI_Requests;
with ACPI_Bootstrap;
with ACPI_Fatal_Diagnostics;
with ACPI_Module_Diagnostics;
with ACPI_Snapshot_Reader;
-- Trusted local startup owner. No wire upload, grant, callback to a client, or
-- raw hardware authority. Heap allocation and adapter calls are trusted native
-- boundaries; this orchestrator is regression-tested, not SPARK proved.
package ACPI_Snapshot_Startup is
   package R renames ACPI_Snapshot_Reader;
   subtype Count is Interfaces.Unsigned_64;
   type Inventory is record
      Ready : Boolean := False;
      Tables, Total_Bytes, Largest_Table : Count := 0;
   end record;
   type Description is record
      Name : R.F.Signature := "    ";
      Revision : R.F.Byte := 0;
      Extent : Count := 0;
   end record;
   type State_Access is access ACPI_Requests.State;
   type Outcome is
     (Loaded, Source_Refused, Inventory_Not_Ready, Invalid_Inventory,
      Representation_Limit, Invalid_Description, Inventory_Mismatch,
      Clock_Unavailable, Clock_Reversed, Deadline_Expired,
      Copy_Refused, Admission_Refused, Startup_Refused, Startup_Already_Consumed);
   type Report is record
      Status : Outcome := Invalid_Inventory;
      Source_Status : R.Copy_Status := R.Copy_OK;
      Catalog : Inventory;
      Table : Natural := 0;
      Descriptor : Description;
      Copy : R.Report;
      Admission : ACPI_Requests.Outcome := ACPI_Requests.Malformed;
      Completion : ACPI_Bootstrap.Finish_Report;
      Fatal : ACPI_Fatal_Diagnostics.Observation;
      Module_Errors : ACPI_Module_Diagnostics.Observation;
      -- Controlled startup only: a failed attempt retains the owning arena.
      -- The caller must stop AML/serving and preserve provider/driver context.
      Owner_Retained : Boolean := False;
   end record;
   generic
      with procedure Info (Item : out Inventory; Status : out R.Copy_Status);
      with procedure Describe
        (ID : Positive; Item : out Description; Status : out R.Copy_Status);
      Chunk_Max : Positive;
      with procedure Read_Part
        (ID : R.S.Table_ID; Offset : Natural; Into : out R.F.Bytes;
         Copied : out R.Byte_Count; Status : out R.Copy_Status);
      with procedure Clock (Now : out R.Tick; Available : out Boolean);
   procedure Load
     (Deadline : R.Tick; Owner : out State_Access; Result : out Report);
   -- Owner is published only after complete local admission and initialization.
   -- On every refusal it is null and temporary bytes/partial arena are released.
   -- Caller invokes once and retains successful Owner for the process lifetime.
   -- Adapters must return normally with explicit statuses, remain bound to
   -- one immutable inventory, and never reenter or retain a buffer. Resource
   -- exhaustion and trusted-callback violations are terminal runtime failures,
   -- not Report outcomes. Never serve or retry a partially failed attempt.
private
   -- Shared implementation only. Public default Load always uses a local,
   -- hardware-free completion and releases failed owners as before.
   type Controlled_Attempt is limited record
      Started : Boolean := False;
      Owner : State_Access;
      Last : Report;
   end record;
   generic
      with procedure Info (Item : out Inventory; Status : out R.Copy_Status);
      with procedure Describe
        (ID : Positive; Item : out Description; Status : out R.Copy_Status);
      Chunk_Max : Positive;
      with procedure Read_Part
        (ID : R.S.Table_ID; Offset : Natural; Into : out R.F.Bytes;
         Copied : out R.Byte_Count; Status : out R.Copy_Status);
      with procedure Clock (Now : out R.Tick; Available : out Boolean);
      with procedure Complete_Startup
        (Server : in out ACPI_Requests.State; Reply : out ACPI_Requests.Response);
      Retain_Failed_Owner : Boolean;
   procedure Load_Using
     (Deadline : R.Tick; Owner : out State_Access; Result : out Report);
end ACPI_Snapshot_Startup;
