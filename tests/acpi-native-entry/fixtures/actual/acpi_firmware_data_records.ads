pragma Ada_2022;
with Interfaces;
with ACPI_Firmware_Data_Requests;
with ACPI_Firmware_Data_Layouts;
with ACPI_Target_Storage;
generic
   type Capability is private;
   No_Capability : Capability;
   with function Present (Item : Capability) return Boolean;
   with procedure Inspect (Item : Capability; Evidence : out ACPI_Firmware_Data_Requests.Grant_Evidence);
   type Reservation is private;
   No_Reservation : Reservation;
   with procedure Describe_Reservation (Item : Reservation; Base, Pages : out Interfaces.Unsigned_64;
      Accepted : out Boolean);
   with procedure Map (Held : Capability; Item : Reservation;
      Physical, Virtual, Pages : Interfaces.Unsigned_64;
      Deadline : ACPI_Firmware_Data_Requests.IPC.Time_Instant; Completed : out Boolean);
   with procedure Clock (Now : out ACPI_Firmware_Data_Requests.IPC.Time_Instant; Available : out Boolean);
   with function Scope_Usable (Requester : Interfaces.Unsigned_64; For_Write : Boolean) return Boolean;
   with procedure Transfer (Virtual : Interfaces.Unsigned_64;
      Width : ACPI_Firmware_Data_Layouts.T.Access_Width; For_Write : Boolean;
      Input : Interfaces.Unsigned_64; Output : out Interfaces.Unsigned_64; Completed : out Boolean);
   Initial_Records : Positive := 1;
-- Backend internal owner, not a wire-selected address API. Requester is the
-- complete opaque authenticated caller identity, never narrowed or word-supplied.
-- Capability/Reservation actuals are plain metadata tokens, not controlled or
-- access-bearing values. Process lifetime retains every attempted mapping.
package ACPI_Firmware_Data_Records with SPARK_Mode is
   package R renames ACPI_Firmware_Data_Requests;
   package L renames ACPI_Firmware_Data_Layouts;
   package P renames R.P;
   subtype Word is Interfaces.Unsigned_64;
   type Record_ID is new Word;
   No_Record : constant Record_ID := 0;
   type Phase is (Reserved, Attached, Mapping_Stopped, Ready);
   type Outcome is (Record_Reserved, Already_Present, Grant_Attached, Mapped, Done,
      Not_Ready, Invalid_Region, Invalid_Record, Wrong_Requester, Invalid_Grant,
      Allocation_Refused, Invalid_Reservation, Reservation_Overlap, Cache_Conflict,
      Invalid_Deadline, Clock_Unavailable, Clock_Reversed, Expired,
      Mapping_Indeterminate, Transfer_Indeterminate, Late_Completed,
      Already_Attempted, Reentered, Invalid_Access, Denied);
   type Context is limited private;
   type Operation is limited private;
   type Statistics is record
      Records, Capacity : Natural;
      Working, Clock_Broken : Boolean;
   end record;
   function Observe (Owner : Context) return Statistics;
   type Capacity_Problem is (No_Capacity_Failure, Process_Heap_Limit, Entry_Limit, Byte_Limit);
   type Capacity_Report is record
      Problem : Capacity_Problem := No_Capacity_Failure;
      Demand, Capacity, Requested_Bytes : Word := 0;
   end record;
   function Observe_Capacity (Owner : Context) return Capacity_Report;
   -- Passive metadata-only geometry check for descriptor selection. No data
   -- load, capability inspection, clock/lease callback, allocation or exposed
   -- virtual address. A True result is not live permission for an effect.
   function Selection_Valid (Owner : Context; Requester : Word; ID : Record_ID;
      Offset : Word; Width : L.T.Access_Width) return Boolean;
   -- Reserve a growable record before requesting external authority. An exact
   -- duplicate returns the existing ID, including its stopped/pending state.
   procedure Reserve (Owner : in out Context; Requester : Word; Item : L.Region;
      ID : out Record_ID; Result : out Outcome);
   procedure Attach (Owner : in out Context; Requester : Word; ID : Record_ID;
      Held : in out Capability; Disposition : R.Wire.Disposition; Result : out Outcome);
   -- Reservation comes from the backend's private VA allocator. After any Map
   -- callback, retain its whole extent/capability even on false/late completion.
   procedure Map_Record (Owner : in out Context; Requester : Word; ID : Record_ID;
      Item : Reservation; Deadline : R.IPC.Time_Instant; Result : out Outcome);
   -- One retained Operation per authenticated protocol operation. It cannot be
   -- reset or replayed; an unknown write may already have affected hardware.
   procedure Access_Record (Owner : in out Context; Attempt : in out Operation;
      Requester : Word; ID : Record_ID; Offset : Word; Width : L.T.Access_Width;
      For_Write : Boolean; Input : Word; Deadline : R.IPC.Time_Instant;
      Output : out Word; Result : out Outcome);
private
   type Entry_Record is record
      Scope : Word := 0;
      Logical : L.Region;
      Stage : Phase := Reserved;
      Held : Capability := No_Capability;
      Kind : R.Wire.Disposition := R.Wire.Malformed;
      VA : Word := 0;
      Reserved_VA : Reservation := No_Reservation;
   end record;
   package Store is new ACPI_Target_Storage (Entry_Record, Initial_Records);
   type Context is limited record
      Items : Store.Context;
      Working, Seen_Clock, Clock_Broken : Boolean := False;
      Last_Clock : R.IPC.Time_Instant := 0;
   end record;
   type Operation is limited record
      Consumed : Boolean := False;
   end record;
end ACPI_Firmware_Data_Records;
