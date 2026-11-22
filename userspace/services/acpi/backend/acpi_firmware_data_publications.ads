pragma Ada_2022;
with Interfaces;
with ACPI_Firmware_Data_Client;
with ACPI_Firmware_Data_Arena;
with ACPI_Firmware_Data_Records;
with ACPI_Target_Storage;
generic
   type Transport is limited private;
   type Capability is private;
   No_Capability : Capability;
   with package Client is new ACPI_Firmware_Data_Client
     (Transport => Transport, Capability => Capability, No_Capability => No_Capability, others => <>);
   with package Arena is new ACPI_Firmware_Data_Arena (<>);
   Reservations : in out Arena.Context;
   with package Records is new ACPI_Firmware_Data_Records
     (Capability => Capability, No_Capability => No_Capability,
      Reservation => Arena.Reservation, No_Reservation => Arena.No_Reservation, others => <>);
   Rows : in out Records.Context;
   with function May_Publish (Requester : Interfaces.Unsigned_64) return Boolean;
   Initial_Publications : Positive := 1;
-- Trusted backend orchestration, independent of wire labels. Requester is the
-- full authenticated caller word, never a payload field. The frontend remains
-- responsible for original sealed-table declaration provenance.
package ACPI_Firmware_Data_Publications is
   package R renames Client.R;
   package L renames Records.L;
   subtype Word is Interfaces.Unsigned_64;
   type Publication_ID is new Word;
   No_Publication : constant Publication_ID := 0;
   type Phase is (Preparing, Requesting, Grant_Received, Mapping_Pending,
      Published, Refused, Stopped);
   type Outcome is (Record_Ready, Pending, Backend_Refused, Publication_Stopped,
      Capacity_Refused, Invalid_Request, Invalid_Deadline, Request_Changed,
      Not_Found, Busy, Reentered, Denied);
   type Reply is record
      Status : Outcome := Not_Found;
      ID : Records.Record_ID := Records.No_Record;
      Stage : Phase := Preparing;
      Remote : R.Wire.Disposition := R.Wire.Malformed;
      Request_Result : R.Outcome := R.Not_Ready;
      Record_Result : Records.Outcome := Records.Not_Ready;
      Arena_Result : Arena.Outcome := Arena.Not_Ready;
   end record;
   type Context is limited private;
   type Statistics is record
      Publications, Capacity, Pending_Ordinal, Retained_Unattached : Natural;
      Working, Grant_Cleanup_Due : Boolean;
   end record;
   function Observe (Owner : Context) return Statistics;
   procedure Publish (Owner : in out Context; Channel : in out Transport;
      Requester : Word; ID : Publication_ID; Logical : L.Region;
      Deadline : R.IPC.Time_Instant; Result : out Reply);
   -- Passive history: never sends IPC, maps, retries or issues authority.
   procedure Describe (Owner : in out Context; Requester : Word; ID : Publication_ID; Result : out Reply);
   -- Explicit bounded setup continuation, not a replay of AML or a Field data
   -- operation. Lost grant requests retain their original devmgr ID/envelope.
   -- Any possibly attempted mapping is terminal and is never tried again.
   procedure Recover (Owner : in out Context; Channel : in out Transport;
      Requester : Word; ID : Publication_ID; Deadline : R.IPC.Time_Instant; Result : out Reply);
private
   type Entry_Record is record
      Scope : Word := 0;
      ID : Publication_ID := No_Publication;
      Logical : L.Region;
      Row : Records.Record_ID := Records.No_Record;
      VA : Arena.Reservation := Arena.No_Reservation;
      Has_VA : Boolean := False;
      Stage : Phase := Preparing;
      -- Owns a taken capability until Attach moves it into the reserved row.
      -- Attach failure retains this token in the journal for process lifetime.
      Held : Capability := No_Capability;
      Remote : R.Wire.Disposition := R.Wire.Malformed;
      Grant_Request : R.P.Request_ID := R.P.No_Request;
      Request_Result : R.Outcome := R.Not_Ready;
      Record_Result : Records.Outcome := Records.Not_Ready;
      Arena_Result : Arena.Outcome := Arena.Not_Ready;
   end record;
   package Journal is new ACPI_Target_Storage (Entry_Record, Initial_Publications);
   type Context is limited record
      Items : Journal.Context;
      Grants : Client.State;
      Pending_Ordinal : Natural := 0;
      Working : Boolean := False;
   end record;
end ACPI_Firmware_Data_Publications;
