pragma Ada_2022;
with ACPI_Firmware_Data_Frames;
with ACPI_Target_Storage;
generic
   with procedure Clock (Now : out ACPI_Firmware_Data_Frames.IPC.Time_Instant;
      Available : out Boolean);
   with function Authorized (Caller : ACPI_Firmware_Data_Frames.Word;
      For_Write : Boolean) return Boolean;
   -- Bind to the EXISTING physical global-lock request owner's Check_Access.
   -- No acquisition/release, register address or new physical lock owner here.
   with procedure Check_Lease (Caller, Domain, Sequence : ACPI_Firmware_Data_Frames.Word;
      Accepted : out Boolean);
   with procedure Check_Selection (Caller : ACPI_Firmware_Data_Frames.Word;
      Record_ID : ACPI_Firmware_Data_Frames.Identifier;
      Offset : ACPI_Firmware_Data_Frames.Word;
      Width : ACPI_Firmware_Data_Frames.T.Access_Width; Accepted : out Boolean);
   with procedure Transfer_Record (Caller : ACPI_Firmware_Data_Frames.Word;
      Record_ID : ACPI_Firmware_Data_Frames.Identifier;
      Offset : ACPI_Firmware_Data_Frames.Word;
      Width : ACPI_Firmware_Data_Frames.T.Access_Width;
      For_Write : Boolean; Input : ACPI_Firmware_Data_Frames.Word;
      Deadline : ACPI_Firmware_Data_Frames.IPC.Time_Instant;
      Output : out ACPI_Firmware_Data_Frames.Word;
      Status : out ACPI_Firmware_Data_Frames.IPC.Outcome);
   Initial_Scopes : Positive := 1;
   Initial_Descriptors : Positive := 1;
-- Serialized backend-internal data admission. IDs last for this endpoint's
-- process lifetime; no reset, release, mutable descriptor or lease renewal.
package ACPI_Firmware_Data_Scopes with SPARK_Mode is
   package F renames ACPI_Firmware_Data_Frames;
   package IPC renames F.IPC;
   subtype Word is F.Word;
   subtype Identifier is F.Identifier;
   type Outcome is (Ready, Closed, Stopped, Not_Found, Invalid_Request,
      Request_Changed, Denied, Clock_Unavailable, Clock_Reversed, Expired,
      Lease_Refused, Selection_Refused, Allocation_Refused, Reentered);
   type Reply is record
      Status : Outcome := Not_Found;
      ID : Identifier := F.No_ID;
   end record;
   type Context is limited private;
   type Statistics is record
      Scopes, Descriptors, Scope_Capacity, Descriptor_Capacity : Natural;
      Working, Active, Clock_Broken : Boolean;
   end record;
   function Observe (Owner : Context) return Statistics;
   type Capacity_Problem is (No_Capacity_Failure, Process_Heap_Limit,
      Entry_Limit, Byte_Limit);
   type Capacity_Report is record
      Problem : Capacity_Problem := No_Capacity_Failure;
      Demand, Capacity, Requested_Bytes : Word := 0;
   end record;
   function Observe_Scope_Capacity (Owner : Context) return Capacity_Report;
   function Observe_Descriptor_Capacity (Owner : Context) return Capacity_Report;
   procedure Begin_Scope (Owner : in out Context; Caller : Word;
      Client_ID : Identifier; Deadline : IPC.Time_Instant; Domain, Sequence : Word;
      Result : out Reply);
   -- Passive history, not evidence of a fresh physical lease or time sample.
   function Describe_Scope (Owner : Context; Caller : Word;
      Client_ID : Identifier) return Reply;
   procedure Select_Datum (Owner : in out Context; Caller : Word;
      Scope_ID, Client_ID, Record_ID : Identifier; Offset : Word;
      Width : F.T.Access_Width; Result : out Reply);
   -- Data admission only. Physical lock release stays with its existing owner.
   procedure Close_Scope (Owner : in out Context; Caller : Word;
      Scope_ID : Identifier; Result : out Reply);
   -- Used as the retained History.Execute callback, never directly exposed as
   -- a retryable IPC handler. The history owns at-most-once operation identity.
   procedure Perform (Owner : in out Context; Caller : Word;
      Descriptor : Identifier; For_Write : Boolean; Input : Word;
      Output : out Word; Status : out IPC.Outcome);
   -- Bind Records.Scope_Usable to this fresh check while Perform is active.
   -- No caller-selected scope or cached permission is accepted at effect time.
   procedure Check_Active (Owner : in out Context; Caller : Word;
      For_Write : Boolean; Accepted : out Boolean);
private
   type Scope_Record is record
      Caller : Word := 0;
      Client_ID : Identifier := F.No_ID;
      Deadline : IPC.Time_Instant := 0;
      Domain, Sequence : Word := 0;
      Status : Outcome := Stopped;
   end record;
   type Descriptor_Record is record
      Caller : Word := 0;
      Client_ID, Scope_ID, Record_ID : Identifier := F.No_ID;
      Offset : Word := 0;
      Width : F.T.Access_Width := F.T.Byte_Access;
   end record;
   package Scope_Store is new ACPI_Target_Storage (Scope_Record, Initial_Scopes);
   package Descriptor_Store is new ACPI_Target_Storage
      (Descriptor_Record, Initial_Descriptors);
   type Context is limited record
      Scopes : Scope_Store.Context;
      Descriptors : Descriptor_Store.Context;
      Working, Checking, Active, Active_Failed : Boolean := False;
      Active_Caller : Word := 0;
      Active_Scope : Natural := 0;
      Active_Write : Boolean := False;
      Seen_Clock, Clock_Broken : Boolean := False;
      Last_Clock : IPC.Time_Instant := 0;
   end record;
end ACPI_Firmware_Data_Scopes;
