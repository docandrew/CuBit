pragma Ada_2022;
with Interfaces;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Messages;
with ACPI_Firmware_Data_Native;
with ACPI_Firmware_Data_Client;
with ACPI_Firmware_Data_Arena;
with ACPI_Firmware_Data_Mapping_Native;
with ACPI_Firmware_Data_Records;
with ACPI_Firmware_Data_Record_Access;
with ACPI_Firmware_Data_Publications;
with ACPI_Firmware_Data_Scopes;
with ACPI_Firmware_Data_History;
with ACPI_Firmware_Data_Dispatch;
with ACPI_Firmware_Data_Receive_Native;
with ACPI_Firmware_Data_Replies;
generic
   with function Close_Carried (Item : CuBit.Handles.IPC.Carried)
      return CuBit.Handles.IPC.Close_Result;
   -- Actual must check the EXISTING physical request owner's live ticket.
   -- This assembly never instantiates, acquires or releases a physical lock.
   with procedure Check_Lease (Caller, Domain, Sequence : Interfaces.Unsigned_64; Accepted : out Boolean);
   with function Caller_Allowed (Caller : Interfaces.Unsigned_64) return Boolean;
   -- Passive current admission state from the existing serialized loop.
   with function New_Effects_Allowed return Boolean;
   Maximum_Map_Chunk : Positive;
   Allow_Writes : Boolean := False;
package ACPI_Native_Firmware_Backend is
   subtype Word is Interfaces.Unsigned_64;
   type Phase is (Fresh, Ready, Failed);
   type Initialization_Outcome is (Not_Attempted,Initialized,Endpoint_Rejected,Arena_Rejected);
   type Report is record
      Stage : Phase := Fresh;
      Initialization : Initialization_Outcome := Not_Attempted;
      Working, Pending_Setup, Retained_Unattached : Boolean := False;
      Publications, Records, Scopes, Descriptors, Operations : Natural := 0;
      Accepted, Malformed, Denied, Reentered : Word := 0;
   end record;
   function Observe return Report;
   -- Approved_Endpoint is the launcher's separately badged hardware-data
   -- entry, NOT ordinary device-manager role7. Native.Initialize validates it.
   -- First/Limit come only from the sealed catalog's one-shot unused device
   -- arena handoff; no IPC payload can supply these virtual bounds.
   procedure Initialize (Approved_Endpoint : CuBit.Handles.Endpoint;
      First, Limit : Word; Accepted : out Boolean);
   -- Insert before the legacy generic quiescing fallback. Kernel metadata
   -- authenticates From/authorityTag; passive queries/Close remain available
   -- while New_Effects_Allowed=False. No second receive loop is created.
   procedure Additional_Request (From : CuBit.Messages.Process_ID;
      Request : CuBit.Messages.Message; Response : in out CuBit.Messages.Message;
      Handled : in out Boolean);
private
   package Wire renames ACPI_Firmware_Data_Replies;
   package F renames Wire.F;
   package Native is new ACPI_Firmware_Data_Native (Close_Carried);
   No_Carried : constant CuBit.Handles.IPC.Carried := (Kind => CuBit.Handles.IPC.Absent);
   package Client is new ACPI_Firmware_Data_Client
      (Native.State,CuBit.Handles.IPC.Carried,No_Carried,Native.Present,
       Native.Clock,Native.Exchange,Native.Inspect,Native.Dispose);
   package Arena is new ACPI_Firmware_Data_Arena;
   Reservations : Arena.Context;
   package Mapping is new ACPI_Firmware_Data_Mapping_Native (Native,Arena,Reservations,Maximum_Map_Chunk);
   function Scope_Usable (Requester : Word; For_Write : Boolean) return Boolean;
   package Records is new ACPI_Firmware_Data_Records
      (CuBit.Handles.IPC.Carried,No_Carried,Native.Present,Native.Inspect,
       Arena.Reservation,Arena.No_Reservation,Mapping.Describe_Reservation,
       Mapping.Map,Native.Clock,Scope_Usable,Mapping.Transfer);
   Rows : Records.Context;
   package Accesses is new ACPI_Firmware_Data_Record_Access (Records,Rows);
   function Authorized (Caller : Word) return Boolean;
   function Effects_Authorized (Caller : Word; For_Write : Boolean) return Boolean;
   function May_Publish (Requester : Word) return Boolean;
   package Publications is new ACPI_Firmware_Data_Publications
      (Native.State,CuBit.Handles.IPC.Carried,No_Carried,Client,Arena,Reservations,Records,Rows,May_Publish);
   Publication_State : Publications.Context;
   Broker : Native.State;
   package Scopes is new ACPI_Firmware_Data_Scopes
      (Native.Clock,Effects_Authorized,Check_Lease,Accesses.Check_Selection,Accesses.Transfer_Record);
   Data_State : Scopes.Context;
   procedure Execute_Effect (Caller : Word; Descriptor : F.Identifier; For_Write : Boolean;
      Input : Word; Value : out Word; Status : out F.IPC.Outcome);
   package History is new ACPI_Firmware_Data_History (Execute_Effect);
   Operations : History.Context;
   procedure Handle (Caller : Word; Item : F.Request; Status : out Wire.Outcome;
      Effect : out Wire.Effect_Kind; Value : out Word);
   package Dispatch is new ACPI_Firmware_Data_Dispatch (Authorized,Handle);
   Dispatcher : Dispatch.Context;
   procedure Dispatch_Frame (Caller : Word; Request : F.IPC.Packet; Has_Capability : Boolean;
      Response : out F.IPC.Packet; Handled : out Boolean);
   package Receiver is new ACPI_Firmware_Data_Receive_Native (Dispatch_Frame);
   Stage : Phase := Fresh;
   Initializing : Boolean := False;
   Startup_Result : Initialization_Outcome := Not_Attempted;
end ACPI_Native_Firmware_Backend;
