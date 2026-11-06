pragma Ada_2022;
with AML_Field_Access;
with AML_Field_Layout;
with ACPI_Capacity_Diagnostics;
with ACPI_Firmware_Data_Transport;
with ACPI_Firmware_Data_Field_Client;
with ACPI_Firmware_Data_Provider;
with ACPI_Global_Lock_Protocol;
generic
   type Endpoint is private;
   No_Endpoint : Endpoint;
   type Channel is limited private;
   with package RPC is new ACPI_Firmware_Data_Transport (Channel => Channel,others => <>);
   -- Passive endpoint validation/binding only: no RPC or new authority.
   with procedure Initialize_Connection (Connection : in out Channel;
      Target : Endpoint; Accepted : out Boolean);
   with procedure Read_Clock (Now : out RPC.IPC.Time_Instant; Available : out Boolean);
   type Declaration is private;
   with function Original_Declaration (Item : Declaration;
      Geometry : AML_Field_Layout.Register_Geometry) return Boolean;
   Allow_Writes : Boolean := False;
package ACPI_Firmware_Data_Startup is
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   package IPC renames RPC.IPC;
   -- One package instance is one serialized frontend startup lifetime. These
   -- actuals match ACPI_Field_Startup_Owner; no caller can replace the session,
   -- endpoint, catalog or original startup deadline after initialization.
   procedure Initialize (Destination : Endpoint; Symbol : A.Endpoint_ID;
      Deadline : IPC.Time_Instant; Accepted : in out Boolean);
   procedure Inspect (Is_Ready, Is_Pending : in out Boolean;
      Observation : in out A.Observation; Capacity : in out ACPI_Capacity_Diagnostics.Report);
   procedure Describe (Target : A.Target; Geometry : in out L.Register_Geometry; Accepted : in out Boolean);
   procedure Authorize (Target : A.Target; For_Write : Boolean; Accepted : in out Boolean);
   procedure Dispatch (Item : A.Request; Reply : in out A.Response);
   procedure Resolve (Item : Declaration; Geometry : L.Register_Geometry;
      Target : in out A.Target; Accepted : in out Boolean);
   -- Existing physical guard already accepted. Metadata is borrowed; this
   -- adapter never takes a physical lock. Nested data scopes clamp themselves.
   procedure Begin_Field (Requires_Lock, For_Write : Boolean; Deadline : IPC.Time_Instant;
      Ticket : ACPI_Global_Lock_Protocol.Ticket; Accepted : in out Boolean);
   procedure End_Field (Accepted : in out Boolean);
   procedure Cleanup (Accepted : in out Boolean);
   -- Passive copied diagnostic text only; no clock/RPC/cleanup or policy use.
   function Diagnostic_Summary return String;
   function Diagnostic_Line (Ordinal : Positive) return String;
   function Completed_Fault return A.Observation;
private
   Connection : Channel;
   package Client is new ACPI_Firmware_Data_Field_Client (Channel,RPC,Connection);
   function Session_Pending (IO : Client.Context) return Boolean;
   procedure Publish (IO : in out Client.Context; Region : RPC.F.L.Region; Deadline : IPC.Time_Instant;
      Record_ID : out RPC.F.Identifier; Status : out A.Completion);
   procedure Begin_Data (IO : in out Client.Context; Requires_Lock : Boolean;
      Deadline : IPC.Time_Instant; Domain, Sequence : RPC.F.Word; Accepted : out Boolean);
   procedure Datum (IO : in out Client.Context; Record_ID : RPC.F.Identifier; Offset : RPC.F.Word;
      Width : RPC.F.T.Access_Width; For_Write : Boolean; Input : RPC.F.Word;
      Value : out RPC.F.Word; Status : out A.Completion);
   procedure End_Data (IO : in out Client.Context; Deadline : IPC.Time_Instant; Accepted : out Boolean);
   procedure Stop_Data (IO : in out Client.Context; Deadline : IPC.Time_Instant; Accepted : out Boolean);
   procedure Publication_Deadline (Deadline : out IPC.Time_Instant);
   procedure Field_Context (Requires_Lock, For_Write : Boolean; Deadline : out IPC.Time_Instant;
      Domain, Sequence : out RPC.F.Word; Accepted : out Boolean);
   procedure Cleanup_Deadline (Deadline : out IPC.Time_Instant);
   procedure Session_Capacity (IO : Client.Context; Capacity : in out ACPI_Capacity_Diagnostics.Report);
   package Provider is new ACPI_Firmware_Data_Provider
      (Client.Context,Declaration,Original_Declaration,Session_Pending,Publish,Begin_Data,Datum,
       End_Data,Stop_Data,Publication_Deadline,Field_Context,Cleanup_Deadline,Allow_Writes,Inspect_Session_Capacity => Session_Capacity);
   Owner : Provider.Context;
   type Activity is (Idle, Initializing, Resolving, Beginning, Dispatching, Ending, Cleaning);
   Phase : Activity := Idle;
   Consumed, Initialized, Working, Stopped, Clock_Broken : Boolean := False;
   Startup_Until, Last_Clock, Field_Until : IPC.Time_Instant := 0;
   Field_Ticket : ACPI_Global_Lock_Protocol.Ticket := ACPI_Global_Lock_Protocol.No_Ticket;
end ACPI_Firmware_Data_Startup;
