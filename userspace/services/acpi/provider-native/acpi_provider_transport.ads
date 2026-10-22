pragma Ada_2022;
with Interfaces;
with ACPI_Requests;
with ACPI_Snapshot_Provider;
with ACPI_Notification_Consumer;
with Firmware_Tables;
with CuBit.Handles;
-- Provider-process unit only; not linked into the ACPI server. One serialized,
-- process-lifetime singleton. No reset/finalizer/free path exists. Endpoint and
-- grant owner binding are trusted launcher inputs, never payload PID authority.
package ACPI_Provider_Transport is
   Max_Catalog_Bytes : constant := 1024 * 1024;
   Max_Catalog_Tables : constant := 32;
   subtype Window is Interfaces.Unsigned_64 range 1 .. 60_000;
   type Deadlines is record
      Query_And_Start : Window := 1_000;
      Import_Table : Window := 5_000;
      Finish : Window := 30_000;
   end record;
   type Table_Slice is record
      Item : ACPI_Snapshot_Provider.Table_Descriptor;
      Offset : Natural; -- zero-based offset in already admitted immutable bytes
   end record;
   type Table_Slices is array (Positive range <>) of Table_Slice;
   type Startup_Notifications is (Disabled, Required);
   type Interest_Result is (Interest_Armed, Interest_Not_Eligible,
      Interest_Remote_Rejected, Interest_Invalid_Response,
      Interest_Unknown, Interest_Deadline_Unavailable);
   -- Bounded copied observations; session/request exhaustion fails closed.
   Notification_Path_Limit : constant := 32;
   Notification_Session_Limit : constant Interfaces.Unsigned_64 := ACPI_Requests.Max_Revision;
   Notification_Request_Limit : constant Interfaces.Unsigned_64 := ACPI_Requests.Max_Revision;
   package Notification_Policy is new ACPI_Notification_Consumer
     (Notification_Path_Limit, Notification_Session_Limit, Notification_Request_Limit);
   -- Policy types are public; the transport's actual state and endpoint are not.
   -- Required blocks all upload mutations until one known successful arming.
   -- Disabled is the explicit upload-only policy, retained for existing callers.
   procedure Initialize (Target : CuBit.Handles.Endpoint; Items : Table_Slices;
      Data : Firmware_Tables.Bytes; Limits : Deadlines; Accepted : out Boolean;
      Startup : Startup_Notifications := Disabled);
   -- Consumes the single attempt before its Idle query. Unknown/rejected or
   -- malformed outcomes permanently block Required upload; no query repairs it.
   procedure Configure_Interest (Result : out Interest_Result);
   function Last_Interest_Error return ACPI_Requests.Outcome;
   -- Only after a known exact successful Finish in Required mode. Calling
   -- again reconnects with the existing monotonic policy lifetime, never reset.
   procedure Start_Notifications (Accepted : out Boolean);
   procedure Advance_Notifications (Result : out Notification_Policy.Result);
   function Notification_Session return Notification_Policy.Word;
   function Has_Notification return Boolean;
   function Copy_Notification return Notification_Policy.Observation
      with Pre => Has_Notification;
   procedure Confirm_Notification_Delivery
     (Session : Notification_Policy.Word; ID : Notification_Policy.Receipt;
      Result : out Notification_Policy.Result);
   -- Confirmation comes only from trusted broker delivery. Handlers receive
   -- copied observations and tokens, never the provider endpoint or raw ACK.
   function Quarantined return Boolean;
   function Grant_Pending return Boolean;
   function Revoke_Accepted return Boolean;
   procedure Exchange (Request : ACPI_Requests.Packet;
      Reply : out ACPI_Requests.Packet; Known : out Boolean);
   procedure Import_Table (Index : Positive; Token : ACPI_Snapshot_Provider.Revision;
      Item : ACPI_Snapshot_Provider.Table_Descriptor;
      Reply : out ACPI_Requests.Packet; Known : out Boolean;
      Cleanup : out ACPI_Snapshot_Provider.Cleanup_State);
   procedure Release_Provider_Grant (Released : out Boolean);
end ACPI_Provider_Transport;
