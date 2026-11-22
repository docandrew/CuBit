pragma Ada_2022;
with Interfaces;
with ACPI_Firmware_Data_Protocol;
-- One serialized requester lifetime bound by its embedding backend to one
-- retained devmgr endpoint. No reset or retarget API; request IDs never rewind.
-- Store completed grants in an input-derived/growable owner, not in this
-- single-in-flight state. This is protocol policy, not a native IPC adapter.
package ACPI_Firmware_Data_Requests with SPARK_Mode, Pure is
   package P renames ACPI_Firmware_Data_Protocol;
   package IPC renames P.IPC;
   package Wire renames P.Wire;
   use type P.Request_ID;
   use type IPC.Time_Instant;
   type Phase is (Idle, Prepared, In_Flight, Uncertain, Grant_Unchecked,
      Grant_Verified, Refused);
   type Outcome is (Request_Prepared, Attempt_Prepared, Not_Ready,
      Invalid_Envelope, Invalid_Deadline, Clock_Unavailable, Clock_Reversed,
      Expired, Exhausted, Not_Sent, Completion_Unknown, Bad_Reply,
      Inspect_Grant, Backend_Refused, Invalid_Grant, Grant_Ready, Acknowledged);
   type State is limited private;
   type Observation is record
      Stage : Phase;
      ID : P.Request_ID;
      Extent : P.Envelope;
      Clock_Broken : Boolean;
      Reply_Known : Boolean;
      Remote : Wire.Disposition;
   end record;
   function Observe (Context : State) return Observation;
   procedure Prepare (Context : in out State; Extent : P.Envelope; Result : out Outcome);
   -- Trusted transport reports current time in the IPC epoch. Only finite
   -- explicit deadlines may be used. Reconciliation may have a new finite
   -- deadline, but retains the exact original ID/envelope and destination.
   procedure Begin_Attempt (Context : in out State; Now, Deadline : IPC.Time_Instant;
      Clock_Available : Boolean; Request : out IPC.Packet; Result : out Outcome);
   -- Mark In_Flight before calling IPC, then finish exactly once. Any possibly
   -- delivered malformed/lost reply keeps the original request uncertain.
   -- The native adapter must dispose of a carried capability unless the
   -- result is Inspect_Grant; reply words alone never authorize access.
   procedure Finish_Attempt (Context : in out State; Delivered : IPC.Delivery;
      Reply : IPC.Packet; Has_Capability : Boolean; Result : out Outcome);
   type Cache_Mode is (Uncached, Write_Back, Other_Cache);
   type Grant_Evidence is record
      -- Fresh inspection of the carried capability in this process's table.
      -- This is not a shared-memory Grant_References acquisition. Those
      -- separate grants still require the full opaque expected Process_ID.
      Fresh_Current_Handle : Boolean := False;
      Device_Memory : Boolean := False;
      Tag : Interfaces.Unsigned_64 := 0;
      Extent : P.Envelope;
      Readable, Writable, Grantable : Boolean := False;
      Executable, Revocable : Boolean := False;
      Cache : Cache_Mode := Other_Cache;
   end record;
   function Admitted (Evidence : Grant_Evidence; D : Wire.Disposition;
      Extent : P.Envelope) return Boolean;
   -- Called only after inspecting the carried handle. If refused, the adapter
   -- closes it and retains this same request for reconciliation. A late valid
   -- handle is not exposed: close it, reconcile the original request later.
   procedure Check_Grant (Context : in out State; Evidence : Grant_Evidence;
      Now : IPC.Time_Instant; Clock_Available : Boolean; Result : out Outcome);
   -- Only after the validated grant has been committed to its owning record,
   -- or a typed refusal has been recorded. Cannot erase unresolved requests.
   procedure Acknowledge (Context : in out State; Result : out Outcome);
private
   type State is limited record
      Stage : Phase := Idle;
      Last_ID : P.Request_ID := P.No_Request;
      Extent : P.Envelope;
      Last_Clock, Deadline : IPC.Time_Instant := 0;
      Seen_Clock, Clock_Broken, Previously_Uncertain : Boolean := False;
      Reply_Known : Boolean := False;
      Remote : Wire.Disposition := Wire.Malformed;
   end record;
end ACPI_Firmware_Data_Requests;
