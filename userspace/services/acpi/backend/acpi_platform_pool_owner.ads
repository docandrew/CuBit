pragma Ada_2022;
with Interfaces;
with Capability_ABI;
with CuBit.Platform_Pool_Exchange;
generic
   type Endpoint is private;
   type Authority is private;
   No_Authority : Authority;
   type Carried is private;
   No_Carried : Carried;
   with function Inspect_Endpoint (Target : Endpoint; Expected_Owner : Interfaces.Unsigned_64) return Boolean;
   with function Is_Present (Item : Carried) return Boolean;
   with function Is_Refused (Item : Carried) return Boolean;
   with procedure Inspect_Pool (Item : Carried; Value : out Authority; Accepted : out Boolean);
   with procedure Dispose (Item : in out Carried; Closed : out Boolean);
   with procedure Read_Clock (Milliseconds : out Interfaces.Unsigned_64; Available : out Boolean);
package ACPI_Platform_Pool_Owner with SPARK_Mode is
   subtype Word is Interfaces.Unsigned_64;
   package Wire renames CuBit.Platform_Pool_Exchange;
   type Delivery is (Not_Dispatched, Completion_Unknown, Answered);
   Maximum_Message_Words : constant := 4;
   type Words is array (Natural range 0 .. Maximum_Message_Words - 1) of Word;
   type Response is record
      Ending : Delivery := Not_Dispatched;
      Label : Interfaces.Unsigned_32 := 0;
      Length : Interfaces.Unsigned_8 := 0;
      Protocol : Capability_ABI.Protocol_Flags := 0;
      IPC : Capability_ABI.IPC_Flags := Capability_ABI.No_IPC_Flags;
      Reserved : Interfaces.Unsigned_16 := 0;
      Data : Words := [others => 0];
   end record;
   type Outcome is (Not_Attempted, Acquired, Bad_Request, Bad_Endpoint,
      Invalid_Deadline, Clock_Unavailable, Clock_Reversed, Deadline_Expired,
      Not_Sent, Unknown_Completion, Bad_Reply, Server_Refused, Transfer_Refused);
   type Phase is (Fresh, Working, Ready, Rejected, Uncertain, Cleanup_Required, Closed);
   type Status is record
      Stage : Phase := Fresh;
      Result : Outcome := Not_Attempted;
      Request, Peer, Members, Disposition : Word := 0;
      Retained_Carriage : Boolean := False;
   end record;
   type Context is limited private;
   function Observe (State : Context) return Status;
   function Pool (State : Context) return Authority;
   -- One attempt per owner lifetime, including a timeout or refused reply.
   -- Expected_Owner is the authenticated launcher's full process identity.
   -- Exchange is downward only; no callbacks to this service while it waits.
   -- The deadline is absolute kernel milliseconds, never Wait_Forever.
   generic
      with procedure Exchange (Target : Endpoint; Request, Deadline_Milliseconds : Word;
         Reply : out Response; Received : out Carried);
   procedure Acquire (State : in out Context; Target : Endpoint; Expected_Owner,
      Request, Deadline_Milliseconds : Word; Accepted : out Boolean);
   -- Only an unaccepted carriage is disposable. Once admitted, the pool may
   -- own mappings: closing admission retains it until process teardown.
   procedure Retry_Disposal (State : in out Context; Completed : out Boolean);
   procedure Close_Admission (State : in out Context);
   -- Preserve Working/Cleanup_Required/Uncertain observations. Closing entry
   -- cannot turn unresolved completion into an apparently settled phase.
private
   type Context is limited record
      View : Status;
      Held : Authority := No_Authority;
      Received : Carried := No_Carried;
      Resume : Phase := Rejected;
   end record;
end ACPI_Platform_Pool_Owner;
