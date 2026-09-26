with Interfaces;
with CCL.Objects;
with CuBit.Messages;
with Config_Authority;
with Config_Collections;
with Config_Objects;
with Config_Typed_Store;
with Config_Object_Receiver;
with Config_Worker_Channel;

--  Event-driven owner of the typed Config path. No SQL or blocking storage
--  calls run here. The outer service loop drains authenticated kernel requests
--  and completions separately, then sleeps on Wait_For_Activity_Until.
--  Instantiate once per process: this package owns the Config worker token
--  domain and its reply slot. One service instance is active at a time; a
--  replacement must wait for retirement and retain the same package instance.
generic
   with package Receiver is new Config_Object_Receiver (<>);
package Config_Object_Service is
   type State is limited private;
   type Worker_Status is (Unattached, Online, Recovery_Required);
   function Status (Object : State) return Worker_Status;
   function Waiting (Object : State) return Boolean;
   procedure Attach
     (Object : in out State; Endpoint : CuBit.Messages.CapabilitySlot;
      Session : Interfaces.Unsigned_64; Success : out Boolean);
   --  Once per instance. Endpoint authority comes from the trusted launcher;
   --  an arbitrary request cannot select a worker. Preserve this object's
   --  address/lifetime until Retire reports its channel grant unmapped.
   procedure Restore
     (Object : in out State; ID : Config_Collections.Registered_ID;
      Result : out Config_Objects.Outcome);
   procedure Handle
     (Object : in out State; Authority : Config_Authority.Authority_State;
      Sender : CuBit.Messages.ProcessID; Request : CuBit.Messages.Message);
   --  Same owning thread and current reply-cap requirements as Receiver.Handle.
   procedure Complete
     (Object : in out State; Authority : Config_Authority.Authority_State;
      Completion : CuBit.Messages.CompletionEntry);
   --  Kernel completion queue ONLY; never route ordinary IPC/events here.
   procedure Revoke_Subject (Object : in out State; Subject : Config_Authority.Subject_ID);
   procedure Retire (Object : in out State; Confirmed : out Boolean);
   --  Stops worker submissions, resolves a saved reply as Unavailable and
   --  marks cached objects stale. Does not roll back a possible disk commit.
   --  May be called again to check retirement; never retries a database write.
   --  Cached reads/Close remain possible while transport is being retired.
   --  A replacement instance must not reuse this instance's live grant/state.
private
   type State is limited record
      Store : Config_Typed_Store.State;
      Requests : Receiver.State;
      Worker : Config_Worker_Channel.Channel;
      Current : Worker_Status := Unattached;
      Session : Interfaces.Unsigned_64 := 0;
      Provisioning : Boolean := False;
      Metadata_Pending : Boolean := False;
   end record;
end Config_Object_Service;
