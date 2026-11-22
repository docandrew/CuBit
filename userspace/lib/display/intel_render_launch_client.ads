with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Handles;
with CuBit.Capability_Grants;

-- procmgr's side of render admission (CAP-002, PLAN section 9 item 6).
-- For each suspended child the policy approves, procmgr asks devmgr's
-- broker (Submit_Accepting, asynchronous) to admit the child by its process
-- identity; an admitted reply carries the child's render endpoint (devmgr's
-- intel-gpu endpoint badged with the session tag, Read+Write+Grant), which
-- procmgr installs into the child through its CSPACE and then Forgets.
-- Nothing is offered or delegated to devmgr and no slot is reserved.
generic
   with function Broker return CuBit.Handles.Endpoint;
   First_Token, Last_Token : Unsigned_64;
package Intel_Render_Launch_Client is
   -- Reserve First_Token..Last_Token exclusively in the process completion
   -- dispatcher; only real kernel completions may be passed to Complete.
   Capacity : constant := 16;
   subtype Ticket is Natural range 0 .. Capacity;
   type Phase is (Absent, Rejected, Pending, Admitted, Uncertain);
   type Launcher is limited private;
   -- Trusted startup/policy selection supplies Approved. A manifest request
   -- is not approval. Child must have been captured before policy
   -- evaluation (its incarnation names it to the broker and the driver).
   procedure Start
     (Object : in out Launcher; Approved : Boolean;
      Child : CuBit.Capability_Grants.Recipient;
      ID : out Ticket);
   procedure Complete
     (Object : in out Launcher; Receipt : CuBit.Messages.CompletionEntry;
      Consumed : out Boolean);
   function State (Object : Launcher; ID : Ticket) return Phase;
   -- The render endpoint an Admitted reply carried; No_Endpoint otherwise
   -- and after Forget.
   function Render (Object : Launcher; ID : Ticket) return CuBit.Handles.Endpoint;
   -- Close the render endpoint this launcher holds (after installing a copy
   -- into the child, or on failure). The ticket is not recycled.
   procedure Forget (Object : in out Launcher; ID : Ticket);
   -- Only Admitted permits the caller to continue its existing child launch.
   -- Pending has no local timeout: broker owns the admission deadline. A
   -- missing/uncertain reply must not resume the child or imply retirement.
private
   type Entry_Record is record
      Current : Phase := Absent;
      Child, Broker, Token, Nonce : Unsigned_64 := 0;
      Endpoint : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
   end record;
   type Entry_Array is array (Positive range 1 .. Capacity) of Entry_Record;
   type Launcher is limited record
      Entries : Entry_Array;
      Used : Ticket := 0;
   end record;
end Intel_Render_Launch_Client;
