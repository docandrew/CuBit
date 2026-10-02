with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Capability_Grants;
with Intel_GPU_Broker_Request;
generic
   Broker_Slot : CuBit.Messages.CapabilitySlot;
   First_Token, Last_Token : Unsigned_64;
package Intel_Render_Launch_Client is
   -- Exactly one retained instance owns devmgr source slots40..55. Do not
   -- reset/reconstruct it or share that remote range with another issuer.
   -- Reserve First_Token..Last_Token exclusively in the process completion
   -- dispatcher; only real kernel completions may be passed to Complete.
   Capacity : constant := 16;
   subtype Ticket is Natural range 0 .. Capacity;
   type Phase is (Absent, Rejected, Pending, Admitted, Uncertain);
   type Launcher is limited private;
   -- Trusted startup/policy selection supplies Approved and reserves the
   -- destination in the suspended child. A manifest request is not approval.
   -- Child must have been captured before policy evaluation. Source and
   -- Broker_Slot are immutable throughout this single-owner call.
   procedure Start
     (Object : in out Launcher; Approved : Boolean;
      Child : CuBit.Capability_Grants.Recipient;
      Application_Source, Destination : CuBit.Messages.CapabilitySlot;
      ID : out Ticket);
   procedure Complete
     (Object : in out Launcher; Receipt : CuBit.Messages.CompletionEntry;
      Consumed : out Boolean);
   function State (Object : Launcher; ID : Ticket) return Phase;
   -- Only Admitted permits the caller to continue its existing child launch.
   -- Do not recycle any broker source slot or token, even after rejection.
   -- Pending has no local timeout: broker owns the admission deadline. A
   -- missing/uncertain reply must not resume the child or imply retirement.
private
   type Entry_Record is record
      Current : Phase := Absent;
      Child, Broker, Token : Unsigned_64 := 0;
      Destination : CuBit.Messages.CapabilitySlot := 0;
   end record;
   type Entry_Array is array (Positive range 1 .. Capacity) of Entry_Record;
   type Launcher is limited record
      Entries : Entry_Array;
      Used : Ticket := 0;
   end record;
end Intel_Render_Launch_Client;
