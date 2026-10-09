with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Process_IDs;
package CuBit.Capability_Grants is
   --  Capture before making the policy decision; retain this value until
   --  installation. Recapturing a PID later defeats incarnation binding.
   type Recipient is private;
   function Valid (Target : Recipient) return Boolean;
   --  The process identity (KERN-003) captured from the selected capability:
   --  one word naming one life, the same word a receive reports as the
   --  sender. Use this same value for broker reserve/activate messages;
   --  never recapture between policy evaluation and delegation. Zero means
   --  invalid. Correlation data, not authority or proof of current liveness.
   function Process_ID (Target : Recipient) return CuBit.Process_IDs.Process_ID;
   --  The same identity (kept as a name for broker messages).
   function Incarnation (Target : Recipient) return CuBit.Process_IDs.Process_ID;
   --  Inspect an existing process-referencing capability in this process.
   --  Reply capabilities identify threads, not process incarnations: rejected.
   function Capture (Slot : CuBit.Messages.CapabilitySlot) return Recipient;
   --  Read-only validation for a grant recipient endpoint against a previously
   --  admitted process identity. Requires CAP_ENDPOINT + READ;
   --  process/CSPACE/reply caps do not qualify. Does not mint or grant.
   --  Keep this slot unchanged through grant creation: inspection is a
   --  snapshot, not an atomic inspect-and-grant operation.
   function Endpoint_Matches
     (Slot : CuBit.Messages.CapabilitySlot;
      Identity : CuBit.Process_IDs.Process_ID) return Boolean;
   --  The captured identity is not authority. Kernel CSPACE checks still
   --  apply. Returns zero on installation, U64'Last on rejection.
   function Install
     (Target : Recipient; Kind, Object, Parameter, Rights : Unsigned_64;
      Destination : CuBit.Messages.CapabilitySlot) return Unsigned_64;
   --  Broker-only endpoint derivation: requires recipient CSPACE authority
   --  and RIGHT_GRANT on Source. Source must remain the policy-selected slot
   --  through the call. Object/generation are inherited, never re-resolved.
   --  Success installs a cap; it does not prove the referenced service live
   --  or complete a GPU-session activation handshake.
   function Delegate_Endpoint
     (Target : Recipient; Source, Destination : CuBit.Messages.CapabilitySlot;
      Rights, Authority_Tag : Unsigned_64) return Unsigned_64;
private
   type Recipient is record
      Wire : Unsigned_64 := 0;
   end record;
end CuBit.Capability_Grants;
