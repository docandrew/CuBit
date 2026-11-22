------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Whom a held capability names (CAP-002): a process identity captured from
--  an endpoint or process handle, for policy decisions and broker
--  correlation. Inspection only: installing goes through
--  CuBit.Handles.Control.Install (a child's CSPACE) and handing a running
--  process a capability through CuBit.Handles.IPC.Reply_With.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with CuBit.Handles;
with CuBit.Process_IDs;
package CuBit.Capability_Grants is
   --  Capture before making the policy decision; retain this value until
   --  installation. Recapturing later defeats incarnation binding.
   type Recipient is private;
   function Valid (Target : Recipient) return Boolean;
   --  The process identity (KERN-003) captured from the selected handle:
   --  one word naming one life, the same word a receive reports as the
   --  sender. Correlation data, not authority or proof of liveness. Zero
   --  means invalid.
   function Process_ID (Target : Recipient) return CuBit.Process_IDs.Process_ID;
   --  The same identity (kept as a name for broker messages).
   function Incarnation (Target : Recipient) return CuBit.Process_IDs.Process_ID;
   --  The process an endpoint or process handle of this process names.
   function Capture (Item : CuBit.Handles.Endpoint) return Recipient;
   function Capture (Item : CuBit.Handles.Process) return Recipient;
   --  Read-only validation: Item is an endpoint with Read to Identity.
   function Endpoint_Matches
     (Item : CuBit.Handles.Endpoint;
      Identity : CuBit.Process_IDs.Process_ID) return Boolean;
private
   type Recipient is record
      Wire : Unsigned_64 := 0;
   end record;
end CuBit.Capability_Grants;
