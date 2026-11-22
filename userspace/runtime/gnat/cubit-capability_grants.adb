------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with CuBit.Handles.IPC;
package body CuBit.Capability_Grants is
   use CuBit.Process_IDs;
   use type CuBit.Handles.ABI.Kind;
   package IPC renames CuBit.Handles.IPC;

   function Valid (Target : Recipient) return Boolean is (Target.Wire /= 0);
   function Process_ID (Target : Recipient) return CuBit.Process_IDs.Process_ID is
     (From_Word (Target.Wire));
   function Incarnation (Target : Recipient) return CuBit.Process_IDs.Process_ID is
     (From_Word (Target.Wire));

   function From (D : IPC.Description) return Recipient is
     (if D.Current and then D.Kind in CuBit.Handles.ABI.Endpoint | CuBit.Handles.ABI.Process
         and then D.Object /= 0
      then (Wire => D.Object) else (Wire => 0));

   function Capture (Item : CuBit.Handles.Endpoint) return Recipient is
     (From (IPC.Describe (Item)));
   function Capture (Item : CuBit.Handles.Process) return Recipient is
     (From (IPC.Describe (Item)));

   function Endpoint_Matches
     (Item : CuBit.Handles.Endpoint;
      Identity : CuBit.Process_IDs.Process_ID) return Boolean
   is
      D : constant IPC.Description := IPC.Describe (Item);
   begin
      return Is_Process (Identity) and then D.Current
        and then D.Kind = CuBit.Handles.ABI.Endpoint and then D.Held.Read
        and then D.Object = To_Word (Identity);
   end Endpoint_Matches;
end CuBit.Capability_Grants;
