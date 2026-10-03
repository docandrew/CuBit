with System;
with CuBit.Messages;
with CuBit.Grant_References;
package CuBit.Memory_Grants is
   -- Controlled transport outcomes only; real driver view/registry code runs.
   Create_OK : Boolean := True;
   Revoke_OK : Boolean := True;
   Gone : Boolean := False;
   Creates, Revokes : Natural := 0;
   procedure Create_Via_Capability
     (Slot : CuBit.Messages.CapabilitySlot; LocalAddr : System.Address;
      NumPages : Natural; ReadWrite : Boolean;
      Reference : out CuBit.Grant_References.Reference; Success : out Boolean);
   procedure Create_Forwardable_Via_Capability
     (Slot : CuBit.Messages.CapabilitySlot; LocalAddr : System.Address;
      NumPages : Natural; ReadWrite : Boolean;
      Reference : out CuBit.Grant_References.Reference; Success : out Boolean);
   procedure Revoke
     (Reference : CuBit.Grant_References.Reference; Success : out Boolean);
   function Retirement_Confirmed (Reference : CuBit.Grant_References.Reference)
      return Boolean;
end CuBit.Memory_Grants;
