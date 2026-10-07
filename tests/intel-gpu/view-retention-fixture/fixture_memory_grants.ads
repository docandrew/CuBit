with System;
with Interfaces;
with CuBit.Messages;
with CuBit.Grant_References;
package CuBit.Memory_Grants is
   -- Controlled transport outcomes only; real driver view/registry code runs.
   Create_OK : Boolean := True;
   Revoke_OK : Boolean := True;
   Gone : Boolean := False;
   -- Selective acknowledgement for independent reader lifetimes. Gone retains
   -- the existing all-confirmed mode; otherwise only this exact wire retires.
   Completed_Wire : Interfaces.Unsigned_64 := 0;
   Creates, Revokes : Natural := 0;
   Retirement_Queries : Natural := 0;
   Forwardable_Creates : Natural := 0;
   Last_Writable : Boolean := False;
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
