with Interfaces; use Interfaces;
with System; with CuBit.Messages;
package CuBit.Memory_Grants is
 type Grant_Reference is record slot, generation : Unsigned_64 := 0; end record;
 Captured : System.Address := System.Null_Address;
 procedure Create_Via_Capability (Slot : CuBit.Messages.CapabilitySlot;
  Address : System.Address; Pages : Natural; Writable : Boolean;
  Ref : out Grant_Reference; Created : out Boolean);
 procedure Revoke (Ref : Grant_Reference; Accepted : out Boolean);
 function Retirement_Confirmed (Ref : Grant_Reference) return Boolean;
end CuBit.Memory_Grants;
