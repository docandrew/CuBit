with Control;
package body CuBit.Memory_Grants is
 procedure Create_Via_Capability (Slot : CuBit.Messages.CapabilitySlot;
  Address : System.Address; Pages : Natural; Writable : Boolean;
  Ref : out Grant_Reference; Created : out Boolean) is
 begin
  Control.Creates := Control.Creates + 1; Captured := Address;
  if Pages /= 1 or not Writable then raise Program_Error; end if;
  Ref := (1, 1); Created := Control.Create_OK;
 end;
 procedure Revoke (Ref : Grant_Reference; Accepted : out Boolean) is
 begin Control.Revokes := Control.Revokes + 1; Accepted := Control.Revoke_OK; end;
 function Retirement_Confirmed (Ref : Grant_Reference) return Boolean is (Control.Retired);
end CuBit.Memory_Grants;
