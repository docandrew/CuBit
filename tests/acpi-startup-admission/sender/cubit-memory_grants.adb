-- Hosted test model only; never a native runtime/provider implementation.
with Sender_Model; package body CuBit.Memory_Grants is
 use type CuBit.Handles.Endpoint, System.Address, Interfaces.Unsigned_64;
 procedure Create_Via_Capability(slot : CuBit.Handles.Endpoint; localAddr : System.Address; numPages : Natural; readWrite : Boolean; reference : out Grant_Reference; success : out Boolean) is
 begin
 Sender_Model.Assert(slot=77 and localAddr=Sender_Model.Buffer'Address and numPages=1 and not readWrite);
 Sender_Model.Creates:=Sender_Model.Creates+1; reference:=(slot=>5,generation=>16#1234_5678#); success:=Sender_Model.Create_OK;
 end Create_Via_Capability;
 procedure Revoke(reference : Grant_Reference; success : out Boolean) is begin
 Sender_Model.Assert(reference.slot=5 and reference.generation=16#1234_5678#);
 Sender_Model.Revokes:=Sender_Model.Revokes+1; success:=Sender_Model.Revoke_OK;
 end Revoke;
 function Retirement_Confirmed(reference : Grant_Reference) return Boolean is begin
 Sender_Model.Assert(reference.slot=5 and reference.generation=16#1234_5678#);
 Sender_Model.Queries:=Sender_Model.Queries+1; return Sender_Model.Confirmed;
 end Retirement_Confirmed;
end CuBit.Memory_Grants;