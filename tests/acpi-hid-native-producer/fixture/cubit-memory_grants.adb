with Sender_Model; with Interfaces;
package body CuBit.Memory_Grants is
   use type CuBit.Handles.Endpoint, System.Address, Interfaces.Unsigned_64;
   procedure Create_Via_Capability (slot : CuBit.Handles.Endpoint; localAddr : System.Address;
      numPages : Natural; readWrite : Boolean; reference : out Grant_Reference; success : out Boolean) is
   begin
      Sender_Model.Assert (slot = 77 and then numPages = 1 and then not readWrite
        and then (localAddr = Sender_Model.Buffer'Address or else localAddr = Sender_Model.Second_Buffer'Address));
      Sender_Model.Creates := Sender_Model.Creates + 1;
      reference := (slot => (if localAddr = Sender_Model.Buffer'Address then 5 else 6), generation => 16#1234_5678#);
      success := Sender_Model.Create_OK;
   end Create_Via_Capability;
   procedure Revoke (reference : Grant_Reference; success : out Boolean) is
   begin
      Sender_Model.Assert (reference.slot in 5 .. 6 and then reference.generation = 16#1234_5678#);
      Sender_Model.Revokes := Sender_Model.Revokes + 1; success := Sender_Model.Revoke_OK;
   end Revoke;
   function Retirement_Confirmed (reference : Grant_Reference) return Boolean is
   begin
      Sender_Model.Assert (reference.slot in 5 .. 6 and then reference.generation = 16#1234_5678#);
      Sender_Model.Queries := Sender_Model.Queries + 1;
      return (if reference.slot = 5 then Sender_Model.Confirmed else Sender_Model.Second_Retired);
   end Retirement_Confirmed;
end CuBit.Memory_Grants;
