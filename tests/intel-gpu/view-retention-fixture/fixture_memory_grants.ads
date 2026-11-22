with System;
with Interfaces;
with CuBit.Handles;
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
   --  Grants go to the saved reply of the client's request (CAP-002); the
   --  fixture accepts only the reply the tests name their client by (7).
   procedure Create_Via_Reply
     (Peer : CuBit.Handles.Reply; LocalAddr : System.Address;
      NumPages : Natural; ReadWrite : Boolean;
      Reference : out CuBit.Grant_References.Reference; Success : out Boolean);
   procedure Create_Forwardable_Via_Reply
     (Peer : CuBit.Handles.Reply; LocalAddr : System.Address;
      NumPages : Natural; ReadWrite : Boolean;
      Reference : out CuBit.Grant_References.Reference; Success : out Boolean);
   procedure Revoke
     (Reference : CuBit.Grant_References.Reference; Success : out Boolean);
   function Retirement_Confirmed (Reference : CuBit.Grant_References.Reference)
      return Boolean;
end CuBit.Memory_Grants;
