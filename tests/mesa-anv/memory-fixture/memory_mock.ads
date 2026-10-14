with Interfaces; use Interfaces;
with System;
with CuBit.Handles;
with CuBit.Grant_References;
package CuBit.Memory_Grants is
   subtype Grant_Reference is CuBit.Grant_References.Reference;
   type Required_Access is (Read_Access, Write_Access);
   Calls, Returns : Natural := 0;
   Creates, Revokes, Polls : Natural := 0;
   Forwardable_Creates : Natural := 0;
   Gone : Boolean := False;
   Succeed : Boolean := True;
   Expected_Slot, Expected_Offset, Expected_Bytes : Unsigned_64 := 0;
   Expected_Reference : Grant_Reference;
   Expected_Access : Required_Access := Read_Access;
   Separate_Create_Expectations : Boolean := False;
   Create_Slot, Create_Offset : Unsigned_64 := 0;
   --  Expected_Slot / Create_Slot: the handle words expected (CAP-002).
   procedure Acquire_Via_Capability
     (slot : CuBit.Handles.Endpoint; reference : Grant_Reference;
      byteOffset, byteLength : Unsigned_64; requiredAccess : Required_Access;
      mappedAddress : out System.Address; success : out Boolean);
   procedure Return_Acquisition (reference : Grant_Reference; success : out Boolean);
   procedure Create_Via_Reply
     (peer : CuBit.Handles.Reply; localAddr : System.Address;
      numPages : Natural; readWrite : Boolean;
      reference : out Grant_Reference; success : out Boolean);
   procedure Revoke (reference : Grant_Reference; success : out Boolean);
   procedure Create_Forwardable_Via_Reply
     (peer : CuBit.Handles.Reply; localAddr : System.Address;
      numPages : Natural; readWrite : Boolean;
      reference : out Grant_Reference; success : out Boolean);
   function Retirement_Confirmed (reference : Grant_Reference) return Boolean;
end CuBit.Memory_Grants;
