with System;
with CuBit.Messages;
with CuBit.Grant_References;
with Interfaces; use Interfaces;
package CuBit.Memory_Grants is
   subtype Grant_Reference is CuBit.Grant_References.Reference;
   procedure Create_Via_Capability
     (slot : CuBit.Messages.CapabilitySlot; localAddr : System.Address;
      numPages : Natural; readWrite : Boolean;
      reference : out Grant_Reference; success : out Boolean);
   procedure Revoke (reference : Grant_Reference; success : out Boolean);
   function Retirement_Confirmed (reference : Grant_Reference) return Boolean;
   type Required_Access is (Read_Access, Write_Access);
   procedure Acquire_Via_Capability
     (slot : CuBit.Messages.CapabilitySlot; reference : Grant_Reference;
      byteOffset, byteLength : Unsigned_64; requiredAccess : Required_Access;
      mappedAddress : out System.Address; success : out Boolean);
   procedure Return_Acquisition (reference : Grant_Reference; success : out Boolean);
   Mapping : System.Address := System.Null_Address;
   Allow_Grant : Boolean := True;
   Allow_Revoke : Boolean := True;
   Is_Retired : Boolean := True;
   Revocations : Natural := 0;
   Expected_Pages : Natural := 1;
   Expected_Transfer_Bytes : Unsigned_64 := 0;
   Expected_Owner_Endpoint : CuBit.Messages.CapabilitySlot := 12;
   Acquisitions, Returns, Active_Acquisitions : Natural := 0;
   Deny_Acquisition, Fail_Return : Natural := 0;
end CuBit.Memory_Grants;
