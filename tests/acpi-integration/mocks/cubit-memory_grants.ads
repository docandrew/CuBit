with Interfaces; use Interfaces;
with System;
with CuBit.Grant_References;
with CuBit.Messages;
package CuBit.Memory_Grants is
   MAXIMUM_GLOBAL_SLOT : constant Unsigned_64 := CuBit.Grant_References.Maximum_Slot;
   MAXIMUM_GENERATION : constant Unsigned_64 := CuBit.Grant_References.Maximum_Generation;
   subtype Grant_Reference is CuBit.Grant_References.Reference;
   type Required_Access is (Read_Access, Write_Access);
   procedure Acquire_Via_Capability
     (slot : CuBit.Messages.CapabilitySlot; reference : Grant_Reference;
      byteOffset, byteLength : Unsigned_64; requiredAccess : Required_Access;
      mappedAddress : out System.Address; success : out Boolean);
   procedure Return_Acquisition (reference : Grant_Reference; success : out Boolean);
   -- Hosted mock controls and captured syscall arguments, never native code.
   Acquire_OK, Return_OK : Boolean := True;
   Source : System.Address := System.Null_Address;
   Acquisitions, Returns : Natural := 0;
   Last_Slot : CuBit.Messages.CapabilitySlot;
   Last_Reference, Returned_Reference : Grant_Reference;
   Last_Length, Last_Offset : Unsigned_64;
   Last_Access : Required_Access;
end CuBit.Memory_Grants;
