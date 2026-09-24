with Interfaces; use Interfaces;
with System;
with CuBit.Messages;
with CuBit.Grant_References;
package CuBit.Memory_Grants is
   MAXIMUM_GLOBAL_SLOT : constant Unsigned_64 := CuBit.Grant_References.Maximum_Slot;
   MAXIMUM_GENERATION : constant Unsigned_64 := CuBit.Grant_References.Maximum_Generation;
   subtype Global_Grant_Slot is CuBit.Grant_References.Global_Slot;
   subtype Grant_Generation is CuBit.Grant_References.Generation;
   subtype Grant_Reference is CuBit.Grant_References.Reference;
   type Required_Access is (Read_Access, Write_Access);
   procedure Acquire
     (reference : Grant_Reference; expectedOwner : CuBit.Messages.ProcessID;
      byteOffset, byteLength : Unsigned_64; requiredAccess : Required_Access;
      mappedAddress : out System.Address; success : out Boolean);
   procedure Return_Acquisition (reference : Grant_Reference; success : out Boolean);
   Buffer : String (1 .. 4096) := [others => '?'];
   Allow_Read, Allow_Write, Allow_Return : Boolean := True;
   Acquires, Returns, Active : Natural := 0;
end CuBit.Memory_Grants;
