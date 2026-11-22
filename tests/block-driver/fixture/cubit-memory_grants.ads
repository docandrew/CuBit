--  Test fixture: grants 1 .. Grants (generation 7) owned by process 42,
--  each mapping its own Grant_Bytes buffer. Several may be held at once.
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
     (reference : Grant_Reference; expectedOwner : CuBit.Messages.Process_ID;
      byteOffset, byteLength : Unsigned_64; requiredAccess : Required_Access;
      mappedAddress : out System.Address; success : out Boolean);
   procedure Return_Acquisition (reference : Grant_Reference; success : out Boolean);

   Grants : constant := 8;
   Grant_Bytes : constant := 4096;
   subtype Grant_Index is Positive range 1 .. Grants;
   type Grant_Buffer is array (1 .. Grant_Bytes) of Unsigned_8;
   Buffers : array (Grant_Index) of Grant_Buffer := [others => [others => 16#3F#]];
   Held : array (Grant_Index) of Boolean := [others => False];
   Allow_Read, Allow_Write, Allow_Return : Boolean := True;
   Acquires, Returns : Natural := 0;
   function Active return Natural is (Acquires - Returns);
end CuBit.Memory_Grants;
