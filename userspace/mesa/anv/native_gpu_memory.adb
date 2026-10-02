with System;
with System.Storage_Elements;
with CuBit.Messages;
with CuBit.Grant_References;
with CuBit.Memory_Grants;

package body Native_GPU_Memory is
   package References renames CuBit.Grant_References;
   package Grants renames CuBit.Memory_Grants;

   function Acquire
     (Slot, Reference, Offset, Bytes, Writable : Unsigned_64;
      Output : access Unsigned_64) return Unsigned_32
   is
      Address : System.Address;
      Success : Boolean;
   begin
      if Output = null then return 1; end if;
      Output.all := 0;
      if Slot > Unsigned_64 (CuBit.Messages.CapabilitySlot'Last) or else
         not References.Valid_Wire (Reference) or else Writable > 1 or else
         Bytes = 0 or else Bytes - 1 > Unsigned_64'Last - Offset
      then
         return 1;
      end if;
      Grants.Acquire_Via_Capability
        (CuBit.Messages.CapabilitySlot (Slot), References.Decode (Reference),
         Offset, Bytes,
         (if Writable = 1 then Grants.Write_Access else Grants.Read_Access),
         Address, Success);
      if not Success then return 1; end if;
      Output.all := Unsigned_64 (System.Storage_Elements.To_Integer (Address));
      return 0;
   end Acquire;

   function Return_Borrow (Reference : Unsigned_64) return Unsigned_32 is
      Success : Boolean;
   begin
      if not References.Valid_Wire (Reference) then return 1; end if;
      Grants.Return_Acquisition (References.Decode (Reference), Success);
      return (if Success then 0 else 1);
   end Return_Borrow;
end Native_GPU_Memory;
