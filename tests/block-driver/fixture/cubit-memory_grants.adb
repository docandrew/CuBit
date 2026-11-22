package body CuBit.Memory_Grants is
   procedure Acquire
     (reference : Grant_Reference; expectedOwner : CuBit.Messages.Process_ID;
      byteOffset, byteLength : Unsigned_64; requiredAccess : Required_Access;
      mappedAddress : out System.Address; success : out Boolean)
   is
   begin
      success := expectedOwner = 42
        and then reference.slot in 1 .. Grants
        and then reference.generation = 7
        and then not Held (Grant_Index (reference.slot))
        and then byteOffset = 0 and then byteLength <= Grant_Bytes
        and then (if requiredAccess = Read_Access then Allow_Read else Allow_Write);
      mappedAddress := System.Null_Address;
      if success then
         Acquires := Acquires + 1;
         Held (Grant_Index (reference.slot)) := True;
         mappedAddress := Buffers (Grant_Index (reference.slot))'Address;
      end if;
   end Acquire;

   procedure Return_Acquisition (reference : Grant_Reference; success : out Boolean) is
   begin
      pragma Assert (reference.slot in 1 .. Grants
                     and then Held (Grant_Index (reference.slot)));
      Held (Grant_Index (reference.slot)) := False;
      Returns := Returns + 1;
      success := Allow_Return;
   end Return_Acquisition;
end CuBit.Memory_Grants;
