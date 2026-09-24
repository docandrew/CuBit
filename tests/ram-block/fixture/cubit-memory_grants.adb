package body CuBit.Memory_Grants is
   procedure Acquire
     (reference : Grant_Reference; expectedOwner : CuBit.Messages.ProcessID;
      byteOffset, byteLength : Unsigned_64; requiredAccess : Required_Access;
      mappedAddress : out System.Address; success : out Boolean)
   is
   begin
      Acquires := Acquires + 1;
      success := expectedOwner = 42 and then reference.slot = 1 and then
        reference.generation = 7 and then byteOffset = 0 and then
        byteLength <= Buffer'Length and then
        (if requiredAccess = Read_Access then Allow_Read else Allow_Write);
      mappedAddress := System.Null_Address;
      if success then
         Active := Active + 1;
         mappedAddress := Buffer'Address;
      end if;
   end Acquire;
   procedure Return_Acquisition (reference : Grant_Reference; success : out Boolean) is
   begin
      pragma Assert (reference.slot = 1 and reference.generation = 7 and Active = 1);
      Returns := Returns + 1;
      Active := Active - 1;
      success := Allow_Return;
   end Return_Acquisition;
end CuBit.Memory_Grants;
