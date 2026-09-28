with Interfaces;
with CuBit.Memory_Grants;
generic
   with function Spawn return Interfaces.Unsigned_64;
   with procedure Borrow
     (Owner : Interfaces.Unsigned_64;
      Reference : out CuBit.Memory_Grants.Grant_Reference;
      Success : out Boolean);
procedure DMA_Retention_Check;
