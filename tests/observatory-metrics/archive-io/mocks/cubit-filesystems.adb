package body CuBit.Filesystems is
   -- Test messages identify operations; this mock is not filesystem ABI evidence.
   function Open_Request (Loan : CuBit.Memory_Grants.Grant_Reference; Count : Natural; Options : Unsigned_64) return CuBit.Messages.Message is
     ((tag => (1, 4, 0, 0), words => [Loan.slot, Unsigned_64 (Count), Options, 0]));
   function Read_At_Request (Handle : File_Handle; Loan : CuBit.Memory_Grants.Grant_Reference; Count, Offset : Unsigned_64) return CuBit.Messages.Message is
     ((tag => (2, 4, 0, 0), words => [Unsigned_64 (Handle), Loan.slot, Count, Offset]));
   function Close_Request (Handle : File_Handle) return CuBit.Messages.Message is
     ((tag => (3, 1, 0, 0), words => [Unsigned_64 (Handle), 0, 0, 0]));
end CuBit.Filesystems;
