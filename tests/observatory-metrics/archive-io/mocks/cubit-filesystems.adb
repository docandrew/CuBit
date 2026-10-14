package body CuBit.Filesystems is
   -- Test messages identify operations; this mock is not filesystem ABI evidence.
   function Open_Request (Loan : CuBit.Memory_Grants.Grant_Reference; Count : Natural; Options : Unsigned_64) return CuBit.Messages.Message is
     ((tag => (label => 1, length => 4,
               protocol => 0, ipc => <>, reserved => 0), words => [Loan.slot, Unsigned_64 (Count), Options, 0]));
   function Read_At_Request (Handle : File_Handle; Loan : CuBit.Memory_Grants.Grant_Reference; Count, Offset : Unsigned_64) return CuBit.Messages.Message is
     ((tag => (label => 2, length => 4,
               protocol => 0, ipc => <>, reserved => 0), words => [Unsigned_64 (Handle), Loan.slot, Count, Offset]));
   function Close_Request (Handle : File_Handle) return CuBit.Messages.Message is
     ((tag => (label => 3, length => 1,
               protocol => 0, ipc => <>, reserved => 0), words => [Unsigned_64 (Handle), 0, 0, 0]));
end CuBit.Filesystems;
