package body Heap is
   function Arena_Count return Natural is
      A, R, C : Unsigned_64;
   begin
      CuAlloc_Host.Statistics (A, R, C);
      return Natural (A);
   end Arena_Count;
   function Reserved_Bytes return Unsigned_64 is
      A, R, C : Unsigned_64;
   begin
      CuAlloc_Host.Statistics (A, R, C);
      return R;
   end Reserved_Bytes;
   function Committed_Bytes return Unsigned_64 is
      A, R, C : Unsigned_64;
   begin
      CuAlloc_Host.Statistics (A, R, C);
      return C;
   end Committed_Bytes;
end Heap;
