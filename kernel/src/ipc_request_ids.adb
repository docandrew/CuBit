package body IPC_Request_Ids with SPARK_Mode is
   function Next (Last_Issued : Sequence) return Allocation is
   begin
      if Last_Issued = Sequence'Last then
         return (Available => False);
      else
         return (True, Unsigned_64 (Last_Issued) + 1);
      end if;
   end Next;
end IPC_Request_Ids;
