package body CuBit.Messages is
   Data : array (1 .. 256) of CompletionEntry;
   Head, Tail : Natural := 0;
   procedure debugPrint (Text : String) is
      Length : constant Natural := Natural'Min (Text'Length, Last_Serial'Length);
   begin
      Serial_Lines := Serial_Lines + 1;
      Last_Serial_Length := Length;
      Last_Serial (1 .. Length) := Text (Text'First .. Text'First + Length - 1);
   end debugPrint;
   function Registered_Driver (Driver : Natural) return Process_ID is
     (if Driver = DRIVER_LOGSTORE then Logstore else No_Process);
   procedure Inject (Value : CompletionEntry) is
   begin Tail := Tail + 1; Data (Tail) := Value; end Inject;
   function Queued return Natural is (Tail - Head);
   function Poll_Completion (Result : System.Address) return Unsigned_64 is
      Value : CompletionEntry with Import, Address => Result;
   begin
      if Head = Tail then return 0; end if;
      Head := Head + 1; Value := Data (Head); return 1;
   end Poll_Completion;
end CuBit.Messages;
