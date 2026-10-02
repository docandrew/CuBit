package body CuBit.Messages is
   Data : array (1 .. 32) of CompletionEntry;
   Head, Tail : Natural := 0;
   procedure debugPrint (Text : String) is begin null; end;
   function syscall (Op : Natural) return Unsigned_64 is (Now);
   function getInfo (Op, Arg : Natural) return Unsigned_64 is (1);
   function capSubmit (Slot : Natural; Msg : Message; Token : Unsigned_64) return Boolean is
   begin Submissions := Submissions + 1; return True; end;
   procedure Inject (Value : CompletionEntry) is
   begin Tail := Tail + 1; Data (Tail) := Value; end;
   function Poll_Completion (Result : System.Address) return Unsigned_64 is
      Value : CompletionEntry with Import, Address => Result;
   begin
      if Head = Tail then return 0; end if;
      Head := Head + 1; Value := Data (Head); return 1;
   end;
end CuBit.Messages;
