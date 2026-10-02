package body CuBit.Messages is
   Last_Request : Message;
   function syscall (Op : Natural) return Unsigned_64 is
   begin
      pragma Assert (Op = SYSCALL_GETTIME);
      Now := Now + Step;
      return Now;
   end syscall;
   function capSubmit (Slot : Natural; Msg : Message; Token : Unsigned_64) return Boolean is
   begin
      pragma Assert (Slot = 15);
      pragma Assert (Shift_Right (Token, 32) = 16#4947_5000#);
      pragma Assert ((Token and 16#FFFF_FFFF#) /= 0);
      if Msg.tag = (16#0237#, 2, 0, 0) then
         pragma Assert (Msg.words (2 .. 3) = [0, 0]);
         pragma Assert (Msg.words (0) <= 15 and Msg.words (1) /= 0);
      elsif Msg.tag = (16#0239#, 4, 0, 0) then
         pragma Assert (Msg.words (0) in 1 .. 16 and Msg.words (1) in 1 .. 2 ** 32 - 2);
         pragma Assert (Msg.words (2) = 7 and Msg.words (3) = 1);
      else
         pragma Assert (Msg.tag = (16#0236#, 3, 0, 0));
         pragma Assert (Msg.words (2) in 1 .. 2 ** 32 - 1 and Msg.words (3) = 0);
         pragma Assert (Msg.words (0) in 1 .. 16 and Msg.words (1) in 1 .. 4096);
         Submissions := Submissions + 1;
      end if;
      Last_Token := Token; Last_Request := Msg;
      return not Submit_Fails;
   end capSubmit;
   function Wait_For_Activity_Until (Deadline : Unsigned_64) return Activity_Result is
   begin
      pragma Assert (Deadline = Now + 1);
      return Idle;
   end Wait_For_Activity_Until;
   function Poll (Result : System.Address) return Unsigned_64 is
      Receipt : CompletionEntry with Import, Address => Result;
   begin
      Polls := Polls + 1;
      if Pending then return 0; end if;
      Receipt := (Last_Token + (if Bad_Token then 1 else 0),
                  (if Bad_Status then 1 else 0), Response);
      if Last_Request.tag.label = 16#0237# then
         Receipt.msg := ((16#F003#, 4, 0, 0),
           [Last_Request.words (0),
            16#0200_0000# + Last_Request.words (0) * 4 * 1024 * 1024,
            16#7000_0000_0000# + Last_Request.words (0) * 2 * 1024 * 1024,
            Last_Request.words (1)]);
      end if;
      if Retry_First and Polls = 1 then
         Receipt.msg := (tag => (16#F002#, 0, 0, 0), words => [others => 0]);
      end if;
      return 1;
   end Poll;
end CuBit.Messages;
