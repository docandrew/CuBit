with CuBit.Handles.Fixture;
package body CuBit.Messages is
   function replyCap (saved : CuBit.Handles.Reply; msg : Message) return Unsigned_64 is
   begin
      Reply_Count := Reply_Count + 1;
      Replied_Word := CuBit.Handles.Fixture.Word_Of (saved);
      Last_Reply := msg;
      return Reply_Result;
   end replyCap;
   function capSubmit (target : CuBit.Handles.Endpoint; msg : Message;
                       token : Unsigned_64) return Boolean is
   begin
      Last_Target_Word := CuBit.Handles.Fixture.Word_Of (target);
      Last_Submit := msg;
      Last_Token := token;
      return Submit_Result;
   end capSubmit;
   function capCall (target : CuBit.Handles.Endpoint; msg : in out Message;
                     Deadline : Unsigned_64) return MessageTag is
      pragma Unreferenced (target, msg, Deadline);
   begin
      raise Program_Error with "unexpected synchronous call";
      return (label => 0, length => 0, protocol => 0, ipc => <>, reserved => 0);
   end capCall;
   function Syscall (Number : Unsigned_64;
     A, B, C, D, E, F : Unsigned_64 := 0) return Unsigned_64 is
   begin
      case Number is
         when SYSCALL_GETPID => return 10;
         when others => raise Program_Error;
      end case;
   end Syscall;
end CuBit.Messages;
