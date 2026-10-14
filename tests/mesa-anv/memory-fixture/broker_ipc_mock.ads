with Interfaces; use Interfaces;
with CuBit.Messages;
--  The broker tests' CuBit.Handles.IPC: saved replies and their answers
--  recorded; Reply_Saved_With's outcome chosen by the test.
package CuBit.Handles.IPC is
   subtype Message is CuBit.Messages.Message;
   type Saved_Reply (Saved : Boolean := False) is record
      case Saved is
         when True  => Handle : Reply;
         when False => Why    : Refusal := ABI.Invalid_Argument;
      end case;
   end record;
   function Save_Reply return Saved_Reply;
   function Answer (Item : Reply; Msg : Message) return Boolean;
   type Reply_Outcome is (Transferred, Delivered_Refused, Refused);
   type Reply_Result (Outcome : Reply_Outcome := Refused) is record
      case Outcome is
         when Transferred => null;
         when Delivered_Refused | Refused => Why : Refusal := ABI.Not_Accepted;
      end case;
   end record;
   function Reply_Saved_With (Item : Reply; Msg : Message; Copy : Derivation)
     return Reply_Result;

   Save_OK : Boolean := True;
   Next_Reply : Unsigned_64 := 16#0000_0001_0000_0040#;
   Saves, Answers, Transfers : Natural := 0;
   Answer_OK : Boolean := True;
   With_Outcome : Reply_Outcome := Transferred;
   Last_Answered : Reply;
   Last_Message : Message := CuBit.Messages.NULL_MESSAGE;
   Last_Copy : Derivation;
end CuBit.Handles.IPC;
