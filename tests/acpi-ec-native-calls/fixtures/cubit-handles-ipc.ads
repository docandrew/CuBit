pragma Ada_2022;
with CuBit.Messages;
package CuBit.Handles.IPC is
   type Saved_Reply (Saved : Boolean := False) is record
      case Saved is
         when True => Handle : Reply;
         when False => Why : Refusal := ABI.Invalid_Argument;
      end case;
   end record;
   function Save_Reply return Saved_Reply
     with Import, Convention => Ada, External_Name => "unused_model_save_reply";
   function Answer (Item : Reply; Msg : CuBit.Messages.Message) return Boolean
     with Import, Convention => Ada, External_Name => "unused_model_saved_answer";
end CuBit.Handles.IPC;
