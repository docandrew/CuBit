pragma Ada_2022;
with System.Storage_Elements;
with CuBit.Handles.Kernel;
with CuBit.Messages;

package body CuBit.Handles.Tracing is
   package K renames CuBit.Handles.Kernel;

   function Perform (Object : Trace; Operation : ABI.Operation) return Outcome is
      Msg : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
      Result : Unsigned_64;
      Word : Unsigned_64;
   begin
      Msg.tag.label := ABI.Operation'Enum_Rep (Operation);
      Msg.tag.length := ABI.Request_Words;
      --  The one Call: the kernel resolves the handle and, for a kernel
      --  object, answers in Msg.
      Result := K.Call (K.Call_Via_Endpoint, K.Word (Object),
                        Unsigned_64 (System.Storage_Elements.To_Integer (Msg'Address)),
                        CuBit.Messages.Wait_Forever);
      Word := Msg.words (ABI.Outcome_Word);
      if Result = 0 or else Result = Unsigned_64'Last
        or else Msg.tag.label /= ABI.Operation'Enum_Rep (Operation)
        or else Msg.tag.length /= ABI.Reply_Words
        or else not ABI.Is_Outcome_Word (Word)
      then
         return (Answered => False);
      end if;
      for Why in ABI.Outcome loop
         if ABI.Outcome_Word_Of (Why) = Word then
            return (Answered => True, Result => Why);
         end if;
      end loop;
      return (Answered => False);
   end Perform;

end CuBit.Handles.Tracing;
