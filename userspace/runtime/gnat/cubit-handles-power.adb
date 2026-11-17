pragma Ada_2022;
with System.Storage_Elements;
with CuBit.Handles.Kernel;
with CuBit.Messages;

package body CuBit.Handles.Power is
   package K renames CuBit.Handles.Kernel;

   function Enter_State
     (Object : Platform_Power; State : ABI.Power_State;
      Type_A, Type_B : ABI.Sleep_Type := 0) return Outcome
   is
      Msg : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
      Result : Unsigned_64;
      Word : Unsigned_64;
   begin
      Msg.tag.label := ABI.Operation'Enum_Rep (ABI.Enter_State);
      Msg.tag.length := ABI.Request_Words;
      Msg.words (ABI.State_Word) := ABI.Power_State'Enum_Rep (State);
      Msg.words (ABI.Type_A_Word) := Type_A;
      Msg.words (ABI.Type_B_Word) := Type_B;
      --  The one Call: the kernel resolves the handle and, for a kernel
      --  object, answers in Msg.
      Result := K.Call (K.Call_Via_Endpoint, K.Word (Object),
                        Unsigned_64 (System.Storage_Elements.To_Integer (Msg'Address)),
                        CuBit.Messages.Wait_Forever);
      Word := Msg.words (ABI.Outcome_Word);
      if Result = 0 or else Result = Unsigned_64'Last
        or else Msg.tag.label /= ABI.Operation'Enum_Rep (ABI.Enter_State)
        or else Msg.tag.length /= ABI.Reply_Words
        or else not ABI.Is_Refusal_Word (Word)
      then
         return (Answered => False);
      end if;
      for Why in ABI.Refusal loop
         if ABI.Refusal_Word (Why) = Word then
            return (Answered => True, Why => Why);
         end if;
      end loop;
      return (Answered => False);
   end Enter_State;

end CuBit.Handles.Power;
