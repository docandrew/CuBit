with CuBit.Handles.Fixture;
package body CuBit.Handles.IPC is
   function Save_Reply return Saved_Reply is
   begin
      Saves := Saves + 1;
      if not Save_OK then
         return (Saved => False, Why => ABI.Invalid_Argument);
      end if;
      Next_Reply := Next_Reply + 1;
      return (Saved => True, Handle => CuBit.Handles.Fixture.Reply_Of (Next_Reply));
   end Save_Reply;

   function Answer (Item : Reply; Msg : Message) return Boolean is
   begin
      Answers := Answers + 1;
      Last_Answered := Item;
      Last_Message := Msg;
      return Answer_OK;
   end Answer;

   function Reply_Saved_With (Item : Reply; Msg : Message; Copy : Derivation)
     return Reply_Result is
   begin
      Last_Copy := Copy;
      case With_Outcome is
         when Transferred =>
            Transfers := Transfers + 1;
            Last_Answered := Item;
            Last_Message := Msg;
            return (Outcome => Transferred);
         when Delivered_Refused =>
            Last_Answered := Item;
            Last_Message := Msg;
            return (Outcome => Delivered_Refused, Why => ABI.Limit_Reached);
         when Refused =>
            return (Outcome => Refused, Why => ABI.Not_Accepted);
      end case;
   end Reply_Saved_With;
end CuBit.Handles.IPC;
