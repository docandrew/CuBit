package body CuBit.Handles.IPC is
   function Submit_Accepting (Target : Endpoint; Msg : Message; Token : Unsigned_64)
     return Boolean is
   begin
      Submits := Submits + 1;
      pragma Assert (not Msg.tag.ipc.Accepts_Capability,
                     "the client sets no flag itself; Submit_Accepting adds it");
      Last_Target := Target; Last_Submit := Msg; Last_Token := Token;
      return Submit_Result;
   end Submit_Accepting;
   function Carried_By (Reply_Message : Message) return Carried is
     (if Reply_Message.tag.ipc.Capability_Transferred
        and then ABI.Is_Handle_Word (Reply_Message.authorityTag)
      then (Kind => Present, Word => Reply_Message.authorityTag)
      else (Kind => Absent));
   function Accept_Endpoint (Item : Carried) return Accepted_Endpoint is
     (if Item.Kind = Present and then Endpoint_Kind
      then (OK => True, Value => (Word => Item.Word))
      else (OK => False, Why => ABI.Wrong_Kind));
   function Close (Item : Endpoint) return Close_Result is
   begin
      Closes := Closes + 1;
      Last_Closed := Item.Word;
      return (Closed => True);
   end Close;
end CuBit.Handles.IPC;
