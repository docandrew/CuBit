pragma Ada_2022;
package body HID_Native_Queues is
   package M renames CuBit.Messages;
   use type Interfaces.Unsigned_64;
   Reply_Status : Interfaces.Unsigned_64 := 0;
   function Last_Reply_Status return Interfaces.Unsigned_64 is (Reply_Status);
   procedure Drain_Request (Found : out Boolean) is
      Caller : CuBit.Process_IDs.Process_ID;
      Request : M.Message;
   begin
      M.Poll_Service_Request (Caller, Request, Found);
      if Found then Reply_Status := M.reply (Caller, M.NULL_MESSAGE); end if;
   end Drain_Request;
   procedure Poll_Completion (Item : out M.CompletionEntry; Found : out Boolean) is
      Entry_Value : aliased M.CompletionEntry := M.NULL_COMPLETION;
      Count : Interfaces.Unsigned_64;
   begin
      Count := M.Poll_Completion (Entry_Value'Address);
      Found := Count = 1;
      Item := (if Found then Entry_Value else M.NULL_COMPLETION);
   end Poll_Completion;
   procedure Poll_Event (Publisher : out CuBit.Process_IDs.Process_ID;
      Item : out M.Message; Found : out Boolean) is
   begin Found := M.Poll_Event (Item, Publisher); end Poll_Event;
end HID_Native_Queues;
