with CuBit.Handles.Fixture;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Sharing;
with Intel_GPU_Buffer_Backing;
package body CuBit.Messages is
   function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (if Sender = 42 and Stamp = 99 then 99 else 0);
   function Ready return Boolean is (True);
   --  The saved reply of the map request being handled (the driver keeps
   --  it before sharing); the identity is zero once admission moved on.
   procedure Recipient_Of
     (Sender, Stamp : Unsigned_64; Grantee : out CuBit.Handles.Reply;
      Identity : out Unsigned_64) is
   begin
      pragma Assert (Sender = 42 and Stamp = 99);
      Grantee := CuBit.Handles.Fixture.Reply_Of (Grantee_Word);
      Identity := (if Recipient_Generation = 7 then 7 * 2 ** 32 + 42 else 0);
   end Recipient_Of;
   package Server is new Intel_GPU_Buffer_Requests (Session_Of, Ready);
   package Maps is new Server.Sharing (Recipient_Of);
   Object : Server.Service;
   Table : Maps.Mapping_Table;
   function capCall (Target : CuBit.Handles.Endpoint; Msg : in out Message;
                     Deadline : Unsigned_64) return MessageTag is
      -- This synchronous transport fixture does not model kernel deadlines.
      pragma Unreferenced (Deadline);
      Stamp : constant Unsigned_64 :=
        (if CuBit.Handles.Fixture.Word_Of (Target) = Render_Word then 99 else 0);
      Result : Server.Words;
      Ticket : Server.Ticket;
      Created : Maps.Mapping_ID := 0;
      Consumed : Boolean;
   begin
      pragma Assert (Msg.authorityTag = 0);
      if Msg.tag.label = Maps.Map_Label then
         Maps.Handle (Object, Table, 42, Stamp, Msg.tag.label, Msg.tag.length,
           Flag_Byte (Msg.tag), Msg.tag.reserved, Server.Words (Msg.words), Result, Created);
      else
         Server.Handle (Object, 42, Stamp, Msg.tag.label, Msg.tag.length,
           Flag_Byte (Msg.tag), Msg.tag.reserved, Server.Words (Msg.words), Result, Ticket);
         if Ticket /= 0 then
            pragma Assert (Ticket = 1 and Msg.words (2) = 8192);
            Server.Complete (Object, Ticket,
              Intel_GPU_Buffer_Reply.From_Linear (16#1000_0000#, Intel_GPU_Buffer_Backing.CPU_Base, 8192,
               16#1000_0000#), Result, Consumed);
            pragma Assert (Consumed);
         end if;
      end if;
      Msg.words := MessageWords (Result);
      if Fail_Delivery then
         Maps.Reject_Delivery (Object, Table, Created);
         return (label => 0, length => 0, protocol => 0, ipc => <>, reserved => 0);
      end if;
      return Msg.tag;
   end capCall;
end CuBit.Messages;
