package body Intel_Render_Admission_Native is
   package Core renames Intel_Render_Admission;
   use CuBit.Messages;
   use type Core.Phase;
   Control_Tag : constant MessageTag := (label => 16#0A21#, length => 4,
                                         protocol => 0, ipc => <>, reserved => 0);
   function State (Item : Broker_Request) return Core.Phase is
     (Core.State (Item.Transaction));
   function Session (Item : Broker_Request) return Unsigned_64 is
     (Core.Session (Item.Transaction));
   procedure Start
     (Item : in out Broker_Request; Identity : Unsigned_64;
      Driver : CuBit.Handles.Endpoint; Driver_PID : Unsigned_64) is
   begin
      if State (Item) /= Core.Idle then return; end if;
      if Identity = 0 or else Driver_PID = 0 or else not CuBit.Handles.Valid (Driver) then
         Core.Cancel (Item.Transaction);
         return;
      end if;
      Item.Driver := Driver;
      Item.Driver_PID := Driver_PID;
      Core.Start (Item.Transaction, Identity);
   end Start;
   procedure Advance (Item : in out Broker_Request; Token : Unsigned_64) is
      Accepted : Boolean;
      Payload : Core.Words;
      Msg : Message := NULL_MESSAGE;
   begin
      if State (Item) in Core.Reserve_Ready | Core.Activate_Ready |
        Core.Abort_Ready then
         Payload := Core.Request (Item.Transaction);
         Core.Prepare (Item.Transaction, Token, Accepted);
         if not Accepted then return; end if;
         Msg.tag := Control_Tag;
         Msg.words := [Payload (0), Payload (1), Payload (2), Payload (3)];
         Accepted := capSubmit (Item.Driver, Msg, Token);
         Core.Submitted (Item.Transaction, Accepted);
      end if;
   end Advance;
   procedure Complete (Item : in out Broker_Request;
     Receipt : CompletionEntry; Consumed : out Boolean) is
   begin
      Consumed := False;
      if not Receipt.valid then return; end if;
      Core.Complete (Item.Transaction, Receipt.token,
        Receipt.status = COMPLETION_OK and
        Receipt.from = Item.Driver_PID and Receipt.msg.tag = Control_Tag,
        [Receipt.msg.words (0), Receipt.msg.words (1),
         Receipt.msg.words (2), Receipt.msg.words (3)], Consumed);
   end Complete;
   procedure Cancel (Item : in out Broker_Request) is
   begin
      Core.Cancel (Item.Transaction);
   end Cancel;
end Intel_Render_Admission_Native;
