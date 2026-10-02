with Intel_GPU_Render_Sessions;
package body Intel_Render_Admission_Native is
   package Core renames Intel_Render_Admission;
   package Grants renames CuBit.Capability_Grants;
   use CuBit.Messages;
   use type Core.Phase;
   Control_Tag : constant MessageTag := (16#0A21#, 4, 0, 0);
   function State (Item : Broker_Request) return Core.Phase is
     (Core.State (Item.Transaction));
   procedure Start
     (Item : in out Broker_Request; Target : Grants.Recipient;
      Source, Application_Source, Destination : CapabilitySlot) is
      Driver : Grants.Recipient;
   begin
      if State (Item) /= Core.Idle then return; end if;
      Driver := Grants.Capture (Source);
      if not Grants.Valid (Target) or not Grants.Valid (Driver) then
         Core.Cancel (Item.Transaction);
         return;
      end if;
      Item.Recipient := Target;
      Item.Driver := Driver;
      Item.Source := Source;
      Item.Application_Source := Application_Source;
      Item.Destination := Destination;
      Item.Driver_PID := Grants.Process_ID (Driver);
      Core.Start (Item.Transaction, Grants.Incarnation (Target));
   end Start;
   procedure Advance (Item : in out Broker_Request; Token : Unsigned_64) is
      Accepted : Boolean;
      Payload : Core.Words;
      Msg : Message := NULL_MESSAGE;
      Result : Unsigned_64;
      Session : Unsigned_64;
   begin
      if State (Item) = Core.Delegate_Ready then
         if not Item.Recipient_Installed then
            Session := Core.Session (Item.Transaction);
            -- Do not turn an arbitrary service reply into a CSPACE index.
            if Session <= Intel_GPU_Render_Sessions.Tag_Base or else
              Session > Intel_GPU_Render_Sessions.Tag_Base +
                Intel_GPU_Render_Sessions.Capacity
            then
               Core.Delegated (Item.Transaction, False);
               return;
            end if;
            -- Sources remain immutable under the single dispatcher owner.
            -- Derivation preserves the endpoint's object generation; never
            -- mint from Process_ID (Recipient), which could be reused.
            if not Grants.Endpoint_Matches
              (Item.Application_Source, Grants.Incarnation (Item.Recipient))
            then
               Core.Delegated (Item.Transaction, False);
               return;
            end if;
            Result := Grants.Delegate_Endpoint
              (Item.Driver, Item.Application_Source,
               CapabilitySlot (39 + Session - Intel_GPU_Render_Sessions.Tag_Base),
               1, Session);
            Item.Recipient_Installed := Result = 0;
            if not Item.Recipient_Installed then
               Core.Delegated (Item.Transaction, False);
            end if;
            -- Yield even on success. Cancellation before the next Advance
            -- aborts the reservation without installing the client endpoint.
            return;
         end if;
         Result := Grants.Delegate_Endpoint
           (Item.Recipient, Item.Source, Item.Destination, 3,
            Core.Session (Item.Transaction));
         Core.Delegated (Item.Transaction, Result = 0);
      elsif State (Item) in Core.Reserve_Ready | Core.Activate_Ready |
        Core.Abort_Ready then
         Payload := Core.Request (Item.Transaction);
         Core.Prepare (Item.Transaction, Token, Accepted);
         if not Accepted then return; end if;
         Msg.tag := Control_Tag;
         Msg.words := [Payload (0), Payload (1), Payload (2), Payload (3)];
         Accepted := capSubmit (Item.Source, Msg, Token);
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
