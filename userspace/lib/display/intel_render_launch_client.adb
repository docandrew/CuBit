package body Intel_Render_Launch_Client is
   package G renames CuBit.Capability_Grants;
   package M renames CuBit.Messages;
   package R renames Intel_GPU_Broker_Request;
   use type M.CapabilitySlot;
   use type M.MessageTag;
   function State (Object : Launcher; ID : Ticket) return Phase is
     (if ID = 0 or else ID > Object.Used then Absent
      else Object.Entries (ID).Current);

   procedure Start
     (Object : in out Launcher; Approved : Boolean; Child : G.Recipient;
      Application_Source, Destination : M.CapabilitySlot; ID : out Ticket)
   is
      Broker : G.Recipient;
      Request : M.Message := M.NULL_MESSAGE;
      Token : Unsigned_64;
   begin
      ID := 0;
      if not Approved or else not G.Valid (Child) or else
        Object.Used = Capacity or else First_Token = 0 or else
        Last_Token < First_Token or else Last_Token = Unsigned_64'Last or else
        Unsigned_64 (Object.Used) > Last_Token - First_Token or else
        not G.Endpoint_Matches (Application_Source, G.Incarnation (Child))
      then return; end if;
      for I in 1 .. Object.Used loop
         if Object.Entries (I).Child = G.Incarnation (Child) and then
           Object.Entries (I).Destination = Destination
         then return; end if;
      end loop;
      Broker := G.Capture (Broker_Slot);
      if not G.Valid (Broker) or else
        not G.Endpoint_Matches (Broker_Slot, G.Incarnation (Broker))
      then return; end if;
      Token := First_Token + Unsigned_64 (Object.Used);
      Object.Used := Object.Used + 1;
      ID := Object.Used;
      Object.Entries (ID) :=
        (Rejected, G.Incarnation (Child), G.Incarnation (Broker), Token, Destination);
      -- Slot consumption is irreversible even if submission later fails.
      -- READ|GRANT lets devmgr derive READ-only recipient identity into GPU;
      -- it does not give devmgr arbitrary access to application memory.
      if G.Delegate_Endpoint (Broker, Application_Source,
        M.CapabilitySlot (R.Source_Slot'First + ID - 1), 9, 0) /= 0
      then return; end if;
      Request.tag := (R.Label, 4, 0, 0);
      Request.words := [R.Version, Unsigned_64 (R.Source_Slot'First + ID - 1),
                        Unsigned_64 (Destination), Token];
      if M.capSubmit (Broker_Slot, Request, Token) then
         Object.Entries (ID).Current := Pending;
      end if;
   end Start;

   procedure Complete
     (Object : in out Launcher; Receipt : M.CompletionEntry;
      Consumed : out Boolean) is
   begin
      Consumed := False;
      if not Receipt.valid then return; end if;
      for I in 1 .. Object.Used loop
         if Object.Entries (I).Current = Pending and then
           Receipt.token = Object.Entries (I).Token
         then
            Consumed := True;
            Object.Entries (I).Current := Uncertain;
            if Receipt.status = M.COMPLETION_OK and then
              Receipt.from = Object.Entries (I).Broker mod 2 ** 32 and then
              Receipt.msg.tag = (R.Label, 4, 0, 0) and then
              Receipt.msg.words (0) = R.Version and then
              Receipt.msg.words (2) = Object.Entries (I).Token and then
              Receipt.msg.words (3) = Object.Entries (I).Child
            then
               case Receipt.msg.words (1) is
                  when 0 => Object.Entries (I).Current := Admitted;
                  when 1 => Object.Entries (I).Current := Rejected;
                  when others => null;
               end case;
            end if;
            return;
         end if;
      end loop;
   end Complete;
end Intel_Render_Launch_Client;
