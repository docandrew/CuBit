with CuBit.Handles.IPC;
with CuBit.Process_IDs;
with Intel_GPU_Broker_Request;
package body Intel_Render_Launch_Client is
   package G renames CuBit.Capability_Grants;
   package M renames CuBit.Messages;
   package R renames Intel_GPU_Broker_Request;
   package IPC renames CuBit.Handles.IPC;
   use type M.MessageTag;
   use type IPC.Carried_Kind;

   function State (Object : Launcher; ID : Ticket) return Phase is
     (if ID = 0 or else ID > Object.Used then Absent
      else Object.Entries (ID).Current);

   function Render (Object : Launcher; ID : Ticket) return CuBit.Handles.Endpoint is
     (if ID = 0 or else ID > Object.Used
        or else Object.Entries (ID).Current /= Admitted
      then CuBit.Handles.No_Endpoint else Object.Entries (ID).Endpoint);

   procedure Forget (Object : in out Launcher; ID : Ticket) is
      Closed : IPC.Close_Result;
      pragma Unreferenced (Closed);
   begin
      if ID = 0 or else ID > Object.Used then return; end if;
      if CuBit.Handles.Valid (Object.Entries (ID).Endpoint) then
         Closed := IPC.Close (Object.Entries (ID).Endpoint);
      end if;
      Object.Entries (ID).Endpoint := CuBit.Handles.No_Endpoint;
   end Forget;

   procedure Start
     (Object : in out Launcher; Approved : Boolean; Child : G.Recipient;
      ID : out Ticket)
   is
      Target : constant CuBit.Handles.Endpoint := Broker;
      Server : constant G.Recipient := G.Capture (Target);
      Identity : Unsigned_64;
      Request : M.Message := M.NULL_MESSAGE;
      Token : Unsigned_64;
   begin
      ID := 0;
      if not Approved or else not G.Valid (Child) or else not G.Valid (Server) or else
        Object.Used = Capacity or else First_Token = 0 or else
        Last_Token < First_Token or else Last_Token = Unsigned_64'Last or else
        Unsigned_64 (Object.Used) > Last_Token - First_Token
      then return; end if;
      Identity := CuBit.Process_IDs.To_Word (G.Incarnation (Child));
      if Identity = 0 then return; end if;
      for I in 1 .. Object.Used loop
         if Object.Entries (I).Child = Identity then return; end if;
      end loop;
      Token := First_Token + Unsigned_64 (Object.Used);
      Object.Used := Object.Used + 1;
      ID := Object.Used;
      -- The token doubles as the nonce: unique within this launcher.
      Object.Entries (ID) :=
        (Current => Rejected, Child => Identity,
         Broker => CuBit.Process_IDs.To_Word (G.Incarnation (Server)),
         Token => Token, Nonce => Token, Endpoint => CuBit.Handles.No_Endpoint);
      Request.tag := (label => R.Label, length => 4, protocol => 0, ipc => <>, reserved => 0);
      Request.words := [R.Version, Identity, 0, Token];
      if IPC.Submit_Accepting (Target, Request, Token) then
         Object.Entries (ID).Current := Pending;
      end if;
   end Start;

   procedure Complete
     (Object : in out Launcher; Receipt : M.CompletionEntry;
      Consumed : out Boolean)
   is
      Envelope : constant M.MessageTag :=
        (label    => Receipt.msg.tag.label,
         length   => Receipt.msg.tag.length,
         protocol => Receipt.msg.tag.protocol,
         ipc      => M.No_IPC_Flags,  --  the envelope without the IPC layer's flags
         reserved => Receipt.msg.tag.reserved);
   begin
      Consumed := False;
      if not Receipt.valid then return; end if;
      for I in 1 .. Object.Used loop
         if Object.Entries (I).Current = Pending and then
           Receipt.token = Object.Entries (I).Token
         then
            Consumed := True;
            Object.Entries (I).Current := Uncertain;
            declare
               Carried : constant IPC.Carried := IPC.Carried_By (Receipt.msg);
               Accepted : IPC.Accepted_Endpoint;
               Closed : IPC.Close_Result;
               pragma Unreferenced (Closed);
            begin
               if Carried.Kind = IPC.Present then
                  Accepted := IPC.Accept_Endpoint (Carried);
               end if;
               if Receipt.status = M.COMPLETION_OK and then
                 Receipt.from = Object.Entries (I).Broker and then
                 Envelope = (label => R.Label, length => 4, protocol => 0, ipc => <>, reserved => 0) and then
                 Receipt.msg.words (0) = R.Version and then
                 Receipt.msg.words (2) = Object.Entries (I).Nonce and then
                 Receipt.msg.words (3) = Object.Entries (I).Child
               then
                  if Receipt.msg.words (1) = R.Admitted and then
                    Carried.Kind = IPC.Present and then Accepted.OK
                  then
                     Object.Entries (I).Endpoint := Accepted.Value;
                     Object.Entries (I).Current := Admitted;
                     return;
                  elsif Receipt.msg.words (1) = R.Rejected and then
                    Carried.Kind = IPC.Absent
                  then
                     Object.Entries (I).Current := Rejected;
                  end if;
               end if;
               -- Anything carried by a reply not accepted above is closed.
               if Carried.Kind = IPC.Present and then Accepted.OK then
                  Closed := IPC.Close (Accepted.Value);
               end if;
            end;
            return;
         end if;
      end loop;
   end Complete;
end Intel_Render_Launch_Client;
