with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Capability_ABI;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Handles.Fixture;
with CuBit.Capability_Grants; use CuBit.Capability_Grants;
with Intel_GPU_Broker_Request;
with Intel_Render_Launch_Client;
--  procmgr's render admission client (CAP-002): one asynchronous
--  Submit_Accepting per child, naming it by identity; an admitted reply
--  carries the render endpoint, which Render returns and Forget closes.
procedure Launch_Client_Tests is
   package R renames Intel_GPU_Broker_Request;
   package IPC renames CuBit.Handles.IPC;
   package F renames CuBit.Handles.Fixture;
   use type CuBit.Handles.Endpoint;
   Broker_Handle : constant Unsigned_64 := 16#0000_0002_0000_0005#;
   Render_Handle : constant Unsigned_64 := 16#0000_0003_0000_0009#;
   function Broker return CuBit.Handles.Endpoint is (F.Endpoint_Of (Broker_Handle));
   package C is new Intel_Render_Launch_Client (Broker, 1000, 1015);
   use type C.Phase;
   Object : C.Launcher;
   ID, Bad : C.Ticket;
   Used : Boolean;
   Child_ID : constant Unsigned_64 := 16#1234_5678_0700_002A#;
   Other_Child_ID : constant Unsigned_64 := 16#1234_5678_0800_002A#;
   Broker_ID : constant Unsigned_64 := 16#2345_6789_0900_004D#;
   Other_Broker_ID : constant Unsigned_64 := 16#2345_6789_0A00_004D#;
   Child : constant Recipient := Of_Identity (Child_ID);
   --  The admitted reply: words [version, admitted, nonce, child] and the
   --  render endpoint in the envelope (the kernel's transfer flag).
   Reply : constant CompletionEntry :=
     (token => 1000, from => Broker_ID, status => COMPLETION_OK,
      msg => ((label => R.Label, length => 4, protocol => 0,
               ipc => (Capability_Transferred => True, others => False), reserved => 0),
              Render_Handle,
              [R.Version, R.Admitted, 1000, Child_ID]),
      valid => True);
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with What; end if;
   end Check;
begin
   Endpoint_Identity := Broker_ID;
   C.Start (Object, False, Child, Bad);
   Check (Bad = 0 and IPC.Submits = 0, "unapproved start submitted");
   C.Start (Object, True, Of_Identity (0), Bad);
   Check (Bad = 0 and IPC.Submits = 0, "invalid child submitted");
   C.Start (Object, True, Child, ID);
   Check (ID = 1 and C.State (Object, ID) = C.Pending, "first start");
   Check (IPC.Last_Target = Broker and IPC.Last_Token = 1000, "target/token");
   Check (IPC.Last_Submit.tag = (label => R.Label, length => 4,
                                 protocol => 0, ipc => <>, reserved => 0), "request envelope");
   Check (IPC.Last_Submit.words = [R.Version, Child_ID, 0, 1000], "request words");
   Check (R.Decode (7, 7, R.Authority_Tag, R.Label, 4, R.Request_Flags, 0,
          R.Words (IPC.Last_Submit.words)).Valid, "request decodes as v2");
   Check (C.Render (Object, ID) = CuBit.Handles.No_Endpoint, "render before reply");
   C.Start (Object, True, Child, Bad);
   Check (Bad = 0, "second admission for the same child");
   C.Complete (Object, Reply, Used);
   Check (Used and C.State (Object, ID) = C.Admitted, "admitted");
   Check (F.Word_Of (C.Render (Object, ID)) = Render_Handle, "render endpoint");
   C.Complete (Object, Reply, Used);
   Check (not Used, "replayed completion consumed");
   C.Forget (Object, ID);
   Check (IPC.Closes = 1 and IPC.Last_Closed = Render_Handle, "forget closes");
   Check (C.Render (Object, ID) = CuBit.Handles.No_Endpoint, "render after forget");
   C.Forget (Object, ID);
   Check (IPC.Closes = 1, "second forget closes again");
   declare
      Fresh : C.Launcher;
      Receipt : CompletionEntry := Reply;
   begin
      IPC.Submit_Result := False;
      C.Start (Fresh, True, Child, ID);
      Check (ID = 1 and C.State (Fresh, ID) = C.Rejected, "refused submit");
      IPC.Submit_Result := True;
      C.Start (Fresh, True, Of_Identity (Other_Child_ID), ID);
      Check (ID = 2 and IPC.Last_Token = 1001, "second token");
      -- Invalid receipt and unrelated token leave the real request pending.
      Receipt.valid := False;
      Receipt.token := 1001;
      C.Complete (Fresh, Receipt, Used);
      Check (not Used and C.State (Fresh, ID) = C.Pending, "invalid receipt");
      Receipt.valid := True;
      Receipt.token := 999;
      C.Complete (Fresh, Receipt, Used);
      Check (not Used and C.State (Fresh, ID) = C.Pending, "unrelated token");
   end;
   --  Every malformed or refused reply: nothing admitted, a carried
   --  endpoint is closed, and no late reply turns it into success.
   for Fault in 0 .. 11 loop
      declare
         Fresh : C.Launcher;
         Receipt : CompletionEntry := Reply;
         Closes_Before : constant Natural := IPC.Closes;
      begin
         C.Start (Fresh, True, Child, ID);
         case Fault is
            when 0 => Receipt.status := 1;
            when 1 => Receipt.from := Other_Broker_ID;
            when 2 => Receipt.msg.tag.label := 0;
            when 3 => Receipt.msg.tag.length := 3;
            when 4 => Receipt.msg.words (0) := 1;
            when 5 => Receipt.msg.words (2) := 1001;
            when 6 => Receipt.msg.words (3) := Other_Child_ID;
            when 7 => Receipt.msg.words (1) := R.Uncertain;
            when 8 =>  --  admitted without a capability
               Receipt.msg.tag.protocol := 0; Receipt.msg.authorityTag := 0;
            when 9 =>  --  a carried capability of another kind
               IPC.Endpoint_Kind := False;
            when 10 => --  rejection, correctly without a capability
               Receipt.msg.words (1) := R.Rejected;
               Receipt.msg.tag.protocol := 0; Receipt.msg.authorityTag := 0;
            when others => Receipt.msg.words (1) := R.Rejected;  --  carrying one
         end case;
         C.Complete (Fresh, Receipt, Used);
         IPC.Endpoint_Kind := True;
         Check (Used and C.State (Fresh, ID) =
           (if Fault = 10 then C.Rejected else C.Uncertain), "fault outcome" & Fault'Image);
         Check (C.Render (Fresh, ID) = CuBit.Handles.No_Endpoint, "fault render" & Fault'Image);
         Check (IPC.Closes = Closes_Before +
           (if Fault in 8 | 9 | 10 then 0 else 1), "carried capability closed" & Fault'Image);
         C.Complete (Fresh, Reply, Used);
         Check (not Used, "late success after fault" & Fault'Image);
      end;
   end loop;
   --  Capacity: tickets and tokens are never recycled.
   for I in 2 .. C.Capacity loop
      C.Start (Object, True, Of_Identity (Child_ID + Unsigned_64 (I) * 2 ** 32), ID);
      Check (ID = I and C.State (Object, ID) = C.Pending, "fill" & I'Image);
   end loop;
   C.Start (Object, True, Of_Identity (Child_ID + 99 * 2 ** 32), Bad);
   Check (Bad = 0, "capacity exceeded");
   Put_Line ("launch client PASS checks=" & Checks'Image);
end Launch_Client_Tests;
