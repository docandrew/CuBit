with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Handles;
with CuBit.Handles.Fixture;
with CuBit.Handles.IPC;
with CuBit.Messages; use CuBit.Messages;
with Intel_GPU_Broker_Request;
with Intel_GPU_Broker_Launches;
with Intel_Render_Broker;
--  CAP-002: procmgr's admission request accepts a capability; an admitted
--  one is answered with devmgr's driver endpoint badged with the session tag
--  (Read, Write, Grant), every other outcome without one.
procedure Broker_Tests is
   package R renames Intel_GPU_Broker_Request;
   package L renames Intel_GPU_Broker_Launches;
   package F renames CuBit.Handles.Fixture;
   package IPC renames CuBit.Handles.IPC;
   use type L.Phase;
   use type IPC.Reply_Outcome;
   Base : constant Unsigned_64 := 16#4750_0000_0000_0000#;
   Launcher_ID : constant Unsigned_64 := 16#3456_789A_0100_000C#;
   Other_Launcher_ID : constant Unsigned_64 := 16#3456_789A_0200_000C#;
   Child_ID : constant Unsigned_64 := 16#1234_5678_0700_002A#;
   Driver_ID : constant Unsigned_64 := 16#2345_6789_0900_004D#;
   Driver_Word : constant Unsigned_64 := 16#0000_0004_0000_0011#;
   Source_Word : constant Unsigned_64 := 16#0000_0004_0000_0012#;
   function Driver return CuBit.Handles.Endpoint is (F.Endpoint_Of (Driver_Word));
   function Driver_PID return Unsigned_64 is (Driver_ID);
   function Source return CuBit.Handles.Endpoint is (F.Endpoint_Of (Source_Word));
   package B is new Intel_Render_Broker (Driver, Driver_PID, Source, 1000, 2000);
   Object : B.Broker;
   ID, Bad : B.Ticket;
   Request : Message := ((label => R.Label, length => 4, protocol => 0,
                                 ipc => (Accepts_Capability => True, others => False), reserved => 0), 0, [R.Version, Child_ID, 0, 123]);
   Used : Boolean;
   procedure Receipt (Into : in out B.Broker; Token : Unsigned_64; Now : Unsigned_64 := 1) is
   begin
      B.Complete (Into,
        (Token, Driver_ID, 0, ((label => 16#0A21#, length => 4, protocol => 0, ipc => <>, reserved => 0), 0, [0, 1, Base + 1, 0]), True), Now, Used);
      pragma Assert (Used);
   end Receipt;
   Read_Write_Grant : constant Unsigned_64 :=
     CuBit.Handles.Mask ((Read | Write | Grant => True, others => False));
begin
   --  Not procmgr, not accepting a capability, or no saved reply: refused,
   --  and the caller keeps the duty to answer.
   B.Begin_Request (Object, Launcher_ID, Other_Launcher_ID, R.Authority_Tag, Request, 0, 100, Bad);
   pragma Assert (Bad = 0 and IPC.Saves = 0);
   declare
      Plain : Message := Request;
   begin
      Plain.tag.protocol := 0;
      B.Begin_Request (Object, Launcher_ID, Launcher_ID, R.Authority_Tag, Plain, 0, 100, Bad);
      pragma Assert (Bad = 0 and IPC.Saves = 0);
   end;
   IPC.Save_OK := False;
   B.Begin_Request (Object, Launcher_ID, Launcher_ID, R.Authority_Tag, Request, 0, 100, Bad);
   pragma Assert (Bad = 0 and not B.Runnable (Object));
   IPC.Save_OK := True;
   B.Begin_Request (Object, Launcher_ID, Launcher_ID, R.Authority_Tag, Request, 0, 100, ID);
   pragma Assert (ID = 1 and IPC.Saves = 2);
   --  A replayed nonce / second admission for the same child is refused.
   B.Begin_Request (Object, Launcher_ID, Launcher_ID, R.Authority_Tag, Request, 0, 100, Bad);
   pragma Assert (Bad = 0 and IPC.Saves = 2);
   B.Step (Object, 1);
   pragma Assert (Last_Token = 1000 and Last_Submit.words = [1, Child_ID, 0, 0]);
   pragma Assert (F.Word_Of (Last_Target) = Driver_Word);
   Receipt (Object, 1000);
   B.Step (Object, 1); -- activate
   pragma Assert (Last_Token = 1001 and IPC.Transfers = 0);
   Receipt (Object, 1001);
   pragma Assert (B.Runnable (Object)); -- terminal admission still needs reply
   B.Step (Object, 1);
   pragma Assert (B.State (Object, ID) = L.Acknowledged and IPC.Transfers = 1);
   pragma Assert (IPC.Last_Message.words = [R.Version, R.Admitted, 123, Child_ID]);
   --  The copy: the untagged driver endpoint, badged with the session tag.
   pragma Assert (F.Source_Of (IPC.Last_Copy) = Source_Word);
   pragma Assert (F.Tag_Of (IPC.Last_Copy) = Base + 1);
   pragma Assert (F.Rights_Of (IPC.Last_Copy) = Read_Write_Grant);
   pragma Assert (not B.Runnable (Object));
   --  The requester's table refuses the copy: the outcome is uncertain to
   --  procmgr and the session is aborted.
   declare
      Fresh : B.Broker;
      First : B.Ticket;
   begin
      Request.words := [R.Version, Child_ID + 1, 0, 124];
      B.Begin_Request (Fresh, Launcher_ID, Launcher_ID, R.Authority_Tag, Request, 1, 100, First);
      B.Step (Fresh, 2);
      Receipt (Fresh, 1000, 2);
      B.Step (Fresh, 2);
      Receipt (Fresh, 1001, 2);
      IPC.With_Outcome := IPC.Delivered_Refused;
      B.Step (Fresh, 2);
      IPC.With_Outcome := IPC.Transferred;
      pragma Assert (B.State (Fresh, First) = L.Retained and B.Runnable (Fresh));
      B.Step (Fresh, 2);
      pragma Assert (Last_Token = 1002 and Last_Submit.words (3) = 2);
   end;
   --  The kernel refuses the copy outright (would amplify): procmgr is
   --  answered "uncertain" without a capability, and the session aborted.
   declare
      Fresh : B.Broker;
      First : B.Ticket;
      Answers : constant Natural := IPC.Answers;
   begin
      Request.words := [R.Version, Child_ID + 2, 0, 125];
      B.Begin_Request (Fresh, Launcher_ID, Launcher_ID, R.Authority_Tag, Request, 1, 100, First);
      B.Step (Fresh, 2);
      Receipt (Fresh, 1000, 2);
      B.Step (Fresh, 2);
      Receipt (Fresh, 1001, 2);
      IPC.With_Outcome := IPC.Refused;
      B.Step (Fresh, 2);
      IPC.With_Outcome := IPC.Transferred;
      pragma Assert (IPC.Answers = Answers + 1 and
        IPC.Last_Message.words (1) = R.Uncertain and B.State (Fresh, First) = L.Retained);
      B.Step (Fresh, 2);
      pragma Assert (Last_Token = 1002 and Last_Submit.words (3) = 2);
   end;
   --  A refused submission rejects with a plain answer, no capability.
   declare
      Fresh : B.Broker;
      First : B.Ticket;
      Transfers : constant Natural := IPC.Transfers;
   begin
      Request.words := [R.Version, Child_ID + 3, 0, 126];
      B.Begin_Request (Fresh, Launcher_ID, Launcher_ID, R.Authority_Tag, Request, 1, 100, First);
      Submit_Result := False;
      B.Step (Fresh, 2);
      Submit_Result := True;
      B.Step (Fresh, 2);
      pragma Assert (IPC.Transfers = Transfers and IPC.Last_Message.words (1) = R.Rejected and
        IPC.Last_Message.words (2) = 126 and B.State (Fresh, First) = L.Retained);
   end;
   --  Late reserve success after the deadline must abort, never admit.
   declare
      Fresh : B.Broker;
      First : B.Ticket;
      Transfers : constant Natural := IPC.Transfers;
   begin
      Request.words := [R.Version, Child_ID + 4, 0, 127];
      B.Begin_Request (Fresh, Launcher_ID, Launcher_ID, R.Authority_Tag, Request, 1, 10, First);
      B.Step (Fresh, 2);
      B.Step (Fresh, 10);
      pragma Assert (not B.Runnable (Fresh) and B.Next_Deadline (Fresh) = Unsigned_64'Last);
      Receipt (Fresh, 1000, 11);
      B.Step (Fresh, 11);
      pragma Assert (Last_Token = 1002 and IPC.Transfers = Transfers);
      Receipt (Fresh, 1002, 12);
      B.Step (Fresh, 12);
      pragma Assert (IPC.Transfers = Transfers and IPC.Last_Message.words (1) = R.Rejected);
   end;
   Put_Line ("saved-reply broker PASS (badged render endpoint by handle)");
end Broker_Tests;
