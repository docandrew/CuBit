with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with Intel_GPU_Broker_Request;
with Intel_GPU_Broker_Launches;
with Intel_Render_Broker;
procedure Broker_Tests is
   package R renames Intel_GPU_Broker_Request;
   package L renames Intel_GPU_Broker_Launches;
   use type L.Phase;
   function Slot (ID : Positive) return CapabilitySlot is
     (CapabilitySlot (ID + 14));
   package B is new Intel_Render_Broker (31, 1000, 2000, Slot);
   Object : B.Broker;
   ID, Bad : B.Ticket;
   Request : Message := ((R.Label, 4, 0, 0), [1, 40, 4, 123]);
   Used : Boolean;
   Base : constant Unsigned_64 := 16#4750_0000_0000_0000#;
   Launcher_ID : constant Unsigned_64 := 16#3456_789A_0100_000C#;
   Other_Launcher_ID : constant Unsigned_64 := 16#3456_789A_0200_000C#;
   Child_ID : constant Unsigned_64 := 16#1234_5678_0700_002A#;
   Driver_ID : constant Unsigned_64 := 16#2345_6789_0900_004D#;
   procedure Receipt (Token : Unsigned_64) is
   begin
      B.Complete (Object,
        (Token, Driver_ID, 0, ((16#0A21#, 4, 0, 0), [0, 1, Base + 1, (if Token = 1000 then 40 else 0)]), True),
        1, Used);
      pragma Assert (Used);
   end Receipt;
begin
   Inspection := [1, 11, 0, Driver_ID, 0, 0];
   Application_Inspection := [1, 9, 0, Child_ID, 0, 0];
   B.Begin_Request (Object, Launcher_ID, Other_Launcher_ID, R.Authority_Tag, Request, 0, 100, Bad);
   pragma Assert (Bad = 0 and Save_Count = 0);
   Save_Result := 0;
   B.Begin_Request (Object, Launcher_ID, Launcher_ID, R.Authority_Tag, Request, 0, 100, Bad);
   pragma Assert (Bad = 0 and not B.Runnable (Object));
   Save_Result := 1;
   B.Begin_Request (Object, Launcher_ID, Launcher_ID, R.Authority_Tag, Request, 0, 100, ID);
   pragma Assert (ID = 1 and Saved_Slot = 15);
   B.Begin_Request (Object, Launcher_ID, Launcher_ID, R.Authority_Tag, Request, 0, 100, Bad);
   pragma Assert (Bad = 0 and Save_Count = 2);
   B.Step (Object, 1);
   pragma Assert (Last_Token = 1000 and not B.Runnable (Object));
   Receipt (1000);
   B.Step (Object, 1); -- recipient endpoint
   B.Step (Object, 1); -- application endpoint
   B.Step (Object, 1); -- activate
   pragma Assert (Last_Token = 1001 and Reply_Count = 0);
   Receipt (1001);
   pragma Assert (B.Runnable (Object)); -- terminal admission still needs reply
   Reply_Result := 0;
   B.Step (Object, 1);
   pragma Assert (B.State (Object, ID) = L.Retained and Reply_Count = 1);
   pragma Assert (Replied_Slot = 15 and Last_Reply.words (1) = 0 and
     Last_Reply.words (2) = 123 and Last_Reply.words (3) = Child_ID);
   pragma Assert (B.Runnable (Object)); -- failed delivery schedules abort
   B.Step (Object, 1);
   pragma Assert (Last_Token = 1002 and Last_Submit.words (3) = 2);
   Receipt (1002);
   B.Step (Object, 1);
   pragma Assert (Reply_Count = 1 and not B.Runnable (Object));
   Request.words := [1, 40, 5, 124];
   Reply_Result := 1;
   B.Begin_Request (Object, Launcher_ID, Launcher_ID, R.Authority_Tag, Request, 1, 1, ID);
   pragma Assert (ID = 2 and B.Runnable (Object));
   B.Step (Object, 1);
   pragma Assert (Reply_Count = 2 and Last_Reply.words (1) = 1 and
     Last_Reply.words (2) = 124 and B.State (Object, ID) = L.Retained);
   declare
      Fresh : B.Broker;
      First : B.Ticket;
      Before : constant Natural := Reply_Count;
      procedure Fresh_Receipt (Token : Unsigned_64) is
      begin
         B.Complete (Fresh,
           (Token, Driver_ID, 0, ((16#0A21#, 4, 0, 0), [0, 1, Base + 1, (if Token = 1000 then 40 else 0)]), True),
           2, Used);
         pragma Assert (Used);
      end Fresh_Receipt;
   begin
      B.Begin_Request (Fresh, Launcher_ID, Launcher_ID, R.Authority_Tag, Request, 1, 100, First);
      B.Step (Fresh, 2);
      Fresh_Receipt (1000);
      B.Step (Fresh, 2);
      B.Step (Fresh, 2);
      B.Step (Fresh, 2);
      Fresh_Receipt (1001);
      B.Step (Fresh, 2);
      pragma Assert (B.State (Fresh, First) = L.Acknowledged and
        Reply_Count = Before + 1 and not B.Runnable (Fresh));
      B.Step (Fresh, 3);
      pragma Assert (Reply_Count = Before + 1);
      -- Explicit cancellation after delivery still drives remote abort.
      B.Cancel (Fresh, First);
      B.Step (Fresh, 3);
      pragma Assert (Last_Token = 1002);
   end;
   declare
      Fresh : B.Broker;
      First : B.Ticket;
      Before : constant Natural := Reply_Count;
   begin
      B.Begin_Request (Fresh, Launcher_ID, Launcher_ID, R.Authority_Tag, Request, 1, 10, First);
      B.Step (Fresh, 2);
      B.Step (Fresh, 10);
      pragma Assert (not B.Runnable (Fresh) and
        B.Next_Deadline (Fresh) = Unsigned_64'Last);
      -- Late reserve success must abort, never send launch success.
      B.Complete (Fresh,
        (1000, Driver_ID, 0, ((16#0A21#, 4, 0, 0), [0, 1, Base + 1, 40]), True),
        11, Used);
      B.Step (Fresh, 11);
      pragma Assert (Used and Last_Token = 1002 and Reply_Count = Before);
      B.Complete (Fresh,
        (1002, Driver_ID, 0, ((16#0A21#, 4, 0, 0), [0, 1, Base + 1, 0]), True),
        12, Used);
      B.Step (Fresh, 12);
      pragma Assert (Used and Reply_Count = Before + 1 and
        Last_Reply.words (1) = 1);
   end;
   declare
      function Unsafe_Slot (ID : Positive) return CapabilitySlot is (40);
      package Unsafe is new Intel_Render_Broker (31, 3000, 4000, Unsafe_Slot);
      Fresh : Unsafe.Broker;
      First : Unsafe.Ticket;
      Before : constant Natural := Save_Count;
   begin
      Unsafe.Begin_Request (Fresh, Launcher_ID, Launcher_ID, R.Authority_Tag, Request, 1, 100, First);
      pragma Assert (First = 0 and Save_Count = Before);
   end;
   Put_Line ("saved-reply broker PASS");
end Broker_Tests;
