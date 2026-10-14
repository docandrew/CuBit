with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Handles;
with CuBit.Handles.Fixture;
with CuBit.Messages; use CuBit.Messages;
with Intel_Render_Admission;
with Intel_Render_Admission_Native; use Intel_Render_Admission_Native;
--  CAP-002: admission by asynchronous submissions to the driver's endpoint;
--  the reservation's session tag is the only authority (no delegation).
procedure Admission_Native_Tests is
   package Core renames Intel_Render_Admission;
   use type Core.Phase;
   use type CuBit.Handles.Endpoint;
   Item : Broker_Request;
   Receipt : CompletionEntry;
   Used : Boolean;
   Session : constant Unsigned_64 := 16#A123_0000_0000_0087#;
   Client_ID : constant Unsigned_64 := 16#1234_5678_0700_002A#;
   Driver_ID : constant Unsigned_64 := 16#2345_6789_0900_004D#;
   Other_Driver_ID : constant Unsigned_64 := 16#2345_6789_0A00_004D#;
   Driver : constant CuBit.Handles.Endpoint :=
     CuBit.Handles.Fixture.Endpoint_Of (16#0000_0003_0000_001F#);
   Control : constant MessageTag := (label => 16#0A21#, length => 4,
                                     protocol => 0, ipc => <>, reserved => 0);
begin
   --  No identity, no driver: refused before any submission.
   declare
      Bad : Broker_Request;
   begin
      Start (Bad, 0, Driver, Driver_ID);
      pragma Assert (State (Bad) = Core.Failed);
      Start (Bad, Client_ID, CuBit.Handles.No_Endpoint, Driver_ID);
      pragma Assert (State (Bad) = Core.Failed);
   end;
   Start (Item, Client_ID, Driver, Driver_ID);
   Advance (Item, 100);
   pragma Assert (State (Item) = Core.Reserve_Pending);
   pragma Assert (Last_Submit.tag = Control and Last_Submit.words = [1, Client_ID, 0, 0]);
   pragma Assert (Last_Target = Driver and Last_Token = 100);
   Receipt := (token => 99, from => Driver_ID, status => 0,
               msg => (Control, 0, [0, 1, Session, 0]), valid => True);
   Complete (Item, Receipt, Used); pragma Assert (not Used);
   Receipt.token := 100;
   Complete (Item, Receipt, Used);
   pragma Assert (Used and State (Item) = Core.Activate_Ready);
   pragma Assert (Intel_Render_Admission_Native.Session (Item) = Session);
   Advance (Item, 101);
   pragma Assert (Last_Submit.words = [1, Client_ID, Session, 1]);
   Receipt.token := 101;
   Complete (Item, Receipt, Used);
   pragma Assert (Used and State (Item) = Core.Active);
   Cancel (Item);
   pragma Assert (State (Item) = Core.Abort_Ready);
   Advance (Item, 102);
   pragma Assert (Last_Submit.words = [1, Client_ID, Session, 2]);
   Receipt.token := 102;
   Complete (Item, Receipt, Used);
   pragma Assert (Used and State (Item) = Core.Failed);
   --  Every malformed or misattributed receipt quarantines (or is ignored).
   for Fault in 0 .. 8 loop
      declare
         Broken : Broker_Request;
         Bad : CompletionEntry :=
           (token => 200, from => Driver_ID, status => 0,
            msg => (Control, 0, [0, 1, 123, 0]), valid => True);
      begin
         Start (Broken, Client_ID, Driver, Driver_ID);
         Advance (Broken, 200);
         case Fault is
            when 0 => Bad.from := Other_Driver_ID;
            when 1 => Bad.status := 1;
            when 2 => Bad.msg.tag.label := 0;
            when 3 => Bad.msg.tag.length := 3;
            when 4 => Bad.msg.tag.protocol := 1;
            when 5 => Bad.msg.tag.reserved := 1;
            when 6 => Bad.msg.words (1) := 2;
            --  A reply naming a recipient slot (the retired protocol).
            when 7 => Bad.msg.words (3) := 40;
            when others => Bad.valid := False;
         end case;
         Complete (Broken, Bad, Used);
         if Fault = 8 then
            pragma Assert (not Used and State (Broken) = Core.Reserve_Pending);
            Bad.valid := True;
            Cancel (Broken);
            Complete (Broken, Bad, Used);
            pragma Assert (Used and State (Broken) = Core.Abort_Ready);
         else
            pragma Assert (Used and State (Broken) = Core.Quarantined);
            Complete (Broken, Bad, Used); pragma Assert (not Used);
         end if;
      end;
   end loop;
   --  A refused submission fails the reservation; nothing is retried.
   declare
      Rejected : Broker_Request;
   begin
      Start (Rejected, Client_ID, Driver, Driver_ID);
      Submit_Result := False;
      Advance (Rejected, 300);
      pragma Assert (State (Rejected) = Core.Failed);
      Submit_Result := True;
      Advance (Rejected, 301);
      pragma Assert (State (Rejected) = Core.Failed and Last_Token = 300);
   end;
   --  A refused activation submission aborts the reservation.
   declare
      Late : Broker_Request;
      R : constant CompletionEntry :=
        (token => 400, from => Driver_ID, status => 0,
         msg => (Control, 0, [0, 1, Session, 0]), valid => True);
   begin
      Start (Late, Client_ID, Driver, Driver_ID);
      Advance (Late, 400);
      Complete (Late, R, Used);
      Submit_Result := False;
      Advance (Late, 401);
      Submit_Result := True;
      pragma Assert (State (Late) = Core.Abort_Ready);
      Advance (Late, 402);
      pragma Assert (Last_Submit.words (3) = 2);
   end;
   Put_Line ("NATIVE-ADMISSION-ADAPTER: PASS mocked IPC, no delegation");
end Admission_Native_Tests;
