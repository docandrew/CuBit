with Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables;
with ACPI_Requests;
with ACPI_Launch;
with ACPI_Endpoint;
with ACPI_Native_Blocks;
with ACPI_Native_Server; use ACPI_Native_Server;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
procedure Loop_Tests is
   package Grants renames CuBit.Memory_Grants;
   Server, Server_2 : ACPI_Requests.State (2, 128, 64, 0);
   Before : ACPI_Requests.State_Model (2, 128, 64) with Ghost;
   Adapter : ACPI_Native_Blocks.State;
   Config : constant ACPI_Endpoint.Configuration := (17, 31);
   Reason : Stop_Reason;
   Raw : aliased Firmware_Tables.Bytes (1 .. 36) := [others => 0];
   Checks : Natural := 0;
   use type ACPI_Requests.State_Model;
   use type ACPI_Requests.Phase;
   procedure Check (B : Boolean) is
   begin
      Checks := Checks + 1;
      if not B then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Reset_Transport is
   begin
      Used := 0; Next := 0; Sent := 0; Waits := 0; Polls := 0; Now := 0;
      Completion_Ready := False; Fail_Reply := False; Repair_On_Wait := False;
   end Reset_Transport;
   procedure Queue_Snapshot is
   begin
      Used := 3;
      Incoming (1) := ((ACPI_Requests.Start_Snapshot, 4, 0, 0), 31, [0, 1, 0, 0]);
      Incoming (2) := ((ACPI_Native_Blocks.Import_Table_Grant, 4, 0, 0), 31,
        [1, 27 * 2 ** 32 + 19, 1, 36]);
      Incoming (3) := ((ACPI_Requests.Finish_Snapshot, 4, 0, 0), 31, [2, 0, 0, 0]);
   end Queue_Snapshot;
begin
   Before := ACPI_Requests.Model (Server);
   Run (Server, Adapter, (0, 0), 7, Reason);
   pragma Assert (ACPI_Requests.Model (Server) = Before);
   Check (Reason = Invalid_Configuration and Polls = 0 and Waits = 0);
   Run (Server, Adapter, Config, 7, Reason);
   Check (Reason = Wait_Unavailable and Last_Deadline = Unsigned_64'Last and Sent = 0);
   Reset_Transport; Completion_Ready := True;
   Run (Server, Adapter, Config, 7, Reason);
   Check (Reason = Unexpected_Completion and Polls = 0 and Waits = 0);
   Reset_Transport; Now := Unsigned_64'Last;
   Run (Server, Adapter, Config, 7, Reason);
   Check (Reason = Clock_Unavailable and Polls = 0 and Waits = 0);
   Raw (1 .. 4) := [16#44#, 16#53#, 16#44#, 16#54#]; Raw (5) := 36; Raw (9) := 2;
   declare Sum : Firmware_Tables.Byte := 0; begin
      for B of Raw loop Sum := Sum + B; end loop; Raw (10) := 0 - Sum;
   end;
   Reset_Transport; Queue_Snapshot;
   Grants.Source := Raw'Address; Grants.Return_OK := False;
   Repair_On_Wait := True; Fail_Reply := True;
   Run (Server, Adapter, Config, 7, Reason);
   Check (Reason = Wait_Unavailable and ACPI_Requests.Current (Server) = ACPI_Requests.Complete);
   Check (Sent = 3 and Next = 3 and Grants.Acquisitions = 1 and Grants.Returns = 2);
   Check (not ACPI_Native_Blocks.Pending (Adapter));
   Check (Waits = 2 and Now = 100 and Last_Deadline = Unsigned_64'Last);
   for I in 1 .. Sent loop
      Check (Replies (I).tag.label = ACPI_Endpoint.Reply_OK);
      Check (Replies (I).authorityTag = 0 and Replies (I).tag.reserved = 0);
   end loop;
   -- Fatal wait preserves a completed import and its pending cleanup in caller state.
   Reset_Transport; Queue_Snapshot;
   Grants.Return_OK := False;
   Run (Server_2, Adapter, Config, 7, Reason);
   Check (Reason = Wait_Unavailable and ACPI_Native_Blocks.Pending (Adapter));
   Check (Last_Deadline = 100 and Grants.Acquisitions = 2 and Sent = 3);
   Check (ACPI_Requests.Current (Server_2) = ACPI_Requests.Complete);
   Before := ACPI_Requests.Model (Server_2);
   Reset_Transport; Now := Unsigned_64'Last - 50;
   Run (Server_2, Adapter, Config, 7, Reason);
   Check (Reason = Wait_Unavailable and Last_Deadline = Unsigned_64'Last - 1);
   pragma Assert (ACPI_Requests.Model (Server_2) = Before);
   Check (ACPI_Native_Blocks.Pending (Adapter));
   Reset_Transport; Completion_Ready := True;
   Run (Server_2, Adapter, Config, 7, Reason);
   Check (Reason = Unexpected_Completion and ACPI_Native_Blocks.Pending (Adapter));
   Reset_Transport;
   Run (Server_2, Adapter, (0, 0), 7, Reason);
   Check (Reason = Invalid_Configuration and ACPI_Native_Blocks.Pending (Adapter) and Waits = 0);
   -- Restart the loop with the same state; cleanup only, no repeated import.
   Reset_Transport; Repair_On_Wait := True;
   Run (Server_2, Adapter, Config, 7, Reason);
   pragma Assert (ACPI_Requests.Model (Server_2) = Before);
   Check (not ACPI_Native_Blocks.Pending (Adapter));
   Check (Grants.Acquisitions = 2 and Sent = 0 and Waits = 2);
   declare
      Configured : ACPI_Endpoint.Configuration;
      Provider : CapabilitySlot;
      Accepted : Boolean;
      Request : ACPI_Requests.Packet :=
        (ACPI_Launch.Configure, 4, 0, 0, [17, 31, 7, 0]);
   begin
      Check (not ACPI_Launch.Decode (0, Request).Accepted);
      for Field in 0 .. 9 loop
         Request := (ACPI_Launch.Configure, 4, 0, 0, [17, 31, 7, 0]);
         case Field is
            when 0 => Request.Label := 0;
            when 1 => Request.Length := 3;
            when 2 => Request.Flags := 1;
            when 3 => Request.Reserved := 1;
            when 4 => Request.Data (0) := 0;
            when 5 => Request.Data (1) := 17;
            when 6 => Request.Data (2) := 63;
            when 7 => Request.Data (3) := 1;
            when 8 => Request.Data (0) := ACPI_Launch.Bootstrap_Tag;
            when 9 => Request.Data (1) := ACPI_Launch.Bootstrap_Tag;
         end case;
         Check (not ACPI_Launch.Decode (ACPI_Launch.Bootstrap_Tag, Request).Accepted);
      end loop;
      Reset_Transport;
      Await_Configuration (Configured, Provider, Accepted);
      Check (not Accepted and Configured.Observer_Tag = 0 and Configured.Provider_Tag = 0);
      Reset_Transport; Used := 3; Fail_Reply := True;
      Incoming (1) := ((ACPI_Launch.Configure, 4, 0, 0), 0, [17, 31, 7, 0]);
      Incoming (2) := ((ACPI_Launch.Configure, 4, 0, 0), ACPI_Launch.Bootstrap_Tag, [17, 31, 7, 0]);
      Incoming (3) := ((ACPI_Launch.Configure, 4, 0, 0), ACPI_Launch.Bootstrap_Tag, [41, 43, 9, 0]);
      Await_Configuration (Configured, Provider, Accepted);
      Check (Accepted and Provider = 7 and Configured.Observer_Tag = 17 and Configured.Provider_Tag = 31);
      Check (Next = 2 and Sent = 2 and Replies (1).tag.label = ACPI_Endpoint.Reply_Error);
      Run (Server_2, Adapter, Configured, Provider, Reason);
      Check (Next = 3 and Sent = 3 and Replies (3).tag.label = ACPI_Endpoint.Reply_Error);
      Check (Configured.Observer_Tag = 17 and Configured.Provider_Tag = 31);
   end;
   Ada.Text_IO.Put_Line ("ACPI-NATIVE-LOOP: PASS" & Checks'Image);
end Loop_Tests;
