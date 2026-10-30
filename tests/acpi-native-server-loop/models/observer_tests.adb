pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Requests;
with ACPI_Service;
with ACPI_Endpoint;
with ACPI_Protocol;
with ACPI_Observer_Endpoint;
with ACPI_Native_Endpoint;
with ACPI_Native_Server;
with Firmware_Tables;
with CuBit.Messages;
with CuBit.Refusal_Notes;
with Log_Capture;
procedure Observer_Tests is
   package Q renames ACPI_Requests;
   package M renames CuBit.Messages;
   use type Q.Outcome;
   use type Q.Words;
   use type ACPI_Native_Server.Stop_Reason;
   use type M.MessageWords;
   use type M.Process_ID;
   use type ACPI_Protocol.Role_Tag;
   Server : Q.State (2, 4_096, 4_096, 0);
   Checks : Natural := 0;
   Untrusted : constant ACPI_Protocol.Role_Tag := ACPI_Protocol.Observer_Tag xor 1;
   Stamps : constant array (Positive range <>) of ACPI_Protocol.Role_Tag :=
     [0, Untrusted, ACPI_Protocol.Role_Tag'Last, ACPI_Protocol.Observer_Tag];
   Labels : constant array (Positive range <>) of Unsigned_32 :=
     [Q.Read_Metrics, Q.Start_Snapshot, Q.Begin_Table, Q.Write_Chunk,
      Q.Commit_Table, Q.Finish_Snapshot, Q.Read_Table_Info, Q.Read_Table_Chunk,
      8, Q.Read_MCFG_Info, Q.Read_MCFG_Allocation, Q.Read_SLIT_Info,
      Q.Read_SLIT_Distance, Q.Read_MADT_Info, Q.Read_MADT_Record,
      Q.Read_MADT_Fields, Q.Read_SRAT_Info, Q.Read_SRAT_Record,
      Q.Read_SRAT_Fields, Q.Read_DMAR_Info, Q.Read_DMAR_Record,
      Q.Read_DMAR_Fields, Q.Read_DMAR_Scope, Q.Read_DMAR_Path,
      Q.Configure_Notifications, Q.Claim_Notification, Q.Read_Notification,
      Q.Acknowledge_Notification, Q.Read_Notification_State, Unsigned_32'Last];
   Request, Reply : Q.Packet;
   Native_Request, Native_Reply : M.Message;
   Reason : ACPI_Native_Server.Stop_Reason;
   High_Process : constant M.Process_ID := M.From_Word (16#FEED_BEEF_1200_0001#);
   procedure Check (Condition : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Label; end if;
   end Check;
   procedure Prepare is
      DSDT : Firmware_Tables.Bytes (1 .. 37) := [others => 0];
      Sum : Firmware_Tables.Byte := 0;
      Response : Q.Response;
   begin
      DSDT (1 .. 4) := [16#44#, 16#53#, 16#44#, 16#54#];
      DSDT (5) := DSDT'Length; DSDT (9) := 2; DSDT (37) := 16#A3#;
      for B of DSDT loop Sum := Sum + B; end loop;
      DSDT (10) := 0 - Sum;
      Q.Handle (Server, Q.Snapshot_Provider,
        (Label => Q.Start_Snapshot, Data => [Q.Revision (Server), 1, 0, 0], others => <>), Response);
      Check (Response.Status = Q.OK, "local start");
      Q.Import_Block (Server, Q.Snapshot_Provider, Q.Revision (Server), 1,
                      ACPI_Service.DSDT, DSDT, Response);
      Check (Response.Status = Q.OK, "local DSDT import");
      Q.Handle (Server, Q.Snapshot_Provider,
        (Label => Q.Finish_Snapshot, Data => [Q.Revision (Server), 0, 0, 0], others => <>), Response);
      Check (Response.Status = Q.OK, "local startup");
   end Prepare;
   function Mutator (Label : Unsigned_32) return Boolean is
     (Label in Q.Start_Snapshot .. Q.Finish_Snapshot |
       Q.Configure_Notifications | Q.Claim_Notification | Q.Acknowledge_Notification);
begin
   for Phase in 0 .. 1 loop
      if Phase = 1 then Prepare; end if;
      for Stamp of Stamps loop
         for Label of Labels loop
            declare
               Version : constant Q.Revision_Number := Q.Revision (Server);
               Tables : constant Natural := Q.Observe (Server).Tables;
            begin
               Request := (Label => Label, Data => [Version, 1, 0, 0], others => <>);
               ACPI_Observer_Endpoint.Dispatch (Server, Stamp, Request, Reply);
               Check (Q.Revision (Server) = Version and Q.Observe (Server).Tables = Tables,
                      "every observer dispatch leaves state unchanged");
               Check (Reply.Length = 4 and Reply.Flags = 0 and Reply.Reserved = 0,
                      "canonical reply envelope");
               if not ACPI_Protocol.Is_Observer (Stamp) then
                  Check (Reply.Label = ACPI_Endpoint.Reply_Error and Reply.Data =
                    [Unsigned_64 (Q.Outcome'Pos (Q.Denied)), 0, 0, 0], "unknown badge reveals no data");
               elsif Mutator (Label) then
                  Check (Reply.Label = ACPI_Endpoint.Reply_Error and
                    Reply.Data (0) = Unsigned_64 (Q.Outcome'Pos (Q.Denied)), "observer cannot mutate");
               end if;
            end;
         end loop;
      end loop;
   end loop;
   for Page in 0 .. Natural (Q.Metrics_Last_Page) loop
      Request := (Label => Q.Read_Metrics,
                  Data => [Unsigned_64 (Page), 0, 0, 0], others => <>);
      ACPI_Observer_Endpoint.Dispatch (Server, ACPI_Protocol.Observer_Tag, Request, Reply);
      Check (Reply.Label = ACPI_Endpoint.Reply_OK, "metrics page readable");
   end loop;
   -- Malformed envelope remains malformed even when the stamp is legitimate.
   Request := (Label => Q.Read_Metrics, Length => 3, others => <>);
   ACPI_Observer_Endpoint.Dispatch (Server, ACPI_Protocol.Observer_Tag, Request, Reply);
   Check (Reply.Label = ACPI_Endpoint.Reply_Error, "short packet refused");
   Request := (Label => Q.Read_Metrics, Flags => 1, others => <>);
   ACPI_Observer_Endpoint.Dispatch (Server, ACPI_Protocol.Observer_Tag, Request, Reply);
   Check (Reply.Label = ACPI_Endpoint.Reply_Error, "packet flags refused");
   -- The native boundary uses only authorityTag. Words/reserved/PID cannot confer it.
   Log_Capture.Reset (False);
   Native_Request := M.NULL_MESSAGE;
   Native_Request.tag := (label => Q.Read_Metrics, length => 4,
     protocol => 0, ipc => <>, reserved => 16#5356#);
   Native_Request.words := [Unsigned_64 (ACPI_Protocol.Observer_Tag), 0, 0, 0];
   ACPI_Native_Endpoint.Dispatch (Server, High_Process, Native_Request, Native_Reply);
   Check (Native_Reply.words = [Unsigned_64 (Q.Outcome'Pos (Q.Denied)), 0, 0, 0], "payload cannot badge request");
   Check (Log_Capture.Attempts = 1, "first refusal logged");
   ACPI_Native_Endpoint.Dispatch (Server, High_Process, Native_Request, Native_Reply);
   Check (Log_Capture.Attempts = 1 and CuBit.Refusal_Notes.Repeated = 1, "repeats counted");
   Log_Capture.Reset (True);
   ACPI_Native_Endpoint.Dispatch (Server, M.From_Word (16#DEAD_BEEF_1200_0001#), Native_Request, Native_Reply);
   Check (Native_Reply.words (0) = Unsigned_64 (Q.Outcome'Pos (Q.Denied)), "logging exception never authorizes");
   Log_Capture.Reset (False);
   Native_Request := M.NULL_MESSAGE;
   Native_Request.tag := (label => Q.Read_Metrics, length => 4, protocol => 0, ipc => <>, reserved => 0);
   Native_Request.authorityTag := Unsigned_64 (ACPI_Protocol.Observer_Tag);
   Native_Request.words := [Q.Revision (Server), 0, 0, 0];
   M.Reset;
   M.Enqueue (High_Process, Native_Request);
   M.Test.Reply_Result := 0; -- Lost/one-way delivery never repeats dispatch.
   ACPI_Native_Server.Run (Server, Reason);
   Check (Reason = ACPI_Native_Server.Wait_Unavailable, "wait unavailable terminates");
   Check (M.Test.Reply_Count = 1 and M.Test.Taken = 1, "one request one reply attempt");
   Check (M.Test.Reply_To (1) = High_Process, "full opaque process identity retained");
   Check (M.Test.Replies (1).tag.label = ACPI_Endpoint.Reply_OK, "real observer served without handshake");
   Check (M.Test.Wait_Count = 1 and M.Test.Deadlines (1) = M.Wait_Forever, "explicit idle deadline");
   Check (M.Test.Clock_Count = 0, "no obsolete provider retry timer");
   M.Reset; M.Test.Completion := 1;
   ACPI_Native_Server.Run (Server, Reason);
   Check (Reason = ACPI_Native_Server.Unexpected_Completion and
     M.Test.Completion_Count = 1 and M.Test.Poll_Count = 0 and M.Test.Wait_Count = 0,
     "unexpected completion stops instead of spinning");
   M.Reset; M.Test.Wake_Credits := 1;
   ACPI_Native_Server.Run (Server, Reason);
   Check (Reason = ACPI_Native_Server.Wait_Unavailable and M.Test.Wait_Count = 2,
     "spurious wake is repolled and stops on unavailable");
   Put_Line ("Observer and native runner checks" & Checks'Image);
end Observer_Tests;
