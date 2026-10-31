pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
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
with CuBit.Control_Events;
with CuBit.Channel_Bells;
with Channel_Wake_ABI;
with ACPI_Native_Events;
with ACPI_Native_Log;
procedure Observer_Tests is
   package Q renames ACPI_Requests;
   package M renames CuBit.Messages;
   use type Q.Outcome;
   use type Q.Words;
   use type ACPI_Native_Server.Stop_Reason;
   use type M.MessageWords;
   use type M.Process_ID;
   use type ACPI_Protocol.Role_Tag;
   Server : Q.State (2, 4_096, 4_096, 2, 0);
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
   Owner_Calls : Natural := 0;
   Owner_Turns : Natural := 0;
   Refuse_Owner : Boolean := False;
   Expected_Events : Natural := 0;
   procedure Advance (State : in out Q.State; Result : out ACPI_Native_Server.Owner_Outcome) is
   begin
      Check (Q.Valid (State), "owner receives valid state");
      Check (M.Test.Events_Taken = Expected_Events, "event consumed before owner work");
      Check (M.Test.Poll_Count = Owner_Calls, "one IPC opportunity between owner batches");
      Owner_Calls := Owner_Calls + 1;
      if Refuse_Owner then Result := ACPI_Native_Server.Owner_Refused;
      elsif Owner_Calls <= Owner_Turns then Result := ACPI_Native_Server.Owner_Pending;
      else Result := ACPI_Native_Server.Owner_Idle;
      end if;
   end Advance;
   package Owner_Loop is new ACPI_Native_Server.With_Dispatch
     (ACPI_Native_Endpoint.Dispatch, Advance_Owner => Advance);
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
   declare
      Raised : Boolean := False;
      Prior_Revision : constant Unsigned_64 := Q.Revision (Server);
   begin
      --  Only the hosted harness observes this fault; native execution terminates.
      begin
         ACPI_Native_Endpoint.Dispatch (Server, M.From_Word (16#DEAD_BEEF_1200_0001#), Native_Request, Native_Reply);
      exception
         when Error : Program_Error =>
            Check (Ada.Exceptions.Exception_Message (Error) = "logger unavailable",
              "exact modeled logger contract fault");
            Raised := True;
      end;
      Check (Raised, "logger contract fault cannot return a synthetic reply");
      Check (Q.Revision (Server) = Prior_Revision and then Log_Capture.Attempts = 1,
        "logger fault precedes dispatch and leaves server revision unchanged");
   end;
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
   declare
      package CV renames CuBit.Control_Events;
      Event : M.Message := M.NULL_MESSAGE;
      Stop, Handled : Boolean;
      Bell_Count : Natural;
      Other_Process : constant M.Process_ID := M.From_Word (16#1122_3345_5566_7701#);
   begin
      Event.tag.label := CV.Control_Label; Event.tag.length := CV.Control_Words;
      Event.words (0) := CV.Control_Kind'Enum_Rep (CV.Stop);
      M.Reset; M.Enqueue_Event (High_Process, Event); M.Enqueue (High_Process, Native_Request);
      ACPI_Native_Server.Run (Server, Reason);
      Check (Reason = ACPI_Native_Server.Wait_Unavailable and M.Test.Reply_Count = 1,
        "process-published stop cannot terminate observer service");
      M.Reset; M.Enqueue_Event (M.No_Process, Event); M.Enqueue (High_Process, Native_Request);
      ACPI_Native_Server.Run (Server, Reason);
      Check (Reason = ACPI_Native_Server.Stop_Requested and M.Test.Poll_Count = 0
        and M.Test.Reply_Count = 0 and M.Test.Wait_Count = 0, "kernel stop precedes new request dispatch");
      declare
         First : M.Message := M.NULL_MESSAGE;
         Seen : M.Message;
         Publisher : M.Process_ID;
         Found : Boolean;
      begin
         First.tag.label := 16#0120#;
         -- Model control: mixed receive is allowed to consume the event lane.
         M.Reset; M.Enqueue_Event (High_Process, First); M.Enqueue (High_Process, Native_Request);
         M.Poll_Any_Ipc (Publisher, Seen, Found);
         Check (Found and Publisher = High_Process and Seen.tag.label = First.tag.label
           and M.Test.Events_Taken = 1 and M.Test.Taken = 0,
           "mixed receive model exposes event consumption");
         M.Reset; M.Enqueue_Event (High_Process, First); M.Enqueue (High_Process, Native_Request);
         M.Poll_Service_Request (Publisher, Seen, Found);
         Check (Found and Seen.tag.label = Native_Request.tag.label and M.Test.Events_Taken = 0,
           "service receive leaves queued events to their handler");
         M.Reset; M.Enqueue_Event (High_Process, First); M.Enqueue_Event (M.No_Process, Event);
         M.Enqueue (High_Process, Native_Request); M.Enqueue (High_Process, Native_Request);
         ACPI_Native_Server.Run (Server, Reason);
         Check (Reason = ACPI_Native_Server.Stop_Requested and M.Test.Events_Taken = 2,
           "second queued stop reaches event handler");
         Check (M.Test.Taken = 1 and M.Test.Reply_Count = 1 and M.Test.Wait_Count = 0,
           "second queued stop wins before second service request");
         Check (M.Test.Mixed_Polls = 0, "native server never consumes mixed IPC");
      end;
      for Invalid in 1 .. 4 loop
         Event.tag.protocol := 0; Event.tag.ipc := M.No_IPC_Flags; Event.tag.reserved := 0; Event.tag.length := CV.Control_Words;
         case Invalid is
            when 1 => Event.tag.protocol := 1;
            when 2 => Event.tag.ipc := (others => True);
            when 3 => Event.tag.reserved := 1;
            when others => Event.tag.length := 3;
         end case;
         M.Reset; M.Enqueue_Event (M.No_Process, Event); ACPI_Native_Server.Run (Server, Reason);
         Check (Reason = ACPI_Native_Server.Wait_Unavailable and M.Test.Events_Taken = 1
           and M.Test.Wait_Count = 1, "malformed stop drained without stopping or busy waiting");
      end loop;
      Event := M.NULL_MESSAGE; Event.tag.label := 16#0120#;
      M.Reset;
      for I in 1 .. 8 loop M.Enqueue_Event (High_Process, Event); end loop;
      M.Enqueue (High_Process, Native_Request); ACPI_Native_Server.Run (Server, Reason);
      Check (M.Test.Events_Taken = 8 and M.Test.Reply_Count = 1 and M.Test.First_IPC_Event_Count = 1,
        "event traffic permits IPC after first event,not after draining the entire lane");
      Check (M.Test.Wait_Count = 1 and Reason = ACPI_Native_Server.Wait_Unavailable,
        "unknown events consumed before explicit idle wait");
      Event := M.NULL_MESSAGE; Event.tag.label := Channel_Wake_ABI.Wake_Label;
      Event.tag.length := 1; Event.words (0) := 16#0000_0001_0000_0001#;
      Bell_Count := CuBit.Channel_Bells.Notices;
      ACPI_Native_Events.Handle (M.No_Process, Event, Stop, Handled);
      Check (Handled and not Stop and CuBit.Channel_Bells.Notices = Bell_Count + 1,
        "valid kernel channel notice routed to process registry");
      ACPI_Native_Events.Handle (High_Process, Event, Stop, Handled);
      Check (not Stop and CuBit.Channel_Bells.Notices = Bell_Count + 1, "peer cannot forge kernel channel wake");
      Event.words (0) := 0; ACPI_Native_Events.Handle (M.No_Process, Event, Stop, Handled);
      Check (CuBit.Channel_Bells.Notices = Bell_Count + 1, "invalid generation never reaches registry");
      ACPI_Native_Log.Peer := High_Process; ACPI_Native_Log.Notices := 0; ACPI_Native_Log.Disconnects := 0;
      Event := M.NULL_MESSAGE; Event.tag.label := CV.Grant_Returned_Label;
      Event.tag.length := CV.Grant_Event_Words; Event.words := [1, 1, M.To_Word (Other_Process), 0];
      ACPI_Native_Events.Handle (M.No_Process, Event, Stop, Handled);
      Check (ACPI_Native_Log.Notices = 1 and ACPI_Native_Log.Disconnects = 0,
        "other full-width peer lifetime does not disable logstore");
      Event.words (2) := M.To_Word (High_Process);
      ACPI_Native_Events.Handle (M.No_Process, Event, Stop, Handled);
      Check (ACPI_Native_Log.Notices = 2 and ACPI_Native_Log.Disconnects = 1,
        "matching full-width logstore peer reaches actual lifetime policy");
      Event.words (1) := 0; ACPI_Native_Events.Handle (M.No_Process, Event, Stop, Handled);
      Check (ACPI_Native_Log.Notices = 2, "invalid grant generation rejected before logger callback");
      Event.words (1) := 1; ACPI_Native_Events.Handle (High_Process, Event, Stop, Handled);
      Check (ACPI_Native_Log.Notices = 2, "nonkernel lifetime event rejected");
   end;

   -- Pending local work must not sleep; IPC remains fair between batches.
   M.Reset; Owner_Calls := 0; Owner_Turns := 3;
   M.Enqueue (High_Process, Native_Request);
   Owner_Loop.Serve (Server, Reason);
   Check (Owner_Calls = 4 and M.Test.Poll_Count = 4 and M.Test.Wait_Count = 1,
     "pending batches continue until explicit idle then wait once");
   Check (M.Test.Reply_Count = 1 and M.Test.Taken = 1 and
     Reason = ACPI_Native_Server.Wait_Unavailable, "owner work preserves one dispatch per request");
   -- Refusal is terminal for this serving attempt,not a hot retry or dispatch.
   M.Reset; Owner_Calls := 0; Refuse_Owner := True;
   M.Enqueue (High_Process, Native_Request);
   Owner_Loop.Serve (Server, Reason);
   Check (Reason = ACPI_Native_Server.Owner_Work_Refused and Owner_Calls = 1
     and M.Test.Poll_Count = 0 and M.Test.Reply_Count = 0 and M.Test.Wait_Count = 0,
     "owner refusal stops before new IPC or waiting");
   Refuse_Owner := False;
   declare
      Event : M.Message := M.NULL_MESSAGE;
   begin
      Event.tag.label := CuBit.Control_Events.Control_Label;
      Event.tag.length := CuBit.Control_Events.Control_Words;
      Event.words (0) := CuBit.Control_Events.Control_Kind'Enum_Rep (CuBit.Control_Events.Stop);
      M.Reset; Owner_Calls := 0; M.Enqueue_Event (M.No_Process, Event);
      Owner_Loop.Serve (Server, Reason);
      Check (Reason = ACPI_Native_Server.Stop_Requested and Owner_Calls = 0,
        "kernel stop precedes even pending owner work");
      M.Reset; Owner_Calls := 0; M.Test.Completion := 1;
      Owner_Loop.Serve (Server, Reason);
      Check (Reason = ACPI_Native_Server.Unexpected_Completion and Owner_Calls = 0,
        "unexpected completion precedes owner work");
      M.Reset; Owner_Calls := 0; Owner_Turns := 0; Expected_Events := 1;
      Event := M.NULL_MESSAGE; Event.tag.label := 16#0120#;
      M.Enqueue_Event (High_Process, Event); M.Enqueue (High_Process, Native_Request);
      Owner_Loop.Serve (Server, Reason);
      Check (Owner_Calls = 2 and M.Test.Reply_Count = 1 and M.Test.Wait_Count = 1,
        "event and owner work and IPC share the same turn");
   end;
   Put_Line ("Observer and native runner checks" & Checks'Image);
end Observer_Tests;
