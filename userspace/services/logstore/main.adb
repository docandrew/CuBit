pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Process_IDs.Text;
with CuBit.Log_Protocol; use CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.Config_Inspection;
with CuBit.Config_Reader;
with Log_Fanout;
with Log_Budgets;
with Stream_Writers;
with Publisher_Rings;
with CuBit.Channel_Protocol;
with CuBit.Channels;
with CuBit.Control_Events;
with CuBit.Process_Events;

procedure Main is
   function Image (Process : Process_ID) return String
     renames CuBit.Process_IDs.Text.Image;
   package Logs renames CuBit.Log_Records;
   package Settings renames CuBit.Config_Inspection;
   use type Logs.Severity;
   use type Settings.Status;
   --  The setting naming the least severe record kept, as a CCL Severity
   --  value ("Severity.Information"). Without it everything is kept.
   MINIMUM_SETTING : constant String := "logs.minimum-level";
   --  Kept: records at Minimum and above. Others are acknowledged and dropped.
   Minimum : Logs.Severity := Logs.Trace;
   --  Until Config answers (or log-control sets it), the setting is retried.
   Minimum_Settled : Boolean := False;
   Store : Log_Fanout.Broker;
   Writers : Stream_Writers.Table;
   --  Publishers' channels: drained every pass.
   Publishers : Publisher_Rings.Table;
   --  A publisher ring still held records after a pass, or records arrived
   --  while arming: do not sleep.
   Waiting_Records : Boolean := False;
   Send_Reply : Boolean;
   --  A reader's ring was full: drain again soon rather than in a second.
   Backlog : Boolean := False;
   IDLE_WAKE_MS : constant := 1_000;
   BACKLOG_WAKE_MS : constant := 20;
   Budgets : Log_Budgets.Limiter;
   From : Process_ID;
   Request, Response : Message;
   Received, Known, Acquired : Boolean;
   Now_Ms, Handle, Ignore : Unsigned_64;
   Result : Status;
   Op : Operation := Get_Minimum;
   Ended_Event : CuBit.Control_Events.Event;
   Have_Event : Boolean;
   Decoded : Logs.Decoded;


   --  logstore's own record about what it keeps, published straight into
   --  the store and never itself subject to the minimum.
   procedure Note (Text : String; Level : Logs.Severity := Logs.Information) is
      Made : constant Logs.Decoded := Logs.Make (Text, Level);
   begin
      if Made.Success then
         Log_Fanout.Publish
           (Store, (Source => syscall (SYSCALL_GETPID), Node => This_Node, Publication_Tag => 0,
                    Monotonic_Ms => Now_Ms, Data => Made.Value));
      end if;
   end Note;

   procedure Read_Minimum_Setting is
      Value : Settings.Text;
      Result : Settings.Status;
   begin
      CuBit.Config_Reader.Query (Settings.Read_Value, MINIMUM_SETTING, Value, Result);
      if Result = Settings.OK then
         Minimum_Settled := True;
         if Logs.Is_Severity_Literal (Value.Data (1 .. Value.Length)) then
            Minimum := Logs.Severity_Of (Value.Data (1 .. Value.Length));
            Note ("logstore: keeping " & Logs.Severity_Literal (Minimum) & " and above (" & MINIMUM_SETTING & ")");
         else
            Note ("logstore: " & MINIMUM_SETTING & " is not a Severity (for example Severity.Information); " &
                  "keeping everything", Logs.Warning);
         end if;
      elsif Result in Settings.Missing | Settings.Denied then
         --  Nothing to wait for: no setting, or no grant to read it.
         Minimum_Settled := True;
      end if;
   end Read_Minimum_Setting;
begin
   Now_Ms := syscall (SYSCALL_GETTIME);
   Decoded := Logs.Make ("logstore: typed diagnostics ready");
   if Decoded.Success then
      Log_Fanout.Publish
        (Store, (Source => syscall (SYSCALL_GETPID), Node => This_Node, Publication_Tag => 0,
                 Monotonic_Ms => Now_Ms, Data => Decoded.Value));
   end if;
   Ignore := registerDriver (DRIVER_LOGSTORE);
   if Ignore = Unsigned_64'Last then
      debugPrint ("logstore: registration failed" & ASCII.LF);
      return;
   end if;
   debugPrint ("logstore: authorized typed diagnostics ready" & ASCII.LF);
   Read_Minimum_Setting;
   loop
      --  Deadline is for idle subscription reclamation, not input polling.
      --  Ask publishers for a Kick while asleep; records that arrived
      --  while arming mean there is no sleeping this time.
      if not Waiting_Records then
         Publisher_Rings.Arm (Publishers, Waiting_Records);
      end if;
      receiveUntil
        ((if Waiting_Records then Now_Ms
          else Now_Ms + Unsigned_64'Min ((if Backlog then BACKLOG_WAKE_MS else IDLE_WAKE_MS),
                                         Unsigned_64'Last - Now_Ms)),
         From, Request, Received);
      Publisher_Rings.Disarm (Publishers);
      Now_Ms := syscall (SYSCALL_GETTIME);
      if not Minimum_Settled then
         Read_Minimum_Setting;
      end if;
      Log_Fanout.Advance_Time (Store, Now_Ms);
      Log_Budgets.Advance_Time (Budgets, Now_Ms);
      --  Publishers that closed or died: their last records first.
      loop
         CuBit.Process_Events.Next (Ended_Event, Have_Event);
         exit when not Have_Event;
         Publisher_Rings.Ended (Publishers, Store, Budgets, Ended_Event, Minimum, Now_Ms);
         Stream_Writers.Ended (Writers, Ended_Event);
      end loop;
      if Received and then Request.tag.label = CuBit.Channel_Protocol.OP_OPEN_PRODUCING then
         --  A publisher opening its channel.
         if From /= No_Process and then May_Publish (Request.authorityTag) then
            Publisher_Rings.Open (Publishers, Store, Budgets, From, Request.authorityTag, Request,
                                  Minimum, Now_Ms, Response);
         else
            Response := CuBit.Channels.Refusal_Reply (CuBit.Channel_Protocol.Unsupported);
         end if;
         Ignore := reply (From, Response);
      elsif Received and then Request.tag.label = CuBit.Channel_Protocol.OP_OPEN_CONSUMING then
         --  A reader opening its stream (Subscribe binds it).
         if From /= No_Process and then May_Invoke (Request.authorityTag, Subscribe) then
            Stream_Writers.Open (Writers, From, Request.authorityTag, Request, Response);
         else
            Response := CuBit.Channels.Refusal_Reply (CuBit.Channel_Protocol.Unsupported);
         end if;
         Ignore := reply (From, Response);
      elsif Received and then Request.tag.label = CuBit.Channel_Protocol.OP_CLOSE then
         if CuBit.Channels.Number_Of (Request) > Stream_Writers.First_Number then
            Stream_Writers.Close (Writers, From, Request);
         else
            Publisher_Rings.Close (Publishers, Store, Budgets, From, Request, Minimum, Now_Ms);
         end if;
      elsif Received and then Request.tag.label = CuBit.Channel_Protocol.OP_KICK then
         --  Records are waiting: drained below, every pass.
         null;
      elsif Received then
         Response := NULL_MESSAGE;
         Response.tag := (label => Status'Enum_Rep (Denied), length => 4,
                          flags => 0, reserved => 0);
         Known := False;
         for Candidate in Operation loop
            if Request.tag.label = Operation'Enum_Rep (Candidate) then
               Op := Candidate;
               Known := True;
            end if;
         end loop;
         Result := Denied;
         Send_Reply := True;
         if From /= No_Process and then Known and then
           May_Invoke (Request.authorityTag, Op)
         then
            Result := Invalid_Request;
            if Request.tag.length = 4 and then Request.tag.flags = 0
              and then Request.tag.reserved = 0
            then
               case Op is
                  when Subscribe =>
                     --  Words: minimum, source, the reader's stream channel.
                     if Request.words (0) <= Logs.Severity'Pos (Logs.Severity'Last) and then
                       Request.words (3) = 0
                     then
                        Log_Fanout.Subscribe
                          (Store, From, Request.authorityTag, Handle, Result,
                           Logs.Severity'Val (Request.words (0)), Request.words (1));
                        if Result = OK then
                           --  The reader's stream channel, filled by Drain.
                           Stream_Writers.Bind
                             (Writers, Request.words (2), Handle, From, Request.authorityTag,
                              Acquired);
                           if Acquired then
                              --  The handle, and the source filter it applies.
                              Response.words (0) := Handle;
                              Response.words (1) := Request.words (1);
                              --  Replay is in the ring when the reply arrives.
                              Stream_Writers.Drain (Writers, Store, Backlog);
                           else
                              Log_Fanout.Close (Store, From, Request.authorityTag, Handle, Result);
                              Result := Invalid_Request;
                           end if;
                        end if;
                     end if;
                  when Set_Minimum =>
                     if Request.words (0) <= Logs.Severity'Pos (Logs.Severity'Last) and then
                       Request.words (1 .. 3) = [0, 0, 0]
                     then
                        Response.words (0) := Logs.Severity'Pos (Minimum);
                        Minimum := Logs.Severity'Val (Request.words (0));
                        Minimum_Settled := True;
                        Result := OK;
                        --  Who changed it, kept whatever the new minimum.
                        Note ("logstore: keeping " & Logs.Severity_Literal (Minimum) &
                              " and above (set by pid" & Image (From) & ")",
                              Logs.Warning);
                     end if;
                  when Get_Minimum =>
                     if Request.words = [0, 0, 0, 0] then
                        Response.words (0) := Logs.Severity'Pos (Minimum);
                        Result := OK;
                     end if;
                  when Close =>
                     if Request.words (1 .. 3) = [0, 0, 0] then
                        Log_Fanout.Close (Store, From, Request.authorityTag,
                          Request.words (0), Result);
                        if Result = OK then
                           Stream_Writers.Unbind (Writers, Request.words (0));
                        end if;
                     end if;
               end case;
            end if;
         end if;
         if Send_Reply then
            Response.tag.label := Status'Enum_Rep (Result);
            Ignore := reply (From, Response);
         end if;
      end if;
      --  Published records, new events, freed ring space, ended
      --  subscriptions: every pass.
      Publisher_Rings.Drain (Publishers, Store, Budgets, Minimum, Now_Ms, Waiting_Records);
      Stream_Writers.Drain (Writers, Store, Backlog);
      Publisher_Rings.Release (Publishers);
   end loop;
end Main;
