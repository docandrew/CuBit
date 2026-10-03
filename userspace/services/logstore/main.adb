pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Log_Protocol; use CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.Config_Inspection;
with CuBit.Config_Reader;
with Log_Fanout;
with Log_Budgets;
with Stream_Writers;

procedure Main is
   package Grants renames CuBit.Memory_Grants;
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
   --  A reader's ring was full: drain again soon rather than in a second.
   Backlog : Boolean := False;
   IDLE_WAKE_MS : constant := 1_000;
   BACKLOG_WAKE_MS : constant := 20;
   Budgets : Log_Budgets.Limiter;
   Admitted : Boolean;
   From : ProcessID;
   Request, Response : Message;
   Received, Known, Acquired, Returned : Boolean;
   Now_Ms, Handle, Ignore : Unsigned_64;
   Result : Status;
   Op : Operation := Publish;
   Ref : Grants.Grant_Reference;
   Address : System.Address;
   Bytes : Logs.Wire_Buffer;
   Used : Logs.Wire_Count;
   Decoded : Logs.Decoded;

   function Valid_Grant (Slot, Generation : Unsigned_64) return Boolean is
     (Slot <= Grants.MAXIMUM_GLOBAL_SLOT and Generation /= 0 and
      Generation <= Grants.MAXIMUM_GENERATION);

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
      receiveUntil
        (Now_Ms + Unsigned_64'Min ((if Backlog then BACKLOG_WAKE_MS else IDLE_WAKE_MS), Unsigned_64'Last - Now_Ms),
         From, Request, Received);
      Now_Ms := syscall (SYSCALL_GETTIME);
      if not Minimum_Settled then
         Read_Minimum_Setting;
      end if;
      Log_Fanout.Advance_Time (Store, Now_Ms);
      Log_Budgets.Advance_Time (Budgets, Now_Ms);
      if Received then
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
         if From /= NO_PROCESS and then Known and then
           May_Invoke (Request.authorityTag, Op)
         then
            Result := Invalid_Request;
            if Request.tag.length = 4 and then Request.tag.flags = 0
              and then Request.tag.reserved = 0
            then
               case Op is
                  when Publish =>
                     if Request.words (3) = 0 and then
                       Request.words (2) in
                         Unsigned_64 (Logs.Header_Bytes) ..
                         Unsigned_64 (Logs.Wire_Count'Last) and then
                       Valid_Grant (Request.words (0), Request.words (1))
                     then
                        Log_Budgets.Admit
                          (Budgets, Publication_Budget (Request.authorityTag),
                           Admitted);
                        if not Admitted then
                           Result := Rate_Limited;
                        else
                           Ref := (Request.words (0), Request.words (1));
                           Used := Logs.Wire_Count (Request.words (2));
                           Grants.Acquire (Ref, From, 0, Request.words (2),
                             Grants.Read_Access, Address, Acquired);
                           if Acquired then
                              Bytes := [others => 0];
                              declare
                                 Shared : Logs.Wire_Buffer
                                   with Import, Address => Address;
                              begin
                                 --  Decode only a private snapshot.
                                 Bytes (1 .. Used) := Shared (1 .. Used);
                              end;
                              Grants.Return_Acquisition (Ref, Returned);
                              Decoded := Logs.Decode (Bytes, Used);
                              if Returned and then Decoded.Success then
                                 if Logs.Level (Decoded.Value) >= Minimum then
                                    Log_Fanout.Publish (Store,
                                      (Source => From,
                                       Node => This_Node,
                                       Publication_Tag => Request.authorityTag,
                                       Monotonic_Ms => Now_Ms,
                                       Data => Decoded.Value));
                                    Result := OK;
                                 else
                                    Result := Below_Minimum;
                                 end if;
                                 --  Publishers learn the minimum and can skip what it drops.
                                 Response.words (0) := Logs.Severity'Pos (Minimum);
                              end if;
                           end if;
                        end if;
                     end if;
                  when Subscribe =>
                     if Request.words (0) <= Logs.Severity'Pos (Logs.Severity'Last) and then
                       Valid_Grant (Request.words (2), Request.words (3))
                     then
                        Log_Fanout.Subscribe
                          (Store, From, Request.authorityTag, Handle, Result,
                           Logs.Severity'Val (Request.words (0)), Request.words (1));
                        if Result = OK then
                           --  The reader's stream region: mapped for the
                           --  subscription's life, filled by Drain.
                           Stream_Writers.Attach
                             (Writers, Handle, Unsigned_64 (From), Request.authorityTag,
                              (Request.words (2), Request.words (3)), Acquired);
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
                              " and above (set by pid" & Unsigned_64'Image (Unsigned_64 (From)) & ")",
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
                           --  The region goes back before the reply.
                           Stream_Writers.Detach (Writers, Request.words (0));
                        end if;
                     end if;
               end case;
            end if;
         end if;
         Response.tag.label := Status'Enum_Rep (Result);
         Ignore := reply (From, Response);
      end if;
      --  New events, freed ring space, ended subscriptions: every pass.
      Stream_Writers.Drain (Writers, Store, Backlog);
   end loop;
end Main;
