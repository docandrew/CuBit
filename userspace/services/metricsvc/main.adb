pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with CuBit.Metric_Records;
with CuBit.Metric_Protocol; use CuBit.Metric_Protocol;
with CuBit.Metric_Query_Pages;
with CuBit.Channel_Protocol;
with CuBit.Channels;
with CuBit.Control_Events;
with CuBit.Process_Events;
with CuBit.Process_IDs.Text;
with CuBit.Log_Sources;
with CuBit.Refusal_Notes;
with CuBit.Log;
with Metric_Store;
with Metric_Raw_Query;
with Metric_Series;
with Metric_Outlets;
with Metric_Sources;
with Metric_Scopes;
with Metric_Reads;

--  metrics.svc (MET-001, MET-002): typed metric collection and the series
--  store. Each declared com.cubit-os.stdmetric outlet's ring is accepted
--  here and drained into Metric_Series every Metric_Outlets.Drain_Interval_Ms;
--  the launchers' registrar channels say who each publisher is
--  (Metric_Sources); procmgr installs each reader's read scopes
--  (Metric_Scopes) and readers query the store through a lent page
--  (List_Series, Read_Range, Read_Window). The older page publication
--  (Publish_Batch, Query_Summaries, Query_Raw over Metric_Store) is kept
--  for its existing users. Every request is authenticated by the
--  kernel-stamped peer and tag, gated by May_Invoke, and every grant
--  acquisition is returned before the reply.
with Startup;
procedure Main is
   package Grants renames CuBit.Memory_Grants;
   package Records renames CuBit.Metric_Records;
   package Pages renames CuBit.Metric_Query_Pages;
   package Channels renames CuBit.Channels;

   --  With no outlet open, wake only to advance the lease clock.
   Idle_Wake_Ms : constant Unsigned_64 := 1_000;
   Service_Name : constant String := "metrics";

   Store : Metric_Store.Store;
   Series : Metric_Series.Store;
   Outlets : Metric_Outlets.Table;
   Sources : Metric_Sources.Table;
   Scopes : Metric_Scopes.Table;
   From : Process_ID;
   Request, Response : Message;
   Received, Known, Acquired, Returned, Pending, Have_Event : Boolean;
   Now_Ms, Ignore : Unsigned_64;
   Op : Operation := Publish_Batch;
   Result : Status;
   Ref : Grants.Grant_Reference;
   Address : System.Address;
   Batch : Records.Page_Words;
   Outcome : Metric_Store.Ingest_Outcome;
   Rows : Summary_Page;
   Written : Row_Count;
   Next : Metric_Store.Series_Cursor;
   Raw_Rows : Raw_Page;
   Raw_Written : Raw_Row_Count;
   Raw_Next, Raw_Gap : Unsigned_64;
   Raw_Valid : Boolean;
   Ended_Event : CuBit.Control_Events.Event;
   Query : Pages.Page;
   Reported_Totals : Metric_Outlets.Totals;
   Reply_Words : MessageWords := [others => 0];
   --  A ring still held elements after the last pass: look again at once.
   Backlog : Boolean := False;

   function Valid_Grant (Slot, Generation : Unsigned_64) return Boolean is
     (Slot <= Grants.MAXIMUM_GLOBAL_SLOT and Generation /= 0 and
      Generation <= Grants.MAXIMUM_GENERATION);
   function Valid_Batch_Length (Bytes : Unsigned_64) return Boolean is
     (Bytes mod Records.Slot_Bytes = 0 and then
      Bytes in Records.Batch_Bytes (Records.Batch_Record_Count'First) ..
               Records.Batch_Bytes (Records.Batch_Record_Count'Last));
   function Decimal (Value : Unsigned_64) return String;
   function Decimal (Value : Unsigned_64) return String is
      Text : constant String := Value'Image;
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Decimal;

   --  A refusal, logged once per reader and operation (CAP-003).
   procedure Report_Refusal (Reader : Process_ID; What : String);
   procedure Report_Refusal (Reader : Process_ID; What : String) is
      Who : constant String := CuBit.Process_IDs.Text.Image (Reader);
   begin
      CuBit.Refusal_Notes.Warn_Once
        (Service_Name, Who & " " & What,
         "process " & Who & " " & What & ": outside the read scopes its manifest declares (metrics-scope)");
   end Report_Refusal;

   --  The reader's page, privately: acquire, copy in, answer, copy out,
   --  return. Done False when the grant could not be used.
   generic
      with procedure Answer (Item : in out Pages.Page);
   procedure With_Page (Word : Unsigned_64; Reader : Process_ID; Done : out Boolean);
   procedure With_Page (Word : Unsigned_64; Reader : Process_ID; Done : out Boolean) is
      Reference : CuBit.Memory_Grants.Grant_Reference;
   begin
      Done := False;
      if not CuBit.Grant_References.Valid_Wire (Word) then
         return;
      end if;
      Reference := CuBit.Grant_References.Decode (Word);
      Grants.Acquire (Reference, Reader, 0, Pages.Page_Bytes, Grants.Write_Access, Address, Acquired);
      if not Acquired then
         return;
      end if;
      declare
         Shared : Pages.Page with Import, Address => Address;
      begin
         Query := Shared;
         Answer (Query);
         Shared := Query;
      end;
      Grants.Return_Acquisition (Reference, Returned);
      Done := Returned;
   end With_Page;

   procedure List_Answer (Item : in out Pages.Page);
   procedure List_Answer (Item : in out Pages.Page) is
   begin
      Metric_Reads.List (Series, Scopes, To_Word (From), Request.words (1), Item,
                         Reply_Words (0), Reply_Words (1), Result);
   end List_Answer;
   procedure Range_Answer (Item : in out Pages.Page);
   procedure Range_Answer (Item : in out Pages.Page) is
   begin
      Metric_Reads.Read_Range (Series, Scopes, To_Word (From), Request.words (1), Request.words (2),
                               Request.words (3), Item, Reply_Words (0), Reply_Words (1), Reply_Words (2),
                               Reply_Words (3), Result);
   end Range_Answer;
   procedure Window_Answer (Item : in out Pages.Page);
   procedure Window_Answer (Item : in out Pages.Page) is
   begin
      Metric_Reads.Read_Window (Series, Scopes, To_Word (From), Request.words (1), Request.words (2),
                                Request.words (3), Item, Reply_Words (0), Result);
   end Window_Answer;
   procedure List_Page is new With_Page (List_Answer);
   procedure Range_Page is new With_Page (Range_Answer);
   procedure Window_Page is new With_Page (Window_Answer);

   --  procmgr's scope list for one process (Set_Scopes).
   procedure Install_Scopes (Process, Count, Slot, Generation : Unsigned_64; Done : out Boolean);
   procedure Install_Scopes (Process, Count, Slot, Generation : Unsigned_64; Done : out Boolean) is
      Prefixes : Metric_Scopes.Prefix_List (1 .. Natural (Unsigned_64'Min (Count, Maximum_Scopes)));
      Accepted : Boolean := True;
   begin
      Done := False;
      if Count > Maximum_Scopes or else Process = 0 or else not Valid_Grant (Slot, Generation) then
         return;
      end if;
      if Count = 0 then
         Metric_Scopes.Revoke (Scopes, Process);
         Done := True;
         return;
      end if;
      Ref := (Slot, Generation);
      Grants.Acquire (Ref, From, 0, Count * Scope_Entry_Bytes, Grants.Read_Access, Address, Acquired);
      if not Acquired then
         return;
      end if;
      declare
         Shared : constant array (0 .. Natural (Count) * Scope_Entry_Bytes - 1) of Unsigned_8
           with Import, Address => Address;
      begin
         for I in Prefixes'Range loop
            declare
               Base : constant Natural := (I - 1) * Scope_Entry_Bytes;
               Length : constant Natural := Natural (Shared (Base + 1));
            begin
               if Shared (Base) /= Read_Right or else Length > Metric_Scopes.Maximum_Prefix then
                  Accepted := False;
               else
                  Prefixes (I).Length := Length;
                  for C in 1 .. Length loop
                     Prefixes (I).Text (C) := Character'Val (Shared (Base + Scope_Prefix_At + C - 1));
                  end loop;
               end if;
            end;
         end loop;
      end;
      Grants.Return_Acquisition (Ref, Returned);
      if Accepted and then Returned then
         Metric_Scopes.Set (Scopes, Process, Prefixes, Done);
      end if;
   end Install_Scopes;

   --  The outlets' accounting changed: one record says what.
   procedure Report_Accounting;
   procedure Report_Accounting is
      Now_Totals : constant Metric_Outlets.Totals := Metric_Outlets.Accounting (Outlets);
   begin
      if Now_Totals.Malformed /= Reported_Totals.Malformed or else
        Now_Totals.Unknown_Key /= Reported_Totals.Unknown_Key or else
        Now_Totals.Refused_Outlets /= Reported_Totals.Refused_Outlets
      then
         CuBit.Log.Warning
           ("metricsvc: elements malformed=" & Decimal (Now_Totals.Malformed) &
            " for undescribed or refused series=" & Decimal (Now_Totals.Unknown_Key) &
            " outlets refused=" & Decimal (Now_Totals.Refused_Outlets) &
            " series refused=" & Decimal (Metric_Series.Series_Refused (Series)));
         Reported_Totals := Now_Totals;
      end if;
   end Report_Accounting;

   procedure Serve_Store_Read;
   procedure Serve_Store_Read is
      Done : Boolean;
   begin
      --  The latest samples first.
      Metric_Outlets.Drain (Outlets, Series, Sources, Now_Ms, Pending);
      Reply_Words := [others => 0];
      Result := Unavailable;
      case Op is
         when List_Series => List_Page (Request.words (0), From, Done);
         when Read_Range => Range_Page (Request.words (0), From, Done);
         when others => Window_Page (Request.words (0), From, Done);
      end case;
      if not Done then
         Result := Invalid_Request;
      elsif Result = Out_Of_Scope then
         Report_Refusal (From, Operation'Image (Op));
      elsif Result = OK then
         Response.words := Reply_Words;
      end if;
   end Serve_Store_Read;
begin
   Ignore := registerDriver (Publisher_Service_Role, Startup.Registration);
   if Ignore = Unsigned_64'Last then
      debugPrint ("metricsvc: registration failed" & ASCII.LF);
      return;
   end if;
   CuBit.Log.Info ("metricsvc: typed metrics ready; series store in RAM, outlets drained every" &
                   Metric_Outlets.Drain_Interval_Ms'Image & " ms");
   Now_Ms := syscall (SYSCALL_GETTIME);
   loop
      receiveUntil
        ((if Backlog then Now_Ms
          else Now_Ms + Unsigned_64'Min
                 ((if Metric_Outlets.Open_Count (Outlets) > 0 then Metric_Outlets.Drain_Interval_Ms
                   else Idle_Wake_Ms),
                  Unsigned_64'Last - Now_Ms)),
         From, Request, Received);
      Now_Ms := syscall (SYSCALL_GETTIME);
      Metric_Store.Advance_Time (Store, Now_Ms);
      --  Launchers' notices first, so new series get their labels.
      Metric_Sources.Drain (Sources, Pending);
      loop
         CuBit.Process_Events.Next (Ended_Event, Have_Event);
         exit when not Have_Event;
         Metric_Sources.Ended (Sources, Ended_Event);
         Metric_Outlets.Ended (Outlets, Series, Sources, Ended_Event, Now_Ms);
      end loop;
      if Received and then Request.tag.label = CuBit.Channel_Protocol.OP_OPEN_PRODUCING then
         --  A launcher's registrar channel, or a program's outlet.
         if From /= No_Process and then CuBit.Log_Sources.Is_Registrar (Request.authorityTag) then
            Metric_Sources.Open (Sources, From, Request.authorityTag, Request, Response);
         elsif From /= No_Process and then Is_Publisher (Request.authorityTag) then
            Metric_Outlets.Open (Outlets, From, Request.authorityTag, Request, Now_Ms, Response);
         else
            Response := Channels.Refusal_Reply (CuBit.Channel_Protocol.Unsupported);
         end if;
         Ignore := reply (From, Response);
      elsif Received and then Request.tag.label = CuBit.Channel_Protocol.OP_CLOSE then
         if Metric_Sources.Owns (Channels.Number_Of (Request)) then
            Metric_Sources.Close (Sources, From, Request);
         else
            Metric_Outlets.Close (Outlets, Series, Sources, From, Request, Now_Ms);
         end if;
      elsif Received and then Request.tag.label = CuBit.Channel_Protocol.OP_KICK then
         --  A publisher's Flush: drained below.
         null;
      elsif Received then
         Response := NULL_MESSAGE;
         Known := False;
         for Candidate in Operation loop
            if Request.tag.label = Operation'Enum_Rep (Candidate) then
               Op := Candidate;
               Known := True;
            end if;
         end loop;
         Result := Denied;
         if From /= No_Process and then Known and then
           May_Invoke (Request.authorityTag, Op)
         then
            Result := Invalid_Request;
            if Request.tag.length = Message_Words and then
              Request.tag.protocol = 0 and then Request.tag.reserved = 0
            then
               case Op is
                  when Publish_Batch =>
                     if Request.words (3) = 0 and then
                       Valid_Batch_Length (Request.words (2)) and then
                       Valid_Grant (Request.words (0), Request.words (1))
                     then
                        Ref := (Request.words (0), Request.words (1));
                        Grants.Acquire (Ref, From, 0, Request.words (2),
                          Grants.Read_Access, Address, Acquired);
                        if Acquired then
                           Batch := [others => 0];
                           declare
                              Shared : Records.Page_Words
                                with Import, Address => Address;
                              Used : constant Records.Page_Word_Index :=
                                Natural (Request.words (2)) /
                                  Records.Bytes_Per_Word - 1;
                           begin
                              --  Validate only a private snapshot.
                              Batch (0 .. Used) := Shared (0 .. Used);
                           end;
                           Grants.Return_Acquisition (Ref, Returned);
                           if Returned then
                              Metric_Store.Ingest
                                (Store, To_Word (From),
                                 Request.authorityTag, Batch,
                                 Request.words (2), Outcome);
                              Result := Outcome.Result;
                              if Result = OK then
                                 Response.words :=
                                   [Unsigned_64 (Outcome.Accepted),
                                    Unsigned_64 (Outcome.Rejected), 0, 0];
                              end if;
                           else
                              Result := Unavailable;
                           end if;
                        end if;
                     end if;
                  when Query_Summaries =>
                     if Request.words (0) <= Metric_Store.Series_Slots
                       and then Request.words (3) = Records.Page_Bytes
                       and then
                       Valid_Grant (Request.words (1), Request.words (2))
                     then
                        Ref := (Request.words (1), Request.words (2));
                        Grants.Acquire (Ref, From, 0, Request.words (3),
                          Grants.Write_Access, Address, Acquired);
                        if Acquired then
                           Metric_Store.Fill_Summaries
                             (Store,
                              Metric_Store.Series_Cursor (Request.words (0)),
                              Rows, Written, Next);
                           declare
                              Shared : Summary_Page
                                with Import, Address => Address;
                           begin
                              Shared := Rows;
                           end;
                           Grants.Return_Acquisition (Ref, Returned);
                           if Returned then
                              Result := OK;
                              Response.words :=
                                [Unsigned_64 (Written), Unsigned_64 (Next),
                                 Metric_Store.Series_Slots, 0];
                           else
                              Result := Unavailable;
                           end if;
                        end if;
                     end if;
                  when Query_Raw =>
                     if Request.words (0) /= 0 and then
                       Request.words (0) <= Metric_Store.History_Next (Store) and then
                       Request.words (3) = Records.Page_Bytes and then
                       Valid_Grant (Request.words (1), Request.words (2))
                     then
                        Ref := (Request.words (1), Request.words (2));
                        Grants.Acquire (Ref, From, 0, Records.Page_Bytes,
                          Grants.Write_Access, Address, Acquired);
                        if Acquired then
                           Metric_Raw_Query.Fill (Store, Request.words (0),
                             Raw_Rows, Raw_Written, Raw_Next, Raw_Gap, Raw_Valid);
                           if Raw_Valid then
                              declare
                                 Shared : Raw_Page with Import, Address => Address;
                              begin
                                 Shared := Raw_Rows;
                              end;
                           end if;
                           Grants.Return_Acquisition (Ref, Returned);
                           if Returned and Raw_Valid then
                              Result := OK;
                              Response.words := [Unsigned_64 (Raw_Written),
                                Raw_Next, Raw_Gap, Metric_Store.History_Dropped (Store)];
                           else Result := Unavailable;
                           end if;
                        else Result := Unavailable;
                        end if;
                     end if;
                  when List_Series | Read_Range | Read_Window =>
                     Serve_Store_Read;
                  when Set_Scopes =>
                     declare
                        Done : Boolean;
                     begin
                        Install_Scopes (Request.words (0), Request.words (1), Request.words (2),
                                        Request.words (3), Done);
                        Result := (if Done then OK else Invalid_Request);
                     end;
                  when Revoke_Scopes =>
                     if Request.words (1 .. 3) = [0, 0, 0] then
                        Metric_Scopes.Revoke (Scopes, Request.words (0));
                        Result := OK;
                     end if;
               end case;
            end if;
         elsif Known and then From /= No_Process then
            CuBit.Refusal_Notes.Warn_Once
              (Service_Name, CuBit.Process_IDs.Text.Image (From) & " " & Operation'Image (Op),
               "process " & CuBit.Process_IDs.Text.Image (From) & " " & Operation'Image (Op) &
               ": its endpoint lacks the badge for it (the metrics-observer entry its manifest requests)");
         end if;
         Response.tag := (label => Status'Enum_Rep (Result),
                          length => Message_Words, protocol => 0, ipc => <>,
                          reserved => 0);
         Ignore := reply (From, Response);
      end if;
      Metric_Outlets.Drain (Outlets, Series, Sources, Now_Ms, Backlog);
      Report_Accounting;
   end loop;
end Main;
