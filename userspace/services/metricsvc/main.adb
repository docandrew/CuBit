pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Metric_Records;
with CuBit.Metric_Protocol; use CuBit.Metric_Protocol;
with Metric_Store;

--  metrics.svc: typed metric collection. Native adapter around the proved
--  Metric_Store core: authenticates the immediate peer (kernel-stamped PID
--  and tag), gates each operation, copies producer pages privately, and
--  returns every grant acquisition before replying.
procedure Main is
   package Grants renames CuBit.Memory_Grants;
   package Records renames CuBit.Metric_Records;

   --  Idle wake-up only advances the lease clock; no polling of producers.
   Idle_Wake_Ms : constant Unsigned_64 := 1_000;

   Store : Metric_Store.Store;
   From : ProcessID;
   Request, Response : Message;
   Received, Known, Acquired, Returned : Boolean;
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

   function Valid_Grant (Slot, Generation : Unsigned_64) return Boolean is
     (Slot <= Grants.MAXIMUM_GLOBAL_SLOT and Generation /= 0 and
      Generation <= Grants.MAXIMUM_GENERATION);
   function Valid_Batch_Length (Bytes : Unsigned_64) return Boolean is
     (Bytes mod Records.Slot_Bytes = 0 and then
      Bytes in Records.Batch_Bytes (Records.Batch_Record_Count'First) ..
               Records.Batch_Bytes (Records.Batch_Record_Count'Last));
begin
   Ignore := registerDriver (Publisher_Service_Role);
   if Ignore = Unsigned_64'Last then
      debugPrint ("metricsvc: registration failed" & ASCII.LF);
      return;
   end if;
   debugPrint ("metricsvc: typed metrics ready" & ASCII.LF);
   Now_Ms := syscall (SYSCALL_GETTIME);
   loop
      receiveUntil
        (Now_Ms + Unsigned_64'Min (Idle_Wake_Ms, Unsigned_64'Last - Now_Ms),
         From, Request, Received);
      Now_Ms := syscall (SYSCALL_GETTIME);
      Metric_Store.Advance_Time (Store, Now_Ms);
      if Received then
         Response := NULL_MESSAGE;
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
            if Request.tag.length = Message_Words and then
              Request.tag.flags = 0 and then Request.tag.reserved = 0
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
                                (Store, Unsigned_64 (From),
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
               end case;
            end if;
         end if;
         Response.tag := (label => Status'Enum_Rep (Result),
                          length => Message_Words, flags => 0,
                          reserved => 0);
         Ignore := reply (From, Response);
      end if;
   end loop;
end Main;
