pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Log_Protocol; use CuBit.Log_Protocol;
with CuBit.Log_Records;
with Log_Fanout;
with Log_Budgets;

procedure Main is
   package Grants renames CuBit.Memory_Grants;
   package Logs renames CuBit.Log_Records;
   Store : Log_Fanout.Broker;
   Budgets : Log_Budgets.Limiter;
   Admitted : Boolean;
   From : ProcessID;
   Request, Response : Message;
   Received, Known, Acquired, Returned : Boolean;
   Now_Ms, Handle, Lost, Ignore : Unsigned_64;
   Value : Event;
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
begin
   Now_Ms := syscall (SYSCALL_GETTIME);
   Decoded := Logs.Make ("logstore: typed diagnostics ready");
   if Decoded.Success then
      Log_Fanout.Publish
        (Store, (Source => syscall (SYSCALL_GETPID), Publication_Tag => 0,
                 Monotonic_Ms => Now_Ms, Data => Decoded.Value));
   end if;
   Ignore := registerDriver (DRIVER_LOGSTORE);
   if Ignore = Unsigned_64'Last then
      debugPrint ("logstore: registration failed" & ASCII.LF);
      return;
   end if;
   debugPrint ("logstore: authorized typed diagnostics ready" & ASCII.LF);
   loop
      --  Deadline is for idle subscription reclamation, not input polling.
      receiveUntil
        (Now_Ms + Unsigned_64'Min (1_000, Unsigned_64'Last - Now_Ms),
         From, Request, Received);
      Now_Ms := syscall (SYSCALL_GETTIME);
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
                                 Log_Fanout.Publish (Store,
                                   (Source => From,
                                    Publication_Tag => Request.authorityTag,
                                    Monotonic_Ms => Now_Ms,
                                    Data => Decoded.Value));
                                 Result := OK;
                              end if;
                           end if;
                        end if;
                     end if;
                  when Subscribe =>
                     if Request.words = [0, 0, 0, 0] then
                        Log_Fanout.Subscribe
                          (Store, From, Request.authorityTag, Handle, Result);
                        Response.words (0) := Handle;
                     end if;
                  when Read_Next =>
                     if Request.words (3) =
                       Unsigned_64 (Logs.Wire_Count'Last) and then
                       Valid_Grant (Request.words (1), Request.words (2))
                     then
                        Ref := (Request.words (1), Request.words (2));
                        Grants.Acquire (Ref, From, 0, Request.words (3),
                          Grants.Write_Access, Address, Acquired);
                        if Acquired then
                           Log_Fanout.Read_Next
                             (Store, From, Request.authorityTag,
                              Request.words (0), Value, Lost, Result);
                           if Result = OK then
                              Logs.Encode (Value.Data, Bytes, Used);
                              declare
                                 Shared : Logs.Wire_Buffer
                                   with Import, Address => Address;
                              begin
                                 Shared := Bytes;
                              end;
                              Response.words :=
                                [Value.Source, Value.Monotonic_Ms,
                                 Unsigned_64 (Used), Value.Publication_Tag];
                           elsif Result = Gap then
                              Response.words (0) := Lost;
                           end if;
                           Grants.Return_Acquisition (Ref, Returned);
                           if not Returned then
                              Result := Unavailable;
                              Response.words := [others => 0];
                           end if;
                        end if;
                     end if;
                  when Close =>
                     if Request.words (1 .. 3) = [0, 0, 0] then
                        Log_Fanout.Close (Store, From, Request.authorityTag,
                          Request.words (0), Result);
                     end if;
               end case;
            end if;
         end if;
         Response.tag.label := Status'Enum_Rep (Result);
         Ignore := reply (From, Response);
      end if;
   end loop;
end Main;
