with Interfaces; use Interfaces;
with ACPI_Native_Endpoint;
with ACPI_Launch;
with CuBit.Handles.IPC;
with CuBit.Handles;
package body ACPI_Native_Server is
   use CuBit.Messages;
   Retry_Milliseconds : constant Unsigned_64 := 100;
   procedure Await_Configuration
     (Config : out ACPI_Endpoint.Configuration;
      Provider : out CuBit.Handles.Endpoint;
      Accepted : out Boolean) is
      Request, Response : Message;
      From : Process_ID;
      Completion : CompletionEntry;
      Found : Boolean;
      Ignore : Unsigned_64;
   begin
      Config := (0, 0); Provider := CuBit.Handles.No_Endpoint; Accepted := False;
      loop
         if Poll_Completion (Completion'Address) /= 0 then return; end if;
         Poll_Any_Ipc (From, Request, Found);
         if Found then
            declare
               Parsed : constant ACPI_Launch.Decision := ACPI_Launch.Decode
                 (Request.authorityTag,
                  --  The offer's kernel flag says nothing about the request.
                  (Request.tag.label, Request.tag.length,
                   Unsigned_8 (Request.tag.protocol),
                   Request.tag.reserved, ACPI_Requests.Words (Request.words)));
            begin
               Response := NULL_MESSAGE;
               Response.tag := (label => ACPI_Endpoint.Reply_Error, length => 4,
                                protocol => 0, ipc => <>, reserved => 0);
               Response.words (0) := ACPI_Requests.Outcome'Pos (ACPI_Requests.Denied);
               --  The provider's endpoint comes offered with the call.
               if Parsed.Accepted and then CuBit.Handles.IPC.Offers (Request) then
                  declare
                     Offered : constant CuBit.Handles.IPC.Accepted_Endpoint :=
                       CuBit.Handles.IPC.Accept_Endpoint (CuBit.Handles.IPC.Accept_Offered (From));
                  begin
                     if Offered.OK then
                        Config := Parsed.Config; Provider := Offered.Value;
                        Accepted := True;
                     end if;
                  end;
               end if;
               if Accepted then
                  Response.tag.label := ACPI_Endpoint.Reply_OK;
                  Response.words := [others => 0];
               end if;
               Ignore := reply (From, Response);
               -- A lost acknowledgment never returns us to bootstrap mode.
               if Accepted then return; end if;
            end;
         elsif Wait_For_Activity_Until (Unsigned_64'Last) = Unavailable then
            return;
         end if;
      end loop;
   end Await_Configuration;
   procedure Run
     (Server : in out ACPI_Requests.State;
      Adapter : in out ACPI_Native_Blocks.State;
      Config : ACPI_Endpoint.Configuration;
      Provider : CuBit.Handles.Endpoint;
      Reason : out Stop_Reason) is
      Request, Response : Message;
      From : Process_ID;
      Completion : CompletionEntry;
      Found : Boolean;
      Now, Retry_At : Unsigned_64 := 0;
      Ignore : Unsigned_64;
   begin
      Reason := Invalid_Configuration;
      if not ACPI_Endpoint.Valid (Config) then return; end if;
      loop
         Now := syscall (SYSCALL_GETTIME);
         if Now >= Unsigned_64'Last - 1 then
            Reason := Clock_Unavailable; return;
         end if;
         if ACPI_Native_Blocks.Pending (Adapter) then
            if Retry_At /= 0 and then Now >= Retry_At then
               ACPI_Native_Blocks.Retry_Return (Adapter);
               Retry_At := 0;
            end if;
            if ACPI_Native_Blocks.Pending (Adapter) and then Retry_At = 0 then
               -- Last means indefinite wait: never use it for pending cleanup.
               Retry_At := (if Now > Unsigned_64'Last - 1 - Retry_Milliseconds
                 then Unsigned_64'Last - 1 else Now + Retry_Milliseconds);
            end if;
         else
            Retry_At := 0;
         end if;
         -- This runner submits no asynchronous requests. Unexpected completion
         -- is fatal instead of leaving a permanently ready queue and spinning.
         if Poll_Completion (Completion'Address) /= 0 then
            Reason := Unexpected_Completion; return;
         end if;
         -- Central dispatcher deliberately consumes all IPC classes. There are
         -- no event subscriptions yet; unknown/untrusted messages are denied by
         -- the endpoint and cannot mutate the snapshot. No PID grants authority.
         Poll_Any_Ipc (From, Request, Found);
         if Found then
            ACPI_Native_Endpoint.Dispatch
              (Adapter, Server, Config, Provider, Request, Response);
            -- A failed reply (including one-way messages or dead callers) does
            -- not undo/repeat dispatch. Kernel reply-cap checks decide delivery.
            Ignore := reply (From, Response);
         else
            if Wait_For_Activity_Until
              (if ACPI_Native_Blocks.Pending (Adapter) then Retry_At
               else Unsigned_64'Last) = Unavailable
            then
               Reason := Wait_Unavailable; return;
            end if;
         end if;
      end loop;
   end Run;
end ACPI_Native_Server;
