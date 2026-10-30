with Interfaces; use Interfaces;
with ACPI_Native_Endpoint;
package body ACPI_Native_Server is
   use CuBit.Messages;
   package body With_Dispatch is
      procedure Serve (Server : in out ACPI_Requests.State; Reason : out Stop_Reason) is
         Request, Response : Message;
         From : Process_ID;
         Completion : CompletionEntry;
         Found : Boolean;
         Ignore : Unsigned_64;
      begin
         loop
            -- This synchronous observer runner submits no asynchronous work.
            -- A completion must stop it, rather than leave a ready queue spinning.
            if Poll_Completion (Completion'Address) /= 0 then
               Reason := Unexpected_Completion; return;
            end if;
            Poll_Any_Ipc (From, Request, Found);
            if Found then
               Dispatch_Message (Server, From, Request, Response);
               -- Kernel reply-cap checks decide delivery; a lost/one-way reply
               -- never replays a dispatch, table import, or AML startup.
               Ignore := reply (From, Response);
            elsif Wait_For_Activity_Until (Wait_Forever) = Unavailable then
               Reason := Wait_Unavailable; return;
            end if;
         end loop;
      end Serve;
   end With_Dispatch;
   package Default_Loop is new With_Dispatch (ACPI_Native_Endpoint.Dispatch);
   procedure Run (Server : in out ACPI_Requests.State; Reason : out Stop_Reason) is
   begin
      Default_Loop.Serve (Server, Reason);
   end Run;
end ACPI_Native_Server;
