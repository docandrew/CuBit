with ACPI_Native_Server.With_Passive_Queries;
with Interfaces; use Interfaces;
with ACPI_Native_Endpoint;
with ACPI_Native_Module_Notes;
package body ACPI_Native_Server is
   use CuBit.Messages;
   procedure Idle_Owner (Server : in out ACPI_Requests.State; Result : out Owner_Outcome) is
      pragma Unreferenced (Server);
   begin
      Result := Owner_Idle;
   end Idle_Owner;
   procedure No_Maintenance (Wake_Deadline_Milliseconds : out Interfaces.Unsigned_64;
      May_Continue : out Boolean) is
   begin
      Wake_Deadline_Milliseconds := Wait_Forever; May_Continue := True;
   end No_Maintenance;
   function Always_Quiet (Server:ACPI_Requests.State) return Boolean is
      pragma Unreferenced(Server);
   begin return True;end Always_Quiet;
   function Reply_Immediately return Boolean is (True);
   procedure Reject_Completion
     (Server : in out ACPI_Requests.State; Item : CompletionEntry;
      Handled : out Boolean) is
      pragma Unreferenced (Server, Item);
   begin
      Handled := False;
   end Reject_Completion;
   package body With_Dispatch is
      procedure Serve (Server : in out ACPI_Requests.State; Reason : out Stop_Reason) is
         Request, Response : Message;
         From : Process_ID;
         Completion : CompletionEntry;
         Found, Completion_Found, Event_Found, Stop, Handled : Boolean;
         Event : Message;
         Publisher : Process_ID;
         Ignore : Unsigned_64;
         Owner_Result : Owner_Outcome;
         Owner_Deadline : Unsigned_64;
         May_Continue : Boolean;
      begin
         loop
            -- One completion per turn; only its original owner can accept it.
            -- Unknown completions stop rather than disappearing or spinning.
            Completion_Found := Poll_Completion (Completion'Address) /= 0;
            if Completion_Found then
               Handle_Completion (Server, Completion, Handled);
               if not Handled then Reason := Unexpected_Completion; return; end if;
            end if;
            -- One event and one IPC opportunity per turn. A readable event
            -- lane must be consumed before another wait; it is also ready work.
            Event_Found := Poll_Event (Event, Publisher);
            if Event_Found then
               Handle_Event (Publisher, Event, Stop, Handled);
               if Stop then Reason := Stop_Requested; return; end if;
            end if;
            -- Control stop wins before owner work; each bounded batch gives
            -- IPC a turn,and a pending local queue prevents an idle sleep.
            Advance_Owner (Server, Owner_Result);
            if Diagnostics_Quiet(Server) then ACPI_Native_Module_Notes.Observe (ACPI_Requests.Module_Errors (Server));end if;
            if Owner_Result = Owner_Refused then
               Reason := Owner_Work_Refused; return;
            end if;
            Maintain_Owner (Owner_Deadline,May_Continue);
            if not May_Continue then
               Reason := Owner_Maintenance_Stopped; return;
            end if;
            -- Keep remaining events on their typed lane for the next turn.
            -- A mixed receive here could consume a second event as a request.
            Poll_Service_Request (From, Request, Found);
            if Found then
               Dispatch_Message (Server, From, Request, Response);
               if Diagnostics_Quiet(Server) then ACPI_Native_Module_Notes.Observe (ACPI_Requests.Module_Errors (Server));end if;
               -- Kernel reply-cap checks decide delivery; a lost/one-way reply
               -- never replays a dispatch, table import, or AML startup.
               if Reply_Now then Ignore := reply (From, Response); end if;
            elsif not Completion_Found and then not Event_Found and then Owner_Result = Owner_Idle
              and then Wait_For_Activity_Until (Owner_Deadline) = Unavailable then
               Reason := Wait_Unavailable; return;
            end if;
         end loop;
      end Serve;
   end With_Dispatch;
   package Default_Queries is new ACPI_Native_Server.With_Passive_Queries;
   package Default_Loop is new With_Dispatch
     (ACPI_Native_Endpoint.Dispatch,Advance_Owner=>Default_Queries.Advance,
      Maintain_Owner=>Default_Queries.Maintain,Reply_Now=>ACPI_Native_Endpoint.Reply_Now);
   procedure Run (Server : in out ACPI_Requests.State; Reason : out Stop_Reason) is
   begin
      Default_Loop.Serve (Server, Reason);
   end Run;
end ACPI_Native_Server;
