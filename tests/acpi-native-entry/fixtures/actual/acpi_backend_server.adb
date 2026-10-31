pragma Ada_2022;
package body ACPI_Backend_Server is
   package M renames CuBit.Messages;
   use type Interfaces.Unsigned_64, M.Activity_Result;
   procedure Serve (Hardware : in out Catalog.Context; Result : out Report) is
      Request, Response, Event : M.Message;
      From, Publisher : M.Process_ID;
      Completed, Event_Found, Found, Stop : Boolean;
      Waited : M.Activity_Result;
      procedure Increment (Value : in out Counter) is
      begin
         if Value < Counter'Last then Value := Value + 1;
         else Result.Counter_Saturated := True; end if;
      end Increment;
   begin
      Result := (others => <>);
      loop
         Poll_Completion (Completed);
         if Completed then Result.Status := Unexpected_Completion; return; end if;
         Event_Found := Poll_Event (Event, Publisher);
         if Event_Found then
            Increment (Result.Events);
            Handle_Event (Publisher, Event, Stop);
            if Stop then Result.Status := Stop_Requested; return; end if;
         end if;
         Poll_Request (From, Request, Found);
         if Found then
            Increment (Result.Requests);
            Endpoint.Dispatch (Hardware, Request, Response);
            if Catalog.Observe (Hardware).Quarantined and then not Result.Quarantine_Noted then
               Result.Quarantine_Noted := True;
               Quarantine_Notice (Catalog.Observe (Hardware));
            end if;
            if Reply (From, Response) = 0 then Increment (Result.Lost_Replies); end if;
         elsif not Event_Found then
            Waited := Wait (M.Wait_Forever);
            case Waited is
               when M.Work_Available => null;
               when M.Unavailable => Result.Status := Wait_Unavailable; return;
               when M.Deadline_Reached => Result.Status := Unexpected_Deadline; return;
            end case;
         end if;
      end loop;
   end Serve;
end ACPI_Backend_Server;
