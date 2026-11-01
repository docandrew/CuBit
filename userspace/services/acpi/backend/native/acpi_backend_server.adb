pragma Ada_2022;
package body ACPI_Backend_Server is
   package M renames CuBit.Messages;
   use type Interfaces.Unsigned_64, M.Activity_Result;
   procedure Serve (Hardware : in out Catalog.Context; Result : out Report) is
      Request, Response, Event : M.Message;
      From, Publisher : M.Process_ID;
      Completed, Event_Found, Found, Stop, Handled : Boolean;
      Waited : M.Activity_Result;
      Deadline : Interfaces.Unsigned_64;
      Proceed : Boolean;
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
         Deadline := M.Wait_Forever; Proceed := True;
         Maintain (Deadline, Proceed);
         if not Proceed then Result.Status := Maintenance_Stopped; return; end if;
         Event_Found := Poll_Event (Event, Publisher);
         if Event_Found then
            Increment (Result.Events);
            Handle_Event (Publisher, Event, Stop);
            if Stop then Result.Status := Stop_Requested; return; end if;
         end if;
         Poll_Request (From, Request, Found);
         if Found then
            Increment (Result.Requests);
            Handled := False; Response := M.NULL_MESSAGE;
            Handle_Additional_Request (From, Request, Response, Handled);
            if not Handled then Endpoint.Dispatch (Hardware, Request, Response); end if;
            if Catalog.Observe (Hardware).Quarantined and then not Result.Quarantine_Noted then
               Result.Quarantine_Noted := True;
               Quarantine_Notice (Catalog.Observe (Hardware));
            end if;
            if Reply (From, Response) = 0 then Increment (Result.Lost_Replies); end if;
         elsif not Event_Found then
            Waited := Wait (Deadline);
            case Waited is
               when M.Work_Available => null;
               when M.Unavailable => Result.Status := Wait_Unavailable; return;
               when M.Deadline_Reached =>
                  if Deadline = M.Wait_Forever then Result.Status := Unexpected_Deadline; return;
                  else Increment (Result.Deadline_Wakes); end if;
            end case;
         end if;
      end loop;
   end Serve;
end ACPI_Backend_Server;
