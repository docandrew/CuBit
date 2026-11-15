pragma Ada_2022;
with Platform_Pool_ABI;
package body ACPI_Platform_Pool_Owner with SPARK_Mode is
   use type Word, Interfaces.Unsigned_32, Interfaces.Unsigned_8, Interfaces.Unsigned_16;
   use type Capability_ABI.Protocol_Flags;
   function Observe (State : Context) return Status is (State.View);
   function Pool (State : Context) return Authority is
     (if State.View.Stage = Ready then State.Held else No_Authority);

   procedure Retry_Disposal (State : in out Context; Completed : out Boolean) is
      Done : Boolean;
   begin
      Completed := False;
      if State.View.Stage /= Cleanup_Required then return; end if;
      State.View.Stage := Working;
      Dispose (State.Received, Done);
      if Done then
         State.Received := No_Carried;
         State.View.Retained_Carriage := False;
         State.View.Stage := State.Resume;
         Completed := True;
      else
         State.View.Stage := Cleanup_Required;
      end if;
   end Retry_Disposal;

   procedure Acquire (State : in out Context; Target : Endpoint; Expected_Owner,
      Request, Deadline_Milliseconds : Word; Accepted : out Boolean)
   is
      Reply : Response;
      Start, Now : Word;
      Available, Valid, Present, Refused : Boolean;
      Held : Authority := No_Authority;
      procedure Reject (Result : Outcome; Final : Phase := Rejected) is
         Done : Boolean;
      begin
         State.View.Result := Result;
         State.Resume := Final;
         if Is_Present (State.Received) then
            State.View.Retained_Carriage := True;
            State.View.Stage := Cleanup_Required;
            Retry_Disposal (State, Done);
         else
            State.Received := No_Carried;
            State.View.Stage := Final;
         end if;
      end Reject;
   begin
      Accepted := False;
      if State.View.Stage /= Fresh then return; end if;
      -- Latch before even inspecting the endpoint or reading the clock.
      State.View := (Stage => Working, Request => Request, Peer => Expected_Owner, others => <>);
      if Request = 0 or else Expected_Owner = 0 then Reject (Bad_Request); return; end if;
      if Deadline_Milliseconds = 0 or else Deadline_Milliseconds = Word'Last then
         Reject (Invalid_Deadline); return;
      end if;
      if not Inspect_Endpoint (Target, Expected_Owner) then Reject (Bad_Endpoint); return; end if;
      Read_Clock (Start, Available);
      if not Available then Reject (Clock_Unavailable); return; end if;
      if Start >= Deadline_Milliseconds then Reject (Deadline_Expired); return; end if;
      -- The owner retains the carriage even if the returned frame is invalid.
      Exchange (Target, Request, Deadline_Milliseconds, Reply, State.Received);
      if Reply.Ending = Not_Dispatched then Reject (Not_Sent); return; end if;
      if Reply.Ending /= Answered then Reject (Unknown_Completion, Uncertain); return; end if;
      Read_Clock (Now, Available);
      if not Available then Reject (Clock_Unavailable, Uncertain); return; end if;
      if Now < Start then Reject (Clock_Reversed, Uncertain); return; end if;
      if Now >= Deadline_Milliseconds then Reject (Deadline_Expired, Uncertain); return; end if;
      Present := Is_Present (State.Received);
      Refused := Is_Refused (State.Received);
      if Reply.Label /= Wire.Pool_Request or else Reply.Length /= Wire.Reply_Words
        or else Reply.Protocol /= 0 or else Reply.Reserved /= 0
        or else Reply.IPC.Accepts_Capability or else Reply.IPC.Capability_Offered
        or else (Present and Refused)
        or else Reply.IPC.Capability_Transferred /= Present
        or else Reply.IPC.Capability_Refused /= Refused
        or else Reply.Data (Wire.Reply_Request_Word) /= Request
        or else Reply.Data (Wire.Reply_Words) /= 0
        or else not Wire.Valid_Disposition (Reply.Data (Wire.Disposition_Word))
        or else Reply.Data (Wire.Members_Word) > Platform_Pool_ABI.Maximum_Members
      then Reject (Bad_Reply); return; end if;
      State.View.Disposition := Reply.Data (Wire.Disposition_Word);
      State.View.Members := Reply.Data (Wire.Members_Word);
      if State.View.Disposition /= Wire.Disposition'Enum_Rep (Wire.Granted) then
         if Present or Refused then Reject (Bad_Reply); else Reject (Server_Refused); end if;
         return;
      end if;
      -- Reply_With can deliver GRANTED with the kernel's refused-carriage
      -- flag when this process has no capability-table room. This is valid.
      if Refused then Reject (Transfer_Refused); return; end if;
      if not Present then Reject (Bad_Reply); return; end if;
      Inspect_Pool (State.Received, Held, Valid);
      if not Valid then Reject (Bad_Reply); return; end if;
      State.Held := Held;
      State.View.Retained_Carriage := True;
      State.View.Result := Acquired;
      State.View.Stage := Ready;
      Accepted := True;
   end Acquire;

   procedure Close_Admission (State : in out Context) is
   begin
      if State.View.Stage in Working | Cleanup_Required | Uncertain then return; end if;
      State.View.Stage := Closed;
   end Close_Admission;
end ACPI_Platform_Pool_Owner;
