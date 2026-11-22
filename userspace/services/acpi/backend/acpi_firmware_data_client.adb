pragma Ada_2022;
package body ACPI_Firmware_Data_Client with SPARK_Mode is
   use type R.Outcome, R.Phase;
   function Observe (Context : State) return Observation is
     ((R.Observe (Context.Request), Context.Working, Present (Context.Held),
       Context.Cleanup_Due, Context.Last_Result));
   procedure Prepare (Context : in out State; Extent : P.Envelope; Result : out Outcome) is
   begin
      Result := Not_Ready;
      if Context.Working then Result := Reentered; return;
      elsif Present (Context.Held) or else Context.Cleanup_Due then return; end if;
      R.Prepare (Context.Request, Extent, Context.Last_Result);
      if Context.Last_Result = R.Request_Prepared then Result := Prepared; end if;
   end Prepare;
   procedure Discard (Context : in out State; Closed : out Boolean) is
      Candidate : Capability := Context.Held;
   begin
      Closed := True;
      if Present (Context.Held) then Dispose (Candidate, Closed); end if;
      if Closed then Context.Held := No_Capability; end if;
      Context.Cleanup_Due := not Closed;
   end Discard;
   procedure Attempt (Context : in out State; Channel : in out Transport;
      Deadline : IPC.Time_Instant; Result : out Outcome) is
      Request, Reply : IPC.Packet;
      Received : Capability := No_Capability;
      Delivered : IPC.Delivery;
      Evidence : R.Grant_Evidence;
      Now : IPC.Time_Instant;
      Available, Closed : Boolean;
      procedure Run is
      begin
         Clock (Now, Available);
         R.Begin_Attempt (Context.Request, Now, Deadline, Available, Request, Context.Last_Result);
         if Context.Last_Result /= R.Attempt_Prepared then Result := Attempt_Stopped; return; end if;
         Exchange (Channel, Deadline, Request, Reply, Received, Delivered);
         Context.Held := Received;
         R.Finish_Attempt (Context.Request, Delivered, Reply, Present (Received), Context.Last_Result);
         if Context.Last_Result = R.Inspect_Grant then
            Inspect (Context.Held, Evidence); Clock (Now, Available);
            R.Check_Grant (Context.Request, Evidence, Now, Available, Context.Last_Result);
            if Context.Last_Result = R.Grant_Ready then Result := Grant_Ready; return; end if;
         end if;
         Discard (Context, Closed);
         if not Closed then Result := Cleanup_Required;
         elsif R.Observe (Context.Request).Stage = R.Refused then Result := Refused;
         elsif R.Observe (Context.Request).Stage = R.Uncertain then Result := Completion_Unknown;
         else Result := Attempt_Stopped;
         end if;
      end Run;
   begin
      Result := Not_Ready;
      if Context.Working then Result := Reentered; return;
      elsif Context.Cleanup_Due then Result := Cleanup_Required; return;
      elsif Present (Context.Held) then return; end if;
      Context.Working := True; Run; Context.Working := False;
   end Attempt;
   procedure Retry_Disposal (Context : in out State; Result : out Outcome) is
      Closed : Boolean;
   begin
      Result := Not_Ready;
      if Context.Working then Result := Reentered; return;
      elsif not Context.Cleanup_Due then return; end if;
      Context.Working := True; Discard (Context, Closed); Context.Working := False;
      Result := (if Closed then Cleanup_Complete else Cleanup_Required);
   end Retry_Disposal;
   procedure Take_Grant (Context : in out State; Item : out Capability; Result : out Outcome) is
   begin
      Item := No_Capability; Result := Not_Ready;
      if Context.Working then Result := Reentered; return;
      elsif Context.Cleanup_Due or else not Present (Context.Held)
        or else R.Observe (Context.Request).Stage /= R.Grant_Verified then return; end if;
      Item := Context.Held; Context.Held := No_Capability;
      R.Acknowledge (Context.Request, Context.Last_Result);
      Result := Grant_Taken;
   end Take_Grant;
   procedure Record_Refusal (Context : in out State; Result : out Outcome) is
   begin
      Result := Not_Ready;
      if Context.Working then Result := Reentered; return;
      elsif Context.Cleanup_Due or else Present (Context.Held)
        or else R.Observe (Context.Request).Stage /= R.Refused then return; end if;
      R.Acknowledge (Context.Request, Context.Last_Result); Result := Refusal_Recorded;
   end Record_Refusal;
end ACPI_Firmware_Data_Client;
