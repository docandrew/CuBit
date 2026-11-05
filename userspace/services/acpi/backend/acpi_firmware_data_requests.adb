pragma Ada_2022;
with Capability_ABI;
with Firmware_Data_ABI;
package body ACPI_Firmware_Data_Requests with SPARK_Mode is
   use type P.Envelope, IPC.Delivery, Interfaces.Unsigned_64, Wire.Disposition;
   function Observe (Context : State) return Observation is
     ((Context.Stage, Context.Last_ID, Context.Extent, Context.Clock_Broken,
       Context.Reply_Known, Context.Remote));
   procedure Prepare (Context : in out State; Extent : P.Envelope; Result : out Outcome) is
   begin
      Result := Not_Ready;
      if Context.Stage /= Idle then return;
      elsif not P.Valid (Extent) then Result := Invalid_Envelope; return;
      elsif Context.Last_ID = P.Request_ID'Last then Result := Exhausted; return;
      end if;
      Context.Last_ID := Context.Last_ID + 1; Context.Extent := Extent;
      Context.Remote := Wire.Malformed; Context.Previously_Uncertain := False;
      Context.Reply_Known := False;
      Context.Stage := Prepared; Result := Request_Prepared;
   end Prepare;
   procedure Time_Remains (Context : in out State; Now : IPC.Time_Instant;
      Clock_Available : Boolean; Result : out Outcome) is
   begin
      if Context.Clock_Broken then Result := Clock_Reversed;
      elsif not Clock_Available then Result := Clock_Unavailable;
      elsif Context.Seen_Clock and then Now < Context.Last_Clock then
         Context.Clock_Broken := True; Result := Clock_Reversed;
      else
         Context.Seen_Clock := True; Context.Last_Clock := Now;
         Result := (if Now >= Context.Deadline then Expired else Attempt_Prepared);
      end if;
   end Time_Remains;
   procedure Begin_Attempt (Context : in out State; Now, Deadline : IPC.Time_Instant;
      Clock_Available : Boolean; Request : out IPC.Packet; Result : out Outcome) is
   begin
      Request := (Label => 0, Length => 0, others => <>); Result := Not_Ready;
      if Context.Stage not in Prepared | Uncertain then return;
      elsif Deadline = 0 or else Deadline = IPC.No_Finite_Deadline then Result := Invalid_Deadline; return;
      end if;
      Context.Deadline := Deadline; Time_Remains (Context, Now, Clock_Available, Result);
      if Result /= Attempt_Prepared then return; end if;
      Request := P.Encode (Context.Last_ID, Context.Extent);
      Context.Previously_Uncertain := Context.Stage = Uncertain;
      Context.Stage := In_Flight;
   end Begin_Attempt;
   procedure Finish_Attempt (Context : in out State; Delivered : IPC.Delivery;
      Reply : IPC.Packet; Has_Capability : Boolean; Result : out Outcome) is
   begin
      Result := Not_Ready; if Context.Stage /= In_Flight then return; end if;
      Context.Stage := Uncertain;
      if Delivered = IPC.Not_Dispatched and then not Has_Capability then
         Context.Stage := (if Context.Previously_Uncertain then Uncertain else Prepared);
         Result := Not_Sent; return;
      elsif Delivered /= IPC.Reply_Received then
         Result := (if Has_Capability then Bad_Reply else Completion_Unknown); return;
      end if;
      declare
         Parsed : constant P.Reply := P.Decode (Reply, Context.Last_ID, Context.Extent, Has_Capability);
      begin
         if not Parsed.Valid then Result := Bad_Reply; return; end if;
         if Context.Reply_Known and then Parsed.Disposition /= Context.Remote then
            Result := Bad_Reply; return;
         end if;
         Context.Reply_Known := True;
         Context.Remote := Parsed.Disposition;
         if Wire.Granted (Parsed.Disposition) then
            Context.Stage := Grant_Unchecked; Result := Inspect_Grant;
         else Context.Stage := Refused; Result := Backend_Refused;
         end if;
      end;
   end Finish_Attempt;
   function Admitted (Evidence : Grant_Evidence; D : Wire.Disposition;
      Extent : P.Envelope) return Boolean is
     (P.Valid (Extent) and then Evidence.Fresh_Current_Handle
      and then Evidence.Device_Memory and then Evidence.Extent = Extent
      and then Evidence.Readable and then Evidence.Writable and then not Evidence.Grantable
      and then not Evidence.Executable and then not Evidence.Revocable
      and then (case D is
         when Wire.Granted_Data => Evidence.Tag = Firmware_Data_ABI.Firmware_Data_Tag
           and then Evidence.Cache = Uncached,
         when Wire.Reused_NVS => Evidence.Tag = Capability_ABI.Firmware_NVS_Tag
           and then Evidence.Cache = Write_Back,
         when others => False));
   procedure Check_Grant (Context : in out State; Evidence : Grant_Evidence;
      Now : IPC.Time_Instant; Clock_Available : Boolean; Result : out Outcome) is
   begin
      Result := Not_Ready; if Context.Stage /= Grant_Unchecked then return; end if;
      Context.Stage := Uncertain;
      if not Admitted (Evidence, Context.Remote, Context.Extent) then Result := Invalid_Grant; return; end if;
      Time_Remains (Context, Now, Clock_Available, Result);
      if Result = Attempt_Prepared then Context.Stage := Grant_Verified; Result := Grant_Ready; end if;
   end Check_Grant;
   procedure Acknowledge (Context : in out State; Result : out Outcome) is
   begin
      Result := Not_Ready;
      if Context.Stage in Grant_Verified | Refused then
         Context.Stage := Idle; Context.Extent := P.Empty_Envelope;
         Context.Remote := Wire.Malformed; Context.Previously_Uncertain := False;
         Context.Reply_Known := False;
         Result := Acknowledged;
      end if;
   end Acknowledge;
end ACPI_Firmware_Data_Requests;
