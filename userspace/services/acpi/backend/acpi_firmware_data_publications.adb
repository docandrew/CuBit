pragma Ada_2022;
package body ACPI_Firmware_Data_Publications is
   use type Word, L.Region, Journal.Outcome, Records.Record_ID, Records.Outcome,
     Arena.Outcome, Client.Outcome, R.IPC.Time_Instant;
   function Observe (Owner : Context) return Statistics is
      Held : Natural := 0;
   begin
      for I in 1 .. Journal.Length (Owner.Items) loop
         if Journal.Read (Owner.Items, I).Held /= No_Capability then Held := Held + 1; end if;
      end loop;
      return (Journal.Length (Owner.Items), Journal.Capacity (Owner.Items), Owner.Pending_Ordinal, Held,
        Owner.Working, Client.Observe (Owner.Grants).Cleanup_Due);
   end Observe;
   function Find (Owner : Context; Requester : Word; ID : Publication_ID) return Natural is
   begin
      for I in 1 .. Journal.Length (Owner.Items) loop
         declare Item : constant Entry_Record := Journal.Read (Owner.Items, I); begin
            if Item.Scope = Requester and then Item.ID = ID then return I; end if;
         end;
      end loop;
      return 0;
   end Find;
   function Snapshot (Item : Entry_Record) return Reply is
     ((Status => (case Item.Stage is when Published => Record_Ready,
          when Refused => Backend_Refused, when Stopped => Publication_Stopped, when others => Pending),
       ID => (if Item.Stage = Published then Item.Row else Records.No_Record),
       Stage => Item.Stage, Remote => Item.Remote, Request_Result => Item.Request_Result,
       Record_Result => Item.Record_Result, Arena_Result => Item.Arena_Result));
   procedure Drive (Owner : in out Context; Channel : in out Transport; Index : Positive;
      Deadline : R.IPC.Time_Instant; Result : out Reply) is
      Item : Entry_Record := Journal.Read (Owner.Items, Index);
      Status : Client.Outcome;
      procedure Save is
      begin Journal.Replace (Owner.Items, Index, Item); end Save;
      procedure Stop is
      begin Item.Stage := Stopped; Save; Owner.Pending_Ordinal := 0; end Stop;
   begin
      Result := Snapshot (Item);
      if Item.Stage in Published | Refused | Stopped then return;
      elsif Owner.Pending_Ordinal /= 0 and then Owner.Pending_Ordinal /= Index then Result.Status := Busy; return; end if;
      Owner.Pending_Ordinal := Index;
      if Item.Stage = Preparing then
         if Item.Row = Records.No_Record then
            Records.Reserve (Rows, Item.Scope, Item.Logical, Item.Row, Item.Record_Result); Save;
            if Item.Record_Result = Records.Already_Present then
               declare
                  Published_Index, Refused_Index : Natural := 0;
                  Unsafe_Prior : Boolean := False;
               begin
                  for I in 1 .. Journal.Length (Owner.Items) loop
                     if I /= Index then
                        declare Prior : constant Entry_Record := Journal.Read (Owner.Items, I); begin
                           if Prior.Row = Item.Row and then Prior.Scope = Item.Scope then
                              case Prior.Stage is
                                 when Published => Published_Index := I;
                                 when Refused => Refused_Index := I;
                                 when others => Unsafe_Prior := True;
                              end case;
                           end if;
                        end;
                     end if;
                  end loop;
                  if Published_Index /= 0 then
                     declare Prior : constant Entry_Record := Journal.Read (Owner.Items, Published_Index); begin
                        Item.Stage := Published; Item.Remote := Prior.Remote;
                        Item.Request_Result := Prior.Request_Result; Item.Record_Result := Prior.Record_Result;
                        Item.Arena_Result := Prior.Arena_Result;
                     end;
                     Save; Owner.Pending_Ordinal := 0; Result := Snapshot (Item); return;
                  elsif Unsafe_Prior or else Refused_Index = 0 then
                     Stop; Result := Snapshot (Item); return;
                  else
                     -- A NEW publication after a definitive no-grant refusal
                     -- may reuse the still-unmapped row/reservation. The old
                     -- publication identity remains a terminal refusal.
                     declare Prior : constant Entry_Record := Journal.Read (Owner.Items, Refused_Index); begin
                        Item.VA := Prior.VA; Item.Has_VA := Prior.Has_VA;
                     end;
                     Save;
                  end if;
               end;
            elsif Item.Record_Result /= Records.Record_Reserved then
               Result := Snapshot (Item); Result.Status := Capacity_Refused; return;
            end if;
         end if;
         if not Item.Has_VA then
            Arena.Reserve (Reservations, L.Envelope (Item.Logical).Pages, Item.VA, Item.Arena_Result);
            if Item.Arena_Result /= Arena.Reserved then
               if Item.Arena_Result /= Arena.Allocation_Refused then Stop; else Save; end if;
               Result := Snapshot (Item);
               if Item.Stage /= Stopped then Result.Status := Capacity_Refused; end if;
               return;
            end if;
            Item.Has_VA := True; Save;
         end if;
         Client.Prepare (Owner.Grants, L.Envelope (Item.Logical), Status);
         Item.Request_Result := Client.Observe (Owner.Grants).Last_Result;
         if Status /= Client.Prepared then Stop; Result := Snapshot (Item); return; end if;
         Item.Grant_Request := Client.Observe (Owner.Grants).Request.ID;
         Item.Stage := Requesting; Save;
      end if;
      if Item.Stage = Requesting then
         if Client.Observe (Owner.Grants).Cleanup_Due then
            Client.Retry_Disposal (Owner.Grants, Status);
            if Status /= Client.Cleanup_Complete then Result := Snapshot (Item); return; end if;
         end if;
         Client.Attempt (Owner.Grants, Channel, Deadline, Status);
         Item.Request_Result := Client.Observe (Owner.Grants).Last_Result; Save;
         if Status = Client.Refused then
            Item.Remote := Client.Observe (Owner.Grants).Request.Remote;
            Client.Record_Refusal (Owner.Grants, Status);
            Item.Stage := Refused; Save; Owner.Pending_Ordinal := 0;
         elsif Status = Client.Grant_Ready then
            Item.Remote := Client.Observe (Owner.Grants).Request.Remote;
            Client.Take_Grant (Owner.Grants, Item.Held, Status);
            if Status /= Client.Grant_Taken then Stop; Result := Snapshot (Item); return; end if;
            Item.Stage := Grant_Received; Save;
         else Result := Snapshot (Item); return;
         end if;
      end if;
      if Item.Stage = Grant_Received then
         Records.Attach (Rows, Item.Scope, Item.Row, Item.Held, Item.Remote, Item.Record_Result); Save;
         if Item.Record_Result /= Records.Grant_Attached then Stop; Result := Snapshot (Item); return; end if;
         Item.Stage := Mapping_Pending; Save;
      end if;
      if Item.Stage = Mapping_Pending then
         Records.Map_Record (Rows, Item.Scope, Item.Row, Item.VA, Deadline, Item.Record_Result); Save;
         if Item.Record_Result = Records.Mapped then Item.Stage := Published; Save; Owner.Pending_Ordinal := 0;
         elsif Item.Record_Result not in Records.Invalid_Deadline | Records.Clock_Unavailable | Records.Expired then Stop;
         end if;
      end if;
      Result := Snapshot (Item);
   end Drive;
   procedure Publish (Owner : in out Context; Channel : in out Transport;
      Requester : Word; ID : Publication_ID; Logical : L.Region;
      Deadline : R.IPC.Time_Instant; Result : out Reply) is
      Index : Natural; Status : Journal.Outcome;
      procedure Run is
      begin
         if not May_Publish (Requester) then Result.Status := Denied; return;
         elsif ID = No_Publication or else not L.Valid (Logical) then Result.Status := Invalid_Request; return;
         elsif Deadline = 0 or else Deadline = R.IPC.No_Finite_Deadline then Result.Status := Invalid_Deadline; return; end if;
         Index := Find (Owner, Requester, ID);
         if Index /= 0 then
            if Journal.Read (Owner.Items, Index).Logical /= Logical then Result.Status := Request_Changed; return; end if;
         else
            if Owner.Pending_Ordinal /= 0 then Result.Status := Busy; return; end if;
            Journal.Append (Owner.Items, (Scope => Requester, ID => ID, Logical => Logical, others => <>), Index, Status);
            if Status /= Journal.Stored then Result.Status := Capacity_Refused; return; end if;
         end if;
         Drive (Owner, Channel, Index, Deadline, Result);
      end Run;
   begin
      Result := (others => <>);
      if Owner.Working then Result.Status := Reentered; return; end if;
      Owner.Working := True; Run; Owner.Working := False;
   end Publish;
   procedure Describe (Owner : in out Context; Requester : Word; ID : Publication_ID; Result : out Reply) is
      Index : Natural;
   begin
      Result := (others => <>);
      if Owner.Working then Result.Status := Reentered; return; end if;
      Owner.Working := True;
      if not May_Publish (Requester) then Result.Status := Denied;
      else
         Index := Find (Owner, Requester, ID);
         if Index /= 0 then Result := Snapshot (Journal.Read (Owner.Items, Index)); end if;
      end if;
      Owner.Working := False;
   end Describe;
   procedure Recover (Owner : in out Context; Channel : in out Transport;
      Requester : Word; ID : Publication_ID; Deadline : R.IPC.Time_Instant; Result : out Reply) is
      Index : Natural;
      procedure Run is
      begin
         if not May_Publish (Requester) then Result.Status := Denied; return;
         elsif Deadline = 0 or else Deadline = R.IPC.No_Finite_Deadline then Result.Status := Invalid_Deadline; return; end if;
         Index := Find (Owner, Requester, ID);
         if Index = 0 then return; end if;
         Drive (Owner, Channel, Index, Deadline, Result);
      end Run;
   begin
      Result := (others => <>);
      if Owner.Working then Result.Status := Reentered; return; end if;
      Owner.Working := True; Run; Owner.Working := False;
   end Recover;
end ACPI_Firmware_Data_Publications;
