pragma Ada_2022;
package body ACPI_Firmware_Data_Records with SPARK_Mode is
   use type Word, L.Region, Store.Outcome, R.IPC.Time_Instant, R.Wire.Disposition;
   function Observe (Owner : Context) return Statistics is
     ((Store.Length (Owner.Items), Store.Capacity (Owner.Items), Owner.Working, Owner.Clock_Broken));
   function Exists (Owner : Context; ID : Record_ID) return Boolean is
     (ID /= No_Record and then Word (ID) <= Word (Store.Length (Owner.Items)));
   function Observe_Capacity (Owner : Context) return Capacity_Report is
      Current : constant Store.Report := Store.Observe (Owner.Items);
   begin
      return ((case Current.Status is
        when Store.Stored => No_Capacity_Failure,
        when Store.Allocation_Refused => Process_Heap_Limit,
        when Store.Count_Representation_Exhausted => Entry_Limit,
        when Store.Byte_Representation_Exhausted => Byte_Limit),
        Current.Demand, Word (Current.Capacity), Current.Requested_Bytes);
   end Observe_Capacity;
   function Select_Record (Owner : Context; Requester : Word; ID : Record_ID) return Outcome is
   begin
      if not Exists (Owner, ID) then return Invalid_Record;
      elsif Store.Read (Owner.Items, Positive (ID)).Scope /= Requester then return Wrong_Requester;
      else return Done; end if;
   end Select_Record;
   function Selection_Valid (Owner : Context; Requester : Word; ID : Record_ID;
      Offset : Word; Width : L.T.Access_Width) return Boolean is
   begin
      if Owner.Working or else Owner.Clock_Broken or else Requester = 0
        or else Select_Record (Owner,Requester,ID) /= Done then return False; end if;
      declare Item : constant Entry_Record := Store.Read (Owner.Items,Positive (ID)); begin
         return Item.Stage = Ready and then
            L.Resolve (Item.Logical,Item.VA,Offset,Width,False,0).Valid;
      end;
   end Selection_Valid;
   procedure Reserve (Owner : in out Context; Requester : Word; Item : L.Region;
      ID : out Record_ID; Result : out Outcome) is
      Index : Natural; Status : Store.Outcome;
   begin
      ID := No_Record; Result := Invalid_Region;
      if Owner.Working then Result := Reentered; return;
      elsif not L.Valid (Item) then return; end if;
      for I in 1 .. Store.Length (Owner.Items) loop
         declare Prior : constant Entry_Record := Store.Read (Owner.Items, I); begin
            if Prior.Scope = Requester and then Prior.Logical = Item then
               ID := Record_ID (I); Result := Already_Present; return;
            end if;
         end;
      end loop;
      Owner.Working := True;
      Store.Append (Owner.Items, (Scope => Requester, Logical => Item, others => <>), Index, Status);
      Owner.Working := False;
      if Status /= Store.Stored then Result := Allocation_Refused;
      else ID := Record_ID (Index); Result := Record_Reserved; end if;
   end Reserve;
   procedure Attach (Owner : in out Context; Requester : Word; ID : Record_ID;
      Held : in out Capability; Disposition : R.Wire.Disposition; Result : out Outcome) is
      Evidence : R.Grant_Evidence;
   begin
      if Owner.Working then Result := Reentered; return; end if;
      Result := Select_Record (Owner, Requester, ID); if Result /= Done then return; end if;
      declare Item : Entry_Record := Store.Read (Owner.Items, Positive (ID)); begin
         if Item.Stage /= Reserved or else not Present (Held) then Result := Not_Ready; return; end if;
         Owner.Working := True; Inspect (Held, Evidence); Owner.Working := False;
         if not R.Admitted (Evidence, Disposition, L.Envelope (Item.Logical)) then Result := Invalid_Grant; return; end if;
         Item.Held := Held; Item.Kind := Disposition; Item.Stage := Attached;
         Store.Replace (Owner.Items, Positive (ID), Item); Held := No_Capability; Result := Grant_Attached;
      end;
   end Attach;
   procedure Check_Time (Owner : in out Context; Deadline : R.IPC.Time_Instant; Result : out Outcome) is
      Now : R.IPC.Time_Instant; Available : Boolean;
   begin
      if Deadline = 0 or else Deadline = R.IPC.No_Finite_Deadline then Result := Invalid_Deadline; return;
      elsif Owner.Clock_Broken then Result := Clock_Reversed; return; end if;
      Clock (Now, Available);
      if not Available then Result := Clock_Unavailable;
      elsif Owner.Seen_Clock and then Now < Owner.Last_Clock then
         Owner.Clock_Broken := True; Result := Clock_Reversed;
      else
         Owner.Seen_Clock := True; Owner.Last_Clock := Now;
         Result := (if Now >= Deadline then Expired else Done);
      end if;
   end Check_Time;
   function Overlap (A, Bytes_A, B, Bytes_B : Word) return Boolean is
     (if A <= B then B - A < Bytes_A else A - B < Bytes_B);
   procedure Map_Record (Owner : in out Context; Requester : Word; ID : Record_ID;
      Item : Reservation; Deadline : R.IPC.Time_Instant; Result : out Outcome) is
      procedure Run is
         Record_Item : Entry_Record := Store.Read (Owner.Items, Positive (ID));
         Extent : constant P.Envelope := L.Envelope (Record_Item.Logical);
         Evidence : R.Grant_Evidence;
         VA, Pages : Word; Accepted, Completed : Boolean;
      begin
         if Record_Item.Stage /= Attached then Result := Not_Ready; return; end if;
         Describe_Reservation (Item, VA, Pages, Accepted);
         if not Accepted or else Pages /= Extent.Pages or else not L.Virtual_Valid (VA, Extent) then
            Result := Invalid_Reservation; return;
         end if;
         for I in 1 .. Store.Length (Owner.Items) loop
            declare Prior : constant Entry_Record := Store.Read (Owner.Items, I);
               Prior_Extent : constant P.Envelope := L.Envelope (Prior.Logical);
            begin
               if Prior.Stage in Ready | Mapping_Stopped then
                  if Overlap (VA, Pages * L.Page_Bytes, Prior.VA, Prior_Extent.Pages * L.Page_Bytes) then
                     Result := Reservation_Overlap; return;
                  elsif Prior.Kind /= Record_Item.Kind and then
                    Overlap (Extent.First_Page, Extent.Pages, Prior_Extent.First_Page, Prior_Extent.Pages)
                  then Result := Cache_Conflict; return; end if;
               end if;
            end;
         end loop;
         Check_Time (Owner, Deadline, Result); if Result /= Done then return; end if;
         Inspect (Record_Item.Held, Evidence);
         if not R.Admitted (Evidence, Record_Item.Kind, Extent) then Result := Invalid_Grant; return; end if;
         -- Fresh time after inspection and before any possible mapping effect.
         Check_Time (Owner, Deadline, Result); if Result /= Done then return; end if;
         Record_Item.VA := VA; Record_Item.Reserved_VA := Item; Record_Item.Stage := Mapping_Stopped;
         Store.Replace (Owner.Items, Positive (ID), Record_Item);
         Map (Record_Item.Held, Item, Extent.First_Page * L.Page_Bytes, VA, Pages, Deadline, Completed);
         if not Completed then Result := Mapping_Indeterminate; return; end if;
         Check_Time (Owner, Deadline, Result); if Result /= Done then Result := Late_Completed; return; end if;
         Record_Item.Stage := Ready; Store.Replace (Owner.Items, Positive (ID), Record_Item); Result := Mapped;
      end Run;
   begin
      if Owner.Working then Result := Reentered; return; end if;
      Result := Select_Record (Owner, Requester, ID); if Result /= Done then return; end if;
      Owner.Working := True; Run; Owner.Working := False;
   end Map_Record;
   procedure Access_Record (Owner : in out Context; Attempt : in out Operation;
      Requester : Word; ID : Record_ID; Offset : Word; Width : L.T.Access_Width;
      For_Write : Boolean; Input : Word; Deadline : R.IPC.Time_Instant;
      Output : out Word; Result : out Outcome) is
      procedure Run is
         Item : constant Entry_Record := Store.Read (Owner.Items, Positive (ID));
         Where : constant L.Access_Result := L.Resolve (Item.Logical, Item.VA, Offset, Width, For_Write, Input);
         Evidence : R.Grant_Evidence;
         Value : Word; Completed : Boolean;
      begin
         if Item.Stage /= Ready then Result := Not_Ready; return;
         elsif not Where.Valid then Result := Invalid_Access; return;
         elsif not Scope_Usable (Requester, For_Write) then Result := Denied; return; end if;
         Check_Time (Owner, Deadline, Result); if Result /= Done then return; end if;
         Inspect (Item.Held, Evidence);
         if not R.Admitted (Evidence, Item.Kind, L.Envelope (Item.Logical)) then Result := Invalid_Grant; return; end if;
         Check_Time (Owner, Deadline, Result); if Result /= Done then return; end if;
         if not Scope_Usable (Requester, For_Write) then Result := Denied; return; end if;
         Transfer (Where.Virtual, Width, For_Write, Input, Value, Completed);
         if not Completed then Result := Transfer_Indeterminate; return; end if;
         if (For_Write and then Value /= 0) or else (not For_Write and then Value > L.Mask (Width)) then
            Result := Transfer_Indeterminate; return;
         end if;
         Check_Time (Owner, Deadline, Result); if Result /= Done then Result := Late_Completed; return; end if;
         Output := Value; Result := Done;
      end Run;
   begin
      Output := 0;
      if Owner.Working then Result := Reentered; return;
      elsif Attempt.Consumed then Result := Already_Attempted; return; end if;
      Attempt.Consumed := True;
      Result := Select_Record (Owner, Requester, ID); if Result /= Done then return; end if;
      Owner.Working := True; Run; Owner.Working := False;
   end Access_Record;
end ACPI_Firmware_Data_Records;
