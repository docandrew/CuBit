package body Intel_GPU_Buffer_Requests is
   procedure Query_Accounting
     (Object : Service; Sender, Stamp : Unsigned_64;
      Request_Label : Unsigned_32; Length, Flags : Unsigned_8;
      Reserved : Unsigned_16; Request : Words; Response : out Words) is
      Session : constant Unsigned_64 := Session_Of (Sender, Stamp);
      Usage : Intel_GPU_Client_Budgets.Usage;
   begin
      Response := [Denied, Version, 0, 0];
      if Session = 0 then return; end if;
      Response (0) := Bad_Request;
      if Request_Label /= Accounting_Label or else Length /= 4 or else
        Flags /= 0 or else Reserved /= 0 or else Request /= Words'(Version, 0, 0, 0)
      then return; end if;
      Response (0) := Unavailable;
      if Object.Failed or else not Owner_Ready then return; end if;
      Usage := Client_Usage (Object, Session);
      if not Usage.Known or else Usage.Closed then return; end if;
      Response := [OK, Version, Usage.Limit, Usage.Charged];
   end Query_Accounting;

   procedure Configure_Client_Budgets
     (Object : in out Service; Limit : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Failed or else not Owner_Ready or else Object.Client_Limit /= 0 or else
        Object.Attempted /= First_Slot - 1 or else Object.Pending /= 0 or else
        Object.Private_Pending /= 0 or else Limit = 0 or else Limit mod 4096 /= 0 then return; end if;
      Object.Client_Limit := Limit; Accepted := True;
   end Configure_Client_Budgets;
   procedure Extend_Client_Accounts
     (Object : in out Service; Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Failed or else not Owner_Ready or else Object.Pending /= 0 or else
        Object.Private_Pending /= 0 then return; end if;
      Intel_GPU_Client_Budgets.Extend (Object.Client_Accounts, Base, Bytes, Accepted);
   end Extend_Client_Accounts;
   function Client_Usage (Object : Service; Session : Unsigned_64)
     return Intel_GPU_Client_Budgets.Usage is
     (Intel_GPU_Client_Budgets.Snapshot (Object.Client_Accounts, Session));
   function Charge_Client
     (Object : in out Service; Session, Bytes : Unsigned_64) return Boolean is
      Accepted : Boolean;
   begin
      if Object.Client_Limit = 0 or Session = 0 then return True; end if;
      if not Client_Usage (Object, Session).Known then
         Intel_GPU_Client_Budgets.Open (Object.Client_Accounts, Session, Object.Client_Limit, Accepted);
         if not Accepted then return False; end if;
      end if;
      Intel_GPU_Client_Budgets.Reserve (Object.Client_Accounts, Session, Bytes, Accepted);
      return Accepted;
   end Charge_Client;
   function Refund_Client
     (Object : in out Service; Index : Intel_GPU_Buffer_Backing.Slot) return Boolean is
      Item : constant Allocation_Record := Records.Get (Object.Items, Index);
      Accepted : Boolean;
   begin
      if Object.Client_Limit = 0 or Item.Owner = 0 then return True; end if;
      Intel_GPU_Client_Budgets.Release_Confirmed
        (Object.Client_Accounts, Item.Owner, Item.Charge_Bytes, True, Accepted);
      if not Accepted then Quarantine (Object); end if;
      return Accepted;
   end Refund_Client;
   function Image_Writes_Held (Object : Service; Session : Unsigned_64) return Boolean is
     (Object.Failed or else not Owner_Ready or else
      Intel_GPU_Buffer_Handles.Session_Writes_Excluded (Object.Handles, Session));
   function Close_Diagnostic
     (Object : Service; Sender, Stamp, ID : Unsigned_64)
      return Intel_GPU_Buffer_Handles.Close_Check is
      package Handles renames Intel_GPU_Buffer_Handles;
   begin
      if ID > Unsigned_64 (Handles.Handle'Last) then
         return Handles.Invalid_Handle;
      end if;
      return Handles.Check_Close
        (Object.Handles, Session_Of (Sender, Stamp), Handles.Handle (ID));
   end Close_Diagnostic;
   package Layout renames Intel_GPU_Buffer_Backing;
   package Handles renames Intel_GPU_Buffer_Handles;
   function Record_Capacity (Object : Service) return Positive is
     (Records.Capacity (Object.Items));
   function Committed_Slots (Object : Service) return Positive is
     (Object.Admitted);
   function Next_Fresh_Slot (Object : Service) return Natural is
     (if Object.Attempted = Layout.Slot'Last then 0 else Object.Attempted + 1);
   function Handle_Capacity (Object : Service) return Natural is
     (Handles.Record_Capacity (Object.Handles));
   procedure Admit_Slots
     (Object : in out Service; Count : Positive;
      Supporting : Supporting_Capacities; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Failed or else Object.Pending /= 0 or else
        Object.Private_Pending /= 0 or else not Owner_Ready or else
        Count < Object.Admitted or else Count > Record_Capacity (Object) or else
        Count > Handle_Capacity (Object) or else
        Count > Supporting.Backing or else Count > Supporting.Replacements or else
        Count > Supporting.Retirement or else Count > Supporting.Update_Index
      then return; end if;
      Object.Admitted := Count;
      Accepted := True;
   end Admit_Slots;
   procedure Extend_Tickets
     (Object : in out Service; Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Failed or else Object.Pending /= 0 or else
        Object.Private_Pending /= 0 or else not Owner_Ready then return; end if;
      Records.Extend (Object.Items, Base, Bytes, Accepted);
   end Extend_Tickets;
   procedure Extend_Handles
     (Object : in out Service; Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Failed or else Object.Pending /= 0 or else
        Object.Private_Pending /= 0 or else not Owner_Ready then return; end if;
      Handles.Extend_Storage (Object.Handles, Base, Bytes, Accepted);
   end Extend_Handles;
   function Last_Allocation (Object : Service) return Allocation_Outcome is
     (Object.Outcome);
   function Ticket_Session (Object : Service; ID : Ticket) return Unsigned_64 is
     (if ID = 0 or else Ticket_Slot (ID) > Committed_Slots (Object) or else
       Records.Get (Object.Items, Ticket_Slot (ID)).Identity /= ID then 0
      else Records.Get (Object.Items, Ticket_Slot (ID)).Owner);
   function Pending_For (Object : Service; Session : Unsigned_64) return Boolean is
     (Session /= 0 and then
       ((Object.Pending /= 0 and then Ticket_Session (Object, Object.Pending) = Session)
        or else (Object.Private_Pending /= 0 and then
          Ticket_Session (Object, Object.Private_Pending) = Session)));
   function Closed_At (Object : Service; Index : Layout.Slot) return Closed_Allocation is
      ID : Ticket;
      Issued : Issued_Result;
   begin
      if Index > Committed_Slots (Object) then return (Ready => False); end if;
      ID := Records.Get (Object.Items, Index).Identity;
      Issued := Records.Get (Object.Items, Index).Issued;
      if Object.Failed or else not Owner_Ready or else
        Object.Pending /= 0 or else Object.Private_Pending /= 0 or else
        Records.Get (Object.Items, Index).Reusable or else ID = 0 or else Ticket_Slot (ID) /= Index or else
        Issued.Session = 0 or else Issued.Session /= Records.Get (Object.Items, Index).Owner or else
        Issued.Handle = 0 or else
        not Handles.Closed_Backing (Object.Handles, Issued.Session, Issued.Handle).Ready
      then return (Ready => False); end if;
      return (True, ID, Issued.Session, Issued.Handle,
        Ticket_Generation (ID));
   end Closed_At;
   function Ticket_Bytes (Object : Service; ID : Ticket) return Unsigned_64 is
     (if ID = 0 or else Ticket_Slot (ID) > Committed_Slots (Object) or else
         Records.Get (Object.Items, Ticket_Slot (ID)).Identity /= ID
      then 0 else Records.Get (Object.Items, Ticket_Slot (ID)).Charge_Bytes);
   procedure Reserve_Private
     (Object : in out Service; Session : Unsigned_64; ID : out Ticket;
      Reclaimable : Boolean := False;
      Kind : Private_Table_Kind := Replacement_Tables;
      Pages : Intel_GPU_Buffer_Backing.Page_Count) is
   begin
      ID := 0;
      if Object.Failed or else Object.Pending /= 0 or else
        Object.Private_Pending /= 0 or else not Owner_Ready or else
        (Reclaimable and then Session = 0) or else
        (Kind = Incremental_Tables and then not Reclaimable) then return; end if;
      if Reclaimable and then Object.Private_Reusable_Head /= 0 then
         declare Index : constant Natural := Object.Private_Reusable_Head; begin
            if Index > Committed_Slots (Object) then Quarantine (Object); return; end if;
            declare Item : constant Allocation_Record := Records.Get (Object.Items, Index); begin
               if not Item.Private_Reusable or else Item.Reusable or else Item.Context_Parent or else
                 Item.Issued.Handle /= 0 or else Item.Identity = 0 or else
                 Ticket_Slot (Item.Identity) /= Index or else
                 Item.Identity > Ticket'Last - Ticket_Stride or else Item.Charge_Bytes /= 0 or else
                 Item.Next_Reusable = Index or else Item.Next_Reusable > Committed_Slots (Object)
               then Quarantine (Object); return; end if;
            end;
               if not Charge_Client (Object, Session, Unsigned_64 (Pages) * 4096) then return; end if;
               Object.Private_Reusable_Head := Records.Get (Object.Items, Index).Next_Reusable;
               Object.Private_Pending := Records.Get (Object.Items, Index).Identity + Ticket_Stride;
               Records.Put (Object.Items, Index,
           (Records.Get (Object.Items, Index) with delta Identity => Object.Private_Pending));
               Records.Put (Object.Items, Index,
           (Records.Get (Object.Items, Index) with delta Owner => Session));
               Records.Put (Object.Items, Index,
           (Records.Get (Object.Items, Index) with delta Private_Reusable => False, Next_Reusable => 0));
               Records.Put (Object.Items, Index,
                 (Records.Get (Object.Items, Index) with delta
                  Private_Closed => False, Private_Reclaimable => True, Table_Kind => Kind,
                  Charge_Bytes => Unsigned_64 (Pages) * 4096));
               ID := Object.Private_Pending;
               return;
         end;
      end if;
      if Object.Attempted >= Committed_Slots (Object) then return; end if;
      if not Charge_Client (Object, Session, Unsigned_64 (Pages) * 4096) then return; end if;
      Object.Attempted := Object.Attempted + 1;
      Object.Private_Pending := Ticket (Object.Attempted);
      Records.Put (Object.Items, Object.Attempted,
           (Records.Get (Object.Items, Object.Attempted) with delta Identity => Object.Private_Pending));
      Records.Put (Object.Items, Object.Attempted,
           (Records.Get (Object.Items, Object.Attempted) with delta Owner => Session));
      Records.Put (Object.Items, Object.Attempted,
           (Records.Get (Object.Items, Object.Attempted) with delta
            Private_Reclaimable => Reclaimable, Table_Kind => Kind,
            Charge_Bytes => Unsigned_64 (Pages) * 4096));
      ID := Object.Private_Pending;
   end Reserve_Private;
   function Is_Table_Allocation
     (Object : Service; Session : Unsigned_64; ID : Ticket;
      Kind : Private_Table_Kind) return Boolean is
      Index : constant Layout.Slot := Ticket_Slot (ID);
   begin
      if Session = 0 or else ID = 0 or else Object.Failed or else not Owner_Ready or else
        Index > Committed_Slots (Object)
      then return False; end if;
      declare
         Item : constant Allocation_Record := Records.Get (Object.Items, Index);
      begin
         return Item.Identity = ID and then Item.Owner = Session and then Item.Table_Kind = Kind and then
           (Item.Private_Reclaimable or else Item.Private_Closed) and then
           not Item.Private_Reusable and then not Item.Reusable and then
           not Item.Context_Parent and then Item.Issued.Handle = 0;
      end;
   end Is_Table_Allocation;
   procedure Acknowledge_Private_Retirement
     (Object : in out Service; Session : Unsigned_64; ID : Ticket;
      References_Retired : Boolean; Accepted : out Boolean) is
      Index : constant Layout.Slot := Ticket_Slot (ID);
   begin
      Accepted := False;
      if Object.Failed or else not References_Retired or else not Owner_Ready or else
        Index > Committed_Slots (Object) or else
        Session = 0 or else ID = 0 or else ID > Ticket'Last - Ticket_Stride or else
        Object.Pending /= 0 or else Object.Private_Pending /= 0 or else
        Records.Get (Object.Items, Index).Identity /= ID or else Records.Get (Object.Items, Index).Owner /= Session or else
        Records.Get (Object.Items, Index).Context_Parent or else
        not Records.Get (Object.Items, Index).Private_Reclaimable or else Records.Get (Object.Items, Index).Private_Reusable or else
        Records.Get (Object.Items, Index).Issued.Handle /= 0 or else Records.Get (Object.Items, Index).Reusable
      then return; end if;
      if not Refund_Client (Object, Index) then return; end if;
      Records.Put (Object.Items, Index,
           (Records.Get (Object.Items, Index) with delta Private_Reusable => True,
            Charge_Bytes => 0, Next_Reusable => Object.Private_Reusable_Head));
      Object.Private_Reusable_Head := Index;
      Accepted := True;
   end Acknowledge_Private_Retirement;
   procedure Finish_Private
     (Object : in out Service; ID : Ticket; Consumed : out Boolean) is
   begin
      Consumed := ID /= 0 and then ID = Object.Private_Pending;
      if Consumed then Object.Private_Pending := 0; end if;
   end Finish_Private;
   procedure Handle
     (Object : in out Service; Sender, Stamp : Unsigned_64;
      Request_Label : Unsigned_32; Length, Flags : Unsigned_8;
      Reserved : Unsigned_16; Request : Words; Response : out Words;
      Deferred : out Ticket) is
      Session : constant Unsigned_64 := Session_Of (Sender, Stamp);
      Accepted : Boolean;
   begin
      Deferred := 0;
      Object.Outcome := Not_Create;
      Response := [Denied, Version, 0, 0];
      if Session = 0 then return; end if;
      Response (0) := Bad_Request;
      if Request_Label /= Label or Length /= 4 or Flags /= 0 or Reserved /= 0 or
        Request (0) /= Version or Request (1) > Close or Request (3) /= 0
      then return; end if;
      if Request (1) = Close then
         if Request (2) = 0 or Request (2) > Unsigned_64 (Handles.Handle'Last)
         then return; end if;
         Handles.Close (Object.Handles, Session, Handles.Handle (Request (2)), Accepted);
         Response (0) := (if Accepted then OK else Denied);
         return;
      end if;
      if Request (2) = 0 or Request (2) mod 4096 /= 0 or
        Request (2) > Unsigned_64 (Layout.Page_Count'Last) * 4096
      then return; end if;
      Response (0) := Unavailable;
      if Object.Failed then Object.Outcome := Quarantined; return;
      elsif Object.Pending /= 0 then Object.Outcome := Application_Pending; return;
      elsif Object.Private_Pending /= 0 then Object.Outcome := Private_Pending; return;
      elsif not Owner_Ready then Object.Outcome := Owner_Unavailable; return;
      end if;
      Object.Pending_Previous := 0;
      Object.Pending_Previous_Session := 0;
      if Object.Reusable_Head /= 0 then
         declare
            I : constant Natural := Object.Reusable_Head;
         begin
            if I > Committed_Slots (Object) then Quarantine (Object); return; end if;
            declare Item : constant Allocation_Record := Records.Get (Object.Items, I); begin
               if not Item.Reusable or else Item.Private_Reusable or else
                 Item.Identity = 0 or else Ticket_Slot (Item.Identity) /= I or else
                 Item.Identity > Ticket'Last - Ticket_Stride or else
                 Item.Charge_Bytes /= 0 or else Item.Next_Reusable = I or else
                 Item.Next_Reusable > Committed_Slots (Object)
               then Quarantine (Object); return; end if;
            end;
            if not Charge_Client (Object, Session, Request (2)) then
               Object.Outcome := Client_Quota_Unavailable; return;
            end if;
            Object.Reusable_Head := Records.Get (Object.Items, I).Next_Reusable;
            Object.Pending_Previous := Records.Get (Object.Items, I).Issued.Handle;
            Object.Pending_Previous_Session := Records.Get (Object.Items, I).Issued.Session;
            Object.Pending := Records.Get (Object.Items, I).Identity + Ticket_Stride;
            Records.Put (Object.Items, I,
           (Records.Get (Object.Items, I) with delta Identity => Object.Pending));
            Records.Put (Object.Items, I,
           (Records.Get (Object.Items, I) with delta Owner => Session));
            Records.Put (Object.Items, I,
           (Records.Get (Object.Items, I) with delta Reusable => False, Next_Reusable => 0));
            Records.Put (Object.Items, I,
           (Records.Get (Object.Items, I) with delta Issued => (others => <>)));
         end;
      end if;
      if Object.Pending = 0 then
         if Object.Attempted >= Committed_Slots (Object) then
            Object.Outcome := Slots_Exhausted;
            return;
         end if;
         if not Charge_Client (Object, Session, Request (2)) then
            Object.Outcome := Client_Quota_Unavailable; return;
         end if;
         Object.Attempted := Object.Attempted + 1;
         Object.Pending := Ticket (Object.Attempted);
         Records.Put (Object.Items, Object.Attempted,
           (Records.Get (Object.Items, Object.Attempted) with delta Identity => Object.Pending));
         Records.Put (Object.Items, Object.Attempted,
           (Records.Get (Object.Items, Object.Attempted) with delta Owner => Session));
      end if;
      Object.Cancelled := False;
      Object.Pending_Session := Session;
      Object.Pending_Sender := Sender;
      Object.Pending_Stamp := Stamp;
      Object.Pending_Bytes := Request (2);
      Records.Put (Object.Items, Ticket_Slot (Object.Pending),
        (Records.Get (Object.Items, Ticket_Slot (Object.Pending)) with delta
         Charge_Bytes => Request (2)));
      Deferred := Object.Pending;
      Object.Outcome := Awaiting_Backing;
   end Handle;
   procedure Complete
     (Object : in out Service; ID : Ticket;
      Backing : Intel_GPU_Buffer_Reply.Backing;
      Response : out Words; Consumed : out Boolean) is
      Handle_ID : Handles.Handle;
   begin
      Response := [Unavailable, Version, 0, 0];
      Consumed := ID /= 0 and then ID = Object.Pending;
      if not Consumed then return; end if;
      Object.Pending := 0;
      -- Each generation is one-shot. On an uncertain result the slot/backing
      -- remains retained, never retried or exposed to another client.
      if Object.Failed or else not Owner_Ready then
         Object.Outcome := (if Object.Failed then Quarantined else Owner_Unavailable);
         Quarantine (Object);
         return;
      end if;
      if Object.Cancelled then
         -- Begin_Retire_Session already closed admission and captured the
         -- name sweep. Completion must not drain that sweep synchronously
         -- or bypass its cursor. No handle is published for this ticket.
         Response (0) := Denied;
         return;
      end if;
      if Session_Of (Object.Pending_Sender, Object.Pending_Stamp) /= Object.Pending_Session then
         Handles.Close_Session (Object.Handles, Object.Pending_Session);
         Response (0) := Denied;
         return;
      end if;
      if not Backing.Ready then Object.Outcome := Backing_Unavailable; return; end if;
      if Backing.Bytes /= Object.Pending_Bytes then
         Object.Outcome := Backing_Size_Mismatch;
         Quarantine (Object);
         return;
      end if;
      if Object.Pending_Previous = 0 then
         Handles.Register (Object.Handles, Object.Pending_Session, Backing, Handle_ID);
      else
         Handles.Replace_Retired (Object.Handles, Object.Pending_Previous_Session, Object.Pending_Session,
           Object.Pending_Previous, Backing, True, Handle_ID);
      end if;
      if Handle_ID = Handles.No_Handle then
         Object.Outcome := Handle_Unavailable;
         Quarantine (Object);
         return;
      end if;
      Response := [OK, Version, Unsigned_64 (Handle_ID), Backing.Bytes];
      Object.Outcome := Allocation_Ready;
      Records.Put (Object.Items, Ticket_Slot (ID),
           (Records.Get (Object.Items, Ticket_Slot (ID)) with delta Issued => (Object.Pending_Session, Handle_ID)));
   end Complete;
   procedure Reject_Delivery (Object : in out Service; ID : Ticket) is
      Accepted : Boolean;
   begin
      if ID = 0 or else Ticket_Slot (ID) > Committed_Slots (Object) or else
        Records.Get (Object.Items, Ticket_Slot (ID)).Identity /= ID or else
        Records.Get (Object.Items, Ticket_Slot (ID)).Reusable then return; end if;
      Handles.Close (Object.Handles, Records.Get (Object.Items, Ticket_Slot (ID)).Issued.Session,
                     Records.Get (Object.Items, Ticket_Slot (ID)).Issued.Handle, Accepted);
      Records.Put (Object.Items, Ticket_Slot (ID),
           (Records.Get (Object.Items, Ticket_Slot (ID)) with delta Issued => (others => <>)));
   end Reject_Delivery;
   function Can_Retire
     (Object : Service; Session : Unsigned_64; ID : Ticket) return Boolean is
      Index : constant Layout.Slot := Ticket_Slot (ID);
   begin
      if Object.Failed or else not Owner_Ready or else
        Index > Committed_Slots (Object) or else
        Session = 0 or else ID = 0 or else ID > Ticket'Last - Ticket_Stride or else
        Object.Pending /= 0 or else Object.Private_Pending /= 0 or else
        Records.Get (Object.Items, Index).Identity /= ID or else Records.Get (Object.Items, Index).Owner /= Session or else
        Records.Get (Object.Items, Index).Reusable or else Records.Get (Object.Items, Index).Issued.Session /= Session or else
        Records.Get (Object.Items, Index).Issued.Handle = 0 or else
        not Handles.Can_Issue (Object.Handles) or else
        not Handles.Can_Release_Backing (Object.Handles, Session, Records.Get (Object.Items, Index).Issued.Handle)
      then return False; end if;
      return True;
   end Can_Retire;
   procedure Acknowledge_Retirement
     (Object : in out Service; Session : Unsigned_64; ID : Ticket;
      References_Retired : Boolean; Accepted : out Boolean) is
      Index : constant Layout.Slot := Ticket_Slot (ID);
   begin
      Accepted := False;
      if not References_Retired or else not Can_Retire (Object, Session, ID)
      then return; end if;
      Handles.Release_Retired_Backing
        (Object.Handles, Session, Records.Get (Object.Items, Index).Issued.Handle, True, Accepted);
      if Accepted and then not Refund_Client (Object, Index) then Accepted := False; end if;
      if Accepted then Records.Put (Object.Items, Index,
           (Records.Get (Object.Items, Index) with delta Reusable => True,
            Charge_Bytes => 0, Next_Reusable => Object.Reusable_Head));
         Object.Reusable_Head := Index;
      end if;
   end Acknowledge_Retirement;
   procedure Begin_Retire_Session
     (Object : in out Service; Session : Unsigned_64;
      State : in out Session_Retirement; Accepted : out Boolean) is
   begin
      Accepted := False;
      if State.Phase in Closing_Names | Closing_Records then return; end if;
      if Object.Client_Limit /= 0 and then Session /= 0 and then
        not Client_Usage (Object, Session).Known then
         declare Accepted : Boolean; begin
            Intel_GPU_Client_Budgets.Open
              (Object.Client_Accounts, Session, Object.Client_Limit, Accepted);
            if not Accepted then Quarantine (Object); end if;
         end;
      end if;
      Intel_GPU_Client_Budgets.Close (Object.Client_Accounts, Session);
      if Object.Pending /= 0 and then Object.Pending_Session = Session then
         Object.Cancelled := True;
      end if;
      State.Origin := Object'Address;
      State.Session := Session;
      State.Handle_Last := Handles.Count (Object.Handles);
      State.Record_Last := Object.Attempted;
      State.Cursor := 0;
      State.Phase := Closing_Names;
      Accepted := True;
   end Begin_Retire_Session;
   procedure Retire_Session_Step
     (Object : in out Service; State : in out Session_Retirement;
      Complete : out Boolean) is
      use type System.Address;
      Finish : Natural;
      Names_Done : Boolean;
   begin
      Complete := False;
      if State.Phase = Idle or else State.Origin /= Object'Address then return; end if;
      if State.Phase = Finished then Complete := True; return; end if;
      if State.Phase = Closing_Names then
         Handles.Close_Session_Step
           (Object.Handles, State.Session, State.Handle_Last, State.Cursor, Names_Done);
         if Names_Done then State.Cursor := 0; State.Phase := Closing_Records; end if;
         return;
      end if;
      Finish := State.Cursor + Natural'Min (32, State.Record_Last - State.Cursor);
      while State.Cursor < Finish loop
         declare I : constant Positive := State.Cursor + 1; begin
         if Records.Get (Object.Items, I).Owner = State.Session then
            if Records.Get (Object.Items, I).Context_Parent then
               Records.Put (Object.Items, I,
                 (Records.Get (Object.Items, I) with delta Context_Closed => True));
            end if;
            -- Already retired application backing stays reusable. Live or
            -- uncertain allocations never acquired this flag. Old handles
            -- remain closed and replacement authenticates its new session.
            -- Confirmed private retirement has already discarded the old VM
            -- image and received the exact supervisor acknowledgement. Keep
            -- that reusable slot, but never promote an uncertain allocation.
            if not Records.Get (Object.Items, I).Private_Reusable then
               Records.Put (Object.Items, I,
                 (Records.Get (Object.Items, I) with delta
                  Private_Closed => Records.Get (Object.Items, I).Private_Closed or else
                    Records.Get (Object.Items, I).Private_Reclaimable,
                  Private_Reclaimable => False));
            end if;
         end if;
         State.Cursor := I;
         end;
      end loop;
      if State.Cursor = State.Record_Last then State.Phase := Finished; Complete := True; end if;
   end Retire_Session_Step;
   procedure Retire_Session (Object : in out Service; Session : Unsigned_64) is
      State : Session_Retirement;
      Accepted, Complete : Boolean;
   begin
      Begin_Retire_Session (Object, Session, State, Accepted);
      if not Accepted then return; end if;
      loop
         Retire_Session_Step (Object, State, Complete);
         exit when Complete;
      end loop;
   end Retire_Session;
   procedure Quarantine (Object : in out Service) is
   begin
      Object.Failed := True;
      Intel_GPU_Client_Budgets.Quarantine (Object.Client_Accounts);
      Handles.Quarantine (Object.Handles);
   end Quarantine;
end Intel_GPU_Buffer_Requests;
