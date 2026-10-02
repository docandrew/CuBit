package body Intel_GPU_Buffer_Requests is
   package Layout renames Intel_GPU_Buffer_Backing;
   package Handles renames Intel_GPU_Buffer_Handles;
   function Last_Allocation (Object : Service) return Allocation_Outcome is
     (Object.Outcome);
   function Ticket_Session (Object : Service; ID : Ticket) return Unsigned_64 is
     (if ID = 0 or else Object.Identities (Ticket_Slot (ID)) /= ID then 0
      else Object.Owners (Ticket_Slot (ID)));
   function Pending_For (Object : Service; Session : Unsigned_64) return Boolean is
     (Session /= 0 and then
       ((Object.Pending /= 0 and then Ticket_Session (Object, Object.Pending) = Session)
        or else (Object.Private_Pending /= 0 and then
          Ticket_Session (Object, Object.Private_Pending) = Session)));
   function Closed_At (Object : Service; Index : Layout.Slot) return Closed_Allocation is
      ID : constant Ticket := Object.Identities (Index);
      Issued : Issued_Result renames Object.Issued (Index);
   begin
      if Object.Failed or else not Owner_Ready or else
        Object.Pending /= 0 or else Object.Private_Pending /= 0 or else
        Object.Reusable (Index) or else ID = 0 or else Ticket_Slot (ID) /= Index or else
        Issued.Session = 0 or else Issued.Session /= Object.Owners (Index) or else
        Issued.Handle = 0 or else
        not Handles.Closed_Backing (Object.Handles, Issued.Session, Issued.Handle).Ready
      then return (Ready => False); end if;
      return (True, ID, Issued.Session, Issued.Handle,
        Ticket_Generation (ID));
   end Closed_At;
   procedure Reserve_Private
     (Object : in out Service; Session : Unsigned_64; ID : out Ticket;
      Reclaimable : Boolean := False) is
   begin
      ID := 0;
      if Object.Failed or else Object.Pending /= 0 or else
        Object.Private_Pending /= 0 or else not Owner_Ready or else
        (Reclaimable and then Session = 0) then return; end if;
      if Reclaimable then
         for Index in Layout.Slot loop
            if Object.Private_Reusable (Index) then
               Object.Private_Pending := Object.Identities (Index) + Ticket_Stride;
               Object.Identities (Index) := Object.Private_Pending;
               Object.Owners (Index) := Session;
               Object.Private_Reusable (Index) := False;
               ID := Object.Private_Pending;
               return;
            end if;
         end loop;
      end if;
      if Object.Attempted = Layout.Slot'Last then return; end if;
      Object.Attempted := Object.Attempted + 1;
      Object.Private_Pending := Ticket (Object.Attempted);
      Object.Identities (Object.Attempted) := Object.Private_Pending;
      Object.Owners (Object.Attempted) := Session;
      Object.Private_Reclaimable (Object.Attempted) := Reclaimable;
      ID := Object.Private_Pending;
   end Reserve_Private;
   procedure Acknowledge_Private_Retirement
     (Object : in out Service; Session : Unsigned_64; ID : Ticket;
      References_Retired : Boolean; Accepted : out Boolean) is
      Index : constant Layout.Slot := Ticket_Slot (ID);
   begin
      Accepted := False;
      if Object.Failed or else not References_Retired or else not Owner_Ready or else
        Session = 0 or else ID = 0 or else ID > Ticket'Last - Ticket_Stride or else
        Object.Pending /= 0 or else Object.Private_Pending /= 0 or else
        Object.Identities (Index) /= ID or else Object.Owners (Index) /= Session or else
        not Object.Private_Reclaimable (Index) or else Object.Private_Reusable (Index) or else
        Object.Issued (Index).Handle /= 0 or else Object.Reusable (Index)
      then return; end if;
      Object.Private_Reusable (Index) := True;
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
      for I in Layout.Slot loop
         if Object.Reusable (I) then
            Object.Pending_Previous := Object.Issued (I).Handle;
            Object.Pending_Previous_Session := Object.Issued (I).Session;
            Object.Pending := Object.Identities (I) + Ticket_Stride;
            Object.Identities (I) := Object.Pending;
            Object.Owners (I) := Session;
            Object.Reusable (I) := False;
            Object.Issued (I) := (others => <>);
            exit;
         end if;
      end loop;
      if Object.Pending = 0 then
         if Object.Attempted = Layout.Slot'Last then
            Object.Outcome := Slots_Exhausted;
            return;
         end if;
         Object.Attempted := Object.Attempted + 1;
         Object.Pending := Ticket (Object.Attempted);
         Object.Identities (Object.Attempted) := Object.Pending;
         Object.Owners (Object.Attempted) := Session;
      end if;
      Object.Cancelled := False;
      Object.Pending_Session := Session;
      Object.Pending_Sender := Sender;
      Object.Pending_Stamp := Stamp;
      Object.Pending_Bytes := Request (2);
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
      if Object.Cancelled or else
        Session_Of (Object.Pending_Sender, Object.Pending_Stamp) /= Object.Pending_Session then
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
      Object.Issued (Ticket_Slot (ID)) := (Object.Pending_Session, Handle_ID);
   end Complete;
   procedure Reject_Delivery (Object : in out Service; ID : Ticket) is
      Accepted : Boolean;
   begin
      if ID = 0 or else Object.Identities (Ticket_Slot (ID)) /= ID or else
        Object.Reusable (Ticket_Slot (ID)) then return; end if;
      Handles.Close (Object.Handles, Object.Issued (Ticket_Slot (ID)).Session,
                     Object.Issued (Ticket_Slot (ID)).Handle, Accepted);
      Object.Issued (Ticket_Slot (ID)) := (others => <>);
   end Reject_Delivery;
   procedure Acknowledge_Retirement
     (Object : in out Service; Session : Unsigned_64; ID : Ticket;
      References_Retired : Boolean; Accepted : out Boolean) is
      Index : constant Layout.Slot := Ticket_Slot (ID);
   begin
      Accepted := False;
      if Object.Failed or else not References_Retired or else not Owner_Ready or else
        Session = 0 or else ID = 0 or else ID > Ticket'Last - Ticket_Stride or else
        Object.Pending /= 0 or else Object.Private_Pending /= 0 or else
        Object.Identities (Index) /= ID or else Object.Owners (Index) /= Session or else
        Object.Reusable (Index) or else Object.Issued (Index).Session /= Session or else
        Object.Issued (Index).Handle = 0 or else
        not Handles.Can_Issue (Object.Handles) or else
        not Handles.Closed_Backing (Object.Handles, Session, Object.Issued (Index).Handle).Ready
      then return; end if;
      Handles.Release_Retired_Backing
        (Object.Handles, Session, Object.Issued (Index).Handle, True, Accepted);
      if Accepted then Object.Reusable (Index) := True; end if;
   end Acknowledge_Retirement;
   procedure Retire_Session (Object : in out Service; Session : Unsigned_64) is
   begin
      Handles.Close_Session (Object.Handles, Session);
      for I in Layout.Slot loop
         if Object.Owners (I) = Session then
            -- Already retired application backing stays reusable. Live or
            -- uncertain allocations never acquired this flag. Old handles
            -- remain closed and replacement authenticates its new session.
            -- Confirmed private retirement has already discarded the old VM
            -- image and received the exact supervisor acknowledgement. Keep
            -- that reusable slot, but never promote an uncertain allocation.
            if not Object.Private_Reusable (I) then
               Object.Private_Reclaimable (I) := False;
            end if;
         end if;
      end loop;
      if Object.Pending /= 0 and then Object.Pending_Session = Session then
         Object.Cancelled := True;
      end if;
   end Retire_Session;
   procedure Quarantine (Object : in out Service) is
   begin
      Object.Failed := True;
      Handles.Quarantine (Object.Handles);
   end Quarantine;
end Intel_GPU_Buffer_Requests;
