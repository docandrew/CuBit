package body Intel_GPU_Buffer_Requests is
   package Layout renames Intel_GPU_Buffer_Backing;
   package Handles renames Intel_GPU_Buffer_Handles;
   procedure Reserve_Private (Object : in out Service; ID : out Ticket) is
   begin
      ID := 0;
      if Object.Failed or else Object.Pending /= 0 or else
        Object.Private_Pending /= 0 or else not Owner_Ready or else
        Object.Attempted = Layout.Slot'Last then return; end if;
      Object.Attempted := Object.Attempted + 1;
      Object.Private_Pending := Object.Attempted;
      ID := Object.Private_Pending;
   end Reserve_Private;
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
      if Object.Failed or else Object.Pending /= 0 or else
        Object.Private_Pending /= 0 or else not Owner_Ready or else
        Object.Attempted = Layout.Slot'Last then return; end if;
      Object.Attempted := Object.Attempted + 1;
      Object.Pending := Object.Attempted;
      Object.Cancelled := False;
      Object.Pending_Session := Session;
      Object.Pending_Sender := Sender;
      Object.Pending_Stamp := Stamp;
      Object.Pending_Bytes := Request (2);
      Deferred := Object.Pending;
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
      -- Allocation is one-shot. On an uncertain result the slot/backing
      -- remains retained, never retried or exposed to another client.
      if Object.Failed or else not Owner_Ready then
         Quarantine (Object);
         return;
      end if;
      if Object.Cancelled or else
        Session_Of (Object.Pending_Sender, Object.Pending_Stamp) /= Object.Pending_Session then
         Handles.Close_Session (Object.Handles, Object.Pending_Session);
         Response (0) := Denied;
         return;
      end if;
      if not Backing.Ready then return; end if;
      if Backing.Bytes /= Object.Pending_Bytes then
         Quarantine (Object);
         return;
      end if;
      Handles.Register (Object.Handles, Object.Pending_Session, Backing, Handle_ID);
      if Handle_ID = Handles.No_Handle then
         Quarantine (Object);
         return;
      end if;
      Response := [OK, Version, Unsigned_64 (Handle_ID), Backing.Bytes];
      Object.Issued (Layout.Slot (ID)) := (Object.Pending_Session, Handle_ID);
   end Complete;
   procedure Reject_Delivery (Object : in out Service; ID : Ticket) is
      Accepted : Boolean;
   begin
      if ID = 0 then return; end if;
      Handles.Close (Object.Handles, Object.Issued (Layout.Slot (ID)).Session,
                     Object.Issued (Layout.Slot (ID)).Handle, Accepted);
      Object.Issued (Layout.Slot (ID)) := (others => <>);
   end Reject_Delivery;
   procedure Retire_Session (Object : in out Service; Session : Unsigned_64) is
   begin
      Handles.Close_Session (Object.Handles, Session);
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
