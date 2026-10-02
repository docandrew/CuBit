package body Intel_GPU_Buffer_Requests.Sharing is
   package Views renames Intel_GPU_Buffer_Views;
   use type Views.View_State;
   function Find (Table : Mapping_Table; ID : Mapping_ID) return Natural is
   begin
      if ID /= 0 then
         for Index in 1 .. Table.Used loop
            if Table.Items (Index).ID = ID then return Index; end if;
         end loop;
      end if;
      return 0;
   end Find;
   function Presentation_Held (Table : Mapping_Table; Session : Unsigned_64)
      return Boolean is
   begin
      if Session = 0 or else Table.Failed or else not Owner_Ready then return True; end if;
      for Index in 1 .. Table.Used loop
         if Table.Items (Index).Session = Session and then
           Table.Items (Index).Presentation and then
           Views.State (Table.Items (Index).View) not in Views.Empty | Views.Retired
         then return True; end if;
      end loop;
      return False;
   end Presentation_Held;
   procedure Handle
     (Object : Service; Table : in out Mapping_Table;
      Sender, Stamp : Unsigned_64; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Response : out Words; Created : out Mapping_ID) is
      Operation : constant Unsigned_64 := Shift_Right (Request (0), 32);
      Reference : Unsigned_64;
      Accepted : Boolean;
      State : Views.View_State;
   begin
      Created := 0;
      Response := [Denied, Version, 0, 0];
      if Session_Of (Sender, Stamp) = 0 then return; end if;
      Response (0) := Bad_Request;
      if Request_Label /= Map_Label or Length /= 4 or Flags /= 0 or Reserved /= 0
        or (Request (0) and 16#FFFF_FFFF#) /= Version or Operation > Map_Presentation
      then return; end if;
      if Operation = Retire_Map then
         if Request (1) = 0 or Request (1) > Unsigned_64 (Mapping_ID'Last) or
           Request (2) /= 0 or Request (3) /= 0 then return; end if;
         Retire (Table, Sender, Stamp, Mapping_ID (Request (1)), Accepted, State);
         Response (0) := (if not Accepted then Denied else
           (case State is when Views.Retired => OK,
            when Views.Retiring => Pending_Retirement, when others => Unavailable));
         return;
      end if;
      if Request (1) = 0 or Request (1) > Unsigned_64 (Intel_GPU_Buffer_Handles.Handle'Last)
        or Request (2) mod 4096 /= 0 or Request (3) = 0 or Request (3) mod 4096 /= 0
        or Request (3) > Unsigned_64 (Intel_GPU_Buffer_Backing.Page_Count'Last) * 4096
      then return; end if;
      Response (0) := Unavailable;
      Map (Object, Table, Sender, Stamp, Request (1), Request (2), Request (3),
           Operation = Map_Write, Created, Reference, Operation = Map_Presentation);
      if Created /= 0 then
         Response := [OK, Version, Unsigned_64 (Created), Reference];
      end if;
   end Handle;
   procedure Map
     (Object : Service; Table : in out Mapping_Table;
      Sender, Stamp, ID, Offset, Bytes : Unsigned_64; Writable : Boolean;
      Mapping : out Mapping_ID; Reference : out Unsigned_64;
      Presentation : Boolean := False) is
      Session : constant Unsigned_64 := Session_Of (Sender, Stamp);
      Accepted : Boolean;
      Index : Natural := 0;
   begin
      Mapping := 0;
      Reference := 0;
      if (Presentation and Writable) or else Session = 0 or else Object.Failed or else not Owner_Ready or else
        Table.Failed or else Table.Last_ID = Mapping_ID'Last then return; end if;
      -- Whole-BO writer/presentation exclusion. Existing writable aliases must
      -- be confirmed retired before exporting; presentation descendants must
      -- be confirmed retired before granting a new writer. Read aliases may
      -- coexist. Failed/uncertain grants never silently remove exclusion.
      for Candidate in 1 .. Table.Used loop
         if Table.Items (Candidate).Session = Session and then
           Table.Items (Candidate).Buffer_ID = ID and then
           Views.State (Table.Items (Candidate).View) not in Views.Empty | Views.Retired and then
           ((Presentation and Table.Items (Candidate).Writable) or else
            (Writable and Table.Items (Candidate).Presentation))
         then return; end if;
      end loop;
      if Table.Used < Capacity then
         Table.Used := Table.Used + 1;
         Index := Table.Used;
      else
         for Candidate in 1 .. Table.Used loop
            if Views.State (Table.Items (Candidate).View) = Views.Retired then
               Views.Recycle (Table.Items (Candidate).View, Accepted);
               if Accepted then Index := Candidate; exit; end if;
            end if;
         end loop;
      end if;
      if Index = 0 then return; end if;
      Table.Last_ID := Table.Last_ID + 1;
      Table.Items (Index).ID := Table.Last_ID;
      Table.Items (Index).Session := Session;
      Table.Items (Index).Buffer_ID := ID;
      Table.Items (Index).Writable := Writable;
      Table.Items (Index).Presentation := Presentation;
      Share (Object, Sender, Stamp, ID, Offset, Bytes, Writable,
             Table.Items (Index).View, Accepted, Presentation);
      if Accepted then
         Mapping := Table.Last_ID;
         Reference := Views.Wire_Reference (Table.Items (Index).View);
      end if;
   end Map;
   procedure Retire
     (Table : in out Mapping_Table; Sender, Stamp : Unsigned_64;
      Mapping : Mapping_ID; Accepted : out Boolean;
      State : out Views.View_State) is
      Session : constant Unsigned_64 := Session_Of (Sender, Stamp);
      Index : constant Natural := Find (Table, Mapping);
   begin
      Accepted := False;
      State := Views.Failed;
      if Session = 0 or else Index = 0 or else
        Table.Items (Index).Session /= Session then return; end if;
      Accepted := True;
      Views.Retire (Table.Items (Index).View);
      State := Views.State (Table.Items (Index).View);
   end Retire;
   procedure Reject_Delivery (Table : in out Mapping_Table; Mapping : Mapping_ID) is
      Index : constant Natural := Find (Table, Mapping);
   begin
      if Index /= 0 then
         Views.Retire (Table.Items (Index).View);
      end if;
   end Reject_Delivery;
   procedure Retire_Session (Table : in out Mapping_Table; Session : Unsigned_64) is
   begin
      if Session = 0 then return; end if;
      for Index in 1 .. Table.Used loop
         if Table.Items (Index).Session = Session then
            Views.Retire (Table.Items (Index).View);
         end if;
      end loop;
   end Retire_Session;
   function Observe_Retirement (Table : Mapping_Table; Session : Unsigned_64)
      return Retirement_State is
      Result : Retirement_State := Clear;
   begin
      if Session = 0 or else Table.Failed or else not Owner_Ready then
         return Uncertain;
      end if;
      for Index in 1 .. Table.Used loop
         if Table.Items (Index).Session = Session then
            case Views.State (Table.Items (Index).View) is
               when Views.Empty | Views.Retired => null;
               when Views.Shared | Views.Retiring => Result := Outstanding;
               when Views.Failed => return Uncertain;
            end case;
         end if;
      end loop;
      return Result;
   end Observe_Retirement;
   function Observe_Buffer_Retirement
     (Table : Mapping_Table; Session, Buffer_ID : Unsigned_64)
      return Retirement_State is
      Result : Retirement_State := Clear;
   begin
      if Session = 0 or else Buffer_ID = 0 or else Table.Failed or else
        not Owner_Ready then return Uncertain; end if;
      for Index in 1 .. Table.Used loop
         if Table.Items (Index).Session = Session and then
           Table.Items (Index).Buffer_ID = Buffer_ID
         then
            case Views.State (Table.Items (Index).View) is
               when Views.Empty | Views.Retired => null;
               when Views.Shared | Views.Retiring => Result := Outstanding;
               when Views.Failed => return Uncertain;
            end case;
         end if;
      end loop;
      return Result;
   end Observe_Buffer_Retirement;
   procedure Quarantine (Table : in out Mapping_Table) is
   begin
      Table.Failed := True;
      for Index in 1 .. Table.Used loop
         Views.Retire (Table.Items (Index).View);
      end loop;
   end Quarantine;
   procedure Poll (Table : in out Mapping_Table) is
   begin
      for Index in 1 .. Table.Used loop
         Views.Poll_Retirement (Table.Items (Index).View);
      end loop;
   end Poll;
   procedure Share
     (Object : Service; Sender, Stamp, ID, Offset, Bytes : Unsigned_64;
      Writable : Boolean; View : in out Intel_GPU_Buffer_Views.View;
      Accepted : out Boolean; Presentation : Boolean := False) is
      Session : constant Unsigned_64 := Session_Of (Sender, Stamp);
      Slot : CuBit.Messages.CapabilitySlot;
      Identity : Unsigned_64;
      use type Intel_GPU_Buffer_Views.View_State;
   begin
      Accepted := False;
      if Session = 0 or else Object.Failed or else not Owner_Ready or else
        ID = 0 or else ID > Unsigned_64 (Intel_GPU_Buffer_Handles.Handle'Last)
        or else Intel_GPU_Buffer_Views.State (View) /= Intel_GPU_Buffer_Views.Empty
      then return; end if;
      Recipient_Of (Sender, Stamp, Slot, Identity);
      if Identity = 0 then return; end if;
      Intel_GPU_Buffer_Views.Share
        (View, Object.Handles, Session, Intel_GPU_Buffer_Handles.Handle (ID),
         Slot, Identity, Offset, Bytes, Writable, Presentation);
      Accepted := Intel_GPU_Buffer_Views.State (View) = Intel_GPU_Buffer_Views.Shared;
   end Share;
end Intel_GPU_Buffer_Requests.Sharing;
