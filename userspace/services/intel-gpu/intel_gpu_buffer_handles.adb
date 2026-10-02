package body Intel_GPU_Buffer_Handles with SPARK_Mode is
   package Replies renames Intel_GPU_Buffer_Reply;
   function Count (Object : Registry) return Natural is (Object.Used);
   function Can_Issue (Object : Registry) return Boolean is
     (not Object.Failed and then Object.Last_Issued < Handle'Last);
   function Session_Closed (Object : Registry; Session : Session_ID) return Boolean is
     (for all I in Slot => Object.Entries (I).Session /= Session or else
                           not Object.Entries (I).Open);
   -- Names are monotonic, never table offsets. This bounded bootstrap search
   -- must become indexed lookup with growable storage; no caller may derive
   -- an index from a handle. The fallback is not a match: callers check ID.
   function Slot_Of (Object : Registry; ID : Handle) return Slot is
   begin
      for I in 1 .. Object.Used loop
         if Object.Entries (I).ID = ID then return I; end if;
      end loop;
      return Slot'First;
   end Slot_Of;
   function Is_Open (Object : Registry; Session : Session_ID; ID : Handle) return Boolean is
     (not Object.Failed and then Session /= 0 and then ID /= No_Handle
      and then Object.Entries (Slot_Of (Object, ID)).ID = ID
      and then Object.Entries (Slot_Of (Object, ID)).Open
      and then Object.Entries (Slot_Of (Object, ID)).Session = Session
      and then Object.Entries (Slot_Of (Object, ID)).Backing.Ready);
   function Resolve (Object : Registry; Session : Session_ID; ID : Handle)
     return Replies.Backing is
     (if Is_Open (Object, Session, ID) then Object.Entries (Slot_Of (Object, ID)).Backing
      else (Ready => False));
   function Closed_Backing (Object : Registry; Session : Session_ID; ID : Handle)
     return Replies.Backing is
     (if not Object.Failed and then Session /= 0 and then
         ID /= No_Handle and then Object.Entries (Slot_Of (Object, ID)).ID = ID and then
         Object.Entries (Slot_Of (Object, ID)).Session = Session and then
         not Object.Entries (Slot_Of (Object, ID)).Released and then
         not Object.Entries (Slot_Of (Object, ID)).Open
      then Object.Entries (Slot_Of (Object, ID)).Backing else (Ready => False));
   procedure Register
     (Object : in out Registry; Session : Session_ID;
      Backing : Replies.Backing; ID : out Handle) is
   begin
      ID := No_Handle;
      if not Can_Issue (Object) or else Session = 0 or else Object.Used = Capacity or else
        not Backing.Ready or else not Replies.Valid (Backing)
      then return; end if;
      for I in 1 .. Object.Used loop
         declare Previous : Replies.Backing renames Object.Entries (I).Backing; begin
            -- Closed objects continue reserving their backing range.
            if not Object.Entries (I).Released and then Previous.Ready and then
              (not Replies.Same_Arena (Previous, Backing) or else
               (Previous.CPU_Address < Backing.CPU_Address + Backing.Bytes and then
                Backing.CPU_Address < Previous.CPU_Address + Previous.Bytes))
            then return; end if;
         end;
      end loop;
      Object.Used := Object.Used + 1;
      Object.Last_Issued := Object.Last_Issued + 1;
      ID := Object.Last_Issued;
      Object.Entries (Object.Used) := (ID, Session, True, False, Backing);
   end Register;
   procedure Close
     (Object : in out Registry; Session : Session_ID; ID : Handle;
      Accepted : out Boolean) is
   begin
      Accepted := Is_Open (Object, Session, ID);
      if Accepted then Object.Entries (Slot_Of (Object, ID)).Open := False; end if;
   end Close;
   procedure Close_Session (Object : in out Registry; Session : Session_ID) is
   begin
      for I in Slot loop
         if Object.Entries (I).Session = Session then Object.Entries (I).Open := False; end if;
         pragma Loop_Invariant
           (for all J in 1 .. I => Object.Entries (J).Session /= Session or else
                                  not Object.Entries (J).Open);
      end loop;
   end Close_Session;
   procedure Replace_Retired
     (Object : in out Registry; Previous_Session, Session : Session_ID; Previous : Handle;
      Backing : Replies.Backing; References_Retired : Boolean; ID : out Handle) is
      Index : constant Slot := Slot_Of (Object, Previous);
   begin
      ID := No_Handle;
      if not Can_Issue (Object) or else not References_Retired or else Session = 0 or else Previous_Session = 0 or else
        Previous = No_Handle or else
        Index > Object.Used or else Object.Entries (Index).ID /= Previous or else
        Object.Entries (Index).Session /= Previous_Session or else Object.Entries (Index).Open or else
        (Session /= Previous_Session and then not Object.Entries (Index).Released) or else
        not Object.Entries (Index).Backing.Ready or else not Backing.Ready or else
        not Replies.Valid (Backing) or else
        not Replies.Same_Arena (Object.Entries (Index).Backing, Backing)
      then return; end if;
      for I in 1 .. Object.Used loop
         declare Other : Replies.Backing renames Object.Entries (I).Backing; begin
            if I /= Index and then not Object.Entries (I).Released and then Other.Ready and then
              (not Replies.Same_Arena (Other, Backing) or else
               (Other.CPU_Address < Backing.CPU_Address + Backing.Bytes and then
                Backing.CPU_Address < Other.CPU_Address + Other.Bytes))
            then return; end if;
         end;
      end loop;
      Object.Last_Issued := Object.Last_Issued + 1;
      ID := Object.Last_Issued;
      Object.Entries (Index) := (ID, Session, True, False, Backing);
   end Replace_Retired;
   procedure Release_Retired_Backing
     (Object : in out Registry; Session : Session_ID; ID : Handle;
      References_Retired : Boolean; Accepted : out Boolean) is
   begin
      Accepted := References_Retired and then Closed_Backing (Object, Session, ID).Ready;
      if Accepted then Object.Entries (Slot_Of (Object, ID)).Released := True; end if;
   end Release_Retired_Backing;
   procedure Quarantine (Object : in out Registry) is
   begin Object.Failed := True; end Quarantine;
end Intel_GPU_Buffer_Handles;
