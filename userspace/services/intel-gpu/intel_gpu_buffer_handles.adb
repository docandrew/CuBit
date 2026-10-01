package body Intel_GPU_Buffer_Handles with SPARK_Mode is
   package Replies renames Intel_GPU_Buffer_Reply;
   function Count (Object : Registry) return Natural is (Object.Used);
   function Is_Open (Object : Registry; Session : Session_ID; ID : Handle) return Boolean is
     (not Object.Failed and then Session /= 0 and then ID in 1 .. Handle (Object.Used)
      and then Object.Entries (Slot (ID)).Open
      and then Object.Entries (Slot (ID)).Session = Session
      and then Object.Entries (Slot (ID)).Backing.Ready);
   function Resolve (Object : Registry; Session : Session_ID; ID : Handle)
     return Replies.Backing is
     (if Is_Open (Object, Session, ID) then Object.Entries (Slot (ID)).Backing
      else (Ready => False));
   procedure Register
     (Object : in out Registry; Session : Session_ID;
      Backing : Replies.Backing; ID : out Handle) is
   begin
      ID := No_Handle;
      if Object.Failed or else Session = 0 or else Object.Used = Capacity or else
        not Replies.Valid (Backing)
      then return; end if;
      for I in 1 .. Object.Used loop
         declare Previous : Replies.Backing renames Object.Entries (I).Backing; begin
            -- Closed objects continue reserving their backing range.
            if Previous.Ready and then
              (not Replies.Same_Arena (Previous, Backing) or else
               (Previous.CPU_Address < Backing.CPU_Address + Backing.Bytes and then
                Backing.CPU_Address < Previous.CPU_Address + Previous.Bytes))
            then return; end if;
         end;
      end loop;
      Object.Used := Object.Used + 1;
      Object.Entries (Object.Used) := (Session, True, Backing);
      ID := Handle (Object.Used);
   end Register;
   procedure Close
     (Object : in out Registry; Session : Session_ID; ID : Handle;
      Accepted : out Boolean) is
   begin
      Accepted := Is_Open (Object, Session, ID);
      if Accepted then Object.Entries (Slot (ID)).Open := False; end if;
   end Close;
   procedure Close_Session (Object : in out Registry; Session : Session_ID) is
   begin
      for I in 1 .. Object.Used loop
         if Object.Entries (I).Session = Session then Object.Entries (I).Open := False; end if;
         pragma Loop_Invariant
           (for all J in 1 .. I => Object.Entries (J).Session /= Session or else
                                  not Object.Entries (J).Open);
      end loop;
   end Close_Session;
   procedure Quarantine (Object : in out Registry) is
   begin Object.Failed := True; end Quarantine;
end Intel_GPU_Buffer_Handles;
