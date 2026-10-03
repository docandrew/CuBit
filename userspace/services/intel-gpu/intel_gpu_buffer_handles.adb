with System.Storage_Elements; use System.Storage_Elements;
package body Intel_GPU_Buffer_Handles with SPARK_Mode is
   package Replies renames Intel_GPU_Buffer_Reply;
   Record_Bytes : constant Unsigned_64 := Item'Object_Size / 8;
   function Read_Item (Object : Registry; Index : Slot) return Item;
   procedure Write_Item (Object : in out Registry; Index : Slot; Value : Item);
   function Read_Item (Object : Registry; Index : Slot) return Item
     with SPARK_Mode => Off is
   begin
      if Index <= Initial_Capacity then return Object.Entries (Index); end if;
      if Index > Object.Available or else Object.Storage_Base = 0 then
         return (others => <>);
      end if;
      declare
         Value : Item with Import, Address => To_Address (Integer_Address
           (Object.Storage_Base + Unsigned_64 (Index - Initial_Capacity - 1) * Record_Bytes));
      begin return Value; end;
   end Read_Item;
   procedure Write_Item (Object : in out Registry; Index : Slot; Value : Item)
     with SPARK_Mode => Off is
   begin
      if Index <= Initial_Capacity then Object.Entries (Index) := Value; return; end if;
      if Index > Object.Available or else Object.Storage_Base = 0 then return; end if;
      declare
         Target : Item with Import, Address => To_Address (Integer_Address
           (Object.Storage_Base + Unsigned_64 (Index - Initial_Capacity - 1) * Record_Bytes));
      begin Target := Value; end;
   end Write_Item;
   function Record_Capacity (Object : Registry) return Natural is (Object.Available);
   procedure Extend_Storage
     (Object : in out Registry; Base, Bytes : Unsigned_64; Accepted : out Boolean)
     with SPARK_Mode => Off is
      Records : Unsigned_64;
   begin
      Accepted := False;
      if Object.Failed or else Record_Bytes = 0 or else Item'Object_Size mod 8 /= 0 or else
        Record_Bytes mod Unsigned_64 (Item'Alignment) /= 0 or else
        Base = 0 or else Base mod 4096 /= 0 or else Base mod Unsigned_64 (Item'Alignment) /= 0 or else
        Bytes = 0 or else Bytes mod 4096 /= 0 or else
        Base > Unsigned_64'Last - Bytes or else
        Bytes <= Object.Storage_Bytes or else Bytes - Object.Storage_Bytes > 65536 or else
        (Object.Storage_Base /= 0 and then Object.Storage_Base /= Base)
      then return; end if;
      Records := Bytes / Record_Bytes;
      if Records >= Unsigned_64 (Natural'Last - Initial_Capacity) then return; end if;
      for Index in Object.Available + 1 .. Initial_Capacity + Natural (Records) loop
         declare
            Target : Item with Import, Address => To_Address (Integer_Address
              (Base + Unsigned_64 (Index - Initial_Capacity - 1) * Record_Bytes));
         begin Target := (others => <>); end;
      end loop;
      Object.Storage_Base := Base;
      Object.Storage_Bytes := Bytes;
      Object.Available := Initial_Capacity + Natural (Records);
      Accepted := True;
   end Extend_Storage;
   function Count (Object : Registry) return Natural is (Object.Used);
   function Can_Issue (Object : Registry) return Boolean is
     (not Object.Failed and then Object.Last_Issued < Handle'Last);
   function Session_Closed (Object : Registry; Session : Session_ID) return Boolean is
     (for all I in 1 .. Object.Used => Read_Item (Object, I).Session /= Session or else
                           not Read_Item (Object, I).Open);
   -- Names are monotonic, never table offsets. This bounded bootstrap search
   -- must become indexed lookup with growable storage; no caller may derive
   -- an index from a handle. The fallback is not a match: callers check ID.
   function Slot_Of (Object : Registry; ID : Handle) return Slot is
   begin
      for I in 1 .. Object.Used loop
         if Read_Item (Object, I).ID = ID then return I; end if;
      end loop;
      return Slot'First;
   end Slot_Of;
   function Is_Open (Object : Registry; Session : Session_ID; ID : Handle) return Boolean is
     (not Object.Failed and then Session /= 0 and then ID /= No_Handle
      and then Read_Item (Object, Slot_Of (Object, ID)).ID = ID
      and then Read_Item (Object, Slot_Of (Object, ID)).Open
      and then Read_Item (Object, Slot_Of (Object, ID)).Session = Session
      and then Read_Item (Object, Slot_Of (Object, ID)).Backing.Ready);
   function Check_Close (Object : Registry; Session : Session_ID; ID : Handle)
     return Close_Check is
      Value : Item;
   begin
      if Object.Failed then return Registry_Quarantined; end if;
      if Session = 0 then return Session_Unavailable; end if;
      if ID = No_Handle then return Invalid_Handle; end if;
      Value := Read_Item (Object, Slot_Of (Object, ID));
      if Value.ID /= ID then return Unknown_Handle; end if;
      if Value.Session /= Session then return Foreign_Session; end if;
      if not Value.Open then return Already_Closed; end if;
      if not Value.Backing.Ready then return Backing_Unavailable; end if;
      return Close_Ready;
   end Check_Close;
   function Resolve (Object : Registry; Session : Session_ID; ID : Handle)
     return Replies.Backing is
     (if Is_Open (Object, Session, ID) then Read_Item (Object, Slot_Of (Object, ID)).Backing
      else (Ready => False));
   procedure Retain_Backing
     (Object : in out Registry; Session : Session_ID; ID : Handle;
      Reference : in out Retained_Reference; Accepted : out Boolean)
     with SPARK_Mode => Off is
      Index : constant Slot := Slot_Of (Object, ID);
      Value : Item;
   begin
      Accepted := False;
      if Reference.Active or else not Is_Open (Object, Session, ID) then return; end if;
      Value := Read_Item (Object, Index);
      if Value.Released or else Value.Retained = Natural'Last then return; end if;
      Value.Retained := Value.Retained + 1;
      Write_Item (Object, Index, Value);
      Reference.Origin := Object'Address;
      Reference.Index := Index;
      Reference.Session := Session;
      Reference.ID := ID;
      Reference.Active := True;
      Accepted := True;
   end Retain_Backing;
   function Referenced_Backing
     (Object : Registry; Reference : Retained_Reference) return Replies.Backing
     with SPARK_Mode => Off is
      use type System.Address;
      Value : Item;
   begin
      if Object.Failed or else not Reference.Active or else
        Reference.Origin /= Object'Address or else
        Reference.Index = 0 or else Reference.Index > Object.Used
      then return (Ready => False); end if;
      Value := Read_Item (Object, Reference.Index);
      if Value.ID /= Reference.ID or else Value.Session /= Reference.Session or else
        Value.Released or else Value.Retained = 0 then return (Ready => False); end if;
      return Value.Backing;
   end Referenced_Backing;
   procedure Retain_Referenced_Backing
     (Object : in out Registry; Source : Retained_Reference;
      Destination : in out Retained_Reference; Accepted : out Boolean)
     with SPARK_Mode => Off is
      Value : Item;
   begin
      Accepted := False;
      if Destination.Active or else not Referenced_Backing (Object, Source).Ready then
         return;
      end if;
      Value := Read_Item (Object, Source.Index);
      if Value.Retained = Natural'Last then return; end if;
      Value.Retained := Value.Retained + 1;
      Write_Item (Object, Source.Index, Value);
      Destination.Origin := Source.Origin;
      Destination.Index := Source.Index;
      Destination.Session := Source.Session;
      Destination.ID := Source.ID;
      Destination.Active := True;
      Accepted := True;
   end Retain_Referenced_Backing;
   procedure Return_Reference
     (Object : in out Registry; Reference : in out Retained_Reference;
      References_Retired : Boolean; Accepted : out Boolean)
     with SPARK_Mode => Off is
      Index : Slot;
      Value : Item;
   begin
      Accepted := False;
      if not References_Retired or else not Referenced_Backing (Object, Reference).Ready then return; end if;
      Index := Reference.Index;
      Value := Read_Item (Object, Index);
      Value.Retained := Value.Retained - 1;
      Write_Item (Object, Index, Value);
      Reference.Active := False;
      Reference.Origin := System.Null_Address;
      Reference.Index := 0;
      Reference.Session := 0;
      Reference.ID := No_Handle;
      Accepted := True;
   end Return_Reference;
   function Closed_Backing (Object : Registry; Session : Session_ID; ID : Handle)
     return Replies.Backing is
     (if not Object.Failed and then Session /= 0 and then
         ID /= No_Handle and then Read_Item (Object, Slot_Of (Object, ID)).ID = ID and then
         Read_Item (Object, Slot_Of (Object, ID)).Session = Session and then
         not Read_Item (Object, Slot_Of (Object, ID)).Released and then
         not Read_Item (Object, Slot_Of (Object, ID)).Open
      then Read_Item (Object, Slot_Of (Object, ID)).Backing else (Ready => False));
   procedure Register
     (Object : in out Registry; Session : Session_ID;
      Backing : Replies.Backing; ID : out Handle) is
   begin
      ID := No_Handle;
      if not Can_Issue (Object) or else Session = 0 or else Object.Used = Object.Available or else
        not Backing.Ready or else not Replies.Valid (Backing)
      then return; end if;
      for I in 1 .. Object.Used loop
         declare Previous : Replies.Backing := Read_Item (Object, I).Backing; begin
            -- Closed objects continue reserving their backing range.
            if not Read_Item (Object, I).Released and then Previous.Ready and then
              (not Replies.Same_Arena (Previous, Backing) or else
               (Previous.CPU_Address < Backing.CPU_Address + Backing.Bytes and then
                Backing.CPU_Address < Previous.CPU_Address + Previous.Bytes))
            then return; end if;
         end;
      end loop;
      Object.Used := Object.Used + 1;
      Object.Last_Issued := Object.Last_Issued + 1;
      ID := Object.Last_Issued;
      Write_Item (Object, Object.Used, (ID, Session, True, False, Backing, 0));
   end Register;
   procedure Close
     (Object : in out Registry; Session : Session_ID; ID : Handle;
      Accepted : out Boolean) is
   begin
      Accepted := Is_Open (Object, Session, ID);
      if Accepted then
         Write_Item (Object, Slot_Of (Object, ID),
           (Read_Item (Object, Slot_Of (Object, ID)) with delta Open => False));
      end if;
   end Close;
   procedure Close_Session (Object : in out Registry; Session : Session_ID) is
   begin
      for I in 1 .. Object.Used loop
         if Read_Item (Object, I).Session = Session then
            Write_Item (Object, I, (Read_Item (Object, I) with delta Open => False));
         end if;
         pragma Loop_Invariant
           (for all J in 1 .. I => Read_Item (Object, J).Session /= Session or else
                                  not Read_Item (Object, J).Open);
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
        Index > Object.Used or else Read_Item (Object, Index).ID /= Previous or else
        Read_Item (Object, Index).Session /= Previous_Session or else Read_Item (Object, Index).Open or else
        Read_Item (Object, Index).Retained /= 0 or else
        (Session /= Previous_Session and then not Read_Item (Object, Index).Released) or else
        not Read_Item (Object, Index).Backing.Ready or else not Backing.Ready or else
        not Replies.Valid (Backing) or else
        not Replies.Same_Arena (Read_Item (Object, Index).Backing, Backing)
      then return; end if;
      for I in 1 .. Object.Used loop
         declare Other : Replies.Backing := Read_Item (Object, I).Backing; begin
            if I /= Index and then not Read_Item (Object, I).Released and then Other.Ready and then
              (not Replies.Same_Arena (Other, Backing) or else
               (Other.CPU_Address < Backing.CPU_Address + Backing.Bytes and then
                Backing.CPU_Address < Other.CPU_Address + Other.Bytes))
            then return; end if;
         end;
      end loop;
      Object.Last_Issued := Object.Last_Issued + 1;
      ID := Object.Last_Issued;
      Write_Item (Object, Index, (ID, Session, True, False, Backing, 0));
   end Replace_Retired;
   function Can_Release_Backing
     (Object : Registry; Session : Session_ID; ID : Handle) return Boolean is
     (Closed_Backing (Object, Session, ID).Ready and then
      Read_Item (Object, Slot_Of (Object, ID)).Retained = 0);
   procedure Release_Retired_Backing
     (Object : in out Registry; Session : Session_ID; ID : Handle;
      References_Retired : Boolean; Accepted : out Boolean) is
   begin
      Accepted := References_Retired and then Can_Release_Backing (Object, Session, ID);
      if Accepted then
         Write_Item (Object, Slot_Of (Object, ID),
           (Read_Item (Object, Slot_Of (Object, ID)) with delta Released => True));
      end if;
   end Release_Retired_Backing;
   procedure Quarantine (Object : in out Registry) is
   begin Object.Failed := True; end Quarantine;
end Intel_GPU_Buffer_Handles;
