package body Intel_GPU_Table_Allocations is
   function Capacity (Object : Registry) return Positive is
     (Entries.Capacity (Object.Items));
   function Revision (Object : Registry) return Unsigned_64 is (Object.Epoch);
   function Retained_At (Object : Registry; Slot : Positive) return Retained_Allocation is
      Item : Entry_Record;
   begin
      if Slot > Capacity (Object) then return (Present => False); end if;
      Item := Entries.Get (Object.Items, Slot);
      if Item.Ticket = 0 then return (Present => False); end if;
      return (True, Item.Session, Item.Ticket, Item.Role, Item.Revoked);
   end Retained_At;
   procedure Scan_Session
     (Object : Registry; Session, Expected_Revision : Unsigned_64; First : Positive;
      Found : out Boolean; Next : out Natural; Accepted : out Boolean) is
      Last : Positive;
   begin
      Found := False; Next := 0; Accepted := False;
      if Session = 0 or else Expected_Revision /= Object.Epoch or else
        First > Capacity (Object) then return; end if;
      Last := First + Natural'Min (63, Capacity (Object) - First);
      Accepted := True;
      for I in First .. Last loop
         if Entries.Get (Object.Items, I).Session = Session then
            Found := True; return;
         end if;
      end loop;
      if Last < Capacity (Object) then Next := Last + 1; end if;
   end Scan_Session;
   procedure Extend
     (Object : in out Registry; Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      Entries.Extend (Object.Items, Base, Bytes, Accepted);
   end Extend;
   function Matches (Item : Entry_Record; Session, Ticket : Unsigned_64) return Boolean is
     (Session /= 0 and Ticket /= 0 and Item.Session = Session and Item.Ticket = Ticket);
   procedure Check_Retained_Range
     (Object : Registry; Slot : Positive;
      Session, Ticket, Expected_Revision, First, Bytes : Unsigned_64;
      Overlaps, Accepted : out Boolean) is
      Item : Entry_Record;
   begin
      Overlaps := True; Accepted := False;
      if Expected_Revision /= Object.Epoch or else Slot > Capacity (Object) or else
        Bytes = 0 or else Bytes - 1 > Unsigned_64'Last - First
      then return; end if;
      Item := Entries.Get (Object.Items, Slot);
      if not Matches (Item, Session, Ticket) or else
        not Intel_GPU_Buffer_Reply.Valid (Item.Backing)
      then return; end if;
      Overlaps := Intel_GPU_Buffer_Reply.Overlaps_DMA (Item.Backing, First, Bytes);
      Accepted := True;
   end Check_Retained_Range;
   procedure Install
     (Object : in out Registry; Slot : Positive; Session, Ticket : Unsigned_64;
      Role : Allocation_Role; Backing : Intel_GPU_Buffer_Reply.Backing;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Epoch = Unsigned_64'Last or else
        Slot > Capacity (Object) or else Session = 0 or else Ticket = 0 or else
        Entries.Get (Object.Items, Slot).Ticket /= 0 or else
        not Intel_GPU_Buffer_Reply.Valid (Backing) or else
        not Admitted (Session, Ticket, Slot)
      then return; end if;
      Entries.Put (Object.Items, Slot, (Session, Ticket, Role, False, Backing));
      Object.Epoch := Object.Epoch + 1;
      Accepted := True;
   end Install;
   procedure Lookup
     (Object : Registry; Slot : Positive; Session, Ticket : Unsigned_64;
      Role : Allocation_Role;
      Backing : out Intel_GPU_Buffer_Reply.Backing; Accepted : out Boolean) is
      Item : Entry_Record;
   begin
      Backing := (Ready => False); Accepted := False;
      if Slot > Capacity (Object) then return; end if;
      Item := Entries.Get (Object.Items, Slot);
      if not Matches (Item, Session, Ticket) or else Item.Role /= Role or else Item.Revoked or else
        not Admitted (Session, Ticket, Slot) or else
        not Intel_GPU_Buffer_Reply.Valid (Item.Backing)
      then return; end if;
      Backing := Item.Backing; Accepted := True;
   end Lookup;
   procedure Revoke
     (Object : in out Registry; Slot : Positive; Session, Ticket : Unsigned_64;
      Accepted : out Boolean) is
      Item : Entry_Record;
   begin
      Accepted := False;
      if Slot > Capacity (Object) then return; end if;
      Item := Entries.Get (Object.Items, Slot);
      if not Matches (Item, Session, Ticket) then return; end if;
      if Item.Revoked then Accepted := True; return; end if;
      if Object.Epoch = Unsigned_64'Last then return; end if;
      Item.Revoked := True;
      Entries.Put (Object.Items, Slot, Item); Object.Epoch := Object.Epoch + 1; Accepted := True;
   end Revoke;
   procedure Retire
     (Object : in out Registry; Slot : Positive; Session, Ticket : Unsigned_64;
      Accepted : out Boolean) is
      Item : Entry_Record;
   begin
      Accepted := False;
      if Object.Epoch = Unsigned_64'Last or else Slot > Capacity (Object) then return; end if;
      Item := Entries.Get (Object.Items, Slot);
      if not Matches (Item, Session, Ticket) or else
        not Retirement_Confirmed (Session, Ticket)
      then return; end if;
      Entries.Put (Object.Items, Slot, (others => <>));
      Object.Epoch := Object.Epoch + 1; Accepted := True;
   end Retire;
end Intel_GPU_Table_Allocations;
