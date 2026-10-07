with Intel_GPU_Client_Budget_Policy;

package body Intel_GPU_Client_Budgets is
   function Capacity (Object : Ledger) return Positive is (Entries.Capacity (Object.Items));
   function Last_Probes (Object : Ledger) return Natural is (Object.Probes);
   procedure Locate
     (Object : Ledger; Session : Unsigned_64; Index, Parent, Probes : out Natural;
      Right : out Boolean) is
      Cursor : Natural := Object.Root;
      Item : Entry_State;
   begin
      Index := 0; Parent := 0; Probes := 0; Right := False;
      if Session = 0 then return; end if;
      for Depth in 0 .. 64 loop
         if Cursor = 0 then return; end if;
         if Cursor > Object.Count then Parent := 0; return; end if;
         Probes := Probes + 1;
         Item := Entries.Get (Object.Items, Cursor);
         if Item.Session = Session then Index := Cursor; return; end if;
         if Depth = 64 then Parent := 0; return; end if;
         Parent := Cursor;
         Right := (Session and Shift_Left (Unsigned_64'(1), 63 - Depth)) /= 0;
         Cursor := (if Right then Item.Right else Item.Left);
      end loop;
   end Locate;
   procedure Extend
     (Object : in out Ledger; Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Failed then return; end if;
      Entries.Extend (Object.Items, Base, Bytes, Accepted);
   end Extend;
   procedure Open
     (Object : in out Ledger; Session, Limit : Unsigned_64; Accepted : out Boolean) is
      Index, Parent : Natural;
      Right : Boolean;
      Item : Entry_State;
   begin
      Accepted := False; Object.Probes := 0;
      if Object.Failed or Session = 0 or Limit = 0 or Limit mod 4096 /= 0 or
        Object.Count = Capacity (Object) then return; end if;
      Locate (Object, Session, Index, Parent, Object.Probes, Right);
      if Index /= 0 or else (Object.Root /= 0 and Parent = 0) then return; end if;
      Index := Object.Count + 1;
      Entries.Put (Object.Items, Index,
        (Session => Session, Limit => Limit, others => <>));
      if Parent = 0 then Object.Root := Index;
      else
         Item := Entries.Get (Object.Items, Parent);
         if Right then Item.Right := Index; else Item.Left := Index; end if;
         Entries.Put (Object.Items, Parent, Item);
      end if;
      Object.Count := Index; Accepted := True;
   end Open;
   function Snapshot (Object : Ledger; Session : Unsigned_64) return Usage is
      Index, Parent, Probes : Natural;
      Right : Boolean;
   begin
      Locate (Object, Session, Index, Parent, Probes, Right);
      if Index = 0 then return (others => <>); end if;
      declare Item : constant Entry_State := Entries.Get (Object.Items, Index); begin
         return (True, Item.Closed, Item.Limit, Item.Charged);
      end;
   end Snapshot;
   procedure Reserve
     (Object : in out Ledger; Session, Bytes : Unsigned_64; Accepted : out Boolean) is
      Index, Parent : Natural;
      Right : Boolean;
      Item : Entry_State;
   begin
      Accepted := False; Object.Probes := 0;
      if Object.Failed or Bytes = 0 or Bytes mod 4096 /= 0 then return; end if;
      Locate (Object, Session, Index, Parent, Object.Probes, Right);
      if Index = 0 then return; end if;
      Item := Entries.Get (Object.Items, Index);
      if Item.Closed then return; end if;
      Intel_GPU_Client_Budget_Policy.Reserve
        (Item.Charged, Item.Limit, Bytes, Accepted);
      if Accepted then Entries.Put (Object.Items, Index, Item); end if;
   end Reserve;
   procedure Release_Confirmed
     (Object : in out Ledger; Session, Bytes : Unsigned_64;
      Confirmed : Boolean; Accepted : out Boolean) is
      Index, Parent : Natural;
      Right : Boolean;
      Item : Entry_State;
   begin
      Accepted := False; Object.Probes := 0;
      if Object.Failed or not Confirmed or Bytes = 0 or Bytes mod 4096 /= 0 then return; end if;
      Locate (Object, Session, Index, Parent, Object.Probes, Right);
      if Index = 0 then return; end if;
      Item := Entries.Get (Object.Items, Index);
      Intel_GPU_Client_Budget_Policy.Release (Item.Charged, Bytes, Accepted);
      if Accepted then Entries.Put (Object.Items, Index, Item); end if;
   end Release_Confirmed;
   procedure Close (Object : in out Ledger; Session : Unsigned_64) is
      Index, Parent : Natural;
      Right : Boolean;
      Item : Entry_State;
   begin
      Object.Probes := 0;
      Locate (Object, Session, Index, Parent, Object.Probes, Right);
      if Index = 0 then return; end if;
      Item := Entries.Get (Object.Items, Index); Item.Closed := True;
      Entries.Put (Object.Items, Index, Item);
   end Close;
   procedure Quarantine (Object : in out Ledger) is
   begin Object.Failed := True; end Quarantine;
end Intel_GPU_Client_Budgets;
