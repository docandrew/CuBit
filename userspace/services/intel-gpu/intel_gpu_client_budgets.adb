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
   function Count (Object : Ledger) return Natural is (Object.Count);
   function Record_Bytes return Unsigned_64 is (Unsigned_64 (Entry_State'Object_Size / 8));
   -- Point Parent's link that leads to Child (or the root) at Target.
   procedure Relink (Object : in out Ledger; Parent, Child, Target : Natural) is
      Item : Entry_State;
   begin
      if Parent = 0 then
         Object.Root := Target;
      else
         Item := Entries.Get (Object.Items, Parent);
         if Item.Left = Child then Item.Left := Target; else Item.Right := Target; end if;
         Entries.Put (Object.Items, Parent, Item);
      end if;
   end Relink;
   -- A digital search tree: every node's session matches the path bits to
   -- it, so a leaf from Node's subtree may take Node's place. The record
   -- freed is then filled from the last record, keeping storage dense.
   procedure Forget (Object : in out Ledger; Session : Unsigned_64; Accepted : out Boolean) is
      Index, Parent, Probes : Natural;
      Right : Boolean;
      Node, Leaf_Item, Moved : Entry_State;
      Leaf, Leaf_Parent, Freed : Natural;
   begin
      Accepted := False; Object.Probes := 0;
      if Object.Failed then return; end if;
      Locate (Object, Session, Index, Parent, Probes, Right);
      if Index = 0 then return; end if;
      Node := Entries.Get (Object.Items, Index);
      if not Node.Closed or else Node.Charged /= 0 then return; end if;
      -- Find a leaf under Node (Node itself when it has no children).
      Leaf := Index; Leaf_Parent := Parent;
      for Depth in 0 .. 64 loop
         Leaf_Item := Entries.Get (Object.Items, Leaf);
         exit when Leaf_Item.Left = 0 and then Leaf_Item.Right = 0;
         Leaf_Parent := Leaf;
         Leaf := (if Leaf_Item.Left /= 0 then Leaf_Item.Left else Leaf_Item.Right);
         if Leaf = 0 or else Leaf > Object.Count or else Depth = 64 then
            Quarantine (Object); return;
         end if;
      end loop;
      -- The leaf's account moves into Node's place; the leaf goes.
      Relink (Object, Leaf_Parent, Leaf, 0);
      if Leaf /= Index then
         Leaf_Item := Entries.Get (Object.Items, Leaf);
         Node := Entries.Get (Object.Items, Index);
         Node.Session := Leaf_Item.Session; Node.Limit := Leaf_Item.Limit;
         Node.Charged := Leaf_Item.Charged; Node.Closed := Leaf_Item.Closed;
         Entries.Put (Object.Items, Index, Node);
      end if;
      Freed := Leaf;
      -- Keep records dense: the last record fills the freed one.
      if Freed /= Object.Count then
         Moved := Entries.Get (Object.Items, Object.Count);
         Locate (Object, Moved.Session, Index, Parent, Probes, Right);
         if Index /= Object.Count then Quarantine (Object); return; end if;
         Entries.Put (Object.Items, Freed, Moved);
         Relink (Object, Parent, Object.Count, Freed);
      end if;
      Entries.Put (Object.Items, Object.Count, (others => <>));
      Object.Count := Object.Count - 1;
      Accepted := True;
   end Forget;
   procedure Quarantine (Object : in out Ledger) is
   begin Object.Failed := True; end Quarantine;
end Intel_GPU_Client_Budgets;
