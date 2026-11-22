package body Hardware_Grants with SPARK_Mode,
  Refined_State => (Identity_Allocator => Last_Issued) is
   Last_Issued : Unsigned_64 := 0;
   function Identity (S : State) return Unsigned_64 is (S.Lifetime_ID);
   function Issued_Count return Unsigned_64 is (Last_Issued);
   procedure Initialize (S : in out State; Accepted : out Boolean) is
   begin
      Accepted := False;
      if S.Lifetime_ID /= 0 or else S.Used /= 0 or else
        Last_Issued = Unsigned_64'Last then return; end if;
      Last_Issued := Last_Issued + 1;
      S.Lifetime_ID := Last_Issued;
      Accepted := True;
   end Initialize;
   function Count (S : State) return Natural is (S.Used);
   function Live (S : State; H : Handle) return Boolean is
     (H in 1 .. Handle (S.Used) and then S.Items (Slot (H)).Enabled
      and then (for all I in Slot =>
        (if S.Items (Slot (H)).Parents (I) then S.Items (I).Enabled)));

   function Descends_From (S : State; Child, Parent : Handle) return Boolean is
     (Child in 1 .. Handle (S.Used) and then Parent in 1 .. Handle (S.Used)
      and then S.Items (Slot (Child)).Parents (Slot (Parent)));

   procedure Admit_Group
     (S : in out State; Catalog : Hardware_Catalog.State;
      Group : Hardware_Catalog.Resource_Group; H : out Handle) is
   begin
      H := No_Grant;
      if S.Lifetime_ID = 0 or else S.Used = Maximum_Grants or else not Hardware_Catalog.Active (Catalog)
        or else Hardware_Catalog.Epoch (Catalog) = 0 then return; end if;
      S.Used := S.Used + 1;
      S.Items (S.Used) := (Enabled => True, Is_Group => True,
        Group => Group, Epoch => Hardware_Catalog.Epoch (Catalog), others => <>);
      H := Handle (S.Used);
   end Admit_Group;

   procedure Derive
     (S : in out State; Catalog : Hardware_Catalog.State; Parent : Handle;
      Requested : Hardware_Authority.Permission; H : out Handle) is
      Item : Grant;
   begin
      H := No_Grant;
      if S.Used = Maximum_Grants or else not Live (S, Parent)
        or else not Hardware_Authority.Valid (Requested) then return; end if;
      Item := S.Items (Slot (Parent));
      if not Hardware_Catalog.Can_Select
        (Catalog, Item.Epoch, Item.Group, Requested) then return; end if;
      if not Item.Is_Group and then not Hardware_Authority.Can_Derive
        (Item.Permission, Requested) then return; end if;
      Item.Is_Group := False;
      Item.Permission := Requested;
      Item.Parents (Slot (Parent)) := True;
      pragma Assert (Item.Epoch /= 0);
      pragma Assert (for all I in S.Used + 1 .. Maximum_Grants => not Item.Parents (I));
      S.Used := S.Used + 1;
      S.Items (S.Used) := Item;
      H := Handle (S.Used);
   end Derive;

   procedure Revoke (S : in out State; H : Handle) is
   begin
      if H in 1 .. Handle (S.Used) then S.Items (Slot (H)).Enabled := False; end if;
   end Revoke;

   function Resolve
     (S : State; Catalog : Hardware_Catalog.State; H : Handle;
      For_Write : Boolean; Value : Unsigned_64) return Hardware_Catalog.Decision
     with Refined_Post => Resolve'Result =
       (if not Live (S, H) or else S.Items (Slot (H)).Is_Group then
          Hardware_Catalog.Decision'(Allowed => False)
        else Hardware_Catalog.Resolve (Catalog, S.Items (Slot (H)).Epoch,
          S.Items (Slot (H)).Permission, For_Write, Value))
   is
      Result : Hardware_Catalog.Decision;
   begin
      if not Live (S, H) or else S.Items (Slot (H)).Is_Group then
         return (Allowed => False);
      end if;
      Result := Hardware_Catalog.Resolve (Catalog, S.Items (Slot (H)).Epoch,
        S.Items (Slot (H)).Permission, For_Write, Value);
      return Result;
   end Resolve;
   procedure Begin_Access
     (S : State; Catalog : in out Hardware_Catalog.State; H : Handle;
      For_Write : Boolean; Value : Unsigned_64;
      Result : out Hardware_Catalog.Decision; Ticket : out Unsigned_64) is
   begin
      Result := (Allowed => False);
      Ticket := 0;
      if not Live (S, H) or else S.Items (Slot (H)).Is_Group then return; end if;
      Hardware_Catalog.Begin_Access (Catalog, S.Items (Slot (H)).Epoch,
        S.Items (Slot (H)).Permission, For_Write, Value, Result, Ticket);
   end Begin_Access;
end Hardware_Grants;
