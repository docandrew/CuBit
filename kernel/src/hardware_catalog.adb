package body Hardware_Catalog with SPARK_Mode is
   function Valid (Item : Descriptor) return Boolean is
   begin
      return Hardware_Authority.Valid (Item.Permission) and then Item.Address /= 0
      and then Item.Width in 1 | 2 | 4 | 8
      and then Item.Address mod Unsigned_64 (Item.Width) = 0
      and then Unsigned_64 (Item.Width - 1) <= Unsigned_64'Last - Item.Address
      and then (if Item.Width < 8 then
        Item.Permission.Write_Mask < 2 ** (8 * Item.Width))
      and then (if Item.Space = IO_Space then Item.Width <= 4 and then
        Item.Address <= 65535 and then
        Unsigned_64 (Item.Width - 1) <= 65535 - Item.Address);
   end Valid;
   function Active (S : State) return Boolean is (S.Enabled);
   function Epoch (S : State) return Unsigned_64 is (S.Version);
   function Busy (S : State) return Boolean is (S.In_Flight);
   function Receipt (S : State) return Unsigned_64 is (S.Sequence);
   function Count (S : State) return Natural is (S.Used);
   function Resource_At (S : State; Index : Positive) return Descriptor is (S.Items (Index));
   procedure Begin_Inventory (S : in out State; Accepted : out Boolean) is
   begin
      Accepted := False;
      if S.Enabled or else S.In_Flight or else S.Version = Unsigned_64'Last then return; end if;
      S.Version := S.Version + 1;
      S.Used := 0;
      S.Building := True;
      Accepted := True;
   end Begin_Inventory;
   procedure Add (S : in out State; Item : Descriptor; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not S.Building or else not Valid (Item) or else S.Used = Maximum_Resources then return; end if;
      for I in 1 .. S.Used loop
         if S.Items (I).Permission.Resource_ID = Item.Permission.Resource_ID then return; end if;
         pragma Loop_Invariant (for all J in 1 .. I =>
           S.Items (J).Permission.Resource_ID /= Item.Permission.Resource_ID);
      end loop;
      S.Used := S.Used + 1;
      S.Items (S.Used) := Item;
      Accepted := True;
   end Add;
   procedure Seal (S : in out State; Accepted : out Boolean) is
   begin
      Accepted := S.Building;
      if Accepted then S.Building := False; S.Enabled := True; end if;
   end Seal;
   procedure Revoke (S : in out State) is
   begin
      S.Enabled := False;
      S.Building := False;
   end Revoke;
   function Contains (S : State; Group : Resource_Group; ID : Unsigned_64)
     return Boolean is
     (for some I in 1 .. S.Used =>
       S.Items (I).Group = Group and then S.Items (I).Permission.Resource_ID = ID);
   function Can_Select
     (S : State; Token : Unsigned_64; Group : Resource_Group;
      Child : Hardware_Authority.Permission) return Boolean is
     (S.Enabled and then Token = S.Version and then
       (for some I in 1 .. S.Used => S.Items (I).Group = Group and then
          Hardware_Authority.Can_Derive (S.Items (I).Permission, Child)));
   function Resolve
     (S : State; Token : Unsigned_64; Scope : Hardware_Authority.Permission;
      For_Write : Boolean; Value : Unsigned_64) return Decision is
   begin
      if not S.Enabled or else Token /= S.Version or else
        not Hardware_Authority.Permits (Scope, Scope.Resource_ID, For_Write, Value)
      then return (Allowed => False); end if;
      for I in 1 .. S.Used loop
         if Hardware_Authority.Is_Subset (S.Items (I).Permission, Scope)
           and then Hardware_Authority.Permits
             (S.Items (I).Permission, Scope.Resource_ID, For_Write, Value)
         then return (Allowed => True, Resource => S.Items (I)); end if;
      end loop;
      return (Allowed => False);
   end Resolve;
   procedure Begin_Access
     (S : in out State; Token : Unsigned_64; Scope : Hardware_Authority.Permission;
      For_Write : Boolean; Value : Unsigned_64;
      Result : out Decision; Ticket : out Unsigned_64) is
   begin
      Result := (Allowed => False); Ticket := 0;
      if S.In_Flight or else S.Sequence = Unsigned_64'Last then return; end if;
      Result := Resolve (S, Token, Scope, For_Write, Value);
      if not Result.Allowed then return; end if;
      S.Sequence := S.Sequence + 1;
      S.In_Flight := True;
      Ticket := S.Sequence;
   end Begin_Access;
   procedure Finish_Access
     (S : in out State; Ticket : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := S.In_Flight and then Ticket = S.Sequence;
      if Accepted then S.In_Flight := False; end if;
   end Finish_Access;
end Hardware_Catalog;
