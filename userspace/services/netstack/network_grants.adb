pragma Ada_2022;
package body Network_Grants with SPARK_Mode is
   procedure Install
     (State : in out Table; Owner : Unsigned_64; Item : Scope;
      Capacity : Reservation; Tag : out Unsigned_64; Success : out Boolean)
   is
   begin
      Tag := 0; Success := False;
      if Owner = 0 or not Valid (Item) or State.Next_Tag > Last_Grant_Tag
        or Item.Connections > Capacity - State.Reserved
      then
         return;
      end if;
      for Entry_Item of State.Entries loop
         if Entry_Item.Tag = 0 then
            Tag := State.Next_Tag;
            State.Next_Tag := State.Next_Tag + 1;
            State.Reserved := State.Reserved + Item.Connections;
            Entry_Item := (Owner, Tag, Item, 0);
            Success := True;
            return;
         end if;
      end loop;
   end Install;

   procedure Release (State : in out Table; Owner, Tag : Unsigned_64) is
      Before : constant Reservation := State.Reserved with Ghost;
   begin
      for I in State.Entries'Range loop
         pragma Loop_Invariant (Within_Limits (State));
         pragma Loop_Invariant (State.Reserved <= Before);
         if State.Entries (I).Owner = Owner and State.Entries (I).Tag = Tag
           and State.Entries (I).Tag /= 0
         then
            State.Reserved := State.Reserved -
              Natural'Min (State.Reserved, State.Entries (I).Item.Connections);
            State.Entries (I) := (others => <>);
         end if;
      end loop;
   end Release;

   procedure Release_Owner
     (State : in out Table; Owner : Unsigned_64; Tags : out Tag_List)
   is
      Before : constant Reservation := State.Reserved with Ghost;
   begin
      Tags := [others => 0];
      for I in State.Entries'Range loop
         pragma Loop_Invariant (Within_Limits (State));
         pragma Loop_Invariant (State.Reserved <= Before);
         if Owner /= 0 and State.Entries (I).Owner = Owner
           and State.Entries (I).Tag /= 0
         then
            Tags (I) := State.Entries (I).Tag;
            State.Reserved := State.Reserved -
              Natural'Min (State.Reserved, State.Entries (I).Item.Connections);
            State.Entries (I) := (others => <>);
         end if;
      end loop;
   end Release_Owner;

   function Owned (State : Table; Owner, Tag : Unsigned_64) return Boolean is
     (Owner /= 0 and then Tag >= First_Grant_Tag and then
      (for some E of State.Entries => E.Owner = Owner and E.Tag = Tag));

   function Scope_Of (State : Table; Owner, Tag : Unsigned_64) return Scope is
   begin
      if Owner /= 0 and then Tag /= 0 then
         for E of State.Entries loop
            if E.Owner = Owner and then E.Tag = Tag then
               return E.Item;
            end if;
         end loop;
      end if;
      return Denied_Scope;
   end Scope_Of;

   function May_Resolve
     (State : Table; Owner, Tag : Unsigned_64) return Boolean is
     (Owned (State, Owner, Tag) and then
      (for some E of State.Entries => E.Owner = Owner and E.Tag = Tag and
         E.Item.Action in Connect_TCP | Connect_UDP and E.Item.Resolve_Names));

   function Allows
     (State : Table; Owner, Tag : Unsigned_64; Action : Operation;
      Address : Unsigned_32; Port : Unsigned_16) return Boolean is
     (Owned (State, Owner, Tag) and then
      (for some E of State.Entries => E.Owner = Owner and E.Tag = Tag and
         CuBit.Network_Authority.Allows (E.Item, Action, Address, Port)));

   function In_Use (State : Table; Tag : Unsigned_64) return Connection_Count
   is
   begin
      for E of State.Entries loop
         if Tag /= 0 and then E.Tag = Tag then
            return E.Open;
         end if;
      end loop;
      return 0;
   end In_Use;

   function Limit (State : Table; Tag : Unsigned_64) return Connection_Count
   is
   begin
      for E of State.Entries loop
         if Tag /= 0 and then E.Tag = Tag then
            return E.Item.Connections;
         end if;
      end loop;
      return 0;
   end Limit;

   procedure Charge
     (State : in out Table; Owner, Tag : Unsigned_64; Success : out Boolean)
   is
   begin
      Success := False;
      for E of State.Entries loop
         if Owner /= 0 and then Tag /= 0 and then E.Owner = Owner
           and then E.Tag = Tag
         then
            if E.Open < E.Item.Connections then
               E.Open := E.Open + 1;
               Success := True;
            end if;
            return;
         end if;
      end loop;
   end Charge;

   procedure Refund (State : in out Table; Tag : Unsigned_64) is
   begin
      for E of State.Entries loop
         if Tag /= 0 and then E.Tag = Tag then
            if E.Open > 0 then
               E.Open := E.Open - 1;
            end if;
            return;
         end if;
      end loop;
   end Refund;
end Network_Grants;
