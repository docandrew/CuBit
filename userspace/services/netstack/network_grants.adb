pragma Ada_2022;
package body Network_Grants with SPARK_Mode is
   procedure Install
     (State : in out Table; Owner : Unsigned_64; Item : Scope;
      Tag : out Unsigned_64; Success : out Boolean) is
   begin
      Tag := 0; Success := False;
      if Owner = 0 or not Valid (Item) or State.Next_Tag > Last_Grant_Tag then
         return;
      end if;
      for Entry_Item of State.Entries loop
         if Entry_Item.Tag = 0 then
            Tag := State.Next_Tag;
            State.Next_Tag := State.Next_Tag + 1;
            Entry_Item := (Owner, Tag, Item);
            Success := True;
            return;
         end if;
      end loop;
   end Install;

   procedure Release (State : in out Table; Owner, Tag : Unsigned_64) is
   begin
      for Entry_Item of State.Entries loop
         if Entry_Item.Owner = Owner and Entry_Item.Tag = Tag then
            Entry_Item := (others => <>);
         end if;
      end loop;
   end Release;

   function Owned (State : Table; Owner, Tag : Unsigned_64) return Boolean is
     (Owner /= 0 and then Tag >= First_Grant_Tag and then
      (for some E of State.Entries => E.Owner = Owner and E.Tag = Tag));

   function May_Resolve
     (State : Table; Owner, Tag : Unsigned_64) return Boolean is
     (Owned (State, Owner, Tag) and then
      (for some E of State.Entries => E.Owner = Owner and E.Tag = Tag and
         E.Item.Action = Connect_TCP and E.Item.Resolve_Names));

   function Allows
     (State : Table; Owner, Tag : Unsigned_64; Action : Operation;
      Address : Unsigned_32; Port : Unsigned_16) return Boolean is
     (Owned (State, Owner, Tag) and then
      (for some E of State.Entries => E.Owner = Owner and E.Tag = Tag and
         CuBit.Network_Authority.Allows (E.Item, Action, Address, Port)));
end Network_Grants;
