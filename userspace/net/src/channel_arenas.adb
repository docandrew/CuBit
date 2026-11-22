------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body Channel_Arenas with SPARK_Mode is

   procedure Register
     (Item    : in out Table;
      Owner   : Owner_Id;
      Size    : Buffer_Bytes;
      Count   : Buffer_Count;
      Arena   : out Handle;
      Index   : out Arena_Index;
      Success : out Boolean)
   is
   begin
      Arena := No_Handle;
      Index := Arena_Index'First;
      Success := False;
      if not Fits (Size, Count) or else Item.Next_Id = No_Handle
        or else Item.Next_Id = Handle'Last
      then
         return;
      end if;
      for I in Arena_Index loop
         pragma Loop_Invariant (Consistent (Item));
         if Item.Entries (I).Id = No_Handle then
            Arena := Item.Next_Id;
            Item.Next_Id := Item.Next_Id + 1;
            Item.Entries (I) :=
              (Id => Arena, Owner => Owner, Size => Size, Count => Count,
               Used => [others => False]);
            Index := I;
            Success := True;
            return;
         end if;
      end loop;
   end Register;

   procedure Find
     (Item    : Table;
      Owner   : Owner_Id;
      Arena   : Handle;
      Index   : out Arena_Index;
      Found   : out Boolean)
   is
   begin
      Index := Arena_Index'First;
      Found := False;
      if Arena = No_Handle then
         return;
      end if;
      for I in Arena_Index loop
         if Item.Entries (I).Id = Arena and then Item.Entries (I).Owner = Owner
         then
            Index := I;
            Found := True;
            return;
         end if;
      end loop;
   end Find;

   procedure Claim
     (Item    : in out Table;
      Owner   : Owner_Id;
      Arena   : Handle;
      Buffer  : Unsigned_64;
      Index   : out Arena_Index;
      Slot    : out Buffer_Index;
      Offset  : out Span_Bytes;
      Success : out Boolean)
   is
      Found : Boolean;
   begin
      Slot := 0;
      Offset := 0;
      Success := False;
      Find (Item, Owner, Arena, Index, Found);
      if not Found or else Buffer > Unsigned_64 (Buffer_Index'Last) then
         return;
      end if;
      Slot := Buffer_Index (Buffer);
      if Slot >= Item.Entries (Index).Count then
         return;
      end if;
      if Item.Entries (Index).Used (Slot) then
         return;
      end if;
      declare
         Size : constant Buffer_Bytes := Item.Entries (Index).Size;
         Most : constant Natural := Maximum_Span / Size;
      begin
         pragma Assert (Slot + 1 <= Most);
         pragma Assert (Most * Size <= Maximum_Span);
         pragma Assert (Slot * Size + Size <= Most * Size);
         Offset := Slot * Size;
      end;
      Item.Entries (Index).Used (Slot) := True;
      Success := True;
   end Claim;

   procedure Release
     (Item  : in out Table;
      Index : Arena_Index;
      Slot  : Buffer_Index)
   is
   begin
      if Item.Entries (Index).Id /= No_Handle then
         Item.Entries (Index).Used (Slot) := False;
      end if;
   end Release;

   procedure Unregister
     (Item    : in out Table;
      Owner   : Owner_Id;
      Arena   : Handle;
      Index   : out Arena_Index;
      Success : out Boolean)
   is
      Found : Boolean;
   begin
      Success := False;
      Find (Item, Owner, Arena, Index, Found);
      if Found and then Idle (Item, Index) then
         Item.Entries (Index) := (others => <>);
         Success := True;
      end if;
   end Unregister;

   procedure Release_Owner
     (Item     : in out Table;
      Owner    : Owner_Id;
      Released : out Arena_List)
   is
   begin
      Released := [others => False];
      for I in Arena_Index loop
         pragma Loop_Invariant (Consistent (Item));
         pragma Loop_Invariant
           (for all J in Arena_Index =>
              (if Released (J) then Item.Entries (J).Id = No_Handle));
         if Item.Entries (I).Id /= No_Handle
           and then Item.Entries (I).Owner = Owner
         then
            Item.Entries (I) := (others => <>);
            Released (I) := True;
         end if;
      end loop;
   end Release_Owner;

end Channel_Arenas;
