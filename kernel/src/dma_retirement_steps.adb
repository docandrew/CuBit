package body DMA_Retirement_Steps is
   function Rejected (Position : Cursor) return Boolean is (Position.Poisoned);
   procedure Step
     (Item : Allocation; Position : in out Cursor;
      CPU_And_Grants_Retired : Boolean; Complete : out Boolean) is
      Pages : constant Natural := 2 ** Item.Order;
   begin
      Complete := False;
      if Position.Poisoned then return; end if;
      -- Fail closed on malformed or changed identity, even without assertions.
      if Item.Owner = 0 or else Item.Generation = 0 or else Item.Physical = 0 or else
        Item.Physical mod (Unsigned_64 (Pages) * 4096) /= 0 or else
        Item.Physical > Unsigned_64'Last - Unsigned_64 (Pages) * 4096 or else
        (Position.Started and then Position.Identity /= Item)
      then
         Position.Poisoned := True;
         return;
      end if;
      if not CPU_And_Grants_Retired then return; end if;
      if Position.Finished then Complete := True; return; end if;
      if not Position.Started then
         Position.Identity := Item;
         Position.Started := True;
      end if;
      for Work in 1 .. Maximum_Pages_Per_Step loop
         exit when Position.Next_Page = Pages;
         Release_Owner
           (Item.Physical + Unsigned_64 (Position.Next_Page) * 4096, Item.Owner);
         Position.Next_Page := Position.Next_Page + 1;
      end loop;
      if Position.Next_Page = Pages then
         if not Item.Retained then Free_Block (Item.Physical, Item.Order); end if;
         Position.Finished := True;
         Complete := True;
      end if;
   end Step;
end DMA_Retirement_Steps;
