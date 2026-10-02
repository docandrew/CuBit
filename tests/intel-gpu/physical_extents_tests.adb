with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Physical_Extents; use Intel_GPU_Physical_Extents;
procedure Physical_Extents_Tests is
   Bases, Bad : Addresses;
   Object : Map;
   OK : Boolean;
   Item : Span;
begin
   pragma Assert (not Ready (Object));
   pragma Assert (not Resolve (Object, 0, 4096).Valid);
   -- Descending blocks with holes: no physical-base-plus-offset shortcut.
   for I in Block_Index loop
      Bases (I) := 2 ** 32 - Unsigned_64 (2 * I + 1) * Block_Bytes;
   end loop;
   Admit (Bases, Object, OK); pragma Assert (OK and Ready (Object));
   for Page in 0 .. 8191 loop
      Item := Resolve (Object, Unsigned_64 (Page) * 4096, 4096);
      pragma Assert (Item.Valid and Item.Bytes = 4096 and
        Item.Address = Bases (Block_Index (Page / 512)) +
          Unsigned_64 (Page mod 512) * 4096);
   end loop;
   for I in Block_Index loop
      Item := Resolve (Object, Unsigned_64 (I) * Block_Bytes, Block_Bytes);
      pragma Assert (Item.Valid and Item.Bytes = Block_Bytes and
        Item.Address = Bases (I));
      if I /= Block_Index'Last then
         Item := Resolve (Object, Unsigned_64 (I + 1) * Block_Bytes - 1, 2);
         pragma Assert (Item.Valid and Item.Bytes = 1 and
           Item.Address = Bases (I) + Block_Bytes - 1);
      end if;
   end loop;
   pragma Assert (not Resolve (Object, Capacity, 1).Valid);
   pragma Assert (not Resolve (Object, Capacity - 1, 2).Valid);
   pragma Assert (not Resolve (Object, 0, 0).Valid);
   pragma Assert (not Resolve (Object, Unsigned_64'Last, 1).Valid);
   pragma Assert (not Resolve (Object, 1, Unsigned_64'Last).Valid);
   for I in Block_Index loop
      for Case_ID in 0 .. 3 loop
         Bad := Bases;
         Bad (I) := (case Case_ID is when 0 => 0, when 1 => Bases (I) + 1,
           when 2 => 2 ** 32, when others => Bases ((I + 1) mod 16));
         Admit (Bad, Object, OK);
         pragma Assert (not OK and not Ready (Object));
         pragma Assert (not Resolve (Object, 0, 4096).Valid);
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Physical extents PASS: 8192 pages, scattered/aligned blocks, crossing spans, invalid/aliased backing (hosted geometry only)");
end Physical_Extents_Tests;
