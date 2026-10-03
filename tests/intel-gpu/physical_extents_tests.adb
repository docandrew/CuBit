with Ada.Text_IO;
with Intel_GPU_Extent_Directory;
with Interfaces; use Interfaces;
with Intel_GPU_Physical_Extents; use Intel_GPU_Physical_Extents;
with Intel_GPU_Buffer_Reply;
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
   declare
      package V renames Intel_GPU_Buffer_Reply;
      Prefix : Addresses := [others => 0];
      Old, New_Map : Map;
      Old_View, New_View : V.Extent_View;
      Owner : aliased Intel_GPU_Extent_Directory.Directory;
   begin
      Intel_GPU_Extent_Directory.Initialize (Owner, Capacity, 2 ** 32, OK);
      pragma Assert (OK);
      Admit (Prefix, Old, OK, 0); pragma Assert (not OK);
      Admit (Prefix, Old, OK, 17); pragma Assert (not OK);
      for Count in 1 .. Addresses'Length loop
         Prefix (Count - 1) := Bases (Count - 1);
         Admit (Prefix, New_Map, OK, Count);
         pragma Assert (OK and Committed_Bytes (New_Map) = Unsigned_64 (Count) * Block_Bytes);
         pragma Assert (not Resolve (New_Map, Committed_Bytes (New_Map), 1).Valid);
         pragma Assert (not Resolve (New_Map, Committed_Bytes (New_Map) - 1, 2).Valid);
         pragma Assert (Resolve (New_Map, Committed_Bytes (New_Map) - 4096, 4096).Valid);
         Intel_GPU_Extent_Directory.Append (Owner, Prefix (Count - 1), OK);
         pragma Assert (OK);
         New_View := V.From_Extents (Intel_GPU_Extent_Directory.Borrow (Owner), 7, 0, 4096);
         pragma Assert (V.Valid (New_View));
         pragma Assert (not V.Valid (V.From_Extents
           (Intel_GPU_Extent_Directory.Borrow (Owner), 7, Committed_Bytes (New_Map), 4096)));
         if Count > 1 then
            pragma Assert (Compatible (Old, New_Map) and Compatible (New_Map, Old));
            pragma Assert (V.Same_Arena (Old_View, New_View));
            pragma Assert (V.Page_Address (Old_View, 0) = Bases (0));
            Bad := Prefix;
            Bad (0) := Bases (0) - Block_Bytes;
            Admit (Bad, Object, OK, Count);
            pragma Assert (OK and not Compatible (Old, Object));
            declare Other : aliased Intel_GPU_Extent_Directory.Directory; begin
               Intel_GPU_Extent_Directory.Initialize (Other, Capacity, 2 ** 32, OK);
               pragma Assert (OK);
               Intel_GPU_Extent_Directory.Append (Other, Bad (0), OK);
               pragma Assert (OK);
               pragma Assert (not V.Same_Arena (Old_View,
                 V.From_Extents (Intel_GPU_Extent_Directory.Borrow (Other), 7, 0, 4096)));
            end;
         end if;
         Old := New_Map; Old_View := New_View;
         if Count < Addresses'Length then
            Bad := Prefix; Bad (Count) := Bases (Count);
            Admit (Bad, Object, OK, Count);
            pragma Assert (not OK); -- unpublished suffix must be empty
         end if;
      end loop;
   end;
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
