with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Extent_Directory; use Intel_GPU_Extent_Directory;
with Intel_GPU_Physical_Extents;
procedure Extent_Directory_Tests is
   Block : constant Unsigned_64 := Intel_GPU_Physical_Extents.Block_Bytes;
   Pool, Foreign_Pool : aliased Directory;
   First, Last : View;
   First_Borrow, Last_Borrow : Borrowed_View;
   OK : Boolean;
   type Metadata is array (1 .. 16384) of Unsigned_8 with Alignment => 4096;
   Storage : Metadata := [others => 16#A5#];
   Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Storage'Address));
   DMA_Base : constant Unsigned_64 := 2 ** 40;
begin
   pragma Assert (View'Object_Size <= 128); -- compact, independent of arena size
   pragma Assert (Borrowed_View'Object_Size <= 128);
   Initialize (Pool, 24 * 1024 ** 3, 2 ** 48, OK); pragma Assert (OK);
   Initialize (Foreign_Pool, 24 * 1024 ** 3, 2 ** 48, OK); pragma Assert (OK);
   pragma Assert (Committed_Bytes (Pool) = 0 and not Valid (Pool, Snapshot (Pool)));
   Append (Pool, DMA_Base, OK); pragma Assert (OK);
   Append (Foreign_Pool, DMA_Base, OK); pragma Assert (OK);
   First := Snapshot (Pool);
   First_Borrow := Borrow (Pool);
   pragma Assert (not Same_Owner (First_Borrow, Borrow (Foreign_Pool)));
   pragma Assert (not Valid (Foreign_Pool, First));
   for Index in 2 .. 16 loop
      Append (Pool, DMA_Base + Unsigned_64 (Index - 1) * 2 * Block, OK);
      pragma Assert (OK);
   end loop;
   Append (Pool, DMA_Base + 32 * Block, OK); pragma Assert (not OK);
   pragma Assert (Committed_Bytes (Pool) = 16 * Block);
   Extend_Metadata (Pool, Base, 65537, OK); pragma Assert (not OK);
   Extend_Metadata (Pool, Base + 1, 4096, OK); pragma Assert (not OK);
   Extend_Metadata (Pool, Base, 8192, OK); pragma Assert (OK);
   for Index in 17 .. 528 loop
      Append (Pool, DMA_Base + Unsigned_64 (Index - 1) * 2 * Block, OK);
      pragma Assert (OK);
   end loop;
   Append (Pool, DMA_Base + 1056 * Block, OK); pragma Assert (not OK);
   Extend_Metadata (Pool, Base + 4096, 16384, OK); pragma Assert (not OK);
   Extend_Metadata (Pool, Base, 16384, OK); pragma Assert (OK);
   for Index in 529 .. 600 loop
      Append (Pool, DMA_Base + Unsigned_64 (Index - 1) * 2 * Block, OK);
      pragma Assert (OK);
   end loop;
   Last := Snapshot (Pool);
   Last_Borrow := Borrow (Pool);
   pragma Assert (Same_Owner (First_Borrow, Last_Borrow));
   pragma Assert (Byte_Count (First_Borrow) = Block and Byte_Count (Last_Borrow) = 600 * Block);
   pragma Assert (Resolve (First_Borrow, 0, 4096).Address = DMA_Base);
   pragma Assert (not Resolve (First_Borrow, Block, 4096).Valid);
   pragma Assert (Resolve (Last_Borrow, 599 * Block, 4096).Address = DMA_Base + 1198 * Block);
   pragma Assert (Byte_Count (Pool, First) = Block and Byte_Count (Pool, Last) = 600 * Block);
   for Index in 0 .. 599 loop
      pragma Assert (Resolve (Pool, Last, Unsigned_64 (Index) * Block, 4096).Address =
        DMA_Base + Unsigned_64 (Index) * 2 * Block);
   end loop;
   pragma Assert (Resolve (Pool, First, 0, 4096).Address = DMA_Base);
   pragma Assert (not Resolve (Pool, First, Block, 1).Valid);
   pragma Assert (Resolve (Pool, Last, Block - 1, 2).Bytes = 1);
   pragma Assert (not Resolve (Pool, Last, Unsigned_64'Last, 1).Valid);
   pragma Assert (not Resolve (Pool, Last, 1, Unsigned_64'Last).Valid);
   Append (Pool, DMA_Base, OK); pragma Assert (not OK);
   Append (Pool, 2 ** 48, OK); pragma Assert (not OK);
   Append (Pool, DMA_Base + 1, OK); pragma Assert (not OK);
   pragma Assert (Committed_Bytes (Pool) = 600 * Block);
   Initialize (Pool, 2 ** 40, 2 ** 48, OK); pragma Assert (not OK);
   Quarantine (Pool);
   pragma Assert (not Valid (First_Borrow) and not Valid (Last_Borrow));
   pragma Assert (not Resolve (Last_Borrow, 0, 4096).Valid);
   pragma Assert (not Valid (Pool, First) and not Valid (Pool, Last));
   pragma Assert (not Resolve (Pool, Last, 0, 4096).Valid);
   Append (Pool, DMA_Base + 1200 * Block, OK); pragma Assert (not OK);
   Initialize (Pool, Block, 2 ** 32, OK); pragma Assert (not OK);
   declare Small : Directory; begin
      Initialize (Small, Block, 2 ** 32, OK); pragma Assert (OK);
      Append (Small, Block, OK); pragma Assert (OK);
      Append (Small, 2 * Block, OK); pragma Assert (not OK);
   end;
   for Pattern in 1 .. 4 loop
      declare
         Indexed : aliased Directory;
         type Index_Metadata is array (1 .. 65536) of Unsigned_8
           with Alignment => 4096;
         Index_Storage : Index_Metadata := [others => 0];
         Prefix : Borrowed_View;
         function Key (I : Natural) return Unsigned_64 is
           (case Pattern is
              when 1 => Unsigned_64 (I + 1) * Block,
              when 2 => Unsigned_64 (4096 - I) * Block,
              when 3 => Unsigned_64 ((I * 2053) mod 4096 + 1) * Block,
              when others => 2 ** 63 +
                Unsigned_64 ((I * 2053) mod 4096 + 1) * Block);
      begin
         Initialize (Indexed, 4097 * Block, Unsigned_64'Last - Block + 1, OK);
         pragma Assert (OK);
         Extend_Metadata (Indexed,
           Unsigned_64 (To_Integer (Index_Storage'Address)), 65536, OK);
         pragma Assert (OK);
         for I in 0 .. 4095 loop
            Append (Indexed, Key (I), OK);
            pragma Assert (OK and Last_Admission_Probes (Indexed) <= 44);
            if I = 0 then Prefix := Borrow (Indexed); end if;
            Append (Indexed, Key (I), OK);
            pragma Assert (not OK and Last_Admission_Probes (Indexed) in 1 .. 44);
            pragma Assert (Committed_Bytes (Indexed) = Unsigned_64 (I + 1) * Block);
         end loop;
         for I in 0 .. 4095 loop
            pragma Assert (Resolve (Borrow (Indexed), Unsigned_64 (I) * Block, 1).Address = Key (I));
            declare
               Located : constant DMA_Location := Locate_DMA (Borrow (Indexed), Key (I) + Block - 1);
               Old : constant DMA_Location := Locate_DMA (Prefix, Key (I));
            begin
               pragma Assert (Located.State = Present and
                 Located.Offset = Unsigned_64 (I + 1) * Block - 1 and
                 Located.Probes in 1 .. Max_Admission_Probes);
               pragma Assert ((Old.State = Present) = (I = 0));
               pragma Assert (Old.State /= Unavailable);
            end;
         end loop;
         pragma Assert (Locate_DMA (Borrow (Indexed), 0).State = Absent);
         pragma Assert (Locate_DMA (Borrow (Indexed), Unsigned_64'Last).State = Absent);
         pragma Assert (Byte_Count (Prefix) = Block);
         pragma Assert (Resolve (Prefix, 0, 1).Address = Key (0));
         pragma Assert (not Resolve (Prefix, Block, 1).Valid);
         Quarantine (Indexed);
         pragma Assert (Locate_DMA (Prefix, Key (0)).State = Unavailable);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Extent directory PASS: 600-extents growth; four 4096-key orders, duplicates, <=44 probes, stable prefixes, high-bit DMA");
end Extent_Directory_Tests;
