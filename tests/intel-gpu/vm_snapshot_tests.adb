with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Snapshots;
procedure VM_Snapshot_Tests is
   package VM is new Intel_GPU_VM_Image (4);
   package Snapshots is new VM.Snapshots;
   Current, Stale : VM.Image;
   OK : Boolean;
   function Pages (Base : Unsigned_64) return VM.Backing_Pages is
     ([Base, Base + 4096, Base + 8192, Base + 12288]);
   procedure Reject (Candidate : VM.Image) is
      Before_Root : constant Unsigned_64 := VM.Root_DMA (Current);
      type Entries is array (VM.Page_Number, Table_Index) of Unsigned_64;
      Before : Entries;
   begin
      for P in VM.Page_Number loop
         for I in Table_Index loop Before (P, I) := VM.Entry_Value (Current, P, I); end loop;
      end loop;
      Snapshots.Adopt_Committed (Current, Candidate, OK);
      pragma Assert (not OK and VM.Root_DMA (Current) = Before_Root and VM.Sealed (Current));
      for P in VM.Page_Number loop
         for I in Table_Index loop
            pragma Assert (VM.Entry_Value (Current, P, I) = Before (P, I));
         end loop;
      end loop;
   end Reject;
begin
   -- A disposed receipt is distinct from a new, failed or mutable snapshot.
   -- In particular a failed re-preparation must not inherit the exemption used
   -- by cross-context table-retirement scans.
   for Preparation in 1 .. 4 loop
      declare
         Retiring, Source, Invalid : VM.Image;
         Epoch : Unsigned_64;
      begin
         pragma Assert (not Snapshots.Retired (Retiring));
         VM.Initialize (Retiring, Pages (16#E00000#), OK); pragma Assert (OK);
         pragma Assert (not Snapshots.Retired (Retiring));
         VM.Map_Page (Retiring, 4096, 16#3000000#, Write_Back, Read_Write, OK);
         pragma Assert (OK);
         VM.Seal (Retiring, OK); pragma Assert (OK);
         Epoch := VM.Revision (Retiring);
         Snapshots.Forget_Retired (Retiring, Epoch, 16#E00000#, False, OK);
         pragma Assert (not OK and not Snapshots.Retired (Retiring));
         Snapshots.Forget_Retired (Retiring, Epoch, 16#E00000#, True, OK);
         pragma Assert (OK and Snapshots.Retired (Retiring));
         Snapshots.Forget_Retired (Retiring, Epoch, 16#E00000#, True, OK);
         pragma Assert (not OK and Snapshots.Retired (Retiring));
         case Preparation is
            when 1 => VM.Initialize (Retiring, Pages (16#F00000#), OK);
            when 2 => VM.Initialize (Retiring, [others => 0], OK);
            when 3 =>
               VM.Initialize (Source, Pages (16#F00000#), OK); pragma Assert (OK);
               VM.Map_Page (Source, 4096, 16#3000000#, Write_Back, Read_Write, OK);
               pragma Assert (OK);
               VM.Seal (Source, OK); pragma Assert (OK);
               VM.Prepare_Update (Retiring, Source, Pages (16#E00000#), OK);
            when 4 => VM.Prepare_Update (Retiring, Invalid, Pages (16#E00000#), OK);
         end case;
         pragma Assert (OK = (Preparation = 1 or Preparation = 3));
         pragma Assert (not Snapshots.Retired (Retiring));
         pragma Assert (not VM.Sealed (Retiring));
         pragma Assert (VM.Revision (Retiring) = Epoch + 1);
         Snapshots.Forget_Retired (Retiring, Epoch, 16#E00000#, True, OK);
         pragma Assert (not OK and not Snapshots.Retired (Retiring));
      end;
   end loop;
   declare
      Initial, Empty, Rebound, Invalid : VM.Image;
   begin
      VM.Seal_Update (Invalid, OK); pragma Assert (not OK);
      VM.Initialize (Initial, Pages (16#900000#), OK); pragma Assert (OK);
      VM.Seal_Update (Initial, OK); pragma Assert (not OK);
      VM.Map_Page (Initial, 4096, 16#3000000#, Write_Back, Read_Write, OK);
      pragma Assert (OK);
      VM.Seal_Update (Initial, OK); pragma Assert (not OK);
      VM.Seal (Initial, OK); pragma Assert (OK);
      VM.Prepare_Update (Empty, Initial, Pages (16#910000#), OK); pragma Assert (OK);
      VM.Unmap_Pages (Empty, 4096, [1 => 16#3000000#], OK); pragma Assert (OK);
      VM.Seal (Empty, OK); pragma Assert (not OK);
      VM.Seal_Update (Empty, OK); pragma Assert (OK and VM.Sealed (Empty));
      Snapshots.Adopt_Committed (Initial, Empty, OK); pragma Assert (OK);
      pragma Assert (VM.Lookup (Initial, 4096) = 0);
      VM.Prepare_Update (Rebound, Initial, Pages (16#920000#), OK); pragma Assert (OK);
      VM.Map_Page (Rebound, 4096, 16#3100000#, Write_Back, Read_Write, OK);
      pragma Assert (OK);
      VM.Seal_Update (Rebound, OK); pragma Assert (OK);
      Snapshots.Adopt_Committed (Initial, Rebound, OK); pragma Assert (OK);
      pragma Assert (VM.Lookup (Initial, 4096) = VM.Lookup (Rebound, 4096));
   end;
   VM.Initialize (Current, Pages (16#1000#), OK); pragma Assert (OK);
   VM.Map_Page (Current, 4096, 16#1000000#, Write_Back, Read_Write, OK);
   pragma Assert (OK);
   VM.Seal (Current, OK); pragma Assert (OK);
   VM.Prepare_Update (Stale, Current, Pages (16#800000#), OK); pragma Assert (OK);
   VM.Seal (Stale, OK); pragma Assert (OK);
   for Generation in 1 .. 20 loop
      declare
         Candidate, Unrelated, Empty : VM.Image;
         Base : constant Unsigned_64 := Unsigned_64 (Generation) * 16#10000#;
         Old_Root : constant Unsigned_64 := VM.Root_DMA (Current);
      begin
         Reject (Empty);
         VM.Initialize (Unrelated, Pages (Base + 16#8000#), OK); pragma Assert (OK);
         VM.Map_Page (Unrelated, 4096, 16#2000000#, Write_Back, Read_Write, OK);
         pragma Assert (OK);
         VM.Seal (Unrelated, OK); pragma Assert (OK);
         Reject (Unrelated);
         VM.Prepare_Update (Candidate, Current, Pages (Base), OK); pragma Assert (OK);
         Reject (Candidate); -- unsealed even though its lineage is correct
         VM.Map_Page (Candidate, Unsigned_64 (Generation + 1) * 4096,
           16#1000000# + Unsigned_64 (Generation) * 4096, Write_Back, Read_Write, OK);
         pragma Assert (OK);
         VM.Seal (Candidate, OK); pragma Assert (OK);
         pragma Assert (VM.Root_DMA (Current) = Old_Root);
         Snapshots.Adopt_Committed (Current, Candidate, OK);
         pragma Assert (OK and VM.Root_DMA (Current) = Base and VM.Sealed (Current));
         for Page in 1 .. Generation + 1 loop
            pragma Assert (VM.Lookup (Current, Unsigned_64 (Page) * 4096) =
              VM.Lookup (Candidate, Unsigned_64 (Page) * 4096));
         end loop;
         Reject (Candidate); -- replay must not overwrite the new current image
         Reject (Stale); -- sibling prepared from generation zero
      end;
   end loop;
   declare
      Candidate : VM.Image;
      Data : VM.Data_Pages (1 .. 21);
   begin
      for P in Data'Range loop
         Data (P) := 16#1000000# + Unsigned_64 (P - 1) * 4096;
      end loop;
      VM.Prepare_Update (Candidate, Current, Pages (16#400000#), OK);
      pragma Assert (OK);
      VM.Unmap_Pages (Candidate, 4096, Data, OK); pragma Assert (OK);
      -- Adoption must copy the mapped-page count as well as the PTEs.
      -- Otherwise removing all 21 pages can underflow or seal an empty VM.
      VM.Seal (Candidate, OK); pragma Assert (not OK);
      Reject (Candidate);
      VM.Map_Page (Candidate, 4096, 16#3000000#, Write_Back, Read_Write, OK);
      pragma Assert (OK);
      VM.Seal (Candidate, OK); pragma Assert (OK);
      Snapshots.Adopt_Committed (Current, Candidate, OK); pragma Assert (OK);
      pragma Assert (VM.Lookup (Current, 4096) = VM.Lookup (Candidate, 4096));
      for P in 2 .. 21 loop
         pragma Assert (VM.Lookup (Current, Unsigned_64 (P) * 4096) = 0);
      end loop;
      Reject (Candidate);
   end;
   declare
      Live, Ancient : VM.Image;
      type Images is array (1 .. 2) of VM.Image;
      Reusable : Images;
      Old_Revision : Unsigned_64;
   begin
      VM.Initialize (Live, Pages (16#A00000#), OK); pragma Assert (OK);
      VM.Map_Page (Live, 4096, 16#3000000#, Write_Back, Read_Write, OK); pragma Assert (OK);
      VM.Seal (Live, OK); pragma Assert (OK);
      for Generation in 1 .. 128 loop
         declare
            Index : constant Positive := (Generation - 1) mod 2 + 1;
            Base : constant Unsigned_64 := 16#B00000# + Unsigned_64 (Index) * 16#10000#;
         begin
            Old_Revision := VM.Revision (Reusable (Index));
            if Generation > 2 then
               -- Model a confirmed retirement of the superseded allocation.
               -- Live uses the other slot; the stable hardware root is external.
               pragma Assert (VM.Root_DMA (Live) /= Base);
               Snapshots.Forget_Retired (Reusable (Index), Old_Revision, Base, False, OK);
               pragma Assert (not OK and VM.Sealed (Reusable (Index)));
               pragma Assert (not Snapshots.Retired (Reusable (Index)));
               Snapshots.Forget_Retired (Reusable (Index), Old_Revision + 1, Base, True, OK);
               pragma Assert (not OK);
               Snapshots.Forget_Retired (Reusable (Index), Old_Revision, Base + 4096, True, OK);
               pragma Assert (not OK);
               Snapshots.Forget_Retired (Reusable (Index), Old_Revision, Base, True, OK);
               pragma Assert (OK and not VM.Sealed (Reusable (Index)));
               pragma Assert (Snapshots.Retired (Reusable (Index)));
               pragma Assert (VM.Root_DMA (Reusable (Index)) = 0 and VM.Used (Reusable (Index)) = 0);
               pragma Assert (VM.Lookup (Reusable (Index), 4096) = 0);
               for P in VM.Page_Number loop
                  for I in Table_Index loop
                     pragma Assert (VM.Entry_Value (Reusable (Index), P, I) = 0);
                  end loop;
               end loop;
            end if;
            VM.Prepare_Update (Reusable (Index), Live, Pages (Base), OK); pragma Assert (OK);
            pragma Assert (not Snapshots.Retired (Reusable (Index)));
            pragma Assert (VM.Revision (Reusable (Index)) = Old_Revision + 1);
            VM.Seal_Update (Reusable (Index), OK); pragma Assert (OK);
            -- Same DMA address as the retired incarnation: old ack is invalid.
            Snapshots.Forget_Retired (Reusable (Index), Old_Revision, Base, True, OK);
            pragma Assert (not OK and VM.Sealed (Reusable (Index)));
            Snapshots.Adopt_Committed (Live, Reusable (Index), OK); pragma Assert (OK);
            if Generation = 1 then
               VM.Prepare_Update (Ancient, Live, Pages (16#D00000#), OK); pragma Assert (OK);
               VM.Seal_Update (Ancient, OK); pragma Assert (OK);
            elsif Generation > 2 and then Index = 1 then
               -- Root address equals Ancient's predecessor again, but epoch
               -- differs: address-only lineage would incorrectly accept it.
               pragma Assert (not VM.Direct_Successor (Live, Ancient));
               Snapshots.Adopt_Committed (Live, Ancient, OK);
               pragma Assert (not OK and VM.Root_DMA (Live) = Base);
            end if;
         end;
      end loop;
   end;
   Ada.Text_IO.Put_Line ("VM snapshot reuse PASS:128 alternating commits, exact retirement revision, same-address ABA rejection (offline model)");
   Ada.Text_IO.Put_Line ("VM retired receipt PASS: acknowledged disposal only; successful and failed new incarnations remove alias-scan exemption");
   Ada.Text_IO.Put_Line ("VM snapshot PASS: 20 logical commits, stale/replay/unsealed/unrelated rejection; no hardware or backing reclamation");
   Ada.Text_IO.Put_Line ("VM snapshot lifecycle PASS: full unmap, empty-seal rejection, remap and adoption");
end VM_Snapshot_Tests;
