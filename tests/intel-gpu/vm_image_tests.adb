with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Removal;
with Intel_GPU_VM_Image.Snapshots;
with Intel_GPU_PPGTT_Scratch;
with Intel_GPU_VM_Update;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Binding;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Backing;
procedure VM_Image_Tests is
   package VM is new Intel_GPU_VM_Image (32);
   package Snapshots is new VM.Snapshots;
   use VM;
   Backing : Backing_Pages;
   Object : Image;
   OK : Boolean;
   type Addresses is array (Positive range <>) of Unsigned_64;
   type Saved_Page is array (Table_Index) of Unsigned_64;
   type Saved_Image is array (Page_Number) of Saved_Page;
   -- Independent walk through exported page words, not Lookup or Locate.
   function Read_Leaf (GPU : Unsigned_64) return Unsigned_64 is
      DMA : Unsigned_64 := Root_DMA (Object);
      Word : Unsigned_64;
      P : Natural;
      Shift : Natural := 39;
   begin
      for Depth in 1 .. 4 loop
         P := 0;
         for Candidate in 1 .. Used (Object) loop
            if Page_DMA (Object, Candidate) = DMA then P := Candidate; exit; end if;
         end loop;
         pragma Assert (P /= 0);
         Word := Entry_Value (Object, P,
           Natural (Shift_Right (GPU, Shift) and 511));
         if Word = 0 then return 0; end if;
         if Depth = 4 then return Word; end if;
         pragma Assert ((Word and 4095) = 3);
         DMA := Word and not Unsigned_64'(4095);
         Shift := Shift - 9;
      end loop;
      raise Program_Error;
   end Read_Leaf;
   procedure Reject
     (GPU, DMA : Unsigned_64; Mode : Page_Access := Read_Write) is
      Before : Saved_Image;
      Count : constant Natural := Used (Object);
      Root : constant Unsigned_64 := Root_DMA (Object);
   begin
      for P in Page_Number loop
         for I in Table_Index loop Before (P) (I) := Entry_Value (Object, P, I); end loop;
      end loop;
      Map_Page (Object, GPU, DMA, Write_Back, Mode, OK);
      pragma Assert (not OK and Used (Object) = Count and Root_DMA (Object) = Root);
      for P in Page_Number loop
         for I in Table_Index loop
            pragma Assert (Before (P) (I) = Entry_Value (Object, P, I));
         end loop;
      end loop;
   end Reject;
begin
   -- Native removal captures a full reserved backing arena, while Page_DMA
   -- deliberately returns zero for unused tables. Do not reject those slots.
   declare
      Sparse, Empty, Successor : Image;
      Pages, Changed, Next_Pages : Backing_Pages;
   begin
      for P in Page_Number loop Pages (P) := 16#100000# + Unsigned_64 (P) * 4096; end loop;
      pragma Assert (not Used_Tables_Match (Empty, Pages));
      Initialize (Sparse, Pages, OK); pragma Assert (OK);
      pragma Assert (not Used_Tables_Match (Sparse, Pages));
      Map_Page (Sparse, 16#200000#, 16#800000#, Write_Back, Read_Write, OK);
      pragma Assert (OK);
      Seal (Sparse, OK); pragma Assert (OK);
      pragma Assert (Used (Sparse) < Page_Number'Last);
      pragma Assert (Page_DMA (Sparse, Used (Sparse) + 1) = 0);
      pragma Assert (Pages (Used (Sparse) + 1) /= 0);
      pragma Assert (Used_Tables_Match (Sparse, Pages));
      for P in 1 .. Used (Sparse) loop
         Changed := Pages; Changed (P) := Changed (P) + 4096;
         pragma Assert (not Used_Tables_Match (Sparse, Changed));
      end loop;
      -- After replacement-table adoption, only the new allocation matches.
      -- Reserved entries remain nonzero; matching does not mutate revisions.
      for P in Page_Number loop Next_Pages (P) := Pages (P) + 16#100000#; end loop;
      Prepare_Update (Successor, Sparse, Next_Pages, OK); pragma Assert (OK);
      Seal_Update (Successor, OK); pragma Assert (OK);
      Snapshots.Adopt_Committed (Sparse, Successor, OK); pragma Assert (OK);
      pragma Assert (Used_Tables_Match (Sparse, Next_Pages));
      pragma Assert (not Used_Tables_Match (Sparse, Pages));
      declare
         Epoch : constant Unsigned_64 := Revision (Sparse);
      begin
         for Repeat in 1 .. 100 loop
            pragma Assert (Used_Tables_Match (Sparse, Next_Pages));
         end loop;
         pragma Assert (Revision (Sparse) = Epoch);
      end;
      Snapshots.Forget_Retired (Sparse, Revision (Sparse), Root_DMA (Sparse), True, OK);
      pragma Assert (OK and then not Used_Tables_Match (Sparse, Next_Pages));
   end;
   declare
      package Full_VM is new Intel_GPU_VM_Image (4);
      Full : Full_VM.Image;
      Pages : constant Full_VM.Backing_Pages := [16#100000#, 16#101000#, 16#102000#, 16#103000#];
   begin
      Full_VM.Initialize (Full, Pages, OK); pragma Assert (OK);
      Full_VM.Map_Page (Full, 16#200000#, 16#800000#, Write_Back, Read_Write, OK);
      pragma Assert (OK);
      Full_VM.Seal (Full, OK); pragma Assert (OK);
      pragma Assert (Full_VM.Used (Full) = 4 and then Full_VM.Used_Tables_Match (Full, Pages));
   end;
   -- Cache policy is a property of backing, not just of one GPU alias.
   -- Reject conflicts atomically, including at the end of a multi-page map.
   for Existing in Cache_Policy loop
      for Requested in Cache_Policy loop
         declare
            Alias_Image : Image;
            Before : Saved_Image;
            Count : Natural;
            Pool : Backing_Pages;
         begin
            for P in Page_Number loop Pool (P) := Unsigned_64 (P) * 4096; end loop;
            Initialize (Alias_Image, Pool, OK); pragma Assert (OK);
            Map_Page (Alias_Image, 4096, 16#100000#, Existing, Read_Write, OK);
            pragma Assert (OK);
            Count := Used (Alias_Image);
            for P in Page_Number loop
               for I in Table_Index loop
                  Before (P) (I) := Entry_Value (Alias_Image, P, I);
               end loop;
            end loop;
            Map_Pages (Alias_Image, 2 ** 39,
              [16#200000#, 16#100000#], Requested, Read_Write, OK);
            pragma Assert (OK = (Existing = Requested));
            if not OK then
               pragma Assert (Used (Alias_Image) = Count);
               for P in Page_Number loop
                  for I in Table_Index loop
                     pragma Assert (Entry_Value (Alias_Image, P, I) = Before (P) (I));
                  end loop;
               end loop;
            else
               pragma Assert (Lookup (Alias_Image, 2 ** 39 + 4096) =
                 Encode_Leaf (16#100000#, Existing, Read_Write));
            end if;
         end;
      end loop;
   end loop;
   for P in Page_Number loop Backing (P) := Unsigned_64 (P) * 4096; end loop;
   Map_Page (Object, 4096, 16#100000#, Write_Back, Read_Write, OK);
   pragma Assert (not OK);
   Seal (Object, OK); pragma Assert (not OK);
   Initialize (Object, Backing, OK); pragma Assert (OK and Used (Object) = 1);
   Seal (Object, OK); pragma Assert (not OK and not Sealed (Object));
   -- Reject aliases to every reserved table page, including unused capacity.
   for DMA of Backing loop Reject (4096, DMA); end loop;
   for GPU of Addresses'[0, 1, 4095, 2 ** 48, Unsigned_64'Last] loop
      Reject (GPU, 16#100000#);
   end loop;
   for DMA of Addresses'[0, 1, 4095, 2 ** 32, Unsigned_64'Last] loop
      Reject (4096, DMA);
   end loop;
   Reject (4096, 16#100000#, Read_Only);
   -- Both sides of each paging boundary, including upper raw48 addresses.
   for GPU of Addresses'[4096, 2 ** 21 - 4096, 2 ** 21,
                         2 ** 30 - 4096, 2 ** 30,
                         2 ** 39 - 4096, 2 ** 39,
                         2 ** 47, 2 ** 48 - 4096] loop
      Map_Page (Object, GPU, 16#100000#, Uncached, Read_Write, OK);
      pragma Assert (OK and Read_Leaf (GPU) = 16#10001B#);
      pragma Assert (Lookup (Object, GPU + 17) = Read_Leaf (GPU));
      Reject (GPU, 16#200000#);
   end loop;
   pragma Assert (Read_Leaf (8192) = 0);
   pragma Assert (Read_Leaf (2 ** 47 + 4096) = 0);
   -- A fully populated leaf needs no further directory allocations.
   declare Before : constant Natural := Used (Object); begin
      for I in 2 .. 510 loop
         Map_Page (Object, Unsigned_64 (I) * 4096, 16#200000#,
                   Write_Combining, Read_Write, OK);
         pragma Assert (OK and Read_Leaf (Unsigned_64 (I) * 4096) = 16#20000B#);
      end loop;
      pragma Assert (Used (Object) = Before);
   end;
   Initialize (Object, Backing, OK); pragma Assert (not OK);
   Seal (Object, OK); pragma Assert (OK and Sealed (Object));
   Reject (8192 * 512, 16#300000#);
   Seal (Object, OK); pragma Assert (OK);
   -- Exhaustion cannot leak directories or damage already mapped data.
   declare
      package Tiny is new Intel_GPU_VM_Image (4);
      Small : Tiny.Image;
      Pages : constant Tiny.Backing_Pages := [4096, 8192, 12288, 16384];
   begin
      Tiny.Initialize (Small, Pages, OK); pragma Assert (OK);
      Tiny.Map_Page (Small, 4096, 16#100000#, Write_Back, Read_Write, OK);
      pragma Assert (OK and Tiny.Used (Small) = 4);
      for GPU of Addresses'[2 ** 21, 2 ** 30, 2 ** 39, 2 ** 47] loop
         Tiny.Map_Page (Small, GPU, 16#200000#, Write_Back, Read_Write, OK);
         pragma Assert (not OK and Tiny.Used (Small) = 4);
         pragma Assert (Tiny.Lookup (Small, 4096) = 16#100003#);
         pragma Assert (Tiny.Lookup (Small, GPU) = 0);
      end loop;
      Tiny.Map_Page (Small, 8192, 16#200000#, Write_Back, Read_Write, OK);
      pragma Assert (OK);
   end;
   -- With two pages left, a new PML4 branch needs three and must not consume
   -- either page. A subsequent PDP branch needs exactly two and still fits.
   declare
      package Six is new Intel_GPU_VM_Image (6);
      Small : Six.Image;
      Pages : constant Six.Backing_Pages := [4096, 8192, 12288, 16384, 20480, 24576];
   begin
      Six.Initialize (Small, Pages, OK); pragma Assert (OK);
      Six.Map_Page (Small, 4096, 16#100000#, Write_Back, Read_Write, OK);
      pragma Assert (OK and Six.Used (Small) = 4);
      Six.Map_Page (Small, 2 ** 39, 16#200000#, Write_Back, Read_Write, OK);
      pragma Assert (not OK and Six.Used (Small) = 4);
      pragma Assert (Six.Entry_Value (Small, 1, 1) = 0);
      Six.Map_Page (Small, 2 ** 30, 16#200000#, Write_Through, Read_Write, OK);
      pragma Assert (OK and Six.Used (Small) = 6);
      pragma Assert (Six.Lookup (Small, 2 ** 30) = 16#200013#);
   end;
   -- Exercise every index in each level without relying on the driver's
   -- Locate helper to calculate the expected words.
   for Shift of Addresses'[39, 30, 21, 12] loop
      for Index in Table_Index loop
         declare
            package Four is new Intel_GPU_VM_Image (4);
            Single : Four.Image;
            Pages : constant Four.Backing_Pages := [4096, 8192, 12288, 16384];
            GPU : constant Unsigned_64 :=
              Shift_Left (Unsigned_64 (Index), Natural (Shift)) +
              (if Shift = 12 and Index /= 0 then 0 else 4096);
            Bits : Natural := 39;
         begin
            Four.Initialize (Single, Pages, OK); pragma Assert (OK);
            Four.Map_Page (Single, GPU, 16#100000#, Write_Back, Read_Write, OK);
            pragma Assert (OK and Four.Used (Single) = 4);
            for P in 1 .. 4 loop
               for I in Table_Index loop
                  pragma Assert (Four.Entry_Value (Single, P, I) =
                    (if I /= Natural (Shift_Right (GPU, Bits) and 511) then 0
                     elsif P = 4 then 16#100003# else Pages (P + 1) + 3));
               end loop;
               if P < 4 then Bits := Bits - 9; end if;
            end loop;
         end;
      end loop;
   end loop;
   -- Invalid pool initialization never becomes usable or retries in-place.
   for P in Page_Number loop
      declare Bad : Backing_Pages := Backing; Broken : Image; begin
         Bad (P) := 0;
         Initialize (Broken, Bad, OK); pragma Assert (not OK);
         Initialize (Broken, Backing, OK); pragma Assert (not OK);
         pragma Assert (Used (Broken) = 0 and Root_DMA (Broken) = 0);
      end;
      for Q in Page_Number loop
         if P /= Q then
            declare Bad : Backing_Pages := Backing; Broken : Image; begin
               Bad (P) := Bad (Q);
               Initialize (Broken, Bad, OK); pragma Assert (not OK);
            end;
         end if;
      end loop;
   end loop;
   -- Whole-buffer transactions cross every directory boundary, with arbitrary
   -- array lower bounds and physically discontiguous pages.
   for Boundary of Addresses'[2 ** 21, 2 ** 30, 2 ** 39, 2 ** 47] loop
      declare
         Range_Image : Image;
         Pages : constant Data_Pages (7 .. 9) :=
           [16#300000#, 16#100000#, 16#200000#];
      begin
         Initialize (Range_Image, Backing, OK); pragma Assert (OK);
         Map_Pages (Range_Image, Boundary - 4096, Pages,
                    Write_Back, Read_Write, OK);
         pragma Assert (OK);
         for I in Pages'Range loop
            pragma Assert (Lookup (Range_Image,
              Boundary - 4096 + Unsigned_64 (I - Pages'First) * 4096) = Pages (I) + 3);
         end loop;
         pragma Assert (Lookup (Range_Image, Boundary - 8192) = 0);
         pragma Assert (Lookup (Range_Image, Boundary + 8192) = 0);
      end;
   end loop;
   -- Reject a bad DMA page at EVERY position, including after a directory
   -- boundary. Existing mappings and every exported table word stay intact.
   for Bad_Index in 1 .. 513 loop
      declare
         Range_Image : Image;
         Pages : Data_Pages (1 .. 513) := [others => 16#300000#];
         Before : Saved_Image;
         Count : Natural;
      begin
         Initialize (Range_Image, Backing, OK); pragma Assert (OK);
         Map_Page (Range_Image, 2 ** 39, 16#100000#, Write_Back, Read_Write, OK);
         pragma Assert (OK);
         Count := Used (Range_Image);
         for P in Page_Number loop
            for I in Table_Index loop
               Before (P) (I) := Entry_Value (Range_Image, P, I);
            end loop;
         end loop;
         Pages (Bad_Index) := Backing (32);
         Map_Pages (Range_Image, 4096, Pages, Write_Back, Read_Write, OK);
         pragma Assert (not OK and Used (Range_Image) = Count);
         for P in Page_Number loop
            for I in Table_Index loop
               pragma Assert (Entry_Value (Range_Image, P, I) = Before (P) (I));
            end loop;
         end loop;
         Pages (Bad_Index) := 16#300000#;
         Map_Pages (Range_Image, 4096, Pages, Write_Back, Read_Write, OK);
         pragma Assert (OK and Used (Range_Image) = Count + 4);
         for I in Pages'Range loop
            pragma Assert (Lookup (Range_Image, Unsigned_64 (I) * 4096) = 16#300003#);
         end loop;
      end;
   end loop;
   declare
      Range_Image : Image;
      Empty : Data_Pages (1 .. 0);
      Before : Saved_Image;
      Count : Natural;
   begin
      Initialize (Range_Image, Backing, OK); pragma Assert (OK);
      Map_Page (Range_Image, 2 ** 21, 16#100000#, Write_Back, Read_Write, OK);
      pragma Assert (OK);
      Count := Used (Range_Image);
      for P in Page_Number loop
         for I in Table_Index loop Before (P) (I) := Entry_Value (Range_Image, P, I); end loop;
      end loop;
      -- Collision on the last page must not publish the earlier free page.
      Map_Pages (Range_Image, 2 ** 21 - 4096,
                 [16#200000#, 16#300000#], Write_Back, Read_Write, OK);
      pragma Assert (not OK);
      Map_Pages (Range_Image, 4096, Empty, Write_Back, Read_Write, OK);
      pragma Assert (not OK);
      Map_Pages (Range_Image, 2 ** 48 - 4096,
                 [16#200000#, 16#300000#], Write_Back, Read_Write, OK);
      pragma Assert (not OK and Used (Range_Image) = Count);
      for P in Page_Number loop
         for I in Table_Index loop
            pragma Assert (Entry_Value (Range_Image, P, I) = Before (P) (I));
         end loop;
      end loop;
      Map_Pages (Range_Image, 2 ** 48 - 8192,
                 [16#200000#, 16#300000#], Write_Back, Read_Write, OK);
      pragma Assert (OK and Lookup (Range_Image, 2 ** 48 - 4096) = 16#300003#);
   end;
   declare
      package Four is new Intel_GPU_VM_Image (4);
      Range_Image : Four.Image;
      Pages : constant Four.Backing_Pages := [4096, 8192, 12288, 16384];
   begin
      Four.Initialize (Range_Image, Pages, OK); pragma Assert (OK);
      Four.Map_Pages (Range_Image, 2 ** 21 - 4096,
                      [16#100000#, 16#200000#], Write_Back, Read_Write, OK);
      pragma Assert (not OK and Four.Used (Range_Image) = 1);
      for I in Table_Index loop
         pragma Assert (Four.Entry_Value (Range_Image, 1, I) = 0);
      end loop;
      Four.Map_Pages (Range_Image, 4096,
                      [16#100000#, 16#200000#], Write_Back, Read_Write, OK);
      pragma Assert (OK and Four.Used (Range_Image) = 4);
      -- Full directory capacity must not prevent teardown. Clone the sealed
      -- source just as a live update does, then churn existing leaf routes.
      -- This tests the image builder, not GPU/TLB retirement or service IPC.
      Four.Seal (Range_Image, OK); pragma Assert (OK);
      declare
         Candidate : Four.Image;
         Fresh : constant Four.Backing_Pages :=
           [16#10000#, 16#11000#, 16#12000#, 16#13000#];
         Data : constant Four.Data_Pages := [16#100000#, 16#200000#];
      begin
         Four.Prepare_Update (Candidate, Range_Image, Fresh, OK);
         pragma Assert (OK and Four.Used (Candidate) = 4);
         for Cycle in 1 .. 256 loop
            Four.Unmap_Pages (Candidate, 4096, Data, OK);
            pragma Assert (OK and Four.Used (Candidate) = 4);
            pragma Assert (Four.Lookup (Candidate, 4096) = 0);
            pragma Assert (Four.Lookup (Candidate, 8192) = 0);
            -- A different directory really does exhaust this four-page
            -- builder, without poisoning reuse of the existing directory.
            Four.Map_Page (Candidate, 2 ** 21, 16#300000#,
                           Write_Back, Read_Write, OK);
            pragma Assert (not OK and Four.Used (Candidate) = 4);
            Four.Map_Pages (Candidate, 4096, Data, Write_Back, Read_Write, OK);
            pragma Assert (OK and Four.Used (Candidate) = 4);
            pragma Assert (Four.Lookup (Candidate, 4096) = 16#100003#);
            pragma Assert (Four.Lookup (Candidate, 8192) = 16#200003#);
         end loop;
         Four.Unmap_Pages (Candidate, 4096, Data, OK); pragma Assert (OK);
         Four.Seal_Update (Candidate, OK); pragma Assert (OK);
         pragma Assert (Four.Lookup (Range_Image, 4096) = 16#100003#);
         pragma Assert (Four.Lookup (Range_Image, 8192) = 16#200003#);
      end;
   end;
   for Policy in Cache_Policy loop
      declare
         package Small is new Intel_GPU_VM_Image (5);
         Image : Small.Image;
         Target : Unsigned_64 := 20480;
         function Conflict (Page : Unsigned_64) return Boolean is
           (Page = Target or else Page = 16#300000#);
         function Disjoint is new Small.Backing_Disjoint (Conflict);
      begin
         pragma Assert (not Disjoint (Image));
         pragma Assert (not Small.DMA_Disjoint (Image, 16#100000#, 4096));
         Small.Initialize (Image, [4096, 8192, 12288, 16384, 20480], OK);
         pragma Assert (OK);
         -- Unused reserved backing must also be excluded.
         pragma Assert (not Small.DMA_Disjoint (Image, 20480, 4096));
         pragma Assert (not Disjoint (Image));
         Target := 16#100000#;
         pragma Assert (Disjoint (Image));
         Small.Map_Page (Image, 4096, 16#100000#, Policy, Read_Write, OK);
         pragma Assert (OK);
         pragma Assert (not Disjoint (Image));
         Target := 16#200000#;
         pragma Assert (Disjoint (Image));
         pragma Assert (not Small.DMA_Disjoint (Image, 16#100000#, 4096));
         pragma Assert (not Small.DMA_Disjoint (Image, 16#FF000#, 8192));
         pragma Assert (Small.DMA_Disjoint (Image, 16#FF000#, 4096));
         pragma Assert (Small.DMA_Disjoint (Image, 16#101000#, 4096));
         pragma Assert (Small.DMA_Disjoint (Image, 2 ** 32 - 4096, 4096));
         pragma Assert (not Small.DMA_Disjoint (Image, 2 ** 32 - 4096, 8192));
         pragma Assert (not Small.DMA_Disjoint (Image, 0, 4096));
         pragma Assert (not Small.DMA_Disjoint (Image, 16#101001#, 4096));
         pragma Assert (not Small.DMA_Disjoint (Image, 16#101000#, 0));
         pragma Assert (not Small.DMA_Disjoint (Image, 16#101000#, 4095));
         pragma Assert (not Small.DMA_Disjoint (Image, 16#101000#, Unsigned_64'Last));
      end;
   end loop;
   -- Unmap is a whole-range transaction on an unpublished image. A stale
   -- owner expectation at any position must preserve every table word.
   for Boundary of Addresses'[2 ** 21, 2 ** 30, 2 ** 39, 2 ** 48 - 4096] loop
      for Policy in Cache_Policy loop
         declare
            U : Image;
            Data : constant Data_Pages := [7 => 16#100000#, 8 => 16#200000#, 9 => 16#300000#];
            Wrong : Data_Pages (Data'Range);
            Before : Saved_Image;
            GPU : constant Unsigned_64 := Boundary - 8192;
            Count : Natural;
         begin
            Initialize (U, Backing, OK); pragma Assert (OK);
            Map_Pages (U, GPU, Data, Policy, Read_Write, OK); pragma Assert (OK);
            Count := Used (U);
            for P in Page_Number loop
               for I in Table_Index loop Before (P) (I) := Entry_Value (U, P, I); end loop;
            end loop;
            for Bad in Data'Range loop
               Wrong := Data; Wrong (Bad) := 16#400000#;
               Unmap_Pages (U, GPU, Wrong, OK); pragma Assert (not OK);
               pragma Assert (Used (U) = Count);
               for P in Page_Number loop
                  for I in Table_Index loop
                     pragma Assert (Before (P) (I) = Entry_Value (U, P, I));
                  end loop;
               end loop;
            end loop;
            Unmap_Pages (U, GPU + 1, Data, OK); pragma Assert (not OK);
            Unmap_Pages (U, 2 ** 48 - 4096, Data, OK); pragma Assert (not OK);
            Unmap_Pages (U, GPU, Data_Pages'(1 .. 0 => 0), OK); pragma Assert (not OK);
            Unmap_Pages (U, GPU, Data, OK); pragma Assert (OK and Used (U) = Count);
            for I in Data'Range loop
               pragma Assert (Lookup (U, GPU + Unsigned_64 (I - Data'First) * 4096) = 0);
               pragma Assert (DMA_Disjoint (U, Data (I), 4096));
            end loop;
            Seal (U, OK); pragma Assert (not OK);
            Unmap_Pages (U, GPU, Data, OK); pragma Assert (not OK);
            -- Empty directories remain reusable; no extra table allocation.
            Map_Pages (U, GPU, Data, Policy, Read_Write, OK);
            pragma Assert (OK and Used (U) = Count);
            -- Remove a subset; the remaining leaf still permits sealing.
            Unmap_Pages (U, GPU, Data_Pages'[Data (7), Data (8)], OK);
            pragma Assert (OK and Lookup (U, GPU + 8192) /= 0);
            Seal (U, OK); pragma Assert (OK);
            Unmap_Pages (U, GPU + 8192, Data_Pages'[Data (9)], OK);
            pragma Assert (not OK and Lookup (U, GPU + 8192) /= 0);
         end;
      end loop;
   end loop;
   declare
      U : Image;
   begin
      Unmap_Pages (U, 4096, Data_Pages'[16#100000#], OK);
      pragma Assert (not OK);
      Initialize (U, Backing, OK); pragma Assert (OK);
      Map_Pages (U, 4096, Data_Pages'[16#100000#, 16#100000#], Write_Back, Read_Write, OK);
      pragma Assert (OK);
      Unmap_Pages (U, 4096, Data_Pages'[16#100000#], OK); pragma Assert (OK);
      pragma Assert (Lookup (U, 4096) = 0 and Lookup (U, 8192) = 16#100003#);
      pragma Assert (not DMA_Disjoint (U, 16#100000#, 4096));
      Unmap_Pages (U, 8192, Data_Pages'[16#100000#], OK); pragma Assert (OK);
      pragma Assert (DMA_Disjoint (U, 16#100000#, 4096));
      Seal (U, OK); pragma Assert (not OK);
   end;
   declare
      Source, Target, Third : Image;
      Fresh, Newer : Backing_Pages;
      Snapshot : Saved_Image;
      Locations : constant Addresses := [4096, 2 ** 21, 2 ** 30, 2 ** 39, 2 ** 48 - 4096];
      procedure Reject_Update (Pages : Backing_Pages) is
         Bad : Image;
      begin
         Prepare_Update (Bad, Source, Pages, OK);
         pragma Assert (not OK and Used (Bad) = 0 and Root_DMA (Bad) = 0);
         for P in Page_Number loop
            for I in Table_Index loop pragma Assert (Entry_Value (Bad, P, I) = 0); end loop;
         end loop;
         -- Failed candidates cannot be retried as if no attempt occurred.
         Prepare_Update (Bad, Source, Fresh, OK); pragma Assert (not OK);
      end Reject_Update;
   begin
      for P in Page_Number loop
         Fresh (P) := 16#400000# + Unsigned_64 (P) * 4096;
         Newer (P) := 16#800000# + Unsigned_64 (P) * 4096;
      end loop;
      Initialize (Source, Backing, OK); pragma Assert (OK);
      Reject_Update (Fresh); -- source is not sealed
      for GPU of Locations loop
         Map_Page (Source, GPU, 16#100000#, Write_Back, Read_Write, OK);
         pragma Assert (OK);
      end loop;
      Seal (Source, OK); pragma Assert (OK);
      for P in Page_Number loop
         for I in Table_Index loop Snapshot (P) (I) := Entry_Value (Source, P, I); end loop;
      end loop;
      Reject_Update (Backing); -- includes reserved source tables
      for Position in Page_Number loop
         declare Bad : Backing_Pages := Fresh; begin
            Bad (Position) := 16#100000#; Reject_Update (Bad); -- data alias
            Bad := Fresh; Bad (Position) := 1; Reject_Update (Bad);
         end;
      end loop;
      declare Bad : Backing_Pages := Fresh; begin
         Bad (Bad'Last) := Bad (Bad'First); Reject_Update (Bad);
      end;
      Prepare_Update (Target, Source, Fresh, OK);
      pragma Assert (OK and not Sealed (Target) and Used (Target) = Used (Source));
      pragma Assert (Root_DMA (Target) = Fresh (1));
      for GPU of Locations loop
         pragma Assert (Lookup (Target, GPU) = Lookup (Source, GPU));
      end loop;
      -- Mutable candidate diverges without touching any source word.
      Unmap_Pages (Target, 4096, Data_Pages'[16#100000#], OK); pragma Assert (OK);
      Map_Page (Target, 4096, 16#200000#, Write_Back, Read_Write, OK); pragma Assert (OK);
      for P in Page_Number loop
         for I in Table_Index loop pragma Assert (Entry_Value (Source, P, I) = Snapshot (P) (I)); end loop;
      end loop;
      Seal (Target, OK); pragma Assert (OK);
      Prepare_Update (Third, Target, Newer, OK); pragma Assert (OK);
      pragma Assert (Lookup (Third, 4096) = 16#200003#);
      pragma Assert (Lookup (Source, 4096) = 16#100003#);
      pragma Assert (Root_DMA (Third) = Newer (1));
      Prepare_Update (Target, Source, Fresh, OK); pragma Assert (not OK);
   end;
   for Fault in 0 .. 5 loop
      declare
         T : Image;
         Hardware : Saved_Image;
         Writes, Invalidations : Natural := 0;
         Held : Boolean := True;
         procedure Reenter;
         function Exclusive return Boolean is (Held);
         procedure Write_Leaf
           (Table_DMA : Unsigned_64; Index : Table_Index;
            Expected, Replacement : Unsigned_64; Success : out Boolean) is
         begin
            Writes := Writes + 1;
            Success := False;
            if Fault = Writes then return; end if;
            for P in Page_Number loop
               if Page_DMA (T, P) = Table_DMA then
                  pragma Assert (Hardware (P) (Index) = Expected);
                  Hardware (P) (Index) := Replacement;
                  Success := True;
                  return;
               end if;
            end loop;
         end Write_Leaf;
         procedure Invalidate (Success : out Boolean) is
         begin
            Invalidations := Invalidations + 1;
            -- Metadata must not claim removal before completed invalidation.
            pragma Assert (Lookup (T, 4096) /= 0 and Lookup (T, 8192) /= 0);
            if Fault = 5 then Reenter; end if;
            Success := Fault /= 3;
            if Fault = 4 then Held := False; end if;
         end Invalidate;
         package Removal is new VM.Removal (Exclusive, Write_Leaf, Invalidate);
         State : Removal.Controller;
         Epoch, Root : Unsigned_64;
         procedure Reenter is
            Nested : Boolean;
         begin
            Removal.Commit (State, T, True, Nested);
            pragma Assert (not Nested and Revision (T) = Epoch);
            pragma Assert (Lookup (T, 4096) /= 0 and Lookup (T, 8192) /= 0);
         end Reenter;
      begin
         Initialize (T, Backing, OK); pragma Assert (OK);
         Map_Pages (T, 4096, Data_Pages'[16#900000#, 16#A00000#],
           Write_Back, Read_Write, OK); pragma Assert (OK);
         Seal (T, OK); pragma Assert (OK);
         Epoch := Revision (T); Root := Root_DMA (T);
         for P in Page_Number loop
            for I in Table_Index loop Hardware (P) (I) := Entry_Value (T, P, I); end loop;
         end loop;
         Removal.Execute (State, T, Epoch, 4096,
           Data_Pages'[16#900000#, 16#B00000#], OK);
         pragma Assert (not OK and Writes = 0 and not Removal.Failed (State));
         Removal.Execute (State, T, Epoch + 1, 4096,
           Data_Pages'[16#900000#, 16#A00000#], OK);
         pragma Assert (not OK and Writes = 0);
         Removal.Execute (State, T, Epoch, 4096,
           Data_Pages'[16#900000#, 16#A00000#], OK);
         pragma Assert (OK = (Fault = 0));
         pragma Assert (Root_DMA (T) = Root and Sealed (T) and Used (T) = 4);
         if Fault = 0 then
            pragma Assert (Revision (T) = Epoch + 1 and Lookup (T, 4096) = 0
              and Lookup (T, 8192) = 0 and Writes = 2 and Invalidations = 1);
         else
            pragma Assert (Removal.Failed (State) and Revision (T) = Epoch
              and Lookup (T, 4096) /= 0 and Lookup (T, 8192) /= 0);
            declare
               Before : constant Natural := Writes;
            begin
               Held := True;
               Removal.Execute (State, T, Epoch, 4096,
                 Data_Pages'[16#900000#, 16#A00000#], OK);
               pragma Assert (not OK and Writes = Before);
            end;
         end if;
      end;
   end loop;
   declare
      T : Image;
      Hardware : Saved_Image;
      Scratch : constant Intel_GPU_PPGTT_Scratch.Backing_Pages :=
        [16#D00000#, 16#D01000#, 16#D02000#, 16#D03000#];
      Writes : Natural := 0;
      Held : Boolean := False;
      function Exclusive return Boolean is (Held);
      procedure Write_Leaf
        (Table_DMA : Unsigned_64; Index : Table_Index;
         Expected, Replacement : Unsigned_64; Success : out Boolean) is
      begin
         Writes := Writes + 1;
         Success := False;
         pragma Assert (Replacement = Intel_GPU_PPGTT_Scratch.Fallback (Scratch, 0));
         for P in Page_Number loop
            if Page_DMA (T, P) = Table_DMA then
               pragma Assert (Hardware (P) (Index) = Expected);
               Hardware (P) (Index) := Replacement;
               Success := True;
               return;
            end if;
         end loop;
      end Write_Leaf;
      procedure Invalidate (Success : out Boolean) is
      begin Success := True; end Invalidate;
      package Removal is new VM.Removal (Exclusive, Write_Leaf, Invalidate);
      State : Removal.Controller;
      Count : Natural;
   begin
      Initialize (T, Backing, OK, Scratch); pragma Assert (OK);
      Map_Pages (T, 16#1FF000#, Data_Pages'[16#900000#, 16#A00000#, 16#B00000#],
        Write_Back, Read_Write, OK); pragma Assert (OK);
      Seal (T, OK); pragma Assert (OK);
      Count := Used (T);
      for P in Page_Number loop
         for I in Table_Index loop Hardware (P) (I) := Entry_Value (T, P, I); end loop;
      end loop;
      Removal.Execute (State, T, Revision (T), 16#1FF000#,
        Data_Pages'[16#900000#, 16#A00000#], OK);
      pragma Assert (not OK and Writes = 0 and not Removal.Failed (State));
      Held := True;
      Removal.Execute (State, T, Revision (T), 2 ** 48 - 4096,
        Data_Pages'[16#900000#, 16#A00000#], OK);
      pragma Assert (not OK and Writes = 0);
      Removal.Execute (State, T, Revision (T), 16#1FF000#,
        Data_Pages'[16#900000#, 16#A00000#], OK);
      pragma Assert (OK and Writes = 2 and Used (T) = Count);
      pragma Assert (Lookup (T, 16#1FF000#) = 0 and Lookup (T, 16#200000#) = 0
        and Lookup (T, 16#201000#) = 16#B00003#);
      for P in Page_Number loop
         for I in Table_Index loop
            pragma Assert (Hardware (P) (I) = Entry_Value (T, P, I));
         end loop;
      end loop;
   end;
   -- Reusing a successfully committed controller must discard the old route,
   -- even for a new image with identical root/revision but different topology.
   declare
      Expected_Table : Unsigned_64;
      Writes : Natural := 0;
      function Exclusive return Boolean is (True);
      procedure Write_Leaf
        (Table_DMA : Unsigned_64; Index : Table_Index;
         Expected, Replacement : Unsigned_64; Success : out Boolean) is
      begin
         pragma Assert (Table_DMA = Expected_Table and Index = 1);
         pragma Assert (Expected = Encode_Leaf (16#900000#, Write_Back, Read_Write));
         pragma Assert (Replacement = 0);
         Writes := Writes + 1; Success := True;
      end Write_Leaf;
      procedure Invalidate (Success : out Boolean) is
      begin Success := True; end Invalidate;
      package Removal is new VM.Removal (Exclusive, Write_Leaf, Invalidate);
      State : Removal.Controller;
   begin
      for Layout in 1 .. 2 loop
         declare
            T : Image;
         begin
            Initialize (T, Backing, OK); pragma Assert (OK);
            if Layout = 2 then
               Map_Pages (T, 16#200000#, Data_Pages'[16#A00000#],
                 Write_Back, Read_Write, OK); pragma Assert (OK);
            end if;
            Map_Pages (T, 4096, Data_Pages'[16#900000#],
              Write_Back, Read_Write, OK); pragma Assert (OK);
            Seal (T, OK); pragma Assert (OK);
            Expected_Table := Backing (if Layout = 1 then 4 else 5);
            Removal.Execute (State, T, Revision (T), 4096, Data_Pages'[16#900000#], OK);
            pragma Assert (OK and Writes = Layout and Lookup (T, 4096) = 0);
            if Layout = 2 then pragma Assert (Lookup (T, 16#200000#) = 16#A00003#); end if;
         end;
      end loop;
   end;
   -- A late PT descriptor requires more than the32-comparison turn budget.
   for Route_Fault in 0 .. 2 loop
      declare
         T : Image;
         Held : Boolean := True;
         Calls, Writes : Natural := 0;
         GPU : constant Unsigned_64 := 29 * 2 ** 21;
         function Exclusive return Boolean is (Held);
         procedure Write_Leaf
           (Table_DMA : Unsigned_64; Index : Table_Index;
            Expected, Replacement : Unsigned_64; Success : out Boolean) is
         begin
            pragma Assert (Table_DMA = Backing (32) and Index = 0);
            pragma Assert (Expected = Encode_Leaf (16#900000#, Write_Back, Read_Write));
            pragma Assert (Replacement = 0);
            Writes := Writes + 1; Success := True;
         end Write_Leaf;
         procedure Invalidate (Success : out Boolean) is
         begin Success := True; end Invalidate;
         package Removal is new VM.Removal (Exclusive, Write_Leaf, Invalidate);
         State : Removal.Controller;
         function Expected_Page (Ordinal : Positive) return Unsigned_64 is
         begin
            pragma Assert (Ordinal = 1); Calls := Calls + 1; return 16#900000#;
         end Expected_Page;
         procedure Capture is new Removal.Capture_Step (Expected_Page);
      begin
         Initialize (T, Backing, OK); pragma Assert (OK);
         for Region in 1 .. 29 loop
            Map_Pages (T, Unsigned_64 (Region) * 2 ** 21, Data_Pages'[16#900000#],
              Write_Back, Read_Write, OK); pragma Assert (OK);
         end loop;
         Seal (T, OK); pragma Assert (OK and Used (T) = 32);
         Removal.Begin_Prepare (State, T, Revision (T), GPU, 1, OK); pragma Assert (OK);
         Capture (State, T, OK);
         pragma Assert (OK and Removal.Preparing (State) and Calls = 0 and Writes = 0);
         if Route_Fault = 1 then Held := False;
         elsif Route_Fault = 2 then Removal.Cancel_Prepare (State); end if;
         Capture (State, T, OK);
         pragma Assert (OK = (Route_Fault = 0));
         if Route_Fault = 0 then
            pragma Assert (Calls = 1 and Removal.Publishing (State));
            Removal.Step (State, T);
            pragma Assert (Writes = 1 and Removal.Published (State));
            Removal.Commit (State, T, True, OK);
            pragma Assert (OK and Lookup (T, GPU) = 0);
            pragma Assert (Lookup (T, 28 * 2 ** 21) = 16#900003#);
         else
            pragma Assert (Calls = 0 and Writes = 0 and not Removal.Publishing (State));
            pragma Assert (Lookup (T, GPU) = 16#900003#);
         end if;
      end;
   end loop;
   for Fail_Invalidate in Boolean loop
      declare
         T : Image;
         Held : Boolean := True;
         Invalidated : Boolean := False;
         Writes : Natural := 0;
         function Exclusive return Boolean is (Held);
         procedure Write_Leaf
           (Table_DMA : Unsigned_64; Index : Table_Index;
            Expected, Replacement : Unsigned_64; Success : out Boolean) is
         begin
            pragma Assert (Table_DMA /= 0 and Index = 1 and Expected = 16#900003#
              and Replacement = 0 and not Invalidated);
            Writes := Writes + 1;
            Success := True;
         end Write_Leaf;
         procedure Invalidate (Success : out Boolean) is
         begin
            pragma Assert (Lookup (T, 4096) = 16#900003#);
            Invalidated := not Fail_Invalidate;
            Success := Invalidated;
         end Invalidate;
         package Removal is new VM.Removal (Exclusive, Write_Leaf, Invalidate);
         R : Removal.Controller;
         procedure Drain (Success : out Boolean) is
         begin Success := Held; end Drain;
         procedure Publish (Success : out Boolean) is
         begin
            Removal.Publish (R, T, Revision (T), 4096,
              Data_Pages'[16#900000#], Success);
         end Publish;
         procedure Resume (Success : out Boolean) is
         begin Removal.Commit (R, T, Invalidated, Success); end Resume;
         package Coordinator is new Intel_GPU_VM_Update
           (Exclusive, Drain, Publish, Invalidate, Resume);
         State : Coordinator.State;
         Result : Coordinator.Result;
         use type Coordinator.Result;
         function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64 is
           (if Sender = 42 and Stamp = 99 then 99 else 0);
         package Buffers is new Intel_GPU_Buffer_Requests (Session_Of, Exclusive);
         package Binding is new Buffers.Binding (VM);
         Service : Buffers.Service;
         Ticket : Buffers.Ticket;
         Reply, Request : Buffers.Words;
         Captures : Natural := 0;
         procedure Capture
           (Backing : Intel_GPU_Buffer_Reply.Backing;
            GPU, Offset, Bytes, Revision : Unsigned_64; Accepted : out Boolean) is
         begin
            Captures := Captures + 1;
            pragma Assert (Backing.Ready and GPU = 4096 and Offset = 0 and Bytes = 4096
              and Revision = VM.Revision (T)
              and Intel_GPU_Buffer_Reply.Page_Address (Backing, 0) = 16#900000#);
            Accepted := True;
         end Capture;
         procedure Handle_Removal is new Binding.Handle_In_Place (Coordinator, True, Capture);
      begin
         Initialize (T, Backing, OK); pragma Assert (OK);
         Map_Page (T, 4096, 16#900000#, Write_Back, Read_Write, OK); pragma Assert (OK);
         Seal (T, OK); pragma Assert (OK);
         Buffers.Handle (Service, 42, 99, Buffers.Label, 4, 0, 0,
           [1, Buffers.Create, 4096, 0], Reply, Ticket);
         pragma Assert (Ticket /= 0);
         Buffers.Complete (Service, Ticket, Intel_GPU_Buffer_Reply.From_Linear
           (16#900000#, Intel_GPU_Buffer_Backing.CPU_Base, 4096, 16#900000#), Reply, OK);
         pragma Assert (OK and Reply (0) = Buffers.OK);
         Request := [16#10001#, Reply (2), 4096, 4096];
         Handle_Removal (Service, T, State, 99, 43, 99, Binding.Update_Label,
           4, 0, 0, Request, Reply);
         pragma Assert (Reply (0) = Buffers.Denied and Captures = 0 and Writes = 0);
         Request (0) := 1; -- bind must not accidentally enter the removal path
         Handle_Removal (Service, T, State, 99, 42, 99, Binding.Update_Label,
           4, 0, 0, Request, Reply);
         pragma Assert (Reply (0) = Buffers.Bad_Request and Captures = 0);
         Request (0) := 16#10001# + 2 ** 32;
         Handle_Removal (Service, T, State, 99, 42, 99, Binding.Update_Label,
           4, 0, 0, Request, Reply);
         pragma Assert (Reply (0) = Buffers.Unavailable and Captures = 0);
         Request (0) := 16#10001#;
         Coordinator.Execute (State, 1, Result);
         pragma Assert (Result = Coordinator.Rejected and Writes = 0);
         Handle_Removal (Service, T, State, 99, 42, 99, Binding.Update_Label,
           4, 0, 0, Request, Reply);
         pragma Assert (Captures = 1);
         if Fail_Invalidate then
            pragma Assert (Reply (0) = Buffers.Unavailable
              and not Coordinator.Can_Submit (State)
              and Coordinator.Generation (State) = 0 and Lookup (T, 4096) /= 0);
            Removal.Commit (R, T, False, OK);
            pragma Assert (not OK and Removal.Failed (R));
            Removal.Commit (R, T, True, OK);
            pragma Assert (not OK); -- consumed failure receipt cannot replay
         else
            pragma Assert (Reply (0) = Buffers.OK and Reply (2) = 1
              and Coordinator.Can_Submit (State) and Coordinator.Generation (State) = 1
              and Lookup (T, 4096) = 0 and not Removal.Failed (R));
         end if;
         Coordinator.Execute (State, 0, Result);
         pragma Assert (Result = Coordinator.Rejected and Writes = 1);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Allocation-free removal coordinator PASS: wire generation, stale request denial, commit after invalidate, quarantine and non-replay (hosted)");
   Ada.Text_IO.Put_Line ("Allocation-free removal PASS: whole-range preflight, stable tables, scratch fallback across PT boundary, delayed metadata commit, write/invalidation/authority failures quarantined (hosted callbacks)");
   Ada.Text_IO.Put_Line ("VM update candidate PASS: rebased directories, retained leaves, disjoint backing, source immutability and two generations (not GPU published)");
   Ada.Text_IO.Put_Line ("VM unmap PASS: expected backing, atomic rejection, boundaries, aliases, empty seal, remap, sealed denial (offline only)");
   Ada.Text_IO.Put_Line
     ("VM image PASS: independent 4-level walk, 48-bit boundaries, capacity, aliases, atomic rejection and seal (offline only)");
   Ada.Text_IO.Put_Line
     ("VM range PASS: discontiguous DMA, 513 failure positions, collision, overflow and atomic capacity rejection");
end VM_Image_Tests;
