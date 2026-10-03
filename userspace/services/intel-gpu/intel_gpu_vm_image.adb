package body Intel_GPU_VM_Image is
   function Metadata_Capacity (Object : Image) return Positive is
     (Table_Storage.Capacity (Object.Tables));
   procedure Extend_Metadata (Object : in out Image; Base, Bytes : Unsigned_64;
                              Accepted : out Boolean) is
   begin Table_Storage.Extend (Object.Tables, Base, Bytes, Accepted); end Extend_Metadata;
   function Raw_Word (Object : Image; Page : Page_Number;
                      Index : Intel_GPU_ADLN_PPGTT.Table_Index) return Unsigned_64 is
     (Table_Storage.Word (Object.Tables, Page, Index));
   function Raw_Page (Object : Image; Page : Page_Number) return Table_Storage.Page is
      Result : Table_Storage.Page;
   begin
      for I in Intel_GPU_ADLN_PPGTT.Table_Index loop Result (I) := Raw_Word (Object, Page, I); end loop;
      return Result;
   end Raw_Page;
   procedure Set_Raw_Word (Object : in out Image; Page : Page_Number;
                          Index : Intel_GPU_ADLN_PPGTT.Table_Index; Value : Unsigned_64) is
   begin Table_Storage.Set_Word (Object.Tables, Page, Index, Value); end Set_Raw_Word;
   procedure Clear_Table (Object : in out Image; Page : Page_Number) is
   begin Table_Storage.Clear (Object.Tables, Page); end Clear_Table;
   function Leaf_Table (Object : Image; Page : Page_Number) return Boolean is
     (Object.Valid and then Page <= Object.Count and then Object.Levels (Page) = 0);
   function Revision (Object : Image) return Unsigned_64 is (Object.Epoch);
   function Direct_Successor (Object, Candidate : Image) return Boolean is
     (Object.Valid and then Object.Frozen and then Candidate.Valid and then
      Candidate.Frozen and then Candidate.Predecessor_Root /= 0 and then
      Candidate.Predecessor_Root = Object.DMA (1) and then
      Candidate.Predecessor_Epoch = Object.Epoch and then
      Candidate.DMA (1) /= Object.DMA (1) and then Object.Epoch /= Unsigned_64'Last);
   use Intel_GPU_ADLN_PPGTT;
   type Path is array (Positive range 1 .. 4) of Table_Index;
   function Indices (GPU : Unsigned_64) return Path is
      W : constant Walk := Locate (GPU);
   begin
      return [W.PML4, W.PDP, W.PD, W.PT];
   end Indices;
   function Child (Object : Image; Entry_Word : Unsigned_64) return Natural is
   begin
      for P in 2 .. Object.Count loop
         if Entry_Word = Encode_Directory (Object.DMA (P)) then return P; end if;
      end loop;
      return 0;
   end Child;
   procedure Initialize
     (Object : in out Image; Backing : Backing_Pages; Accepted : out Boolean;
      Scratch : Intel_GPU_PPGTT_Scratch.Backing_Pages := [others => 0]) is
   begin
      Accepted := False;
      if Object.Attempted or else Object.Epoch = Unsigned_64'Last then return; end if;
      Object.Retired_Receipt := False;
      Object.Epoch := Object.Epoch + 1;
      Object.Attempted := True;
      if (for some Page of Scratch => Page /= 0) and then
        not Intel_GPU_PPGTT_Scratch.Valid (Scratch) then return; end if;
      for P in Page_Number loop
         if not Valid_DMA_Page (Backing (P)) then return; end if;
         if Intel_GPU_PPGTT_Scratch.Contains (Scratch, Backing (P)) then return; end if;
         for Q in Page_Number'First .. P - 1 loop
            if Backing (P) = Backing (Q) then return; end if;
         end loop;
      end loop;
      Object.DMA := Backing;
      Object.Scratch := Scratch;
      Object.Levels (1) := 3;
      Object.Count := 1;
      Object.Valid := True;
      Accepted := True;
   end Initialize;
   procedure Prepare_Update
     (Target : in out Image; Source : Image; Backing : Backing_Pages;
      Accepted : out Boolean) is
      Next : Natural;
   begin
      Accepted := False;
      if Target.Attempted or else Target.Epoch = Unsigned_64'Last or else
        Source.Count > Metadata_Capacity (Target) then return; end if;
      Target.Retired_Receipt := False;
      Target.Epoch := Target.Epoch + 1;
      Target.Attempted := True;
      if not Source.Valid or else not Source.Frozen then return; end if;
      -- Include unused reserved source tables and every mapped data page.
      -- New table storage must not overwrite either generation's data.
      for P in Page_Number loop
         if not DMA_Disjoint (Source, Backing (P), 4096) then return; end if;
         for Q in Page_Number'First .. P - 1 loop
            if Backing (P) = Backing (Q) then return; end if;
         end loop;
      end loop;
      for P in 1 .. Source.Count loop
         for I in Table_Index loop
            -- Validated images exclude table/data aliases, so only directory
            -- entries can identify an allocated child table. Never infer an
            -- entry's role from low flags: WB leaves share directory flags.
            Next := Child (Source, Raw_Word (Source, P, I));
            Set_Raw_Word (Target, P, I,
              (if Next /= 0 then Encode_Directory (Backing (Next))
               else Raw_Word (Source, P, I)));
         end loop;
      end loop;
      Target.DMA := Backing;
      Target.Scratch := Source.Scratch;
      Target.Levels := Source.Levels;
      Target.Count := Source.Count;
      Target.Mapped_Pages := Source.Mapped_Pages;
      Target.Predecessor_Root := Source.DMA (1);
      Target.Predecessor_Epoch := Source.Epoch;
      Target.Valid := True;
      Accepted := True;
   end Prepare_Update;
   procedure Map_Page
     (Object : in out Image; GPU, DMA : Unsigned_64;
      Policy : Cache_Policy; Access_Mode : Page_Access;
      Accepted : out Boolean) is
   begin
      Map_Pages (Object, GPU, [1 => DMA], Policy, Access_Mode, Accepted);
   end Map_Page;
   procedure Map_Pages
     (Object : in out Image; GPU : Unsigned_64; Data : Data_Pages;
      Policy : Cache_Policy; Access_Mode : Page_Access;
      Accepted : out Boolean) is
      Route : Path;
      Current : Page_Number := 1;
      Next : Natural;
      Missing : Natural := 0;
      First_Missing : Natural;
      Address, Prefix : Unsigned_64;
      type Prefixes is array (Positive range 1 .. 3) of Unsigned_64;
      Last_Missing : Prefixes := [others => Unsigned_64'Last];
   begin
      Accepted := False;
      if not Object.Valid or Object.Frozen or GPU = 0 or GPU >= 2 ** 48
        or GPU mod 4096 /= 0 or Data'Length = 0 then return; end if;
      if Unsigned_64 (Data'Length) > (2 ** 48 - GPU) / 4096 then return; end if;
      -- Preflight the complete range, with no table copy or mutation. Since
      -- GPU pages are ascending, each absent directory prefix is counted once
      -- by retaining the previous missing prefix at each level.
      for I in Data'Range loop
         if Encode_Leaf (Data (I), Policy, Access_Mode) = 0 then return; end if;
         if Intel_GPU_PPGTT_Scratch.Contains (Object.Scratch, Data (I)) then return; end if;
         for Page of Object.DMA loop
            if Data (I) = Page then return; end if;
         end loop;
         Address := GPU + Unsigned_64 (I - Data'First) * 4096;
         Route := Indices (Address);
         Current := 1;
         First_Missing := 0;
         for Depth in 1 .. 3 loop
            if Raw_Word (Object, Current, Route (Depth)) = 0 then
               First_Missing := Depth;
               exit;
            end if;
            Next := Child (Object, Raw_Word (Object, Current, Route (Depth)));
            if Next = 0 then return; end if;
            Current := Next;
         end loop;
         if First_Missing = 0 then
            if Raw_Word (Object, Current, Route (4)) /= 0 then return; end if;
         else
            for Depth in First_Missing .. 3 loop
               Prefix := Shift_Right (Address, 48 - 9 * Depth);
               if Prefix /= Last_Missing (Depth) then
                  if Missing = Metadata_Capacity (Object) - Object.Count then return; end if;
                  Missing := Missing + 1;
                  Last_Missing (Depth) := Prefix;
               end if;
            end loop;
         end if;
      end loop;
      -- Do not publish different cache types for aliases of one DMA page.
      -- This offline image contains only our own validated encodings. Table
      -- backing was excluded above, so directory entries cannot alias Data.
      -- Skip equal-policy entries before searching the requested pages; the
      -- common single-policy image needs just one pass over used table words.
      for P in 1 .. Object.Count loop
         for Word of Raw_Page (Object, P) loop
            if Word /= 0 and then Word mod 4096 /= 3 + Cache_Bits (Policy) then
               for DMA of Data loop
                  if Word / 4096 = DMA / 4096 then return; end if;
               end loop;
            end if;
         end loop;
      end loop;
      -- All checks complete. No allocation callbacks or other fallible work
      -- occurs while modifying the private image.
      for I in Data'Range loop
         Route := Indices (GPU + Unsigned_64 (I - Data'First) * 4096);
         Current := 1;
         for Depth in 1 .. 3 loop
            if Raw_Word (Object, Current, Route (Depth)) = 0 then
               Object.Count := Object.Count + 1;
               Next := Object.Count;
               Object.Levels (Next) := 3 - Depth;
               Set_Raw_Word (Object, Current, Route (Depth),
                 Encode_Directory (Object.DMA (Next)));
            else
               Next := Child (Object, Raw_Word (Object, Current, Route (Depth)));
            end if;
            Current := Next;
         end loop;
         Set_Raw_Word (Object, Current, Route (4),
           Encode_Leaf (Data (I), Policy, Access_Mode));
      end loop;
      Object.Mapped_Pages := Object.Mapped_Pages + Data'Length;
      Accepted := True;
   end Map_Pages;
   procedure Unmap_Pages
     (Object : in out Image; GPU : Unsigned_64; Expected : Data_Pages;
      Accepted : out Boolean) is
      Route : Path;
      Current : Natural;
      Word : Unsigned_64;
   begin
      Accepted := False;
      if not Object.Valid or Object.Frozen or GPU = 0 or GPU >= 2 ** 48
        or GPU mod 4096 /= 0 or Expected'Length = 0 then return; end if;
      if Unsigned_64 (Expected'Length) > (2 ** 48 - GPU) / 4096 then return; end if;
      for I in Expected'Range loop
         if not Valid_DMA_Page (Expected (I)) then return; end if;
         Word := Lookup (Object, GPU + Unsigned_64 (I - Expected'First) * 4096);
         if Word = 0 or else Word - Word mod 4096 /= Expected (I) then return; end if;
      end loop;
      -- Only our own validated directory/leaf words exist here. No callbacks
      -- can change the image between validation and this serialized commit.
      for I in Expected'Range loop
         Route := Indices (GPU + Unsigned_64 (I - Expected'First) * 4096);
         Current := 1;
         for Depth in 1 .. 3 loop
            Current := Child (Object, Raw_Word (Object, Current, Route (Depth)));
         end loop;
         Set_Raw_Word (Object, Current, Route (4), 0);
      end loop;
      Object.Mapped_Pages := Object.Mapped_Pages - Expected'Length;
      Accepted := True;
   end Unmap_Pages;
   procedure Seal (Object : in out Image; Accepted : out Boolean) is
   begin
      Accepted := Object.Valid and Object.Mapped_Pages /= 0;
      if Accepted then Object.Frozen := True; end if;
   end Seal;
   procedure Seal_Update (Object : in out Image; Accepted : out Boolean) is
   begin
      -- TGL PRM Vol6 pp36-37: a 4KiB leaf needs Present=1. Removing the
      -- last leaf leaves an empty translation space, not an invalid root.
      -- Preserve the stricter bootstrap policy in Seal.
      Accepted := Object.Valid and Object.Predecessor_Root /= 0;
      if Accepted then Object.Frozen := True; end if;
   end Seal_Update;
   function Sealed (Object : Image) return Boolean is (Object.Frozen);
   function Used (Object : Image) return Natural is (Object.Count);
   function Root_DMA (Object : Image) return Unsigned_64 is
     (if Object.Valid then Object.DMA (1) else 0);
   function Page_DMA (Object : Image; Page : Page_Number) return Unsigned_64 is
     (if Page <= Object.Count then Object.DMA (Page) else 0);
   function Used_Tables_Match (Object : Image; Backing : Backing_Pages) return Boolean is
     (Object.Valid and then Object.Frozen and then Object.Count > 0 and then
      (for all P in 1 .. Object.Count => Backing (P) = Object.DMA (P)));
   function DMA_Disjoint (Object : Image; First, Bytes : Unsigned_64) return Boolean is
      function Inside (Page : Unsigned_64) return Boolean is
        (Page >= First and then Page - First < Bytes);
      function Check is new Backing_Disjoint (Inside);
   begin
      if not Object.Valid or else not Valid_DMA_Page (First) or else
        Bytes = 0 or else Bytes mod 4096 /= 0 or else Bytes > 2 ** 32 - First
      then return False; end if;
      return Check (Object);
   end DMA_Disjoint;
   function Backing_Disjoint (Object : Image) return Boolean is
   begin
      if not Object.Valid then return False; end if;
      for Page of Object.DMA loop
         if Conflicts (Page) then return False; end if;
      end loop;
      for Page of Object.Scratch loop
         if Page /= 0 and then Conflicts (Page) then return False; end if;
      end loop;
      -- These are exclusively builder-produced 4KiB directory/leaf entries.
      -- Both refer to DMA pages; include both rather than guessing their type
      -- from low flags (WB leaves and directories have identical low bits).
      for P in 1 .. Object.Count loop
         for Word of Raw_Page (Object, P) loop
            if Word /= 0 and then Conflicts (Word - Word mod 4096) then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Backing_Disjoint;
   function Entry_Value
     (Object : Image; Page : Page_Number; Index : Table_Index) return Unsigned_64 is
     (if Page > Object.Count then 0
      elsif Raw_Word (Object, Page, Index) /= 0 then Raw_Word (Object, Page, Index)
      else Intel_GPU_PPGTT_Scratch.Fallback (Object.Scratch, Object.Levels (Page)));
   function Scratch_DMA
     (Object : Image; L : Intel_GPU_PPGTT_Scratch.Level) return Unsigned_64 is
     (if Object.Valid then Object.Scratch (L) else 0);
   function Scratch_Entry
     (Object : Image; L : Intel_GPU_PPGTT_Scratch.Table_Level) return Unsigned_64 is
     (if Object.Valid then Intel_GPU_PPGTT_Scratch.Fill (Object.Scratch, L) else 0);
   function Lookup (Object : Image; GPU : Unsigned_64) return Unsigned_64 is
      Route : Path;
      Current : Natural := 1;
   begin
      if not Object.Valid or GPU >= 2 ** 48 then return 0; end if;
      Route := Indices (GPU);
      for Depth in 1 .. 3 loop
         Current := Child (Object, Raw_Word (Object, Current, Route (Depth)));
         if Current = 0 then return 0; end if;
      end loop;
      return Raw_Word (Object, Current, Route (4));
   end Lookup;
end Intel_GPU_VM_Image;
