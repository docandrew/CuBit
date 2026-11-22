package body Intel_GPU_VM_Image is
   function Descriptor_Metadata_Bytes return Unsigned_64 is
     ((Unsigned_64 (Capacity - Positive'Min (Capacity, Bootstrap_Descriptors)) *
       Unsigned_64 (Table_Descriptor'Object_Size / 8) + 4095) / 4096 * 4096);
   function Descriptor_Capacity (Object : Image) return Positive is
     (Positive'Min (Capacity, Descriptor_Storage.Capacity (Object.Descriptors)));
   procedure Extend_Descriptors
     (Object : in out Image; Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin Descriptor_Storage.Extend (Object.Descriptors, Base, Bytes, Accepted); end;
   function Descriptor (Object : Image; Page : Page_Number) return Table_Descriptor is
     (if Page <= Descriptor_Capacity (Object) then
        Descriptor_Storage.Get (Object.Descriptors, Page) else (others => <>));
   procedure Set_Descriptor
     (Object : in out Image; Page : Page_Number; Value : Table_Descriptor) is
   begin Descriptor_Storage.Put (Object.Descriptors, Page, Value); end;
   procedure Set_Level
     (Object : in out Image; Page : Page_Number; Level : Intel_GPU_PPGTT_Scratch.Level) is
      Value : Table_Descriptor := Descriptor (Object, Page);
   begin Value.Level := Level; Set_Descriptor (Object, Page, Value); end;
   function Backed_Tables (Object : Image) return Natural is
     (if Object.Valid then Object.Backed else 0);
   function Growth_Capacity (Object : Growth_Receipt) return Positive is
     (Positive'Min (Capacity, Growth_Storage.Capacity (Object.Plan)));
   procedure Extend_Growth_Metadata
     (Object : in out Growth_Receipt; Base, Bytes : Unsigned_64;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      -- An adopted receipt still proves which pages Rearm must authenticate.
      -- Growing its stable prefix does not discard that evidence. No failed,
      -- publishing, or callback-interrupted attempt gains this permission.
      if (Object.Begun or else Object.Commit_Tried) and then
        not (Object.Adopted and then Object.Done and then
             Object.Phase = Publication_Done)
      then return; end if;
      Growth_Storage.Extend (Object.Plan, Base, Bytes, Accepted);
   end Extend_Growth_Metadata;
   function Insertion_Capacity (Object : Insertion_Receipt) return Positive is
     (Positive'Min (Capacity * 512, Leaf_Storage.Capacity (Object.Words)));
   procedure Extend_Insertion_Metadata
     (Object : in out Insertion_Receipt; Base, Bytes : Unsigned_64;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Poisoned or else Object.Pending or else Object.Active or else
        Object.Executing or else Object.Preparing then return; end if;
      Leaf_Storage.Extend (Object.Words, Base, Bytes, Accepted);
   end Extend_Insertion_Metadata;
   function Mirror_Capacity (Object : Image) return Positive is
     (Table_Storage.Capacity (Object.Tables));
   function Metadata_Capacity (Object : Image) return Positive is
     (Positive'Min (Mirror_Capacity (Object), Descriptor_Capacity (Object)));
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
     (Object.Valid and then Page <= Object.Count and then Descriptor (Object, Page).Level = 0);
   function Revision (Object : Image) return Unsigned_64 is (Object.Epoch);
   function Direct_Successor (Object, Candidate : Image) return Boolean is
     (Object.Valid and then Object.Frozen and then Candidate.Valid and then
      Candidate.Frozen and then Candidate.Predecessor_Root /= 0 and then
      Candidate.Predecessor_Root = Descriptor (Object, 1).DMA and then
      Candidate.Predecessor_Epoch = Object.Epoch and then
      Descriptor (Candidate, 1).DMA /= Descriptor (Object, 1).DMA and then Object.Epoch /= Unsigned_64'Last);
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
         if Entry_Word = Encode_Directory (Descriptor (Object, P).DMA) then return P; end if;
      end loop;
      return 0;
   end Child;
   procedure Initialize_From_Pages
     (Object : in out Image; Backing_Count : Page_Number; Accepted : out Boolean;
      Scratch : Intel_GPU_PPGTT_Scratch.Backing_Pages := [others => 0]) is
      DMA : Unsigned_64;
   begin
      Accepted := False;
      if Object.Attempted or else Object.Epoch = Unsigned_64'Last or else
        Backing_Count > Descriptor_Capacity (Object) then return; end if;
      Object.Retired_Receipt := False;
      Object.Epoch := Object.Epoch + 1;
      Object.Attempted := True;
      if (for some Page of Scratch => Page /= 0) and then
        not Intel_GPU_PPGTT_Scratch.Valid (Scratch) then return; end if;
      for P in 1 .. Backing_Count loop
         DMA := Read_Page (P);
         if not Valid_DMA_Page (DMA) then return; end if;
         if Intel_GPU_PPGTT_Scratch.Contains (Scratch, DMA) then return; end if;
         for Q in Page_Number'First .. P - 1 loop
            if DMA = Descriptor (Object, Q).DMA then return; end if;
         end loop;
         Set_Descriptor (Object, P, (DMA, 0));
      end loop;
      Object.Backed := Backing_Count;
      Object.Scratch := Scratch;
      Set_Level (Object, 1, 3);
      Object.Count := 1;
      Object.Valid := True;
      Accepted := True;
   end Initialize_From_Pages;
   procedure Initialize
     (Object : in out Image; Backing : Backing_Pages; Accepted : out Boolean;
      Scratch : Intel_GPU_PPGTT_Scratch.Backing_Pages := [others => 0];
      Backing_Count : Page_Number := Capacity) is
      Tail_Valid : constant Boolean :=
        (for all P in Backing_Count + 1 .. Capacity => Backing (P) = 0);
      function Read_Page (Page : Page_Number) return Unsigned_64 is
        (if Tail_Valid then Backing (Page) else 0);
      procedure From_Pages is new Initialize_From_Pages (Read_Page);
   begin
      From_Pages (Object, Backing_Count, Accepted, Scratch);
   end Initialize;
   procedure Append_Offline_From_Pages
     (Object : in out Image; Page_Count : Natural;
      First : out Natural; Accepted : out Boolean)
   is
      Backed : constant Natural := Object.Backed;
      Count : constant Natural := Object.Count;
      Epoch : constant Unsigned_64 := Object.Epoch;
      Root : constant Unsigned_64 := Root_DMA (Object);
      DMA : Unsigned_64;
      function Current return Boolean is
        (Authorized and then Object.Valid and then not Object.Frozen and then
         Object.Backed = Backed and then Object.Count = Count and then
         Object.Epoch = Epoch and then Root_DMA (Object) = Root);
   begin
      First := 0; Accepted := False;
      if not Current or else Page_Count = 0 or else
        Object.Epoch = Unsigned_64'Last then return; end if;
      if Backed < Object.Count or else Backed > Metadata_Capacity (Object) or else
        Page_Count > Metadata_Capacity (Object) - Backed then return; end if;
      for P in 1 .. Page_Count loop
         DMA := Read_Page (P);
         if not Current or else not DMA_Disjoint (Object, DMA, 4096) then return; end if;
         for Q in 1 .. P - 1 loop
            if DMA = Descriptor (Object, Backed + Q).DMA then return; end if;
         end loop;
         Set_Descriptor (Object, Backed + P, (DMA, 0));
      end loop;
      if not Current then return; end if;
      -- Commit the prefix only after complete validation. Map_Pages initializes
      -- mirrors later; staged descriptors alone never establish live backing.
      Object.Epoch := Object.Epoch + 1;
      Object.Backed := Backed + Page_Count;
      First := Backed + 1; Accepted := True;
   end Append_Offline_From_Pages;
   procedure Append_Offline_Backing
     (Object : in out Image; Pages : Data_Pages;
      First : out Natural; Accepted : out Boolean) is
      function Page (Ordinal : Positive) return Unsigned_64 is
        (Pages (Pages'First + (Ordinal - 1)));
      function Authorized return Boolean is (True);
      procedure Stream is new Append_Offline_From_Pages (Page, Authorized);
   begin
      Stream (Object, Pages'Length, First, Accepted);
   end Append_Offline_Backing;
   procedure Prepare_Update_From_Pages
     (Target : in out Image; Source : Image; Backing_Count : Page_Number;
      Accepted : out Boolean) is
      Next : Natural;
      DMA : Unsigned_64;
   begin
      Accepted := False;
      if Target.Attempted or else Target.Epoch = Unsigned_64'Last or else
        Source.Count > Metadata_Capacity (Target) or else
        Backing_Count > Descriptor_Capacity (Target) then return; end if;
      Target.Retired_Receipt := False;
      Target.Epoch := Target.Epoch + 1;
      Target.Attempted := True;
      if not Source.Valid or else not Source.Frozen or else
        Source.Count > Backing_Count
      then return; end if;
      -- Include unused reserved source tables and every mapped data page.
      -- New table storage must not overwrite either generation's data.
      for P in 1 .. Backing_Count loop
         DMA := Read_Page (P);
         if not DMA_Disjoint (Source, DMA, 4096) then return; end if;
         for Q in Page_Number'First .. P - 1 loop
            if DMA = Descriptor (Target, Q).DMA then return; end if;
         end loop;
         Set_Descriptor (Target, P, (DMA, Descriptor (Source, P).Level));
      end loop;
      for P in 1 .. Source.Count loop
         for I in Table_Index loop
            -- Validated images exclude table/data aliases, so only directory
            -- entries can identify an allocated child table. Never infer an
            -- entry's role from low flags: WB leaves share directory flags.
            Next := Child (Source, Raw_Word (Source, P, I));
            Set_Raw_Word (Target, P, I,
              (if Next /= 0 then Encode_Directory (Descriptor (Target, Next).DMA)
               else Raw_Word (Source, P, I)));
         end loop;
      end loop;
      Target.Backed := Backing_Count;
      Target.Scratch := Source.Scratch;
      Target.Count := Source.Count;
      Target.Mapped_Pages := Source.Mapped_Pages;
      Target.Predecessor_Root := Descriptor (Source, 1).DMA;
      Target.Predecessor_Epoch := Source.Epoch;
      Target.Valid := True;
      Accepted := True;
   end Prepare_Update_From_Pages;
   procedure Prepare_Update
     (Target : in out Image; Source : Image; Backing : Backing_Pages;
      Accepted : out Boolean; Backing_Count : Page_Number := Capacity) is
      Tail_Valid : constant Boolean :=
        (for all P in Backing_Count + 1 .. Capacity => Backing (P) = 0);
      function Read_Page (Page : Page_Number) return Unsigned_64 is
        (if Tail_Valid then Backing (Page) else 0);
      procedure From_Pages is new Prepare_Update_From_Pages (Read_Page);
   begin
      From_Pages (Target, Source, Backing_Count, Accepted);
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
         for P in 1 .. Object.Backed loop
            if Data (I) = Descriptor (Object, P).DMA then return; end if;
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
      -- Metadata room is not physical backing. Reject the whole request before
      -- creating a directory if its next ordinal has not been allocated.
      for P in Object.Count + 1 .. Object.Count + Missing loop
         if not Valid_DMA_Page (Descriptor (Object, P).DMA) then return; end if;
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
               Set_Level (Object, Next, 3 - Depth);
               Set_Raw_Word (Object, Current, Route (Depth),
                 Encode_Directory (Descriptor (Object, Next).DMA));
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
     (if Object.Valid then Descriptor (Object, 1).DMA else 0);
   function Page_DMA (Object : Image; Page : Page_Number) return Unsigned_64 is
     (if Page <= Object.Count then Descriptor (Object, Page).DMA else 0);
   function Table_Backing_DMA (Object : Image; Page : Page_Number) return Unsigned_64 is
     (if Object.Valid and then Page <= Object.Backed then Descriptor (Object, Page).DMA else 0);
   function Used_Tables_Match (Object : Image; Backing : Backing_Pages) return Boolean is
     (Object.Valid and then Object.Frozen and then Object.Count > 0 and then
      (for all P in 1 .. Object.Count => Backing (P) = Descriptor (Object, P).DMA));
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
      for P in 1 .. Object.Backed loop
         if Conflicts (Descriptor (Object, P).DMA) then return False; end if;
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
     (if not Object.Valid or else Page > Object.Count then 0
      elsif Raw_Word (Object, Page, Index) /= 0 then Raw_Word (Object, Page, Index)
      else Intel_GPU_PPGTT_Scratch.Fallback (Object.Scratch, Descriptor (Object, Page).Level));
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
