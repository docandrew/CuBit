with Intel_GPU_Submission_Image;
package body Intel_GPU_Application_Image is
   function Allocation_Disjoint
     (Object : VM.Image; Allocation : Intel_GPU_Buffer_Reply.Backing) return Boolean is
      function Conflicts (Page : Unsigned_64) return Boolean is
        (Intel_GPU_Buffer_Reply.Overlaps_DMA (Allocation, Page, 4096));
      function Check is new VM.Backing_Disjoint (Conflicts);
   begin
      return Intel_GPU_Buffer_Reply.Valid (Allocation) and then Check (Object);
   end Allocation_Disjoint;
   function GPU_Start (Object : State) return Unsigned_64 is
     (if Object.Retirement_Attempted then 0 else Object.Prepared);
   function Retained_Root (Object : State) return Tables.Page_Mapping is (Object.Root);
   function Overlap (Page, First, Bytes : Unsigned_64) return Boolean is
     (if Page >= First then Page - First < Bytes else First - Page < 4096);
   procedure Prepare_From_Mappings
     (Object : in out State; Source : VM.Image; Mapping_Count : Natural;
      Allocation : Intel_GPU_Buffer_Reply.Backing;
      GGTT_Start, Bytes : Unsigned_64; Success : out Boolean;
      Scratch : Tables.Scratch_Mappings := [others => (0, 0)]) is
      Epoch : constant Unsigned_64 := VM.Revision (Source);
      Source_Root : constant Unsigned_64 := VM.Root_DMA (Source);
      function Held return Boolean is
        (Owner_Ready and then VM.Sealed (Source)
         and then VM.Revision (Source) = Epoch
         and then VM.Root_DMA (Source) = Source_Root);
      function Resolve (Ordinal : Positive) return Tables.Page_Mapping is
         Item : Tables.Page_Mapping;
      begin
         if not Held then return (0, 0); end if;
         Item := Lookup (Ordinal);
         if not Held or else Item.DMA /= VM.Page_DMA (Source, Ordinal)
           or else Item.CPU = 0 or else Item.CPU mod 4096 /= 0
           or else Item.CPU > 2 ** 47 - 4096
           or else Overlap (Item.CPU, Allocation.CPU_Address, Allocation.Bytes)
         then return (0, 0); end if;
         return Item;
      end Resolve;
      procedure Prepare_Tables is new Tables.Prepare_From_Mappings (Resolve);
      Root_Mapping, Item : Tables.Page_Mapping := (0, 0);
      Root : Unsigned_64;
      Ready : Boolean;
      Image_Pages : Intel_GPU_Submission_Image.Backing_Pages;
   begin
      Success := False;
      if Object.Attempted then return; end if;
      Object.Attempted := True;
      if not Intel_GPU_Buffer_Reply.Valid (Allocation) or else not VM.Sealed (Source) or else
        Mapping_Count < VM.Used (Source) or else
        Allocation.Bytes < Intel_GPU_Submission_Image.Byte_Count or else
        Bytes /= Intel_GPU_Submission_Image.GGTT_Bytes or else not Owner_Ready
      then return; end if;
      for P in Image_Pages'Range loop
         Image_Pages (P) := Intel_GPU_Buffer_Reply.Page_Address
           (Allocation, Unsigned_64 (P) * 4096);
      end loop;
      -- Validate the context encoding before writing page tables as well.
      if not Intel_GPU_Submission_Image.Valid_For_VM
        (Image_Pages, GGTT_Start, VM.Root_DMA (Source)) then return; end if;
      if not Allocation_Disjoint (Source, Allocation)
      then return; end if;
      -- DMA preflight includes mapped data and unused reserved table capacity.
      -- CPU mappings are required only for the pages actually materialized.
      for P in 1 .. VM.Used (Source) loop
         Item := Resolve (P);
         if Item.CPU = 0 then return; end if;
         if P = 1 then Root_Mapping := Item; end if;
      end loop;
      for Page of Scratch loop
         if Page.CPU /= 0 and then
           Overlap (Page.CPU, Allocation.CPU_Address, Allocation.Bytes)
         then return; end if;
      end loop;
      Prepare_Tables (Object.Table_State, Source, Mapping_Count, Root, Ready, Scratch);
      if not Ready or else not Held then return; end if;
      Contexts.Initialize_For_VM (Object.Context_State, Allocation,
                                 GGTT_Start, Bytes, Root, Ready);
      if not Ready or else not Held then return; end if;
      Object.Prepared := Contexts.Initialized_GPU_Start (Object.Context_State);
      Object.Root := Root_Mapping;
      Object.Scratch := Scratch;
      Object.Allocation := Allocation;
      Success := True;
   end Prepare_From_Mappings;

   procedure Prepare
     (Object : in out State; Source : VM.Image; Backing : Tables.Mapping_View;
      Allocation : Intel_GPU_Buffer_Reply.Backing;
      GGTT_Start, Bytes : Unsigned_64; Success : out Boolean;
      Scratch : Tables.Scratch_Mappings := [others => (0, 0)]) is
      function Page (Ordinal : Positive) return Tables.Page_Mapping is (Backing (Ordinal));
      procedure Stream is new Prepare_From_Mappings (Page);
   begin
      if Backing'First /= 1 then
         Success := False; Object.Attempted := True; return;
      end if;
      Stream (Object, Source, Backing'Length, Allocation, GGTT_Start, Bytes, Success, Scratch);
   end Prepare;
end Intel_GPU_Application_Image;
