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
   function GPU_Start (Object : State) return Unsigned_64 is (Object.Prepared);
   function Retained_Root (Object : State) return Tables.Page_Mapping is (Object.Root);
   function Overlap (Page, First, Bytes : Unsigned_64) return Boolean is
     (if Page >= First then Page - First < Bytes else First - Page < 4096);
   procedure Prepare
     (Object : in out State; Source : VM.Image; Backing : Tables.Mappings;
      Allocation : Intel_GPU_Buffer_Reply.Backing;
      GGTT_Start, Bytes : Unsigned_64; Success : out Boolean) is
      Root : Unsigned_64;
      Ready : Boolean;
      Image_Pages : Intel_GPU_Submission_Image.Backing_Pages;
   begin
      Success := False;
      if Object.Attempted then return; end if;
      Object.Attempted := True;
      if not Intel_GPU_Buffer_Reply.Valid (Allocation) or else not VM.Sealed (Source) or else
        Allocation.Bytes < Intel_GPU_Submission_Image.Byte_Count or else
        Bytes /= Intel_GPU_Submission_Image.GGTT_Bytes or else not Owner_Ready
      then return; end if;
      for P in Image_Pages'Range loop
         Image_Pages (P) := Intel_GPU_Buffer_Reply.Page_Address
           (Allocation, Unsigned_64 (P) * 4096);
      end loop;
      -- Validate the context encoding before writing page tables as well.
      declare
         Image : constant Intel_GPU_Submission_Image.Image :=
           Intel_GPU_Submission_Image.Build_For_VM
             (Image_Pages, GGTT_Start, VM.Root_DMA (Source));
      begin
         if not Image.Valid then return; end if;
      end;
      if not Allocation_Disjoint (Source, Allocation)
      then return; end if;
      -- DMA preflight includes mapped data and unused reserved table capacity.
      -- CPU mappings are required only for the pages actually materialized.
      for P in VM.Page_Number loop
         if P <= VM.Used (Source) and then
           Overlap (Backing (P).CPU, Allocation.CPU_Address, Allocation.Bytes)
         then return; end if;
      end loop;
      Tables.Prepare (Object.Table_State, Source, Backing, Root, Ready);
      if not Ready or else not Owner_Ready then return; end if;
      Contexts.Initialize_For_VM (Object.Context_State, Allocation,
                                 GGTT_Start, Bytes, Root, Ready);
      if not Ready or else not Owner_Ready then return; end if;
      Object.Prepared := Contexts.Initialized_GPU_Start (Object.Context_State);
      Object.Root := Backing (1);
      Object.Allocation := Allocation;
      Success := True;
   end Prepare;
end Intel_GPU_Application_Image;
