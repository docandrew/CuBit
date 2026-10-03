package body Intel_GPU_VM_Image.Growth.Backing is
   use Intel_GPU_ADLN_PPGTT;
   procedure Resolve
     (Source : Image; GPU, Bytes, Retained_Root : Unsigned_64;
      New_Pages : Data_Pages; Output : out Links; Accepted : out Boolean)
   is
      Plan : Node_List (1 .. Capacity);
      Count : Natural;
      OK : Boolean;
      Parent, Child : Unsigned_64;
   begin
      Accepted := False; Output := [others => (others => <>)];
      Describe (Source, GPU, Bytes, Plan, Count, OK);
      if not OK or else Count = 0 or else New_Pages'Length /= Count
        or else Output'Length < Count
        or else not Valid_DMA_Page (Retained_Root)
        or else not Owned_Table (Retained_Root)
      then return; end if;
      for I in New_Pages'Range loop
         Child := New_Pages (I);
         if not Valid_DMA_Page (Child) or else Child = Retained_Root
           or else not DMA_Disjoint (Source, Child, 4096)
           or else not Owned_Table (Child)
         then return; end if;
         for J in New_Pages'First .. I - 1 loop
            if Child = New_Pages (J) then return; end if;
         end loop;
      end loop;
      -- Preflight every existing parent before exposing any output plan.
      for N in 1 .. Count loop
         if Plan (N).Existing_Parent /= 0 then
            Parent := (if Plan (N).Existing_Parent = 1 then Retained_Root
                       else Page_DMA (Source, Plan (N).Existing_Parent));
            if not Valid_DMA_Page (Parent) or else not Owned_Table (Parent)
            then return; end if;
         end if;
      end loop;
      for N in 1 .. Count loop
         Child := New_Pages (New_Pages'First + N - 1);
         if Plan (N).Existing_Parent = 1 then Parent := Retained_Root;
         elsif Plan (N).Existing_Parent /= 0 then
            Parent := Page_DMA (Source, Plan (N).Existing_Parent);
         else Parent := New_Pages (New_Pages'First + Plan (N).New_Parent - 1);
         end if;
         Output (Output'First + N - 1) :=
           (Parent_DMA => Parent, Child_DMA => Child,
            Expected => Intel_GPU_PPGTT_Scratch.Fallback (Source.Scratch, Plan (N).Level + 1),
            Value => Encode_Directory (Child),
            Fill => Intel_GPU_PPGTT_Scratch.Fallback (Source.Scratch, Plan (N).Level),
            Index => Plan (N).Index);
      end loop;
      Accepted := True;
   end Resolve;
end Intel_GPU_VM_Image.Growth.Backing;
