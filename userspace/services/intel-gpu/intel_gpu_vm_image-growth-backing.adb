package body Intel_GPU_VM_Image.Growth.Backing is
   use Intel_GPU_ADLN_PPGTT;
   procedure Resolve_Into
     (Source : Image; GPU, Bytes, Retained_Root : Unsigned_64;
      Page_Count, Output_Capacity : Natural; Accepted : out Boolean)
   is
      Required : constant Requirements := Inspect (Source, GPU, Bytes);
      Count : Natural;
      OK : Boolean;
      Child : Unsigned_64;
      Check_Only : Boolean := True;
      function Authorized return Boolean is (True);
      procedure Emit_Node (Ordinal : Positive; Item : Node; Accepted : out Boolean) is
         Parent, Child : Unsigned_64;
      begin
         Accepted := False;
         Child := Read_Page (Ordinal);
         if Item.Existing_Parent = 1 then Parent := Retained_Root;
         elsif Item.Existing_Parent /= 0 then Parent := Page_DMA (Source, Item.Existing_Parent);
         else Parent := Read_Page (Item.New_Parent); end if;
         if not Valid_DMA_Page (Parent) or else not Owned_Table (Parent) then return; end if;
         if not Check_Only then
            Emit (Ordinal,
              (Parent_DMA => Parent, Child_DMA => Child,
               Expected => Intel_GPU_PPGTT_Scratch.Fallback (Source.Scratch, Item.Level + 1),
               Value => Encode_Directory (Child),
               Fill => Intel_GPU_PPGTT_Scratch.Fallback (Source.Scratch, Item.Level),
               Index => Item.Index), Item, Accepted);
            return;
         end if;
         Accepted := True;
      end Emit_Node;
      procedure Describe_Stream is new Describe_Into (Authorized, Emit_Node);
   begin
      Accepted := False;
      if Required.Status /= Ready or else Required.Additional_Tables = 0 or else
        Page_Count /= Required.Additional_Tables or else Output_Capacity < Page_Count
        or else not Valid_DMA_Page (Retained_Root)
        or else not Owned_Table (Retained_Root)
      then return; end if;
      for I in 1 .. Page_Count loop
         Child := Read_Page (I);
         if not Valid_DMA_Page (Child) or else Child = Retained_Root
           or else not DMA_Disjoint (Source, Child, 4096)
           or else not Owned_Table (Child)
         then return; end if;
         for J in 1 .. I - 1 loop
            if Child = Read_Page (J) then return; end if;
         end loop;
      end loop;
      -- Preflight every existing parent before exposing any output plan.
      Describe_Stream (Source, GPU, Bytes, Output_Capacity, Count, OK);
      if not OK or else Count /= Page_Count then return; end if;
      Check_Only := False;
      Describe_Stream (Source, GPU, Bytes, Output_Capacity, Count, OK);
      Accepted := OK and then Count = Page_Count;
   end Resolve_Into;
   procedure Resolve_From_Pages
     (Source : Image; GPU, Bytes, Retained_Root : Unsigned_64;
      Page_Count : Natural; Output : out Links; Accepted : out Boolean) is
      procedure Emit (Ordinal : Positive; Item : Link; Topology : Node; OK : out Boolean) is
         pragma Unreferenced (Topology);
      begin
         Output (Output'First + (Ordinal - 1)) := Item;
         OK := True;
      end Emit;
      procedure Stream is new Resolve_Into (Read_Page, Emit);
   begin
      Output := [others => (others => <>)];
      Stream (Source, GPU, Bytes, Retained_Root, Page_Count, Output'Length, Accepted);
      if not Accepted then Output := [others => (others => <>)]; end if;
   end Resolve_From_Pages;
   procedure Resolve
     (Source : Image; GPU, Bytes, Retained_Root : Unsigned_64;
      New_Pages : Data_Pages; Output : out Links; Accepted : out Boolean) is
      function Page (Ordinal : Positive) return Unsigned_64 is
        (New_Pages (New_Pages'First + (Ordinal - 1)));
      procedure Stream is new Resolve_From_Pages (Page);
   begin
      Stream (Source, GPU, Bytes, Retained_Root, New_Pages'Length, Output, Accepted);
   end Resolve;
end Intel_GPU_VM_Image.Growth.Backing;
