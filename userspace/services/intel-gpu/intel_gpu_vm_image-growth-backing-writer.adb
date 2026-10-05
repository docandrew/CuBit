package body Intel_GPU_VM_Image.Growth.Backing.Writer is
   use type Intel_GPU_ADLN_PPGTT.Table_Index;
   function Attempted (Object : State) return Boolean is (Object.Begun);
   function Published (Object : State) return Boolean is (Object.Done);
   function Committed (Object : State) return Boolean is (Object.Adopted);
   function Pending (Object : State) return Boolean is
     (Object.Phase in Fill_Child .. Verify_Parent);
   function Retained_Link (Item : Link; Topology : Node) return Growth_Link is
     (Parent_DMA => Item.Parent_DMA, Child_DMA => Item.Child_DMA,
      Expected => Item.Expected, Value => Item.Value, Fill => Item.Fill,
      Index => Item.Index, Existing_Parent => Topology.Existing_Parent,
      New_Parent => Topology.New_Parent, Level => Topology.Level);
   procedure Start_From_Pages
     (Object : in out State; Source : Image; GPU, Bytes, Retained_Root : Unsigned_64;
      Page_Count : Natural; Accepted : out Boolean)
   is
      OK : Boolean;
      Epoch : constant Unsigned_64 := Revision (Source);
      Root : constant Unsigned_64 := Root_DMA (Source);
      DMA : Unsigned_64;
      function Retained_Page (Ordinal : Positive) return Unsigned_64 is
        (Growth_Storage.Get (Object.Plan, Ordinal).Child_DMA);
      function Held (DMA : Unsigned_64) return Boolean is
        (not Object.Commit_Tried and then Exclusive and then Sealed (Source)
         and then Root_DMA (Source) = Root and then Revision (Source) = Epoch
         and then Owned_Table (DMA) and then Exclusive and then not Object.Commit_Tried);
      procedure Store_Link (Ordinal : Positive; Item : Link; Topology : Node; OK : out Boolean) is
      begin
         OK := Held (Retained_Root) and then Item.Child_DMA = Retained_Page (Ordinal);
         if OK then Growth_Storage.Put (Object.Plan, Ordinal, Retained_Link (Item, Topology)); end if;
      end Store_Link;
      procedure Resolve_Retained is new Resolve_Into (Retained_Page, Store_Link);
      -- Provenance resolution is a trusted callback, but may observe/revoke
      -- an owner while returning its lookup result. Do not let an earlier
      -- exclusion sample authorize the next memory access after that callback.
   begin
      Accepted := False;
      if Object.Begun or else Object.Commit_Tried then return; end if;
      if Page_Count > Growth_Capacity (Object) then return; end if;
      -- Receipt storage does not imply source mirror/descriptor capacity.
      -- Reject before publishing directories that software cannot adopt.
      if Source.Count > Metadata_Capacity (Source) or else
        Page_Count > Metadata_Capacity (Source) - Source.Count then return; end if;
      Object.Begun := True;
      Object.Phase := Failed;
      if not Held (Retained_Root) then return; end if;
      for N in 1 .. Page_Count loop
         DMA := Read_Page (N);
         if not Held (Retained_Root) then return; end if;
         Growth_Storage.Put (Object.Plan, N, (Child_DMA => DMA, others => <>));
      end loop;
      Resolve_Retained (Source, GPU, Bytes, Retained_Root, Page_Count, Growth_Capacity (Object), OK);
      if not OK or else not Held (Retained_Root) then return; end if;
      Object.Root := Root_DMA (Source); Object.Epoch := Epoch;
      Object.GPU := GPU; Object.Bytes := Bytes;
      Object.Hardware_Root := Retained_Root;
      Object.Count := Page_Count;
      Object.Cursor := 1; Object.Word := 0; Object.Phase := Fill_Child;
      Accepted := True;
   end Start_From_Pages;
   procedure Start
     (Object : in out State; Source : Image; GPU, Bytes, Retained_Root : Unsigned_64;
      New_Pages : Data_Pages; Accepted : out Boolean) is
      function Page (Ordinal : Positive) return Unsigned_64 is
        (New_Pages (New_Pages'First + (Ordinal - 1)));
      procedure Stream is new Start_From_Pages (Page);
   begin
      Stream (Object, Source, GPU, Bytes, Retained_Root, New_Pages'Length, Accepted);
   end Start;
   procedure Step (Object : in out State; Source : Image) is
      Phase : constant Growth_Phase := Object.Phase;
      Item : constant Growth_Link := Growth_Storage.Get (Object.Plan, Object.Cursor);
      OK : Boolean;
      Value : Unsigned_64;
      function Held (DMA : Unsigned_64) return Boolean is
        (not Object.Commit_Tried and then Exclusive and then Sealed (Source)
         and then Root_DMA (Source) = Object.Root
         and then Revision (Source) = Object.Epoch and then Owned_Table (DMA)
         and then Exclusive and then not Object.Commit_Tried);
      procedure Next_Word (Following : Growth_Phase) is
         use type Intel_GPU_ADLN_PPGTT.Table_Index;
      begin
         if Object.Word = Intel_GPU_ADLN_PPGTT.Table_Index'Last then
            Object.Word := 0; Object.Phase := Following;
         else Object.Word := Object.Word + 1; Object.Phase := Phase; end if;
      end Next_Word;
   begin
      if not Pending (Object) then return; end if;
      -- Any rejection permanently consumes this attempt, including ownership
      -- or epoch loss between service-loop turns. No rollback/retry of stores.
      -- Commit can consume the attempt during a callback while Phase is already
      -- Failed. Recheck that sticky receipt in Held before restoring any phase
      -- or setting Done, including after the final parent readback.
      Object.Phase := Failed;
      if not Held (Object.Hardware_Root) or else not Held (Item.Child_DMA) then return; end if;
      if Phase in Read_Parent .. Verify_Parent and then not Held (Item.Parent_DMA)
      then return; end if;
      case Phase is
         when Fill_Child =>
            Write_Word (Item.Child_DMA, Object.Word, Item.Fill, OK);
            if not OK or else not Held (Item.Child_DMA) then return; end if;
            Next_Word (Flush_Child);
         when Flush_Child =>
            if not Flush_Page (Item.Child_DMA) or else not Held (Item.Child_DMA) then return; end if;
            Object.Phase := Verify_Child;
         when Verify_Child =>
            Read_Word (Item.Child_DMA, Object.Word, Value, OK);
            if not OK or else Value /= Item.Fill or else not Held (Item.Child_DMA) then return; end if;
            Next_Word (Fill_Child);
            if Object.Word = 0 then
               if Object.Cursor = Object.Count then
                  Object.Cursor := 1; Object.Phase := Read_Parent;
               else Object.Cursor := Object.Cursor + 1; end if;
            end if;
         when Read_Parent =>
            Read_Word (Item.Parent_DMA, Item.Index, Value, OK);
            if not OK or else Value /= Item.Expected or else not Held (Item.Parent_DMA)
              or else not Held (Item.Child_DMA) then return; end if;
            Object.Phase := Write_Parent;
         when Write_Parent =>
            Write_Word (Item.Parent_DMA, Item.Index, Item.Value, OK);
            if not OK or else not Held (Item.Parent_DMA) then return; end if;
            Object.Phase := Flush_Parent;
         when Flush_Parent =>
            if not Flush_Page (Item.Parent_DMA) or else not Held (Item.Parent_DMA) then return; end if;
            Object.Phase := Verify_Parent;
         when Verify_Parent =>
            Read_Word (Item.Parent_DMA, Item.Index, Value, OK);
            if not OK or else Value /= Item.Value or else not Held (Item.Parent_DMA)
              or else not Held (Item.Child_DMA) then return; end if;
            if Object.Cursor = Object.Count then
               Object.Done := True; Object.Phase := Publication_Done;
            else Object.Cursor := Object.Cursor + 1; Object.Phase := Read_Parent; end if;
         when others => null;
      end case;
   end Step;
   procedure Commit
     (Object : in out State; Source : in out Image; Accepted : out Boolean)
   is
      Base, Parent : Natural;
      OK : Boolean;
      function Retained_Page (Ordinal : Positive) return Unsigned_64 is
        (Growth_Storage.Get (Object.Plan, Ordinal).Child_DMA);
      procedure Check_Link (Ordinal : Positive; Item : Link; Topology : Node; OK : out Boolean) is
      begin
         OK := Exclusive and then Sealed (Source) and then
           Revision (Source) = Object.Epoch and then Root_DMA (Source) = Object.Root and then
           Growth_Storage.Get (Object.Plan, Ordinal) = Retained_Link (Item, Topology);
      end Check_Link;
      procedure Resolve_Retained is new Resolve_Into (Retained_Page, Check_Link);
   begin
      Accepted := False;
      if Object.Commit_Tried then return; end if;
      Object.Commit_Tried := True;
      if Pending (Object) then Object.Phase := Failed; end if;
      if not Object.Done or else not Exclusive or else not Sealed (Source)
        or else Root_DMA (Source) /= Object.Root or else Revision (Source) /= Object.Epoch
        or else Source.Epoch = Unsigned_64'Last or else not Invalidation_Confirmed
      then return; end if;
      if Source.Count > Metadata_Capacity (Source) or else
        Object.Count > Metadata_Capacity (Source) - Source.Count then return; end if;
      Resolve_Retained (Source, Object.GPU, Object.Bytes, Object.Hardware_Root,
               Object.Count, Object.Count, OK);
      if not OK then return; end if;
      if not Exclusive or else not Sealed (Source) or else Root_DMA (Source) /= Object.Root
        or else Revision (Source) /= Object.Epoch or else not Invalidation_Confirmed
      then return; end if;
      Base := Source.Count;
      -- All fallible checks/callbacks precede mutation. Serialized owner only.
      -- Logical holes stay zero; Entry_Value exports the scratch fallback.
      for N in 1 .. Object.Count loop
         Set_Descriptor (Source, Base + N,
           (Growth_Storage.Get (Object.Plan, N).Child_DMA, Growth_Storage.Get (Object.Plan, N).Level));
         Clear_Table (Source, Base + N);
      end loop;
      for N in 1 .. Object.Count loop
         declare Item : constant Growth_Link := Growth_Storage.Get (Object.Plan, N); begin
            Parent := (if Item.Existing_Parent /= 0 then Item.Existing_Parent
                       else Base + Item.New_Parent);
            Set_Raw_Word (Source, Parent, Item.Index, Item.Value);
         end;
      end loop;
      Source.Count := Base + Object.Count; Source.Epoch := Source.Epoch + 1;
      Source.Backed := Natural'Max (Source.Backed, Source.Count);
      Source.Predecessor_Root := 0; Source.Predecessor_Epoch := 0;
      Object.First_Adopted := Base + 1;
      Object.Adopted := True; Accepted := True;
   end Commit;
   procedure Rearm
     (Object : in out State; Source : Image; Retained_Root : Unsigned_64;
      Accepted : out Boolean)
   is
      Epoch : constant Unsigned_64 := Revision (Source);
   begin
      Accepted := False;
      if not Object.Adopted or else not Exclusive or else not Sealed (Source) or else
        Root_DMA (Source) /= Object.Root or else Epoch <= Object.Epoch or else
        Retained_Root /= Object.Hardware_Root or else Object.Count = 0 or else
        Object.First_Adopted = 0 or else Object.First_Adopted > Source.Count or else
        Object.Count > Source.Count - Object.First_Adopted + 1 or else
        not Owned_Table (Retained_Root)
      then return; end if;
      for N in 1 .. Object.Count loop
         if Descriptor (Source, Object.First_Adopted + N - 1).DMA /=
           Growth_Storage.Get (Object.Plan, N).Child_DMA or else
           not Owned_Table (Growth_Storage.Get (Object.Plan, N).Child_DMA) or else not Exclusive
         then return; end if;
      end loop;
      if not Exclusive or else not Sealed (Source) or else
        Revision (Source) /= Epoch or else Root_DMA (Source) /= Object.Root
      then return; end if;
      -- The source/provenance retains the pages. Only this transaction receipt
      -- is cleared; no hardware writes, TLB actions or allocator releases.
      for N in 1 .. Object.Count loop
         Growth_Storage.Put (Object.Plan, N, (others => <>));
      end loop;
      Object.Begun := False; Object.Done := False;
      Object.Commit_Tried := False; Object.Adopted := False;
      Object.Root := 0; Object.Epoch := 0; Object.GPU := 0; Object.Bytes := 0;
      Object.Hardware_Root := 0; Object.Count := 0; Object.First_Adopted := 0;
      Object.Phase := Idle; Object.Cursor := 1; Object.Word := 0;
      Accepted := True;
   end Rearm;
end Intel_GPU_VM_Image.Growth.Backing.Writer;
