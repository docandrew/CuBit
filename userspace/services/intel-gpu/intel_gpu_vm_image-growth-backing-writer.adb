package body Intel_GPU_VM_Image.Growth.Backing.Writer is
   use type Intel_GPU_ADLN_PPGTT.Table_Index;
   function Attempted (Object : State) return Boolean is (Object.Begun);
   function Published (Object : State) return Boolean is (Object.Done);
   function Committed (Object : State) return Boolean is (Object.Adopted);
   function Pending (Object : State) return Boolean is
     (Object.Phase in Fill_Child .. Verify_Parent);
   procedure Start
     (Object : in out State; Source : Image; GPU, Bytes, Retained_Root : Unsigned_64;
      New_Pages : Data_Pages; Accepted : out Boolean)
   is
      Plan : Links (1 .. Capacity);
      OK : Boolean;
      Epoch : constant Unsigned_64 := Revision (Source);
      function Held (DMA : Unsigned_64) return Boolean is
        (Exclusive and then Sealed (Source) and then Revision (Source) = Epoch
         and then Owned_Table (DMA) and then Exclusive);
      -- Provenance resolution is a trusted callback, but may observe/revoke
      -- an owner while returning its lookup result. Do not let an earlier
      -- exclusion sample authorize the next memory access after that callback.
   begin
      Accepted := False;
      if Object.Begun then return; end if;
      Object.Begun := True;
      Object.Phase := Failed;
      if not Held (Retained_Root) then return; end if;
      Resolve (Source, GPU, Bytes, Retained_Root, New_Pages, Plan, OK);
      if not OK or else not Held (Retained_Root) then return; end if;
      Object.Root := Root_DMA (Source); Object.Epoch := Epoch;
      Object.GPU := GPU; Object.Bytes := Bytes;
      Object.Hardware_Root := Retained_Root;
      Object.Count := New_Pages'Length;
      for N in 1 .. Object.Count loop
         Object.Pages (N) := Plan (N).Child_DMA;
         Object.Plan (N) := (Plan (N).Parent_DMA, Plan (N).Child_DMA,
           Plan (N).Expected, Plan (N).Value, Plan (N).Fill, Plan (N).Index);
      end loop;
      Object.Cursor := 1; Object.Word := 0; Object.Phase := Fill_Child;
      Accepted := True;
   end Start;
   procedure Step (Object : in out State; Source : Image) is
      Phase : constant Growth_Phase := Object.Phase;
      Item : constant Growth_Link := Object.Plan (Object.Cursor);
      OK : Boolean;
      Value : Unsigned_64;
      function Held (DMA : Unsigned_64) return Boolean is
        (Exclusive and then Sealed (Source) and then Root_DMA (Source) = Object.Root
         and then Revision (Source) = Object.Epoch and then Owned_Table (DMA)
         and then Exclusive);
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
      Nodes : Node_List (1 .. Capacity);
      Checked : Links (1 .. Capacity);
      Count, Base, Parent : Natural;
      OK : Boolean;
   begin
      Accepted := False;
      if Object.Commit_Tried then return; end if;
      Object.Commit_Tried := True;
      if Pending (Object) then Object.Phase := Failed; end if;
      if not Object.Done or else not Exclusive or else not Sealed (Source)
        or else Root_DMA (Source) /= Object.Root or else Revision (Source) /= Object.Epoch
        or else Source.Epoch = Unsigned_64'Last or else not Invalidation_Confirmed
      then return; end if;
      Resolve (Source, Object.GPU, Object.Bytes, Object.Hardware_Root,
               Object.Pages (1 .. Object.Count), Checked, OK);
      if not OK then return; end if;
      Describe (Source, Object.GPU, Object.Bytes, Nodes, Count, OK);
      if not OK or else Count /= Object.Count or else not Exclusive
        or else Revision (Source) /= Object.Epoch or else not Invalidation_Confirmed
      then return; end if;
      Base := Source.Count;
      -- All fallible checks/callbacks precede mutation. Serialized owner only.
      -- Logical holes stay zero; Entry_Value exports the scratch fallback.
      for N in 1 .. Count loop
         Source.DMA (Base + N) := Object.Pages (N);
         Source.Levels (Base + N) := Nodes (N).Level;
         Clear_Table (Source, Base + N);
      end loop;
      for N in 1 .. Count loop
         Parent := (if Nodes (N).Existing_Parent /= 0 then Nodes (N).Existing_Parent
                    else Base + Nodes (N).New_Parent);
         Set_Raw_Word (Source, Parent, Nodes (N).Index, Checked (N).Value);
      end loop;
      Source.Count := Base + Count; Source.Epoch := Source.Epoch + 1;
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
         if Source.DMA (Object.First_Adopted + N - 1) /= Object.Pages (N) or else
           not Owned_Table (Object.Pages (N)) or else not Exclusive
         then return; end if;
      end loop;
      if not Exclusive or else not Sealed (Source) or else
        Revision (Source) /= Epoch or else Root_DMA (Source) /= Object.Root
      then return; end if;
      -- The source/provenance retains the pages. Only this transaction receipt
      -- is cleared; no hardware writes, TLB actions or allocator releases.
      Object.Begun := False; Object.Done := False;
      Object.Commit_Tried := False; Object.Adopted := False;
      Object.Root := 0; Object.Epoch := 0; Object.GPU := 0; Object.Bytes := 0;
      Object.Hardware_Root := 0; Object.Count := 0; Object.First_Adopted := 0;
      Object.Pages := [others => 0];
      Object.Plan := [others => (others => <>)];
      Object.Phase := Idle; Object.Cursor := 1; Object.Word := 0;
      Accepted := True;
   end Rearm;
end Intel_GPU_VM_Image.Growth.Backing.Writer;
