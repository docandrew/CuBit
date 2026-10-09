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
   function Preparing (Work : Preparation) return Boolean is
     (Work.Phase in Capture_Pages | Resolve_Pages);
   function Preparation_Current (Work : Preparation; Object : State; Source : Image) return Boolean is
     (not Work.Cancelled and then Object.Begun and then not Object.Commit_Tried and then not Object.Done and then
      Object.Root = Work.Root and then Object.Epoch = Work.Epoch and then
      Object.Hardware_Root = Work.Hardware_Root and then Object.Count = Work.Count and then
      Sealed (Source) and then Root_DMA (Source) = Work.Root and then Revision (Source) = Work.Epoch);
   function Prepared (Work : Preparation; Object : State; Source : Image) return Boolean is
     (Work.Phase = Ready and then Object.Phase = Fill_Child and then
      Preparation_Current (Work, Object, Source));
   procedure Cancel_Preparation (Work : in out Preparation; Object : in out State) is
   begin
      Work.Cancelled := True; Work.Phase := Rejected; Cancel_Resolution (Work.Resolver);
      Object.Phase := Failed;
   end Cancel_Preparation;
   procedure Begin_Preparation
     (Work : in out Preparation; Object : in out State; Source : Image;
      GPU, Bytes, Retained_Root : Unsigned_64; Page_Count : Natural; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Preparing (Work) or else Object.Begun or else Object.Commit_Tried then return; end if;
      if Page_Count > Growth_Capacity (Object) then return; end if;
      if Source.Count > Metadata_Capacity (Source) or else
        Page_Count > Metadata_Capacity (Source) - Source.Count then return; end if;
      Object.Begun := True; Object.Phase := Failed;
      Work.Phase := Rejected; Cancel_Resolution (Work.Resolver);
      Work.Cancelled := False;
      Object.Root := Root_DMA (Source); Object.Epoch := Revision (Source);
      Object.GPU := GPU; Object.Bytes := Bytes; Object.Hardware_Root := Retained_Root;
      Object.Count := Page_Count;
      Work.Root := Object.Root; Work.Epoch := Object.Epoch;
      Work.Hardware_Root := Retained_Root; Work.Count := Page_Count; Work.Cursor := 1;
      if Page_Count = 0 or else not Exclusive or else not Preparation_Current (Work, Object, Source)
        or else not Owned_Table (Retained_Root) or else not Exclusive
        or else not Preparation_Current (Work, Object, Source) then return; end if;
      Work.Phase := Capture_Pages; Accepted := True;
   end Begin_Preparation;
   procedure Prepare_Step (Work : in out Preparation; Object : in out State; Source : Image) is
      OK : Boolean;
      DMA : Unsigned_64;
      function Retained_Page (Ordinal : Positive) return Unsigned_64 is
        (Growth_Storage.Get (Object.Plan, Ordinal).Child_DMA);
      function Held return Boolean is
        (Preparing (Work) and then Preparation_Current (Work, Object, Source) and then
         Exclusive and then Owned_Table (Work.Hardware_Root) and then Exclusive and then
         Preparing (Work) and then Preparation_Current (Work, Object, Source));
      procedure Store_Link (Ordinal : Positive; Item : Link; Topology : Node; OK : out Boolean) is
      begin
         OK := Held and then Item.Child_DMA = Retained_Page (Ordinal);
         if OK then Growth_Storage.Put (Object.Plan, Ordinal, Retained_Link (Item, Topology)); end if;
      end Store_Link;
      procedure Resolve_Step is new Step_Resolution (Held, Retained_Page, Store_Link);
   begin
      if not Preparing (Work) then return; end if;
      if not Held then Cancel_Preparation (Work, Object); return; end if;
      if Work.Phase = Capture_Pages then
         DMA := Read_Page (Work.Cursor);
         if not Held then Cancel_Preparation (Work, Object); return; end if;
         Growth_Storage.Put (Object.Plan, Work.Cursor, (Child_DMA => DMA, others => <>));
         if Work.Cursor < Work.Count then Work.Cursor := Work.Cursor + 1; return; end if;
         Start_Resolution (Work.Resolver, Source, Object.GPU, Object.Bytes,
                           Work.Hardware_Root, Work.Count, Growth_Capacity (Object), OK);
         if not OK then Cancel_Preparation (Work, Object); return; end if;
         Work.Phase := Resolve_Pages;
         return;
      end if;
      Resolve_Step (Work.Resolver, Source);
      if not Held then Cancel_Preparation (Work, Object); return; end if;
      if Phase (Work.Resolver) in Inspecting .. Resolving then return; end if;
      if not Resolution_Valid (Work.Resolver, Source) then Cancel_Preparation (Work, Object); return; end if;
      Object.Cursor := 1; Object.Word := 0; Object.Phase := Fill_Child;
      Work.Phase := Ready;
   end Prepare_Step;
   procedure Start_From_Pages
     (Object : in out State; Source : Image; GPU, Bytes, Retained_Root : Unsigned_64;
      Page_Count : Natural; Accepted : out Boolean) is
      Work : Preparation;
      procedure Advance is new Prepare_Step (Read_Page);
   begin
      Begin_Preparation (Work, Object, Source, GPU, Bytes, Retained_Root, Page_Count, Accepted);
      if not Accepted then return; end if;
      while Preparing (Work) loop Advance (Work, Object, Source); end loop;
      Accepted := Prepared (Work, Object, Source);
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
   function Committing (Work : Adoption) return Boolean is
     (Work.Phase in Validate_Plan | Clear_Mirrors | Link_Mirrors);
   procedure Cancel_Commit (Work : in out Adoption) is
   begin
      Work.Cancelled := True; Work.Phase := Adoption_Failed; Cancel_Resolution (Work.Resolver);
   end Cancel_Commit;
   function Adoption_Current (Work : Adoption; Object : State; Source : Image) return Boolean is
     (not Work.Cancelled and then Committing (Work) and then Object.Commit_Tried and then Object.Done and then
      not Object.Adopted and then Object.Root = Work.Root and then Object.Epoch = Work.Epoch and then
      Object.Count = Work.Count and then Object.Hardware_Root = Work.Hardware_Root and then
      Source.Frozen and then Source.Count = Work.Base and then Source.Epoch = Work.Epoch and then
      Descriptor (Source, 1).DMA = Work.Root and then
      Source.Valid = (Work.Phase = Validate_Plan));
   procedure Begin_Commit
     (Work : in out Adoption; Object : in out State; Source : Image; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Committing (Work) or else Object.Commit_Tried then return; end if;
      Cancel_Commit (Work);
      Work.Cancelled := False;
      Object.Commit_Tried := True;
      if Pending (Object) then Object.Phase := Failed; end if;
      if not Object.Done or else not Exclusive or else not Source.Valid or else not Sealed (Source)
        or else Root_DMA (Source) /= Object.Root or else Revision (Source) /= Object.Epoch
        or else Source.Epoch = Unsigned_64'Last or else not Invalidation_Confirmed or else Work.Cancelled
      then return; end if;
      if Source.Count > Metadata_Capacity (Source) or else
        Object.Count > Metadata_Capacity (Source) - Source.Count then return; end if;
      Work.Root := Object.Root; Work.Epoch := Object.Epoch; Work.Hardware_Root := Object.Hardware_Root;
      Work.Base := Source.Count; Work.Count := Object.Count; Work.Cursor := 0;
      Start_Resolution (Work.Resolver, Source, Object.GPU, Object.Bytes, Object.Hardware_Root,
                        Object.Count, Object.Count, Accepted);
      if Accepted then Work.Phase := Validate_Plan; end if;
   end Begin_Commit;
   procedure Commit_Step (Work : in out Adoption; Object : in out State; Source : in out Image) is
      Parent, Page : Natural;
      OK : Boolean;
      function Held return Boolean is
        (Adoption_Current (Work, Object, Source) and then Exclusive and then
         Invalidation_Confirmed and then Exclusive and then Adoption_Current (Work, Object, Source));
      function Retained_Page (Ordinal : Positive) return Unsigned_64 is
        (Growth_Storage.Get (Object.Plan, Ordinal).Child_DMA);
      procedure Check_Link (Ordinal : Positive; Item : Link; Topology : Node; OK : out Boolean) is
      begin
         OK := Held and then
           Growth_Storage.Get (Object.Plan, Ordinal) = Retained_Link (Item, Topology);
      end Check_Link;
      procedure Resolve_Step is new Step_Resolution (Held, Retained_Page, Check_Link);
   begin
      if not Committing (Work) then return; end if;
      if not Held then Cancel_Commit (Work); return; end if;
      if Work.Phase = Validate_Plan then
         Resolve_Step (Work.Resolver, Source);
         if not Held then Cancel_Commit (Work); return; end if;
         if Phase (Work.Resolver) in Inspecting .. Resolving then return; end if;
         OK := Resolution_Valid (Work.Resolver, Source);
         if not OK then Cancel_Commit (Work); return; end if;
         -- No partial mirror is usable. Only this controller can restore valid
         -- after all retained words are adopted; cancellation leaves it hidden.
         Source.Valid := False; Work.Phase := Clear_Mirrors; Work.Cursor := 0;
         return;
      elsif Work.Phase = Clear_Mirrors then
         for Action in 1 .. 32 loop
            Page := Work.Cursor / 512 + 1;
            if Work.Cursor mod 512 = 0 then
               Set_Descriptor (Source, Work.Base + Page,
                 (Growth_Storage.Get (Object.Plan, Page).Child_DMA,
                  Growth_Storage.Get (Object.Plan, Page).Level));
            end if;
            Set_Raw_Word (Source, Work.Base + Page,
                          Intel_GPU_ADLN_PPGTT.Table_Index (Work.Cursor mod 512), 0);
            Work.Cursor := Work.Cursor + 1;
            if Work.Cursor = Work.Count * 512 then
               Work.Cursor := 1; Work.Phase := Link_Mirrors; return;
            end if;
         end loop;
         return;
      end if;
      for Action in 1 .. 32 loop
         declare Item : constant Growth_Link := Growth_Storage.Get (Object.Plan, Work.Cursor); begin
            Parent := (if Item.Existing_Parent /= 0 then Item.Existing_Parent
                       else Work.Base + Item.New_Parent);
            Set_Raw_Word (Source, Parent, Item.Index, Item.Value);
         end;
         exit when Work.Cursor = Work.Count;
         Work.Cursor := Work.Cursor + 1;
         if Action = 32 then return; end if;
      end loop;
      Source.Count := Work.Base + Work.Count; Source.Epoch := Source.Epoch + 1;
      Source.Backed := Natural'Max (Source.Backed, Source.Count);
      Source.Predecessor_Root := 0; Source.Predecessor_Epoch := 0;
      Object.First_Adopted := Work.Base + 1;
      Object.Adopted := True; Source.Valid := True; Work.Phase := Adoption_Done;
   end Commit_Step;
   procedure Commit
     (Object : in out State; Source : in out Image; Accepted : out Boolean) is
      Work : Adoption;
   begin
      Begin_Commit (Work, Object, Source, Accepted);
      if not Accepted then return; end if;
      while Committing (Work) loop Commit_Step (Work, Object, Source); end loop;
      Accepted := Object.Adopted;
   end Commit;
   function Rearm_Pending (Work : Rearming) return Boolean is
     (Work.Phase = Rearm_Checking);
   function Rearmed (Work : Rearming) return Boolean is (Work.Phase = Rearm_Done);
   procedure Cancel_Rearm (Work : in out Rearming) is
   begin
      Work.Phase := Rearm_Failed;
   end Cancel_Rearm;
   function Rearm_Current (Work : Rearming; Object : State; Source : Image) return Boolean is
     (Rearm_Pending (Work) and then Object.Adopted and then Object.Root = Work.Root
      and then Object.Epoch = Work.Receipt_Epoch and then Object.Count = Work.Count
      and then Object.First_Adopted = Work.First and then
      Object.Hardware_Root = Work.Hardware_Root and then Sealed (Source)
      and then Root_DMA (Source) = Work.Root and then Revision (Source) = Work.Epoch);
   procedure Begin_Rearm
     (Work : in out Rearming; Object : State; Source : Image;
      Retained_Root : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Rearm_Pending (Work) then return; end if;
      Work.Phase := Rearm_Failed;
      if not Object.Adopted or else not Exclusive or else not Sealed (Source) or else
        Root_DMA (Source) /= Object.Root or else Revision (Source) <= Object.Epoch or else
        Retained_Root /= Object.Hardware_Root or else Object.Count = 0 or else
        Object.First_Adopted = 0 or else Object.First_Adopted > Source.Count or else
        Object.Count > Source.Count - Object.First_Adopted + 1
      then return; end if;
      Work.Root := Object.Root; Work.Epoch := Revision (Source);
      Work.Receipt_Epoch := Object.Epoch; Work.Hardware_Root := Retained_Root;
      Work.Count := Object.Count; Work.First := Object.First_Adopted; Work.Cursor := 1;
      Work.Phase := Rearm_Checking; Accepted := True;
   end Begin_Rearm;
   procedure Rearm_Step
     (Work : in out Rearming; Object : in out State; Source : Image) is
      DMA : Unsigned_64;
      function Current return Boolean is
        (Rearm_Current (Work, Object, Source) and then Exclusive and then
         Rearm_Current (Work, Object, Source));
   begin
      if not Rearm_Pending (Work) then return; end if;
      if not Current or else not Owned_Table (Work.Hardware_Root) or else not Current
      then Cancel_Rearm (Work); return; end if;
      DMA := Growth_Storage.Get (Object.Plan, Work.Cursor).Child_DMA;
      if Descriptor (Source, Work.First + Work.Cursor - 1).DMA /= DMA or else
        not Owned_Table (DMA) or else not Current
      then Cancel_Rearm (Work); return; end if;
      if Work.Cursor < Work.Count then Work.Cursor := Work.Cursor + 1; return; end if;
      -- The source/provenance retains the pages. Invalidate the logical receipt
      -- without sweeping its growable storage. Begin_Preparation establishes a
      -- new Count, and Capture_Pages overwrites every used record before the
      -- resolver or publication can consume it; unused tail records stay inert.
      -- This is not page release, TLB retirement, or a security-domain transfer.
      Object.Begun := False; Object.Done := False;
      Object.Commit_Tried := False; Object.Adopted := False;
      Object.Root := 0; Object.Epoch := 0; Object.GPU := 0; Object.Bytes := 0;
      Object.Hardware_Root := 0; Object.Count := 0; Object.First_Adopted := 0;
      Object.Phase := Idle; Object.Cursor := 1; Object.Word := 0;
      Work.Phase := Rearm_Done;
   end Rearm_Step;
   procedure Rearm
     (Object : in out State; Source : Image; Retained_Root : Unsigned_64;
      Accepted : out Boolean) is
      Work : Rearming;
   begin
      Begin_Rearm (Work, Object, Source, Retained_Root, Accepted);
      if not Accepted then return; end if;
      while Rearm_Pending (Work) loop Rearm_Step (Work, Object, Source); end loop;
      Accepted := Rearmed (Work);
   end Rearm;
end Intel_GPU_VM_Image.Growth.Backing.Writer;
