package body Intel_GPU_VM_Image.Insertion is
   use Intel_GPU_ADLN_PPGTT;
   function Failed (State : Controller) return Boolean is (State.Poisoned);
   function Publishing (State : Controller) return Boolean is (State.Active);
   function Published (State : Controller) return Boolean is (State.Pending and not State.Active);
   function Committing (State : Controller) return Boolean is (State.Committing);
   function Publication_Table (State : Controller; Object : Image) return Natural is
     (if State.Active and then State.Executing and then State.Commit_Depth = 3 and then
         Exclusive and then Object.Valid and then Object.Frozen and then
         Object.Epoch = State.Epoch and then Root_DMA (Object) = State.Root and then
         State.Commit_Table <= Object.Count
      then State.Commit_Table else 0);
   function Publication_Matches
     (State : Controller; Object : Image; Table_DMA : Unsigned_64;
      Index : Table_Index; Expected, Replacement : Unsigned_64) return Boolean is
      Page : constant Natural := Publication_Table (State, Object);
   begin
      return Page /= 0 and then State.Cursor < State.Pages and then
        Descriptor (Object, Page).DMA = Table_DMA and then
        Locate (State.First + Unsigned_64 (State.Cursor) * 4096).PT = Index and then
        Expected = Intel_GPU_PPGTT_Scratch.Fallback (Object.Scratch, 0) and then
        Replacement = Leaf_Storage.Get (State.Words, State.Cursor + 1);
   end Publication_Matches;
   -- The retained route cursor is shared by publication and metadata commit;
   -- each phase resets it before use. Never scan all descriptors in one turn.
   procedure Advance_Route
     (State : in out Controller; Object : Image; Address : Unsigned_64;
      Missing : out Boolean)
   is
      W : constant Walk := Locate (Address);
      Route : constant array (0 .. 2) of Table_Index := [W.PML4, W.PDP, W.PD];
   begin
      Missing := False;
      if Address / (512 * 4096) /= State.Commit_Region then
         State.Commit_Depth := 0;
         State.Commit_Table := 1;
         State.Commit_Candidate := 2;
         State.Commit_Region := Address / (512 * 4096);
      end if;
      for Work in 1 .. 32 loop
         exit when State.Commit_Depth = 3;
         if State.Commit_Candidate > Object.Count then
            Missing := True; return;
         end if;
         if Raw_Word (Object, State.Commit_Table, Route (State.Commit_Depth)) =
           Encode_Directory (Descriptor (Object, State.Commit_Candidate).DMA)
         then
            State.Commit_Table := State.Commit_Candidate;
            State.Commit_Depth := State.Commit_Depth + 1;
            State.Commit_Candidate := 2;
         else
            State.Commit_Candidate := State.Commit_Candidate + 1;
         end if;
      end loop;
   end Advance_Route;
   function Leaf_Page (Object : Image; GPU : Unsigned_64) return Natural is
      W : constant Walk := Locate (GPU);
      Route : constant array (1 .. 3) of Table_Index := [W.PML4, W.PDP, W.PD];
      Current : Natural := 1;
      Next_Page : Natural;
   begin
      for Index of Route loop
         Next_Page := 0;
         for P in 2 .. Object.Count loop
            if Raw_Word (Object, Current, Index) = Encode_Directory (Descriptor (Object, P).DMA) then
               Next_Page := P; exit;
            end if;
         end loop;
         if Next_Page = 0 then return 0; end if;
         Current := Next_Page;
      end loop;
      return Current;
   end Leaf_Page;
   function Range_Reusable
     (Object : Image; GPU, Bytes : Unsigned_64) return Boolean
   is
      Page : Natural;
      Address : Unsigned_64;
   begin
      if not Object.Valid or else not Object.Frozen or else
        GPU = 0 or else GPU >= 2 ** 48 or else GPU mod 4096 /= 0 or else
        Bytes = 0 or else Bytes mod 4096 /= 0 or else Bytes > 2 ** 48 - GPU or else
        Bytes / 4096 > Unsigned_64 (Capacity * 512 - Object.Mapped_Pages)
      then return False; end if;
      for I in 0 .. Natural (Bytes / 4096) - 1 loop
         Address := GPU + Unsigned_64 (I) * 4096;
         Page := Leaf_Page (Object, Address);
         if Page = 0 or else Raw_Word (Object, Page, Locate (Address).PT) /= 0
         then return False; end if;
      end loop;
      return True;
   end Range_Reusable;
   function Can_Reuse_From_Pages
     (State : Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Page_Count : Natural;
      Policy : Cache_Policy; Access_Mode : Page_Access) return Boolean
   is
      Page : Natural;
      Address, DMA : Unsigned_64;
   begin
      if State.Poisoned or else State.Pending or else
        not Object.Valid or else not Object.Frozen or else
        Expected_Revision = 0 or else Expected_Revision /= Object.Epoch or else
        Object.Epoch = Unsigned_64'Last or else GPU = 0 or else GPU >= 2 ** 48 or else
        GPU mod 4096 /= 0 or else Page_Count = 0 or else
        Page_Count > Capacity * 512 - Object.Mapped_Pages or else
        Page_Count > Insertion_Capacity (State) or else
        Unsigned_64 (Page_Count) > (2 ** 48 - GPU) / 4096
      then return False; end if;
      -- Validate the entire request before even the first leaf write.
      for I in 1 .. Page_Count loop
         DMA := Data_Page (I);
         if Encode_Leaf (DMA, Policy, Access_Mode) = 0 or else
           Intel_GPU_PPGTT_Scratch.Contains (Object.Scratch, DMA)
         then return False; end if;
         for P in 1 .. Object.Backed loop
            if DMA = Descriptor (Object, P).DMA then return False; end if;
         end loop;
         Address := GPU + Unsigned_64 (I - 1) * 4096;
         Page := Leaf_Page (Object, Address);
         if Page = 0 or else Raw_Word (Object, Page, Locate (Address).PT) /= 0
         then return False; end if;
      end loop;
      -- Same cache-alias rule as the offline Map_Pages builder.
      for P in 1 .. Object.Count loop
         for Word of Raw_Page (Object, P) loop
            if Word /= 0 and then Word mod 4096 /= 3 + Cache_Bits (Policy) then
               for I in 1 .. Page_Count loop
                  DMA := Data_Page (I);
                  if Word / 4096 = DMA / 4096 then return False; end if;
               end loop;
            end if;
         end loop;
      end loop;
      return True;
   end Can_Reuse_From_Pages;
   function Can_Reuse
     (State : Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Data : Data_Pages;
      Policy : Cache_Policy; Access_Mode : Page_Access) return Boolean is
      function Page (Ordinal : Positive) return Unsigned_64 is
        (Data (Data'First + (Ordinal - 1)));
      function Check is new Can_Reuse_From_Pages (Page);
   begin
      return Check (State, Object, Expected_Revision, GPU, Data'Length, Policy, Access_Mode);
   end Can_Reuse;
   function Preparing (State : Controller) return Boolean is (State.Preparing);
   function Captured (State : Controller) return Boolean is
     (State.Preparing and then State.Cursor = State.Pages);
   procedure Begin_Prepare
     (State : in out Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Page_Count : Natural;
      Policy : Cache_Policy; Access_Mode : Page_Access; Accepted : out Boolean)
   is
      Root : constant Unsigned_64 := Root_DMA (Object);
      function Current return Boolean is
        (not State.Poisoned and then Exclusive and then Object.Valid and then
         Object.Frozen and then Object.Epoch = Expected_Revision and then
         Root_DMA (Object) = Root);
   begin
      Accepted := False;
      if State.Executing then State.Poisoned := True; State.Preparing := False; return; end if;
      if State.Preparing then return; end if;
      if not Current or else State.Pending or else Expected_Revision = 0 or else
        Expected_Revision = Unsigned_64'Last or else Page_Count = 0 or else
        Page_Count > Insertion_Capacity (State) or else
        Page_Count > Capacity * 512 - Object.Mapped_Pages or else
        GPU = 0 or else GPU >= 2 ** 48 or else GPU mod 4096 /= 0 or else
        Unsigned_64 (Page_Count) > (2 ** 48 - GPU) / 4096
      then return; end if;
      State.Root := Root;
      State.Epoch := Expected_Revision;
      State.First := GPU;
      State.Pages := Page_Count;
      State.Cursor := 0;
      State.Capture_Policy := Policy;
      State.Capture_Access := Access_Mode;
      State.Check_Phase := Check_DMA;
      State.Check_Data := 1;
      State.Check_Table := 1;
      State.Check_Word := 0;
      State.Check_Alias := 1;
      State.Preparing := True;
      Accepted := True;
   end Begin_Prepare;
   procedure Capture_Step
     (State : in out Controller; Object : Image; Accepted : out Boolean)
   is
      Word : Unsigned_64;
      function Current return Boolean is
        (State.Preparing and then not State.Poisoned and then Exclusive and then
         Object.Valid and then Object.Frozen and then Object.Epoch = State.Epoch and then
         Root_DMA (Object) = State.Root);
   begin
      Accepted := False;
      if State.Executing then State.Poisoned := True; State.Preparing := False; return; end if;
      if not Current then State.Preparing := False; return; end if;
      for Work in 1 .. 32 loop
         exit when State.Cursor = State.Pages;
         State.Executing := True;
         Word := Encode_Leaf (Data_Page (State.Cursor + 1), State.Capture_Policy, State.Capture_Access);
         State.Executing := False;
         if not Current or else Word = 0 then State.Preparing := False; return; end if;
         State.Cursor := State.Cursor + 1;
         Leaf_Storage.Put (State.Words, State.Cursor, Word);
      end loop;
      Accepted := True;
   end Capture_Step;
   procedure Finish_Prepare
     (State : in out Controller; Object : Image; Accepted : out Boolean)
   is
      DMA, Word : Unsigned_64;
      function Current return Boolean is
        (not State.Poisoned and then Exclusive and then Object.Valid and then
         Object.Frozen and then Object.Epoch = State.Epoch and then
         Root_DMA (Object) = State.Root);
   begin
      Accepted := False;
      if State.Executing then State.Poisoned := True; State.Preparing := False; return; end if;
      if not Captured (State) then return; end if;
      if not Current then State.Preparing := False; return; end if;
      -- Each iteration is a single bounded validation action, never a scan
      -- over the backing set, descriptor set or retained request.
      for Work in 1 .. 32 loop
         DMA := Leaf_Storage.Get (State.Words, State.Check_Data) / 4096 * 4096;
         case State.Check_Phase is
            when Check_DMA =>
               if Intel_GPU_PPGTT_Scratch.Contains (Object.Scratch, DMA) then
                  State.Preparing := False; return;
               end if;
               State.Check_Table := 1;
               State.Check_Phase := Check_Tables;
            when Check_Tables =>
               if DMA = Descriptor (Object, State.Check_Table).DMA then
                  State.Preparing := False; return;
               end if;
               if State.Check_Table = Object.Backed then
                  State.Check_Depth := 0;
                  State.Check_Route_Table := 1;
                  State.Check_Candidate := 2;
                  State.Check_Phase := Check_Route;
               else
                  State.Check_Table := State.Check_Table + 1;
               end if;
            when Check_Route =>
               if State.Check_Candidate > Object.Count then
                  State.Preparing := False; return;
               end if;
               declare
                  W : constant Walk := Locate
                    (State.First + Unsigned_64 (State.Check_Data - 1) * 4096);
                  Route : constant array (0 .. 2) of Table_Index := [W.PML4, W.PDP, W.PD];
               begin
                  if Raw_Word (Object, State.Check_Route_Table, Route (State.Check_Depth)) =
                    Encode_Directory (Descriptor (Object, State.Check_Candidate).DMA)
                  then
                     State.Check_Route_Table := State.Check_Candidate;
                     State.Check_Depth := State.Check_Depth + 1;
                     State.Check_Candidate := 2;
                     if State.Check_Depth = 3 then State.Check_Phase := Check_Leaf; end if;
                  else
                     State.Check_Candidate := State.Check_Candidate + 1;
                  end if;
               end;
            when Check_Leaf =>
               if Raw_Word (Object, State.Check_Route_Table,
                 Locate (State.First + Unsigned_64 (State.Check_Data - 1) * 4096).PT) /= 0
               then State.Preparing := False; return; end if;
               if State.Check_Data = State.Pages then
                  State.Check_Phase := Check_Cache;
                  State.Check_Table := 1;
                  State.Check_Word := 0;
                  State.Check_Alias := 1;
               else
                  State.Check_Data := State.Check_Data + 1;
                  State.Check_Phase := Check_DMA;
               end if;
            when Check_Cache =>
               Word := Raw_Word (Object, State.Check_Table, State.Check_Word);
               if Word /= 0 and then Word mod 4096 /= 3 + Cache_Bits (State.Capture_Policy) then
                  if Word / 4096 = Leaf_Storage.Get (State.Words, State.Check_Alias) / 4096 then
                     State.Preparing := False; return;
                  end if;
                  if State.Check_Alias < State.Pages then
                     State.Check_Alias := State.Check_Alias + 1;
                     goto Next_Work;
                  end if;
               end if;
               State.Check_Alias := 1;
               if State.Check_Word < Table_Index'Last then
                  State.Check_Word := State.Check_Word + 1;
               elsif State.Check_Table < Object.Count then
                  State.Check_Table := State.Check_Table + 1;
                  State.Check_Word := 0;
               else
                  State.Preparing := False;
                  State.Poisoned := True;
                  State.Cursor := 0;
                  State.Commit_Depth := 0;
                  State.Commit_Table := 1;
                  State.Commit_Candidate := 2;
                  State.Commit_Region := State.First / (512 * 4096);
                  State.Active := True;
                  Accepted := True;
                  return;
               end if;
         end case;
         <<Next_Work>>
      end loop;
      Accepted := True;
   end Finish_Prepare;
   procedure Start_From_Pages
     (State : in out Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Page_Count : Natural;
      Policy : Cache_Policy; Access_Mode : Page_Access; Accepted : out Boolean)
   is
      procedure Capture is new Capture_Step (Data_Page);
   begin
      Begin_Prepare (State, Object, Expected_Revision, GPU, Page_Count, Policy, Access_Mode, Accepted);
      if not Accepted then return; end if;
      while not Captured (State) loop
         Capture (State, Object, Accepted);
         if not Accepted then return; end if;
      end loop;
      while Preparing (State) loop
         Finish_Prepare (State, Object, Accepted);
         if not Accepted then return; end if;
      end loop;
   end Start_From_Pages;
   procedure Start
     (State : in out Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Data : Data_Pages;
      Policy : Cache_Policy; Access_Mode : Page_Access; Accepted : out Boolean) is
      function Page (Ordinal : Positive) return Unsigned_64 is
        (Data (Data'First + (Ordinal - 1)));
      procedure Stream is new Start_From_Pages (Page);
   begin
      Stream (State, Object, Expected_Revision, GPU, Data'Length, Policy, Access_Mode, Accepted);
   end Start;
   procedure Step (State : in out Controller; Object : Image) is
      Page : Natural;
      Address : Unsigned_64;
      OK, Missing : Boolean;
      function Current return Boolean is
        (Exclusive and then Object.Valid and then Object.Frozen and then
         Descriptor (Object, 1).DMA = State.Root and then Object.Epoch = State.Epoch);
   begin
      if not State.Active then return; end if;
      if State.Executing or else not Current then
         State.Active := False; return;
      end if;
      Address := State.First + Unsigned_64 (State.Cursor) * 4096;
      Advance_Route (State, Object, Address, Missing);
      if Missing then State.Active := False; return; end if;
      if State.Commit_Depth /= 3 then return; end if;
      Page := State.Commit_Table;
      if Page = 0 or else Raw_Word (Object, Page, Locate (Address).PT) /= 0 then
         State.Active := False; return;
      end if;
      State.Executing := True;
      Write_Leaf (Descriptor (Object, Page).DMA, Locate (Address).PT,
        Intel_GPU_PPGTT_Scratch.Fallback (Object.Scratch, 0),
        Leaf_Storage.Get (State.Words, State.Cursor + 1), OK);
      State.Executing := False;
      if not OK or else not Current or else not State.Active then
         State.Active := False; return;
      end if;
      State.Cursor := State.Cursor + 1;
      if State.Cursor = State.Pages then
         State.Active := False; State.Pending := True;
      end if;
   end Step;
   procedure Publish
     (State : in out Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Data : Data_Pages;
      Policy : Cache_Policy; Access_Mode : Page_Access; Accepted : out Boolean) is
   begin
      Start (State, Object, Expected_Revision, GPU, Data, Policy, Access_Mode, Accepted);
      if not Accepted then return; end if;
      while Publishing (State) loop Step (State, Object); end loop;
      Accepted := Published (State);
   end Publish;
   procedure Begin_Commit
     (State : in out Controller; Object : in out Image;
      Invalidation_Completed : Boolean; Accepted : out Boolean)
   is begin
      Accepted := False;
      if State.Committing then return; end if;
      if State.Active or else State.Executing then
         if State.Executing then State.Poisoned := True; end if;
         State.Active := False; State.Pending := False; return;
      end if;
      if not State.Pending then return; end if;
      State.Pending := False;
      if not Invalidation_Completed or else not Exclusive or else
        not Object.Valid or else not Object.Frozen or else
        Descriptor (Object, 1).DMA /= State.Root or else Object.Epoch /= State.Epoch
      then return; end if;
      State.Cursor := 0;
      State.Committing := True;
      State.Commit_Depth := 0;
      State.Commit_Table := 1;
      State.Commit_Candidate := 2;
      State.Commit_Region := State.First / (512 * 4096);
      -- A resumable commit must not expose a partially adopted mirror to
      -- another controller. Only this retained receipt can make it valid again.
      Object.Valid := False;
      Accepted := True;
   end Begin_Commit;
   procedure Commit_Step
     (State : in out Controller; Object : in out Image;
      Complete : out Boolean)
   is
      Page : Natural;
      Address : Unsigned_64;
      Missing : Boolean;
   begin
      Complete := False;
      if not State.Committing then return; end if;
      if not Exclusive or else Object.Valid or else not Object.Frozen or else
        Object.Count = 0 or else
        Descriptor (Object, 1).DMA /= State.Root or else Object.Epoch /= State.Epoch
      then
         State.Committing := False;
         -- The original image remains invalid even if this is another object.
         -- Never invalidate the unrelated object supplied by a bad caller.
         return;
      end if;
      Address := State.First + Unsigned_64 (State.Cursor) * 4096;
      -- Resume descriptor search, bounded independently of the table count.
      -- Adjacent leaves reuse the route until crossing a 2MiB PT boundary.
      Advance_Route (State, Object, Address, Missing);
      if Missing then State.Committing := False; return; end if;
      if State.Commit_Depth /= 3 then return; end if;
      Page := State.Commit_Table;
      if Page = 0 or else Raw_Word (Object, Page, Locate (Address).PT) /= 0 then
         State.Committing := False;
         Object.Valid := False;
         return;
      end if;
      Set_Raw_Word (Object, Page, Locate (Address).PT,
        Leaf_Storage.Get (State.Words, State.Cursor + 1));
      State.Cursor := State.Cursor + 1;
      if State.Cursor /= State.Pages then return; end if;
      Object.Mapped_Pages := Object.Mapped_Pages + State.Pages;
      Object.Epoch := Object.Epoch + 1;
      Object.Predecessor_Root := 0; Object.Predecessor_Epoch := 0;
      Object.Valid := True;
      State.Poisoned := False;
      State.Committing := False;
      Complete := True;
   end Commit_Step;
   procedure Commit
     (State : in out Controller; Object : in out Image;
      Invalidation_Completed : Boolean; Accepted : out Boolean) is
   begin
      Begin_Commit (State, Object, Invalidation_Completed, Accepted);
      if not Accepted then return; end if;
      Accepted := False;
      while Committing (State) loop
         Commit_Step (State, Object, Accepted);
      end loop;
   end Commit;
   procedure Execute
     (State : in out Controller; Object : in out Image;
      Expected_Revision, GPU : Unsigned_64; Data : Data_Pages;
      Policy : Cache_Policy; Access_Mode : Page_Access; Accepted : out Boolean)
   is
      OK : Boolean;
   begin
      Publish (State, Object, Expected_Revision, GPU, Data, Policy, Access_Mode, Accepted);
      if not Accepted then return; end if;
      if not Exclusive then
         Commit (State, Object, False, Accepted); return;
      end if;
      Invalidate (OK);
      Commit (State, Object, OK, Accepted);
   end Execute;
end Intel_GPU_VM_Image.Insertion;
