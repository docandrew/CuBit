package body Intel_GPU_VM_Image.Removal is
   use Intel_GPU_ADLN_PPGTT;
   function Failed (State : Controller) return Boolean is (State.Poisoned);
   function Publishing (State : Controller) return Boolean is (State.Active);
   function Published (State : Controller) return Boolean is (State.Pending and not State.Active);
   function Preparing (State : Controller) return Boolean is (State.Prepare_Active);
   function Committing (State : Controller) return Boolean is (State.Commit_Active);
   procedure Cancel_Commit (State : in out Controller) is
   begin
      if State.Commit_Active then
         State.Commit_Active := False;
         State.Poisoned := True;
      end if;
   end Cancel_Commit;
   procedure Cancel_Prepare (State : in out Controller) is
   begin
      if State.Prepare_Active then
         State.Prepare_Active := False;
         State.Poisoned := True;
      end if;
   end Cancel_Prepare;

   procedure Find_Leaf_Step (State : in out Controller; Object : Image;
                            GPU : Unsigned_64; Finished : out Boolean;
                            Page : out Natural) is
      W : constant Walk := Locate (GPU);
      Route : constant array (1 .. 3) of Table_Index := [W.PML4, W.PDP, W.PD];
   begin
      Finished := True; Page := 0;
      -- Callers have authenticated the retained source root/epoch. Removal
      -- only changes leaf words, never directory topology or table identity.
      if State.Cached_Leaf /= 0 and then State.Cached_Region = GPU / 2 ** 21 then
         Page := State.Cached_Leaf; return;
      end if;
      if State.Route_Level = 0 then
         State.Route_Level := 1; State.Route_Current := 1; State.Route_Next := 2;
      end if;
      for Work in 1 .. 32 loop
         if State.Route_Next > Object.Count then
            State.Route_Level := 0; return;
         end if;
         if Raw_Word (Object, State.Route_Current, Route (State.Route_Level)) =
           Encode_Directory (Descriptor (Object, State.Route_Next).DMA)
         then
            State.Route_Current := State.Route_Next;
            if State.Route_Level = 3 then
               State.Cached_Leaf := State.Route_Current;
               State.Cached_Region := GPU / 2 ** 21;
               State.Route_Level := 0; Page := State.Cached_Leaf; return;
            else
               State.Route_Level := State.Route_Level + 1; State.Route_Next := 2;
            end if;
         elsif State.Route_Next = Object.Count then
            State.Route_Level := 0; return;
         else
            State.Route_Next := State.Route_Next + 1;
         end if;
      end loop;
      Finished := False;
   end Find_Leaf_Step;

   procedure Begin_Prepare
     (State : in out Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Page_Count : Natural;
      Accepted : out Boolean)
   is
   begin
      Accepted := False;
      if State.Executing then
         -- Poisoned is also the normal publication admission guard. Clear the
         -- publication receipt so the outer callback cannot continue/commit.
         State.Poisoned := True; State.Prepare_Active := False;
         State.Active := False; State.Pending := False; return;
      end if;
      if State.Prepare_Active then return; end if;
      if State.Poisoned or else not Exclusive or else
        not Object.Valid or else not Object.Frozen or else
        Expected_Revision = 0 or else Expected_Revision /= Object.Epoch or else
        Object.Epoch = Unsigned_64'Last or else GPU = 0 or else GPU >= 2 ** 48 or else
        GPU mod 4096 /= 0 or else Page_Count = 0 or else
        Page_Count > Object.Mapped_Pages or else
        Unsigned_64 (Page_Count) > (2 ** 48 - GPU) / 4096
      then return; end if;
      State.Root := Descriptor (Object, 1).DMA;
      State.Epoch := Object.Epoch;
      State.First := GPU;
      State.Pages := Page_Count;
      State.Cached_Leaf := 0;
      State.Cached_Region := 0;
      State.Route_Level := 0;
      State.Cursor := 0; State.Prepare_Active := True;
      Accepted := True;
   end Begin_Prepare;

   procedure Capture_Step
     (State : in out Controller; Object : Image; Accepted : out Boolean)
   is
      Page : Natural;
      Address, Word, DMA : Unsigned_64;
      Found : Boolean;
      function Current return Boolean is
        (Exclusive and then Object.Valid and then Object.Frozen and then
         Object.Epoch = State.Epoch and then Root_DMA (Object) = State.Root);
   begin
      Accepted := False;
      if State.Executing then
         State.Poisoned := True; State.Prepare_Active := False;
         State.Active := False; State.Pending := False; return;
      end if;
      if not State.Prepare_Active then return; end if;
      if State.Poisoned or else not Current then
         State.Prepare_Active := False; return;
      end if;
      Address := State.First + Unsigned_64 (State.Cursor) * 4096;
      Find_Leaf_Step (State, Object, Address, Found, Page);
      if not Found then Accepted := True; return; end if;
      if Page = 0 then State.Prepare_Active := False; return; end if;
      State.Executing := True;
      DMA := Expected_Page (State.Cursor + 1);
      State.Executing := False;
      if State.Poisoned or else not State.Prepare_Active or else
        not Current or else not Valid_DMA_Page (DMA)
      then State.Prepare_Active := False; return; end if;
      Word := Raw_Word (Object, Page, Locate (Address).PT);
      if Word = 0 or else Word - Word mod 4096 /= DMA then
         State.Prepare_Active := False; return;
      end if;
      State.Cursor := State.Cursor + 1;
      if State.Cursor = State.Pages then
         -- Own the fully checked immutable source through publication/commit.
         State.Prepare_Active := False; State.Poisoned := True;
         State.Cursor := 0; State.Active := True;
      end if;
      Accepted := True;
   end Capture_Step;

   procedure Start_From_Pages
     (State : in out Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Page_Count : Natural;
      Accepted : out Boolean)
   is
      procedure Capture is new Capture_Step (Expected_Page);
   begin
      Begin_Prepare (State, Object, Expected_Revision, GPU, Page_Count, Accepted);
      if not Accepted then return; end if;
      while Preparing (State) loop
         Capture (State, Object, Accepted);
         if not Accepted then return; end if;
      end loop;
      Accepted := Publishing (State);
   end Start_From_Pages;
   procedure Start
     (State : in out Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Expected : Data_Pages;
      Accepted : out Boolean) is
      function Page (Ordinal : Positive) return Unsigned_64 is
        (Expected (Expected'First + (Ordinal - 1)));
      procedure Stream is new Start_From_Pages (Page);
   begin
      Stream (State, Object, Expected_Revision, GPU, Expected'Length, Accepted);
   end Start;
   procedure Step (State : in out Controller; Object : Image) is
      Page : Natural;
      Address, Word : Unsigned_64;
      OK, Found : Boolean;
      function Current return Boolean is
        (Exclusive and then Object.Valid and then Object.Frozen and then
         Descriptor (Object, 1).DMA = State.Root and then Object.Epoch = State.Epoch);
   begin
      if not State.Active then return; end if;
      if State.Executing or else not Current then State.Active := False; return; end if;
      Address := State.First + Unsigned_64 (State.Cursor) * 4096;
      Find_Leaf_Step (State, Object, Address, Found, Page);
      if not Found then return; end if;
      if Page = 0 then State.Active := False; return; end if;
      Word := Raw_Word (Object, Page, Locate (Address).PT);
      if Word = 0 then State.Active := False; return; end if;
      State.Executing := True;
      Write_Leaf (Descriptor (Object, Page).DMA, Locate (Address).PT, Word,
        Intel_GPU_PPGTT_Scratch.Fallback (Object.Scratch, 0), OK);
      State.Executing := False;
      if not OK or else not Current or else not State.Active then
         State.Active := False; return;
      end if;
      State.Cursor := State.Cursor + 1;
      if State.Cursor = State.Pages then State.Active := False; State.Pending := True; end if;
   end Step;
   procedure Publish
     (State : in out Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Expected : Data_Pages;
      Accepted : out Boolean) is
   begin
      Start (State, Object, Expected_Revision, GPU, Expected, Accepted);
      if not Accepted then return; end if;
      while Publishing (State) loop Step (State, Object); end loop;
      Accepted := Published (State);
   end Publish;

   procedure Begin_Commit
     (State : in out Controller; Object : in out Image;
      Invalidation_Completed : Boolean; Accepted : out Boolean)
   is begin
      Accepted := False;
      if State.Commit_Active then return; end if;
      if State.Active or else State.Executing or else State.Prepare_Active then
         if State.Executing or else State.Prepare_Active then State.Poisoned := True; end if;
         State.Prepare_Active := False;
         State.Active := False; State.Pending := False; return;
      end if;
      if not State.Pending then return; end if;
      State.Pending := False;
      if not Invalidation_Completed or else not Exclusive or else
        not Object.Valid or else not Object.Frozen or else
        Descriptor (Object, 1).DMA /= State.Root or else Object.Epoch /= State.Epoch
      then return; end if;
      State.Cursor := 0; State.Route_Level := 0;
      State.Commit_Active := True;
      Object.Valid := False;
      Accepted := True;
   end Begin_Commit;

   procedure Commit_Step
     (State : in out Controller; Object : in out Image; Complete : out Boolean)
   is
      Page : Natural;
      Address : Unsigned_64;
      Found : Boolean;
   begin
      Complete := False;
      if not State.Commit_Active then return; end if;
      if not Exclusive or else Object.Valid or else not Object.Frozen or else
        Object.Count = 0 or else Descriptor (Object, 1).DMA /= State.Root or else
        Object.Epoch /= State.Epoch
      then
         -- Do not alter an unrelated object supplied by a mistaken caller.
         State.Commit_Active := False; return;
      end if;
      Address := State.First + Unsigned_64 (State.Cursor) * 4096;
      Find_Leaf_Step (State, Object, Address, Found, Page);
      if not Found then return; end if;
      if Page = 0 or else Raw_Word (Object, Page, Locate (Address).PT) = 0 then
         State.Commit_Active := False; return;
      end if;
      Set_Raw_Word (Object, Page, Locate (Address).PT, 0);
      State.Cursor := State.Cursor + 1;
      if State.Cursor /= State.Pages then return; end if;
      Object.Mapped_Pages := Object.Mapped_Pages - State.Pages;
      Object.Epoch := Object.Epoch + 1;
      Object.Predecessor_Root := 0;
      Object.Predecessor_Epoch := 0;
      State.Poisoned := False;
      State.Commit_Active := False;
      Object.Valid := True;
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
      Expected_Revision, GPU : Unsigned_64; Expected : Data_Pages;
      Accepted : out Boolean)
   is
      OK : Boolean;
   begin
      Publish (State, Object, Expected_Revision, GPU, Expected, Accepted);
      if not Accepted then return; end if;
      if not Exclusive then
         Commit (State, Object, False, Accepted);
         return;
      end if;
      -- The pending publication is not a completed invalidation receipt.
      -- Consume it on nested commit/start rather than accepting a callback's
      -- premature completion claim while the invalidation is still running.
      State.Executing := True;
      Invalidate (OK);
      State.Executing := False;
      Commit (State, Object, OK, Accepted);
   end Execute;
end Intel_GPU_VM_Image.Removal;
