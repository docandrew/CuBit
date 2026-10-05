package body Intel_GPU_VM_Image.Removal is
   use Intel_GPU_ADLN_PPGTT;
   function Failed (State : Controller) return Boolean is (State.Poisoned);
   function Publishing (State : Controller) return Boolean is (State.Active);
   function Published (State : Controller) return Boolean is (State.Pending and not State.Active);

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
               Next_Page := P;
               exit;
            end if;
         end loop;
         if Next_Page = 0 then return 0; end if;
         Current := Next_Page;
      end loop;
      return Current;
   end Leaf_Page;

   procedure Start_From_Pages
     (State : in out Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Page_Count : Natural;
      Accepted : out Boolean)
   is
      Page : Natural;
      Address, Word, DMA : Unsigned_64;
      Root : constant Unsigned_64 := Root_DMA (Object);
   begin
      Accepted := False;
      if State.Executing then State.Poisoned := True; return; end if;
      if State.Poisoned or else not Exclusive or else
        not Object.Valid or else not Object.Frozen or else
        Expected_Revision = 0 or else Expected_Revision /= Object.Epoch or else
        Object.Epoch = Unsigned_64'Last or else GPU = 0 or else GPU >= 2 ** 48 or else
        GPU mod 4096 /= 0 or else Page_Count = 0 or else
        Page_Count > Object.Mapped_Pages or else
        Unsigned_64 (Page_Count) > (2 ** 48 - GPU) / 4096
      then return; end if;
      for I in 1 .. Page_Count loop
         State.Executing := True;
         DMA := Expected_Page (I);
         State.Executing := False;
         if State.Poisoned or else not Exclusive or else
           not Object.Valid or else not Object.Frozen or else
           Object.Epoch /= Expected_Revision or else Root_DMA (Object) /= Root or else
           not Valid_DMA_Page (DMA)
         then return; end if;
         Address := GPU + Unsigned_64 (I - 1) * 4096;
         Page := Leaf_Page (Object, Address);
         if Page = 0 then return; end if;
         Word := Raw_Word (Object, Page, Locate (Address).PT);
         if Word = 0 or else Word - Word mod 4096 /= DMA then return; end if;
      end loop;
      -- Own the immutable source/range across every publication turn.
      State.Poisoned := True;
      State.Root := Descriptor (Object, 1).DMA;
      State.Epoch := Object.Epoch;
      State.First := GPU;
      State.Pages := Page_Count;
      State.Cursor := 0; State.Active := True;
      Accepted := True;
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
      OK : Boolean;
      function Current return Boolean is
        (Exclusive and then Object.Valid and then Object.Frozen and then
         Descriptor (Object, 1).DMA = State.Root and then Object.Epoch = State.Epoch);
   begin
      if not State.Active then return; end if;
      if State.Executing or else not Current then State.Active := False; return; end if;
      Address := State.First + Unsigned_64 (State.Cursor) * 4096;
      Page := Leaf_Page (Object, Address);
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

   procedure Commit
     (State : in out Controller; Object : in out Image;
      Invalidation_Completed : Boolean; Accepted : out Boolean)
   is
      Page : Natural;
      Address : Unsigned_64;
   begin
      Accepted := False;
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
      for I in 1 .. State.Pages loop
         Address := State.First + Unsigned_64 (I - 1) * 4096;
         Page := Leaf_Page (Object, Address);
         Set_Raw_Word (Object, Page, Locate (Address).PT, 0);
      end loop;
      Object.Mapped_Pages := Object.Mapped_Pages - State.Pages;
      Object.Epoch := Object.Epoch + 1;
      Object.Predecessor_Root := 0;
      Object.Predecessor_Epoch := 0;
      State.Poisoned := False;
      Accepted := True;
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
      Invalidate (OK);
      Commit (State, Object, OK, Accepted);
   end Execute;
end Intel_GPU_VM_Image.Removal;
