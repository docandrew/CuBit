package body Intel_GPU_VM_Image.Removal is
   use Intel_GPU_ADLN_PPGTT;
   function Failed (State : Controller) return Boolean is (State.Poisoned);

   function Leaf_Page (Object : Image; GPU : Unsigned_64) return Natural is
      W : constant Walk := Locate (GPU);
      Route : constant array (1 .. 3) of Table_Index := [W.PML4, W.PDP, W.PD];
      Current : Natural := 1;
      Next_Page : Natural;
   begin
      for Index of Route loop
         Next_Page := 0;
         for P in 2 .. Object.Count loop
            if Raw_Word (Object, Current, Index) = Encode_Directory (Object.DMA (P)) then
               Next_Page := P;
               exit;
            end if;
         end loop;
         if Next_Page = 0 then return 0; end if;
         Current := Next_Page;
      end loop;
      return Current;
   end Leaf_Page;

   procedure Publish
     (State : in out Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Expected : Data_Pages;
      Accepted : out Boolean)
   is
      Page : Natural;
      Address, Word : Unsigned_64;
      OK : Boolean;
   begin
      Accepted := False;
      if State.Poisoned or else not Exclusive or else
        not Object.Valid or else not Object.Frozen or else
        Expected_Revision = 0 or else Expected_Revision /= Object.Epoch or else
        Object.Epoch = Unsigned_64'Last or else GPU = 0 or else GPU >= 2 ** 48 or else
        GPU mod 4096 /= 0 or else Expected'Length = 0 or else
        Expected'Length > Object.Mapped_Pages or else
        Unsigned_64 (Expected'Length) > (2 ** 48 - GPU) / 4096
      then return; end if;
      for I in Expected'Range loop
         if not Valid_DMA_Page (Expected (I)) then return; end if;
         Address := GPU + Unsigned_64 (I - Expected'First) * 4096;
         Page := Leaf_Page (Object, Address);
         if Page = 0 then return; end if;
         Word := Raw_Word (Object, Page, Locate (Address).PT);
         if Word = 0 or else Word - Word mod 4096 /= Expected (I) then return; end if;
      end loop;
      -- Fail closed from the first external effect, including callback failure
      -- before any confirmed write. A partial hardware update is not rollback.
      State.Poisoned := True;
      for I in Expected'Range loop
         if not Exclusive then return; end if;
         Address := GPU + Unsigned_64 (I - Expected'First) * 4096;
         Page := Leaf_Page (Object, Address);
         Write_Leaf (Object.DMA (Page), Locate (Address).PT,
           Raw_Word (Object, Page, Locate (Address).PT),
           Intel_GPU_PPGTT_Scratch.Fallback (Object.Scratch, 0), OK);
         if not OK then return; end if;
      end loop;
      if not Exclusive then return; end if;
      State.Root := Object.DMA (1);
      State.Epoch := Object.Epoch;
      State.First := GPU;
      State.Pages := Expected'Length;
      State.Pending := True;
      Accepted := True;
   end Publish;

   procedure Commit
     (State : in out Controller; Object : in out Image;
      Invalidation_Completed : Boolean; Accepted : out Boolean)
   is
      Page : Natural;
      Address : Unsigned_64;
   begin
      Accepted := False;
      if not State.Pending then return; end if;
      State.Pending := False;
      if not Invalidation_Completed or else not Exclusive or else
        not Object.Valid or else not Object.Frozen or else
        Object.DMA (1) /= State.Root or else Object.Epoch /= State.Epoch
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
