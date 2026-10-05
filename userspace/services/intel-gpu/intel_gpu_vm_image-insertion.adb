package body Intel_GPU_VM_Image.Insertion is
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
   procedure Start_From_Pages
     (State : in out Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Page_Count : Natural;
      Policy : Cache_Policy; Access_Mode : Page_Access; Accepted : out Boolean)
   is
      Root : constant Unsigned_64 := Root_DMA (Object);
      Word : Unsigned_64;
      function Retained_Page (Ordinal : Positive) return Unsigned_64 is
        (Leaf_Storage.Get (State.Words, Ordinal) / 4096 * 4096);
      function Check is new Can_Reuse_From_Pages (Retained_Page);
      function Current return Boolean is
        (not State.Poisoned and then Exclusive and then Object.Valid and then
         Object.Frozen and then Object.Epoch = Expected_Revision and then
         Root_DMA (Object) = Root);
   begin
      Accepted := False;
      if State.Executing then State.Poisoned := True; return; end if;
      if not Current or else State.Pending or else Expected_Revision = 0 or else
        Expected_Revision = Unsigned_64'Last or else Page_Count = 0 or else
        Page_Count > Insertion_Capacity (State) or else
        Page_Count > Capacity * 512 - Object.Mapped_Pages or else
        GPU = 0 or else GPU >= 2 ** 48 or else GPU mod 4096 /= 0 or else
        Unsigned_64 (Page_Count) > (2 ** 48 - GPU) / 4096
      then return; end if;
      for I in 1 .. Page_Count loop
         State.Executing := True;
         Word := Encode_Leaf (Data_Page (I), Policy, Access_Mode);
         State.Executing := False;
         if not Current or else Word = 0 then return; end if;
         Leaf_Storage.Put (State.Words, I, Word);
      end loop;
      if not Check (State, Object, Expected_Revision, GPU, Page_Count, Policy, Access_Mode)
        or else not Current then return; end if;
      State.Root := Descriptor (Object, 1).DMA;
      State.Epoch := Object.Epoch;
      State.First := GPU;
      State.Pages := Page_Count;
      State.Poisoned := True;
      State.Cursor := 0;
      State.Active := True;
      Accepted := True;
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
      OK : Boolean;
      function Current return Boolean is
        (Exclusive and then Object.Valid and then Object.Frozen and then
         Descriptor (Object, 1).DMA = State.Root and then Object.Epoch = State.Epoch);
   begin
      if not State.Active then return; end if;
      if State.Executing or else not Current then
         State.Active := False; return;
      end if;
      Address := State.First + Unsigned_64 (State.Cursor) * 4096;
      Page := Leaf_Page (Object, Address);
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
      -- Exact retained publication words; caller Data may already have changed.
      -- Serialized owner preserves Object through Publish/Commit.
      for I in 1 .. State.Pages loop
         Address := State.First + Unsigned_64 (I - 1) * 4096;
         Page := Leaf_Page (Object, Address);
         Set_Raw_Word (Object, Page, Locate (Address).PT, Leaf_Storage.Get (State.Words, I));
      end loop;
      Object.Mapped_Pages := Object.Mapped_Pages + State.Pages;
      Object.Epoch := Object.Epoch + 1;
      Object.Predecessor_Root := 0; Object.Predecessor_Epoch := 0;
      State.Poisoned := False;
      Accepted := True;
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
