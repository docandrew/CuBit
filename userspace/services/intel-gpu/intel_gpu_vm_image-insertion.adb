package body Intel_GPU_VM_Image.Insertion is
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
   function Can_Reuse
     (State : Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Data : Data_Pages;
      Policy : Cache_Policy; Access_Mode : Page_Access) return Boolean
   is
      Page : Natural;
      Address : Unsigned_64;
   begin
      if State.Poisoned or else State.Pending or else
        not Object.Valid or else not Object.Frozen or else
        Expected_Revision = 0 or else Expected_Revision /= Object.Epoch or else
        Object.Epoch = Unsigned_64'Last or else GPU = 0 or else GPU >= 2 ** 48 or else
        GPU mod 4096 /= 0 or else Data'Length = 0 or else
        Data'Length > Capacity * 512 - Object.Mapped_Pages or else
        Unsigned_64 (Data'Length) > (2 ** 48 - GPU) / 4096
      then return False; end if;
      -- Validate the entire request before even the first leaf write.
      for I in Data'Range loop
         if Encode_Leaf (Data (I), Policy, Access_Mode) = 0 or else
           Intel_GPU_PPGTT_Scratch.Contains (Object.Scratch, Data (I))
         then return False; end if;
         for DMA of Object.DMA loop
            if Data (I) = DMA then return False; end if;
         end loop;
         Address := GPU + Unsigned_64 (I - Data'First) * 4096;
         Page := Leaf_Page (Object, Address);
         if Page = 0 or else Raw_Word (Object, Page, Locate (Address).PT) /= 0
         then return False; end if;
      end loop;
      -- Same cache-alias rule as the offline Map_Pages builder.
      for P in 1 .. Object.Count loop
         for Word of Raw_Page (Object, P) loop
            if Word /= 0 and then Word mod 4096 /= 3 + Cache_Bits (Policy) then
               for DMA of Data loop
                  if Word / 4096 = DMA / 4096 then return False; end if;
               end loop;
            end if;
         end loop;
      end loop;
      return True;
   end Can_Reuse;
   procedure Publish
     (State : in out Controller; Object : Image;
      Expected_Revision, GPU : Unsigned_64; Data : Data_Pages;
      Policy : Cache_Policy; Access_Mode : Page_Access; Accepted : out Boolean)
   is
      Page : Natural;
      Address : Unsigned_64;
      OK : Boolean;
   begin
      Accepted := False;
      if not Exclusive or else not Can_Reuse
        (State, Object, Expected_Revision, GPU, Data, Policy, Access_Mode)
      then return; end if;
      State.Root := Object.DMA (1);
      State.Epoch := Object.Epoch;
      State.First := GPU;
      State.Pages := Data'Length;
      for I in Data'Range loop
         State.Words (I - Data'First + 1) := Encode_Leaf (Data (I), Policy, Access_Mode);
      end loop;
      State.Poisoned := True;
      for I in 1 .. State.Pages loop
         if not Exclusive then return; end if;
         Address := State.First + Unsigned_64 (I - 1) * 4096;
         Page := Leaf_Page (Object, Address);
         Write_Leaf (Object.DMA (Page), Locate (Address).PT,
           Intel_GPU_PPGTT_Scratch.Fallback (Object.Scratch, 0),
           State.Words (I), OK);
         if not OK then return; end if;
      end loop;
      if not Exclusive then return; end if;
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
      -- Exact retained publication words; caller Data may already have changed.
      -- Serialized owner preserves Object through Publish/Commit.
      for I in 1 .. State.Pages loop
         Address := State.First + Unsigned_64 (I - 1) * 4096;
         Page := Leaf_Page (Object, Address);
         Set_Raw_Word (Object, Page, Locate (Address).PT, State.Words (I));
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
