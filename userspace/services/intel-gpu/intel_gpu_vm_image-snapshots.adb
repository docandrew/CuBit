package body Intel_GPU_VM_Image.Snapshots is
   function Retired (Object : Image) return Boolean is
     (Object.Retired_Receipt);
   procedure Adopt_Committed
     (Object : in out Image; Candidate : Image; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not Direct_Successor (Object, Candidate) or else
        Candidate.Count > Metadata_Capacity (Object) or else
        Candidate.Backed > Descriptor_Capacity (Object)
      then return; end if;
      -- A limited image cannot accidentally be assigned by callers. This is
      -- the explicit metadata-only transition; Candidate remains immutable.
      for P in 1 .. Candidate.Backed loop
         Set_Descriptor (Object, P, Descriptor (Candidate, P));
      end loop;
      for P in Candidate.Backed + 1 .. Object.Backed loop
         Set_Descriptor (Object, P, (others => <>));
      end loop;
      Object.Backed := Candidate.Backed;
      Object.Scratch := Candidate.Scratch;
      for P in 1 .. Candidate.Count loop
         Table_Storage.Copy_Page (Object.Tables, P, Candidate.Tables, P);
      end loop;
      for P in Candidate.Count + 1 .. Metadata_Capacity (Object) loop Clear_Table (Object, P); end loop;
      Object.Count := Candidate.Count;
      Object.Mapped_Pages := Candidate.Mapped_Pages;
      Object.Predecessor_Root := Candidate.Predecessor_Root;
      Object.Predecessor_Epoch := Candidate.Predecessor_Epoch;
      Object.Epoch := Object.Epoch + 1;
      Accepted := True;
   end Adopt_Committed;
   procedure Forget_Retired
     (Object : in out Image; Expected_Revision, Expected_Root : Unsigned_64;
      References_Retired : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not References_Retired or else not Object.Valid or else not Object.Frozen or else
        Expected_Revision = 0 or else Expected_Revision /= Object.Epoch or else
        Expected_Root = 0 or else Expected_Root /= Descriptor (Object, 1).DMA or else
        Object.Epoch = Unsigned_64'Last
      then return; end if;
      Object.Attempted := False; Object.Valid := False; Object.Frozen := False;
      Object.Mapped_Pages := 0; Object.Count := 0;
      Object.Predecessor_Root := 0; Object.Predecessor_Epoch := 0;
      for P in 1 .. Object.Backed loop
         Set_Descriptor (Object, P, (others => <>));
      end loop;
      Object.Backed := 0; Object.Scratch := [others => 0];
      for P in 1 .. Metadata_Capacity (Object) loop Clear_Table (Object, P); end loop;
      Object.Retired_Receipt := True;
      -- Keep Epoch: the next preparation advances it, never resets to one.
      Accepted := True;
   end Forget_Retired;
end Intel_GPU_VM_Image.Snapshots;
