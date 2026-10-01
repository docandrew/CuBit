package body Intel_GPU_VM_Image.Snapshots is
   procedure Adopt_Committed
     (Object : in out Image; Candidate : Image; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not Object.Valid or else not Object.Frozen or else
        not Candidate.Valid or else not Candidate.Frozen or else
        Candidate.Predecessor_Root = 0 or else
        Candidate.Predecessor_Root /= Object.DMA (1) or else
        Candidate.DMA (1) = Object.DMA (1)
      then return; end if;
      -- A limited image cannot accidentally be assigned by callers. This is
      -- the explicit metadata-only transition; Candidate remains immutable.
      Object.DMA := Candidate.DMA;
      Object.Entries := Candidate.Entries;
      Object.Count := Candidate.Count;
      Object.Mapped_Pages := Candidate.Mapped_Pages;
      Object.Predecessor_Root := Candidate.Predecessor_Root;
      Accepted := True;
   end Adopt_Committed;
end Intel_GPU_VM_Image.Snapshots;
