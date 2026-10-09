package body Intel_GPU_VM_Image.Range_Query is
   use Intel_GPU_ADLN_PPGTT;
   function Status (State : Query) return Result is (State.Value);
   procedure Cancel (State : in out Query) is
   begin State.Value := Stale; end Cancel;
   procedure Start (State : in out Query; Object : Image;
                    GPU, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False;
      if State.Value = Scanning then return; end if;
      State.Value := Not_Reusable;
      if not Object.Valid or else not Object.Frozen or else
        GPU = 0 or else GPU >= 2 ** 48 or else GPU mod 4096 /= 0 or else
        Bytes = 0 or else Bytes mod 4096 /= 0 or else Bytes > 2 ** 48 - GPU or else
        Bytes / 4096 > Unsigned_64 (Capacity * 512 - Object.Mapped_Pages)
      then return; end if;
      State.Root := Root_DMA (Object); State.Epoch := Object.Epoch;
      State.First := GPU; State.Region := GPU / (512 * 4096);
      State.Pages := Natural (Bytes / 4096); State.Cursor := 0;
      State.Depth := 0; State.Table := 1; State.Candidate := 2;
      State.Value := Scanning; Accepted := True;
   end Start;
   procedure Step (State : in out Query; Object : Image) is
      Address : Unsigned_64;
   begin
      if State.Value /= Scanning then return; end if;
      if not Object.Valid or else not Object.Frozen or else
        Root_DMA (Object) /= State.Root or else Object.Epoch /= State.Epoch
      then State.Value := Stale; return; end if;
      for Work in 1 .. 32 loop
         Address := State.First + Unsigned_64 (State.Cursor) * 4096;
         if Address / (512 * 4096) /= State.Region then
            State.Region := Address / (512 * 4096);
            State.Depth := 0; State.Table := 1; State.Candidate := 2;
         end if;
         if State.Depth = 3 then
            if Raw_Word (Object, State.Table, Locate (Address).PT) /= 0 then
               State.Value := Not_Reusable; return;
            end if;
            State.Cursor := State.Cursor + 1;
            if State.Cursor = State.Pages then State.Value := Reusable; return; end if;
         else
            if State.Candidate > Object.Count then State.Value := Not_Reusable; return; end if;
            declare
               W : constant Walk := Locate (Address);
               Route : constant array (0 .. 2) of Table_Index := [W.PML4, W.PDP, W.PD];
            begin
               if Raw_Word (Object, State.Table, Route (State.Depth)) =
                 Encode_Directory (Descriptor (Object, State.Candidate).DMA)
               then
                  State.Table := State.Candidate;
                  State.Depth := State.Depth + 1; State.Candidate := 2;
               else State.Candidate := State.Candidate + 1; end if;
            end;
         end if;
      end loop;
   end Step;
end Intel_GPU_VM_Image.Range_Query;
