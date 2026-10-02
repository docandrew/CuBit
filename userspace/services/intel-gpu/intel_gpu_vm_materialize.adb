with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_ADLN_PPGTT;
package body Intel_GPU_VM_Materialize is
   type Words is array (Intel_GPU_ADLN_PPGTT.Table_Index) of Unsigned_64;
   procedure Prepare_Core
     (Object : in out State; Source : VM.Image; Backing : Mappings;
      Root : out Unsigned_64; Success : out Boolean;
      Scratch : Scratch_Mappings; Retained : Boolean) is
      Count : constant Natural := VM.Used (Source);
      Has_Scratch : constant Boolean := VM.Scratch_DMA (Source, 0) /= 0;
      function Scratch_Word (L : Intel_GPU_PPGTT_Scratch.Level) return Unsigned_64 is
        (if L = 0 then 0 else VM.Scratch_Entry (Source, L));
   begin
      Root := 0; Success := False;
      if Object.Attempted then return; end if;
      Object.Attempted := True;
      if not VM.Sealed (Source) or else Count < 4 or else not Owner_Ready then return; end if;
      -- Validate every destination before any write; reject aliases even when
      -- different GPU tables would otherwise fit the same CPU mapping.
      for P in 1 .. Count loop
         if Backing (P).DMA /= VM.Page_DMA (Source, P) or
           Backing (P).CPU = 0 or Backing (P).CPU mod 4096 /= 0 or
           Backing (P).CPU > 2 ** 47 - 4096 then return; end if;
         for Q in 1 .. P - 1 loop
            if Backing (P).CPU = Backing (Q).CPU then return; end if;
         end loop;
      end loop;
      for L in Scratch'Range loop
         if Has_Scratch then
            if Scratch (L).DMA /= VM.Scratch_DMA (Source, L) or else
              Scratch (L).CPU = 0 or else Scratch (L).CPU mod 4096 /= 0 or else
              Scratch (L).CPU > 2 ** 47 - 4096 then return; end if;
            for P in 1 .. Count loop
               if Scratch (L).CPU = Backing (P).CPU then return; end if;
            end loop;
            for K in Scratch'First .. L - 1 loop
               if Scratch (K).CPU = Scratch (L).CPU then return; end if;
            end loop;
         elsif Scratch (L).CPU /= 0 or else Scratch (L).DMA /= 0 then
            return;
         end if;
      end loop;
      if Has_Scratch then
         -- Initial preparation writes data then PT/PD/PDP. Updates only
         -- validate retained tables; GPU-written scratch data stays intact.
         for L in Scratch'Range loop
            if not Owner_Ready then return; end if;
            if not Retained then
               declare
                  Destination : Words with Import, Volatile,
                    Address => To_Address (Integer_Address (Scratch (L).CPU));
               begin
                  for I in Words'Range loop Destination (I) := Scratch_Word (L); end loop;
               end;
               if not Owner_Ready or else not Flush_Page (Scratch (L).CPU) then return; end if;
            end if;
            if not Owner_Ready then return; end if;
            if not Retained or else L /= 0 then
             declare
               Readback : Words with Import, Volatile,
                 Address => To_Address (Integer_Address (Scratch (L).CPU));
            begin
               for I in Words'Range loop
                  if Readback (I) /= Scratch_Word (L) then return; end if;
               end loop;
             end;
            end if;
         end loop;
      end if;
      -- Children first, root last. None of these pages may be GPU-owned yet.
      for P in reverse 1 .. Count loop
         if not Owner_Ready then return; end if;
         declare
            Destination : Words with Import, Volatile,
              Address => To_Address (Integer_Address (Backing (P).CPU));
         begin
            for I in Words'Range loop
               Destination (I) := VM.Entry_Value (Source, P, I);
            end loop;
         end;
      end loop;
      for P in reverse 1 .. Count loop
         if not Owner_Ready or else not Flush_Page (Backing (P).CPU) then return; end if;
      end loop;
      for P in 1 .. Count loop
         if not Owner_Ready then return; end if;
         declare
            Readback : Words with Import, Volatile,
              Address => To_Address (Integer_Address (Backing (P).CPU));
         begin
            for I in Words'Range loop
               if Readback (I) /= VM.Entry_Value (Source, P, I) then return; end if;
            end loop;
         end;
      end loop;
      if not Owner_Ready then return; end if;
      Root := VM.Root_DMA (Source);
      Success := True;
   end Prepare_Core;

   procedure Prepare
     (Object : in out State; Source : VM.Image; Backing : Mappings;
      Root : out Unsigned_64; Success : out Boolean;
      Scratch : Scratch_Mappings := [others => (0, 0)]) is
   begin
      Prepare_Core (Object, Source, Backing, Root, Success, Scratch, False);
   end Prepare;

   procedure Publish_Update
     (Object : in out State; Previous, Candidate : VM.Image;
      Backing : Mappings; Stable_Root : Page_Mapping;
      Success : out Boolean;
      Scratch : Scratch_Mappings := [others => (0, 0)]) is
      Prepared : State;
      Root : Unsigned_64;
      OK : Boolean;
      function Matches (Image : VM.Image) return Boolean is
         Readback : Words with Import, Volatile,
           Address => To_Address (Integer_Address (Stable_Root.CPU));
      begin
         if not Owner_Ready then return False; end if;
         for I in Words'Range loop
            if Readback (I) /= VM.Entry_Value (Image, 1, I) then return False; end if;
         end loop;
         return Owner_Ready;
      end Matches;
   begin
      Success := False;
      if Object.Attempted then return; end if;
      Object.Attempted := True;
      for L in Scratch'Range loop
         if VM.Scratch_DMA (Previous, L) /= VM.Scratch_DMA (Candidate, L) or else
           (Scratch (L).CPU /= 0 and then Scratch (L).CPU = Stable_Root.CPU)
         then return; end if;
      end loop;
      if not VM.Direct_Successor (Previous, Candidate) or else
        not Owner_Ready or else Stable_Root.CPU = 0 or else
        Stable_Root.CPU mod 4096 /= 0 or else
        Stable_Root.CPU > 2 ** 47 - 4096 or else
        not VM.DMA_Disjoint (Candidate, Stable_Root.DMA, 4096)
      then return; end if;
      -- Every materialized candidate page must avoid all old tables/data.
      -- Reverse check excludes candidate data aliases of used old tables.
      for P in 1 .. VM.Used (Candidate) loop
         if not VM.DMA_Disjoint (Previous, VM.Page_DMA (Candidate, P), 4096)
         then return; end if;
      end loop;
      for P in 1 .. VM.Used (Previous) loop
         if not VM.DMA_Disjoint (Candidate, VM.Page_DMA (Previous, P), 4096)
         then return; end if;
      end loop;
      for P in 1 .. VM.Used (Candidate) loop
         if Backing (P).CPU = Stable_Root.CPU then return; end if;
      end loop;
      if not Matches (Previous) then return; end if;
      Prepare_Core (Prepared, Candidate, Backing, Root, OK, Scratch, True);
      if not OK or else Root /= VM.Root_DMA (Candidate) or else
        not Matches (Previous) then return; end if;
      declare
         Destination : Words with Import, Volatile,
           Address => To_Address (Integer_Address (Stable_Root.CPU));
      begin
         for I in Words'Range loop
            Destination (I) := VM.Entry_Value (Candidate, 1, I);
         end loop;
      end;
      if not Owner_Ready or else not Flush_Page (Stable_Root.CPU) then return; end if;
      Success := Matches (Candidate);
   end Publish_Update;
end Intel_GPU_VM_Materialize;
