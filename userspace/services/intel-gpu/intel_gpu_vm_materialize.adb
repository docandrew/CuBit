with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_ADLN_PPGTT;
package body Intel_GPU_VM_Materialize is
   type Words is array (Intel_GPU_ADLN_PPGTT.Table_Index) of Unsigned_64;
   generic
      with function Lookup (Ordinal : Positive) return Page_Mapping;
   procedure Prepare_Core
     (Object : in out State; Source : VM.Image; Mapping_Count : Natural;
      Root : out Unsigned_64; Success : out Boolean;
      Scratch : Scratch_Mappings; Retained : Boolean);
   procedure Prepare_Core
     (Object : in out State; Source : VM.Image; Mapping_Count : Natural;
      Root : out Unsigned_64; Success : out Boolean;
      Scratch : Scratch_Mappings; Retained : Boolean) is
      Count : constant Natural := VM.Used (Source);
      Epoch : constant Unsigned_64 := VM.Revision (Source);
      Source_Root : constant Unsigned_64 := VM.Root_DMA (Source);
      Mapping_Failed : Boolean := False;
      function Held return Boolean is
        (not Mapping_Failed and then Owner_Ready and then VM.Sealed (Source)
         and then VM.Revision (Source) = Epoch
         and then VM.Root_DMA (Source) = Source_Root);
      function Resolve (Ordinal : Positive) return Page_Mapping is
         Item : Page_Mapping;
      begin
         if not Held then
            Mapping_Failed := True; return (0, 0);
         end if;
         Item := Lookup (Ordinal);
         if not Held or else Item.DMA /= VM.Page_DMA (Source, Ordinal)
           or else Item.CPU = 0 or else Item.CPU mod 4096 /= 0
           or else Item.CPU > 2 ** 47 - 4096
         then Mapping_Failed := True; return (0, 0); end if;
         return Item;
      end Resolve;
      M, Other : Page_Mapping;
      Has_Scratch : constant Boolean := VM.Scratch_DMA (Source, 0) /= 0;
      function Scratch_Word (L : Intel_GPU_PPGTT_Scratch.Level) return Unsigned_64 is
        (if L = 0 then 0 else VM.Scratch_Entry (Source, L));
   begin
      Root := 0; Success := False;
      if Object.Attempted then return; end if;
      Object.Attempted := True;
      if not VM.Sealed (Source) or else Count < 4 or else
        Mapping_Count < Count or else
        not Owner_Ready then return; end if;
      -- Validate every destination before any write; reject aliases even when
      -- different GPU tables would otherwise fit the same CPU mapping.
      for P in 1 .. Count loop
         M := Resolve (P);
         if M.CPU = 0 then return; end if;
         for Q in 1 .. P - 1 loop
            Other := Resolve (Q);
            if Other.CPU = 0 or else M.CPU = Other.CPU then return; end if;
         end loop;
      end loop;
      for L in Scratch'Range loop
         if Has_Scratch then
            if Scratch (L).DMA /= VM.Scratch_DMA (Source, L) or else
              Scratch (L).CPU = 0 or else Scratch (L).CPU mod 4096 /= 0 or else
              Scratch (L).CPU > 2 ** 47 - 4096 then return; end if;
            for P in 1 .. Count loop
               M := Resolve (P);
               if M.CPU = 0 or else Scratch (L).CPU = M.CPU then return; end if;
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
            if not Held then return; end if;
            if not Retained then
               declare
                  Destination : Words with Import, Volatile,
                    Address => To_Address (Integer_Address (Scratch (L).CPU));
               begin
                  for I in Words'Range loop Destination (I) := Scratch_Word (L); end loop;
               end;
               if not Held or else not Flush_Page (Scratch (L).CPU) then return; end if;
            end if;
            if not Held then return; end if;
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
         M := Resolve (P);
         if M.CPU = 0 then return; end if;
         declare
            Destination : Words with Import, Volatile,
              Address => To_Address (Integer_Address (M.CPU));
         begin
            for I in Words'Range loop
               Destination (I) := VM.Entry_Value (Source, P, I);
            end loop;
         end;
      end loop;
      for P in reverse 1 .. Count loop
         M := Resolve (P);
         if M.CPU = 0 or else not Flush_Page (M.CPU) or else not Held then return; end if;
      end loop;
      for P in 1 .. Count loop
         M := Resolve (P);
         if M.CPU = 0 then return; end if;
         declare
            Readback : Words with Import, Volatile,
              Address => To_Address (Integer_Address (M.CPU));
         begin
            for I in Words'Range loop
               if Readback (I) /= VM.Entry_Value (Source, P, I) then return; end if;
            end loop;
         end;
      end loop;
      if not Held then return; end if;
      Root := VM.Root_DMA (Source);
      Success := True;
   end Prepare_Core;

   procedure Prepare_From_Mappings
     (Object : in out State; Source : VM.Image; Mapping_Count : Natural;
      Root : out Unsigned_64; Success : out Boolean;
      Scratch : Scratch_Mappings := [others => (0, 0)]) is
      procedure Stream is new Prepare_Core (Lookup);
   begin
      Stream (Object, Source, Mapping_Count, Root, Success, Scratch, False);
   end Prepare_From_Mappings;

   procedure Prepare
     (Object : in out State; Source : VM.Image; Backing : Mapping_View;
      Root : out Unsigned_64; Success : out Boolean;
      Scratch : Scratch_Mappings := [others => (0, 0)]) is
      function Page (Ordinal : Positive) return Page_Mapping is (Backing (Ordinal));
      procedure Stream is new Prepare_Core (Page);
   begin
      if Backing'First /= 1 then
         Root := 0; Success := False; Object.Attempted := True; return;
      end if;
      Stream (Object, Source, Backing'Length, Root, Success, Scratch, False);
   end Prepare;

   procedure Publish_Update_From_Mappings
     (Object : in out State; Previous, Candidate : VM.Image;
      Mapping_Count : Natural; Stable_Root : Page_Mapping;
      Success : out Boolean;
      Scratch : Scratch_Mappings := [others => (0, 0)]) is
      Previous_Root : constant Unsigned_64 := VM.Root_DMA (Previous);
      Previous_Epoch : constant Unsigned_64 := VM.Revision (Previous);
      Candidate_Root : constant Unsigned_64 := VM.Root_DMA (Candidate);
      Candidate_Epoch : constant Unsigned_64 := VM.Revision (Candidate);
      function Held return Boolean is
        (Owner_Ready and then VM.Root_DMA (Previous) = Previous_Root
         and then VM.Revision (Previous) = Previous_Epoch
         and then VM.Root_DMA (Candidate) = Candidate_Root
         and then VM.Revision (Candidate) = Candidate_Epoch);
      function Resolve (Ordinal : Positive) return Page_Mapping is
         Item : Page_Mapping;
      begin
         if not Held then return (0, 0); end if;
         Item := Lookup (Ordinal);
         if not Held or else Item.CPU = Stable_Root.CPU or else
           Item.CPU = 0 or else Item.CPU mod 4096 /= 0 or else
           Item.CPU > 2 ** 47 - 4096 or else
           Item.DMA /= VM.Page_DMA (Candidate, Ordinal)
         then return (0, 0); end if;
         return Item;
      end Resolve;
      procedure Stream is new Prepare_Core (Resolve);
      Item : Page_Mapping;
      Prepared : State;
      Root : Unsigned_64;
      OK : Boolean;
      function Matches (Image : VM.Image) return Boolean is
         Readback : Words with Import, Volatile,
           Address => To_Address (Integer_Address (Stable_Root.CPU));
      begin
         if not Held then return False; end if;
         for I in Words'Range loop
            if Readback (I) /= VM.Entry_Value (Image, 1, I) then return False; end if;
         end loop;
         return Held;
      end Matches;
   begin
      Success := False;
      if Object.Attempted then return; end if;
      Object.Attempted := True;
      if Mapping_Count < VM.Used (Candidate)
      then return; end if;
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
         Item := Resolve (P);
         if Item.CPU = 0 then return; end if;
      end loop;
      if not Matches (Previous) then return; end if;
      Stream (Prepared, Candidate, Mapping_Count, Root, OK, Scratch, True);
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
      if not Held or else not Flush_Page (Stable_Root.CPU) then return; end if;
      Success := Matches (Candidate);
   end Publish_Update_From_Mappings;

   procedure Publish_Update
     (Object : in out State; Previous, Candidate : VM.Image;
      Backing : Mapping_View; Stable_Root : Page_Mapping;
      Success : out Boolean;
      Scratch : Scratch_Mappings := [others => (0, 0)]) is
      function Page (Ordinal : Positive) return Page_Mapping is (Backing (Ordinal));
      procedure Stream is new Publish_Update_From_Mappings (Page);
   begin
      if Backing'First /= 1 then
         Success := False; Object.Attempted := True; return;
      end if;
      Stream (Object, Previous, Candidate, Backing'Length, Stable_Root, Success, Scratch);
   end Publish_Update;
end Intel_GPU_VM_Materialize;
