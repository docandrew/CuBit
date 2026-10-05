with Intel_GPU_VM_Materialize;
with System.Storage_Elements; use System.Storage_Elements;
package body Intel_GPU_Application_Image.Updates is
   function Failed (Object : State) return Boolean is (Object.Update_Failed);
   procedure Write_Mapped_Leaf
     (Object : in out State; Source : VM.Image; Mapping : Tables.Page_Mapping;
      Table_DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Expected, Replacement : Unsigned_64; Inserting : Boolean;
      Success : out Boolean)
   is
      Page : Natural := 0;
      Encoded_Data : Boolean := False;
      Data_DMA : constant Unsigned_64 := Replacement / 4096 * 4096;
      function Gate return Boolean is
        (not Object.Update_Failed and then not Object.Retirement_Attempted and then
         Object.Prepared /= 0 and then Owner_Ready and then Exclusive);
      type Words is array (Intel_GPU_ADLN_PPGTT.Table_Index) of Unsigned_64;
   begin
      Success := False;
      if not Gate or else Object.Updating then
         Object.Update_Failed := True; return;
      end if;
      if not VM.Sealed (Source) then
         Object.Update_Failed := True; return;
      end if;
      if Inserting then
         for Policy in Intel_GPU_ADLN_PPGTT.Cache_Policy loop
            Encoded_Data := Encoded_Data or else
              (Replacement /= 0 and then Replacement =
               Intel_GPU_ADLN_PPGTT.Encode_Leaf
                 (Data_DMA, Policy, Intel_GPU_ADLN_PPGTT.Read_Write));
         end loop;
         if not Encoded_Data or else Expected /= VM.Scratch_Entry (Source, 1) then
            Object.Update_Failed := True; return;
         end if;
         for P in VM.Page_Number loop
            if Data_DMA = VM.Table_Backing_DMA (Source, P) then
               Object.Update_Failed := True; return;
            end if;
         end loop;
         for Mapping of Object.Scratch loop
            if Data_DMA = Mapping.DMA then
               Object.Update_Failed := True; return;
            end if;
         end loop;
      elsif Expected = 0 or else Replacement /= VM.Scratch_Entry (Source, 1) then
         Object.Update_Failed := True; return;
      end if;
      for P in 2 .. VM.Used (Source) loop
         if VM.Page_DMA (Source, P) = Table_DMA then Page := P; exit; end if;
      end loop;
      if Page = 0 or else not VM.Leaf_Table (Source, Page) or else
        Mapping.DMA /= Table_DMA or else
        Mapping.CPU = 0 or else Mapping.CPU mod 4096 /= 0 or else
        Mapping.CPU > 2 ** 47 - 4096 or else
        VM.Entry_Value (Source, Page, Index) /= Expected or else
        Overlap (Mapping.CPU, Object.Allocation.CPU_Address, Object.Allocation.Bytes)
      then Object.Update_Failed := True; return; end if;
      Object.Updating := True;
      declare
         Destination : Words with Import, Volatile,
           Address => To_Address (Integer_Address (Mapping.CPU));
      begin
         if Gate and then Destination (Index) = Expected then
            Destination (Index) := Replacement;
            Success := Gate and then Flush_Page (Mapping.CPU) and then
              Gate and then Destination (Index) = Replacement and then Gate;
         end if;
      end;
      Object.Updating := False;
      if not Success then Object.Update_Failed := True; end if;
   end Write_Mapped_Leaf;
   procedure Write_Leaf
     (Object : in out State; Source : VM.Image; Backing : Tables.Mapping_View;
      Table_DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Expected, Replacement : Unsigned_64; Inserting : Boolean;
      Success : out Boolean) is
      Data_DMA : constant Unsigned_64 := Replacement / 4096 * 4096;
   begin
      Success := False;
      if Backing'First /= 1 or else Backing'Last < VM.Used (Source) then
         Object.Update_Failed := True; return;
      end if;
      if Inserting then
         for Mapping of Backing loop
            if Data_DMA = Mapping.DMA then
               Object.Update_Failed := True; return;
            end if;
         end loop;
      end if;
      for P in 2 .. VM.Used (Source) loop
         if VM.Page_DMA (Source, P) = Table_DMA then
            Write_Mapped_Leaf (Object, Source, Backing (P), Table_DMA, Index,
              Expected, Replacement, Inserting, Success);
            return;
         end if;
      end loop;
      Object.Update_Failed := True;
   end Write_Leaf;
   procedure Insert_Mapped_Leaf
     (Object : in out State; Source : VM.Image; Mapping : Tables.Page_Mapping;
      Table_DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Expected, Replacement : Unsigned_64; Success : out Boolean) is
   begin
      Write_Mapped_Leaf (Object, Source, Mapping, Table_DMA, Index,
        Expected, Replacement, True, Success);
   end Insert_Mapped_Leaf;
   procedure Remove_Mapped_Leaf
     (Object : in out State; Source : VM.Image; Mapping : Tables.Page_Mapping;
      Table_DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Expected, Replacement : Unsigned_64; Success : out Boolean) is
   begin
      Write_Mapped_Leaf (Object, Source, Mapping, Table_DMA, Index,
        Expected, Replacement, False, Success);
   end Remove_Mapped_Leaf;
   procedure Remove_Leaf
     (Object : in out State; Source : VM.Image; Backing : Tables.Mapping_View;
      Table_DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Expected, Replacement : Unsigned_64; Success : out Boolean) is
   begin
      Write_Leaf (Object, Source, Backing, Table_DMA, Index,
                  Expected, Replacement, False, Success);
   end Remove_Leaf;
   procedure Insert_Leaf
     (Object : in out State; Source : VM.Image; Backing : Tables.Mapping_View;
      Table_DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Expected, Replacement : Unsigned_64; Success : out Boolean) is
   begin
      Write_Leaf (Object, Source, Backing, Table_DMA, Index,
                  Expected, Replacement, True, Success);
   end Insert_Leaf;
   procedure Publish_Tables_From_Mappings
     (Object : in out State; Previous, Candidate : VM.Image;
      Mapping_Count : Natural; Success : out Boolean) is
      Old_Root : constant Unsigned_64 := VM.Root_DMA (Previous);
      Old_Epoch : constant Unsigned_64 := VM.Revision (Previous);
      New_Root : constant Unsigned_64 := VM.Root_DMA (Candidate);
      New_Epoch : constant Unsigned_64 := VM.Revision (Candidate);
      function Gate return Boolean is
        (not Object.Update_Failed and then Owner_Ready and then Exclusive
         and then VM.Root_DMA (Previous) = Old_Root and then VM.Revision (Previous) = Old_Epoch
         and then VM.Root_DMA (Candidate) = New_Root and then VM.Revision (Candidate) = New_Epoch);
      package Writer is new Intel_GPU_VM_Materialize (VM, Gate, Flush_Page);
      function Resolve (Ordinal : Positive) return Writer.Page_Mapping is
         Item : Tables.Page_Mapping;
      begin
         if not Gate then return (0, 0); end if;
         Item := Lookup (Ordinal);
         if not Gate or else Item.CPU = 0 or else
           Overlap (Item.CPU, Object.Allocation.CPU_Address, Object.Allocation.Bytes)
         then return (0, 0); end if;
         return Item;
      end Resolve;
      procedure Publish_Stream is new Writer.Publish_Update_From_Mappings (Resolve);
      Item : Writer.Page_Mapping;
      Attempt : Writer.State;
      Scratch : Writer.Scratch_Mappings;
      OK : Boolean := False;
   begin
      Success := False;
      if Object.Update_Failed or else Object.Retirement_Attempted then return; end if;
      if Object.Updating then Object.Update_Failed := True; return; end if;
      Object.Updating := True;
      if Mapping_Count >= VM.Used (Candidate) and then
        Object.Prepared /= 0 and then Object.Allocation.Ready and then Gate and then
        Allocation_Disjoint (Candidate, Object.Allocation)
      then
         OK := True;
         for P in 1 .. VM.Used (Candidate) loop
            Item := Resolve (P);
            if Item.CPU = 0 then OK := False; exit; end if;
         end loop;
         if OK then
            for L in Scratch'Range loop
               Scratch (L) := (Object.Scratch (L).CPU, Object.Scratch (L).DMA);
            end loop;
            Publish_Stream
              (Attempt, Previous, Candidate, Mapping_Count,
               (Object.Root.CPU, Object.Root.DMA), OK, Scratch);
         end if;
      end if;
      Success := OK and then Gate;
      Object.Updating := False;
      if not Success then Object.Update_Failed := True; end if;
   end Publish_Tables_From_Mappings;
   procedure Publish_Tables
     (Object : in out State; Previous, Candidate : VM.Image;
      Backing : Tables.Mapping_View; Success : out Boolean) is
      function Page (Ordinal : Positive) return Tables.Page_Mapping is (Backing (Ordinal));
      procedure Stream is new Publish_Tables_From_Mappings (Page);
   begin
      if Backing'First /= 1 then
         Success := False; Object.Update_Failed := True; return;
      end if;
      Stream (Object, Previous, Candidate, Backing'Length, Success);
   end Publish_Tables;
end Intel_GPU_Application_Image.Updates;
