with Intel_GPU_VM_Materialize;
package body Intel_GPU_Application_Image.Updates is
   function Failed (Object : State) return Boolean is (Object.Update_Failed);
   procedure Publish_Tables
     (Object : in out State; Previous, Candidate : VM.Image;
      Backing : Tables.Mappings; Success : out Boolean) is
      function Gate return Boolean is
        (not Object.Update_Failed and then Owner_Ready and then Exclusive);
      package Writer is new Intel_GPU_VM_Materialize (VM, Gate, Flush_Page);
      Attempt : Writer.State;
      Mappings : Writer.Mappings;
      Scratch : Writer.Scratch_Mappings;
      OK : Boolean := False;
   begin
      Success := False;
      if Object.Update_Failed or else Object.Retirement_Attempted then return; end if;
      if Object.Updating then Object.Update_Failed := True; return; end if;
      Object.Updating := True;
      if Object.Prepared /= 0 and then Object.Allocation.Ready and then Gate and then
        Allocation_Disjoint (Candidate, Object.Allocation)
      then
         OK := True;
         for P in VM.Page_Number loop
            Mappings (P) := (Backing (P).CPU, Backing (P).DMA);
            if P <= VM.Used (Candidate) and then
              Overlap (Backing (P).CPU, Object.Allocation.CPU_Address, Object.Allocation.Bytes)
            then OK := False; end if;
         end loop;
         if OK then
            for L in Scratch'Range loop
               Scratch (L) := (Object.Scratch (L).CPU, Object.Scratch (L).DMA);
            end loop;
            Writer.Publish_Update
              (Attempt, Previous, Candidate, Mappings,
               (Object.Root.CPU, Object.Root.DMA), OK, Scratch);
         end if;
      end if;
      Success := OK and then Gate;
      Object.Updating := False;
      if not Success then Object.Update_Failed := True; end if;
   end Publish_Tables;
end Intel_GPU_Application_Image.Updates;
