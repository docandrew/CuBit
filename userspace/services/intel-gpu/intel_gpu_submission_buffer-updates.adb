with Interfaces; use Interfaces;
package body Intel_GPU_Submission_Buffer.Updates is
   function Failed (Object : Buffer_State) return Boolean is (Object.Update_Failed);
   function Overlap (Page, First, Bytes : Unsigned_64) return Boolean is
     (if Page >= First then Page - First < Bytes else First - Page < 4096);
   procedure Publish_Boot_Tables
     (Object : in out Buffer_State; Candidate : VM.Image;
      Backing : Tables.Mapping_View; Success : out Boolean) is
      function Gate return Boolean is
        (not Object.Update_Failed and then Owner_Ready and then Exclusive);
      package Writer is new Intel_GPU_VM_Materialize (VM, Gate, Flush_Page);
      Attempt : Writer.State;
      OK : Boolean := False;
   begin
      Success := False;
      if Object.Update_Attempted then return; end if;
      Object.Update_Attempted := True;
      if Backing'First = 1 and then Backing'Last >= VM.Used (Candidate) and then
        Object.GPU_Address /= 0 and then Object.Boot_Root.CPU /= 0 and then
        Object.Allocation.Ready and then VM.Sealed (Object.Boot_VM) and then Gate
      then
         OK := True;
         for P in VM.Page_Number loop
            -- The boot VM deliberately maps data within Allocation; exclude
            -- fresh TABLE storage from the entire extent, not its data leaves.
            if Intel_GPU_Buffer_Reply.Overlaps_DMA
              (Object.Allocation, VM.Table_Backing_DMA (Candidate, P), 4096) or else
              (P <= VM.Used (Candidate) and then
               Overlap (Backing (P).CPU, Object.Allocation.CPU_Address,
                        Object.Allocation.Bytes))
            then OK := False; end if;
         end loop;
         if OK then
            Writer.Publish_Update
              (Attempt, Object.Boot_VM, Candidate, Backing,
               (Object.Boot_Root.CPU, Object.Boot_Root.DMA), OK);
         end if;
      end if;
      Success := OK and then Gate;
      if not Success then Object.Update_Failed := True; end if;
   end Publish_Boot_Tables;
end Intel_GPU_Submission_Buffer.Updates;
