package body Intel_GPU_Memory_Admission with SPARK_Mode is
   function Policy (State : Evidence)
      return Intel_GPU_Device_Query.Memory_Contract is
   begin
      if not Live (State) then
         return Intel_GPU_Device_Query.Not_Admitted;
      elsif State.CPU_To_GPU_Checked and then State.GPU_To_CPU_Checked then
         return Intel_GPU_Device_Query.Owned_WB_Coherent;
      else
         return Intel_GPU_Device_Query.Owned_WB_Explicit_Maintenance;
      end if;
   end Policy;
end Intel_GPU_Memory_Admission;
