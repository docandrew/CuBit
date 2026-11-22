package body Compositor_Transfer_Delta with SPARK_Mode is
   function Prepare (Previous_GPU, Previous_CPU, Current_GPU, Current_CPU : Count;
      Saturated : Boolean) return Sample is
   begin
      if Saturated or Current_GPU < Previous_GPU or Current_CPU < Previous_CPU then
         return (Invalid_Counters, 0, 0);
      elsif Current_GPU = Previous_GPU and Current_CPU = Previous_CPU then
         return (No_Work, 0, 0);
      else return (Sample_Ready, Current_GPU - Previous_GPU, Current_CPU - Previous_CPU);
      end if;
   end Prepare;
end Compositor_Transfer_Delta;
