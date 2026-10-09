with Interfaces;
package Compositor_Transfer_Delta with SPARK_Mode, Pure is
   subtype Count is Interfaces.Unsigned_64;
   use type Count;
   type Action is (No_Work, Sample_Ready, Invalid_Counters);
   type Sample is record
      Kind : Action := No_Work;
      GPU, CPU : Count := 0;
   end record;
   function Prepare (Previous_GPU, Previous_CPU, Current_GPU, Current_CPU : Count;
      Saturated : Boolean) return Sample
     with Post =>
       (if Saturated or Current_GPU < Previous_GPU or Current_CPU < Previous_CPU then
          Prepare'Result.Kind = Invalid_Counters and Prepare'Result.GPU = 0 and Prepare'Result.CPU = 0
        else Prepare'Result.GPU = Current_GPU - Previous_GPU and
          Prepare'Result.CPU = Current_CPU - Previous_CPU and
          Prepare'Result.Kind = (if Current_GPU = Previous_GPU and Current_CPU = Previous_CPU
            then No_Work else Sample_Ready));
end Compositor_Transfer_Delta;
