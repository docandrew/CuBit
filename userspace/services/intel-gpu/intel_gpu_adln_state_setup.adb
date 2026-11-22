with Intel_GPU_ADLN_State_Base;
with Intel_GPU_ADLN_Pipe_Control;
with Intel_GPU_ADLN_Pipeline;
package body Intel_GPU_ADLN_State_Setup with SPARK_Mode is
   function Build (MOCS : Unsigned_32) return Image is
      Base : constant Intel_GPU_ADLN_State_Base.Image := Intel_GPU_ADLN_State_Base.Build (MOCS);
      Result : Image;
   begin
      if not Base.Valid then
         return Result;
      end if;
      for I in Intel_GPU_ADLN_Pipeline.Packet'Range loop
         Result.Data (I) := Intel_GPU_ADLN_Pipeline.Initial_3D (I);
      end loop;
      for I in Intel_GPU_ADLN_Pipe_Control.Packet'Range loop
         Result.Data (7 + I) := Intel_GPU_ADLN_Pipe_Control.Before_State_Base (I);
         Result.Data (35 + I) := Intel_GPU_ADLN_Pipe_Control.After_State_Base (I);
      end loop;
      for I in Intel_GPU_ADLN_State_Base.Words'Range loop
         Result.Data (13 + I) := Base.Data (I);
      end loop;
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_ADLN_State_Setup;
