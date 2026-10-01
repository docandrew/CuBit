package body Intel_GPU_ADLN_Coarse_Pixel with SPARK_Mode is
   function Initial_Array return Array_Words is
      Result : Array_Words := [others => 0];
   begin
      for Viewport in 0 .. Viewport_Count - 1 loop
         for I in Disabled'Range loop
            Result (Viewport * 8 + I) := Disabled (I);
         end loop;
      end loop;
      return Result;
   end Initial_Array;
end Intel_GPU_ADLN_Coarse_Pixel;
