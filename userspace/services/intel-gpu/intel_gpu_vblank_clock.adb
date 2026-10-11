package body Intel_GPU_Vblank_Clock with SPARK_Mode is
   function Measure (Elapsed_Us : Unsigned_64; Frames : Unsigned_32) return Period is
   begin
      if Frames > 0 and then Elapsed_Us / Unsigned_64 (Frames) in Period then
         return Elapsed_Us / Unsigned_64 (Frames);
      end if;
      return Default_Period_Us;
   end Measure;

   function Next_Vblank (Last_Vblank, Now : Time_Us; Each : Period) return Time_Us is
   begin
      if Now - Last_Vblank < Each then
         return Last_Vblank + Each;
      end if;
      --  The last observed vblank is stale: poll a period from now.
      return Now + Each;
   end Next_Vblank;
end Intel_GPU_Vblank_Clock;
