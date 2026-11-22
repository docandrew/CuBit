package body Intel_GPU_ADLN_GT_Settings with SPARK_Mode is
   use Intel_GPU_ADLN_Inventory;
   function Build
     (Inventory : Intel_GPU_ADLN_Inventory.Inventory;
      Topology : Intel_GPU_ADLN_Steering.Topology) return Plan
   is
      Result : Plan;
   begin
      if not Inventory.Valid or else not Topology.Valid or else
        not Inventory.Engines (Render) or else
        (Topology.DSS_Mask and Shift_Left (Unsigned_8'(1), Topology.Default_Instance)) = 0
      then return Result; end if;
      for I in 0 .. 5 loop
         if I < Topology.Default_Instance and then
           (Topology.DSS_Mask and Shift_Left (Unsigned_8'(1), I)) /= 0
         then return Result; end if;
      end loop;
      -- Lowest enabled DSS supports render-power-gated minconfig reads.
      -- Explicit multicast adds to upstream's default steering initialization.
      Result.Count := 1;
      Result.Items (1) := (16#FDC#,16#FF000000#,
        16#80000000# or Shift_Left (Unsigned_32 (Topology.Default_Instance), 24),
        16#FF000000#, False);
      for E in Video_0 .. Video_2 loop
         if Inventory.Engines (E) then
            Result.Count := Result.Count + 1;
            Result.Items (Result.Count) :=
              (Engine_Base (E) + 16#3F10#, 0, 16#400000#, 16#400000#, False);
         end if;
      end loop;
      Result.Count := Result.Count + 1;
      Result.Items (Result.Count) := (16#9550#,0,16#200#,16#200#,True);
      Result.Count := Result.Count + 1;
      -- Firmware can lock MISCCPCTL. Upstream explicitly skips verification;
      -- a native executor must report an ignored clear separately, not hide it
      -- or turn it into a claimed successful register update.
      Result.Items (Result.Count) := (16#9424#,2,0,0,False);
      return Result;
   end Build;
end Intel_GPU_ADLN_GT_Settings;
