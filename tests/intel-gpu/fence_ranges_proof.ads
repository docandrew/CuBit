with Intel_GPU_Fence_Ranges;
package Fence_Ranges_Proof with SPARK_Mode is
   package Native_Pool is new Intel_GPU_Fence_Ranges (100, 65535);
   package Edge_Pool is new Intel_GPU_Fence_Ranges (65532, 65535);
end Fence_Ranges_Proof;
