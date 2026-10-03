with Interfaces.C;
package GPU_Metadata_Probe is
   function Run return Interfaces.C.int
     with Export, Convention => C, External_Name => "cubit_test_gpu_metadata";
end GPU_Metadata_Probe;
