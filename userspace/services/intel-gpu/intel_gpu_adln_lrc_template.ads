with Interfaces; use Interfaces;
package Intel_GPU_ADLN_LRC_Template with SPARK_Mode is
   type Register_Page is array (Natural range 0 .. 1023) of Unsigned_32;
   -- Gen12 render-engine register-load skeleton, at context byte offset4096.
   -- NOT executable initialization: values remain zero. Caller must supply
   -- context control, PPGTT root, ring state, RPCS and context workarounds.
   -- The HWSP and remaining render-context pages are separate backing.
   function Build return Register_Page;
end Intel_GPU_ADLN_LRC_Template;
