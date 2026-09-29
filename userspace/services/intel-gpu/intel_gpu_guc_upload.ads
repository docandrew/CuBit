with Interfaces;
with Intel_GPU_Firmware;
with Intel_GPU_GuC_Status;
with Intel_GPU_GuC_Parameters;
generic
   -- ADL-N only. Exclusive reset/forcewake and coherent retained GGTT
   -- mapping are caller prerequisites. Header/source must remain immutable.
   -- Callbacks are bounded, ordered and nonraising. Failed writes may post.
   with function Read32 (Offset : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   with procedure Write32 (Offset, Value : Interfaces.Unsigned_32; Success : out Boolean);
   with procedure Read_Byte (Offset : Interfaces.Unsigned_64;
     Value : out Interfaces.Unsigned_8; Success : out Boolean);
   with function Now return Interfaces.Unsigned_64;
   with procedure Pause;
package Intel_GPU_GuC_Upload is
   -- Firmware ABI startup words, SOFT_SCRATCH(1..14). The caller must
   -- construct these for the selected firmware/device, including retained
   -- ADS/log mappings and supported feature/workaround flags. The typed encoder
   -- validates representation, not ownership or content of pointed-to objects.
   type Result is (Rejected, Parameters_Failed, WOPCM_Failed, Preparation_Failed,
                   Signature_Failed, Transfer_Failed, Startup_Failed,
                   Firmware_Ready);
   function Last_Startup_Raw return Interfaces.Unsigned_32;
   function Last_Startup_State return Intel_GPU_GuC_Status.State;
   -- Explicit text survives native Discard_Names. This inspects only saved
   -- state: no MMIO, clock reads or retry. A rejected repeat preserves the
   -- original diagnostic. "complete" means transfer, not firmware readiness.
   function Last_Transfer_Detail return String;
   -- One call per package instance, including rejected calls. No retry/free.
   -- Firmware_Ready requires post-transfer authenticated READY observation;
   -- it does not establish submission/communication readiness. GPU_Start addresses the SAME
   -- retained blob exposed by Read_Byte, not its physical backing address.
   procedure Execute (Header : Intel_GPU_Firmware.CSS_Header;
     Parameters : Intel_GPU_GuC_Parameters.Parameter_Block;
     Blob_Bytes, GPU_Start, Capacity, Base, Size : Interfaces.Unsigned_64;
     Poll_Limit : Positive; Status : out Result);
end Intel_GPU_GuC_Upload;
