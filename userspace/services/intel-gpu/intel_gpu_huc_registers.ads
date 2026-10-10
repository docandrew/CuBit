with Interfaces; use Interfaces;
-- HuC upload and authentication register interface for Gen12 (ADL-P/ADL-N).
-- Values from Linux v6.16 (hardware facts only, no code):
--   i915/gt/uc/intel_guc_reg.h:45-68   DMA registers, WOPCM offset, HuC status
--   i915/gt/uc/intel_uc_fw.c:1084-1131 uc_fw_xfer: source, WOPCM destination,
--     CSS+ucode size, masked START, wait for START clear, masked flag clear
--   i915/gt/uc/intel_huc_fw.c:277-284  HuC: destination 0, flag HUC_UKERNEL
--   i915/gt/uc/intel_huc.c:299-302      Gen11+ status: 0xC1DC bit 0
--   i915/gt/uc/intel_uc.c:363-400       WOPCM offset carries HUC_LOADING_AGENT_GUC
--   i915/gt/uc/abi/guc_actions_abi.h    INTEL_GUC_ACTION_AUTHENTICATE_HUC 0x4000
package Intel_GPU_HuC_Registers with SPARK_Mode, Pure is
   subtype Register_Offset is Unsigned_32 range 0 .. 16#1F_FFFF#;
   subtype Register_Word is Unsigned_32;

   -- Read failure / absent device pattern.
   Unreadable : constant Register_Word := Register_Word'Last;

   DMA_Address_0_Low  : constant Register_Offset := 16#C300#; -- source
   DMA_Address_0_High : constant Register_Offset := 16#C304#;
   DMA_Address_1_Low  : constant Register_Offset := 16#C308#; -- destination
   DMA_Address_1_High : constant Register_Offset := 16#C30C#;
   DMA_Copy_Size      : constant Register_Offset := 16#C310#;
   DMA_Control        : constant Register_Offset := 16#C314#;
   GuC_WOPCM_Offset   : constant Register_Offset := 16#C340#;
   HuC_Kernel_Load_Info : constant Register_Offset := 16#C1DC#;

   -- DMA_ADDR_1_HIGH address-space selector.
   DMA_Address_Space_WOPCM : constant Register_Word := 16#0007_0000#;
   -- HW ignores the destination for HuC; i915 programs 0.
   HuC_Destination : constant Register_Word := 0;

   -- DMA_CTRL is a masked register: bits 31:16 select which of 15:0 change.
   Start_DMA   : constant Register_Word := 16#0001#;  -- bit 0
   HuC_UKernel : constant Register_Word := 16#0200#;  -- bit 9
   Mask_Shift  : constant := 16;
   function Masked_Enable (Bits : Register_Word) return Register_Word is
     (Shift_Left (Bits and 16#FFFF#, Mask_Shift) or (Bits and 16#FFFF#));
   function Masked_Disable (Bits : Register_Word) return Register_Word is
     (Shift_Left (Bits and 16#FFFF#, Mask_Shift));
   -- Masked_Enable (HuC_UKernel or Start_DMA) and Masked_Disable
   -- (HuC_UKernel), spelled out for preelaboration; tests check equality.
   HuC_DMA_Start : constant Register_Word := 16#0201_0201#;
   HuC_DMA_Clear : constant Register_Word := 16#0200_0000#;

   -- DMA_GUC_WOPCM_OFFSET fields.
   WOPCM_Offset_Valid : constant Register_Word := 16#0001#;  -- bit 0 (lock)
   HuC_Loading_Agent_GuC : constant Register_Word := 16#0002#;  -- bit 1
   WOPCM_Offset_Mask : constant Register_Word := 16#FFFF_C000#; -- 31:14

   -- GEN11_HUC_KERNEL_LOAD_INFO.
   HuC_Load_Successful : constant Register_Word := 16#0001#;  -- bit 0

   function Authenticated (Status : Register_Word) return Boolean is
     (Status /= Unreadable and then (Status and HuC_Load_Successful) /= 0);

   -- GuC action, a regular request with a response
   -- (i915/gt/uc/intel_guc.c:658-666: [action, RSA GGTT offset]).
   Authenticate_HuC_Action : constant Unsigned_32 := 16#4000#;
   Authenticate_HuC_Words : constant := 2;
   -- How the CT round trip for that request ended. Refused is a GuC
   -- RESPONSE_FAILURE: the firmware rejected the request (for example a bad
   -- signature). Transport_Failed covers CT errors, credits and timeouts.
   type Auth_Reply is (Accepted, Refused, Transport_Failed);

   -- Linux budgets, for callers that compute explicit absolute deadlines.
   -- They are not hidden defaults: every operation takes a deadline.
   Linux_DMA_Budget_Us  : constant Unsigned_64 := 100_000; -- uc_fw_xfer 100 ms
   -- intel_huc.c:445-490: three 1 s waits (20 in debug builds); more than
   -- 50 ms is already logged as excessive, so 50 ms is the expected case.
   Linux_Auth_Budget_Us : constant Unsigned_64 := 3_000_000;
   Linux_Auth_Expected_Us : constant Unsigned_64 := 50_000;
end Intel_GPU_HuC_Registers;
