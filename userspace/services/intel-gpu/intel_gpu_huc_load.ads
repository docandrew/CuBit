with Interfaces; use Interfaces;
with Intel_GPU_Firmware;
with Intel_GPU_HuC_Registers; use Intel_GPU_HuC_Registers;
-- HuC load and authentication state machine, Gen12 ADL-P/ADL-N, GuC path.
-- Sequence (Linux v6.16 i915/gt/uc/intel_uc.c:495-530, intel_huc.c:528-571):
--   1. Upload: DMA the CSS+ucode into WOPCM with HUC_UKERNEL, BEFORE the GuC
--      upload, on every GuC load attempt. Needs a locked WOPCM partition whose
--      offset register carries HUC_LOADING_AGENT_GUC and room below the GuC.
--   2. The caller uploads the GuC and enables CT (not part of this unit).
--   3. Begin_Authentication: send GuC AUTHENTICATE_HUC with the GGTT offset of
--      the RSA page; then Poll_Authentication until HuC_Kernel_Load_Info
--      bit 0 is set or the caller's absolute deadline passes.
--   4. Reset: after any GT reset or GuC reload the HuC is gone. Return to
--      Idle and redo from step 1 before the next GuC upload.
-- A HuC failure is not a GPU failure: i915 continues without HuC
-- (intel_uc.c ignores intel_huc_auth's result), so callers degrade video
-- features rather than the device.
generic
   -- Bounded, nonraising, nonreentrant. Caller holds forcewake, owns the
   -- device and excludes other writers to these registers. A failed write
   -- may still have reached hardware.
   with procedure Read32 (Offset : Register_Offset; Value : out Register_Word;
                          Success : out Boolean);
   with procedure Write32 (Offset : Register_Offset; Value : Register_Word;
                           Success : out Boolean);
   -- Monotonic microseconds; Unsigned_64'Last means unavailable.
   with function Now return Unsigned_64;
   with procedure Pause;
   -- One AUTHENTICATE_HUC CT request [Authenticate_HuC_Action, RSA_GGTT]
   -- with its own bounded deadline; must not reuse a fence.
   with procedure Request_Authentication (RSA_GGTT : Unsigned_32;
                                          Reply : out Auth_Reply);
package Intel_GPU_HuC_Load with SPARK_Mode is
   -- Postconditions compare the phase before and after under conditions.
   pragma Unevaluated_Use_Of_Old (Allow);
   type Phase is (Idle, Transferred, Authenticating, Authenticated, Failed);
   type Load_Generation is mod 2 ** 32;

   type Loader is limited private;
   function Current (Object : Loader) return Phase;
   function Generation (Object : Loader) return Load_Generation;

   type Upload_Result is
     (-- Refused before any register write; phase unchanged.
      Rejected, Deadline_Passed, Clock_Unavailable, Invalid_MMIO,
      WOPCM_Not_Ready, Busy,
      -- After the first write; phase Failed until Reset.
      Write_Failed, Transfer_Failed, Clock_Failed, Timed_Out, Cleanup_Failed,
      Transferred);
   subtype Upload_Refused is Upload_Result range Rejected .. Busy;
   subtype Upload_Faulted is Upload_Result range Write_Failed .. Cleanup_Failed;

   -- Header/Blob_Bytes describe the immutable blob mapped at Source_GGTT.
   -- Deadline is absolute (Now units); Poll_Limit bounds a stopped clock.
   procedure Upload
     (Object : in out Loader; Header : Intel_GPU_Firmware.CSS_Header;
      Blob_Bytes, Source_GGTT, Deadline : Unsigned_64; Poll_Limit : Positive;
      Status : out Upload_Result)
   with Post =>
     Generation (Object) = Generation (Object)'Old and then
     (if Status = Transferred then
        Current (Object)'Old = Idle and Current (Object) = Transferred
      elsif Status in Upload_Refused then Current (Object) = Current (Object)'Old
      else Current (Object) = Failed) and then
     (if Current (Object)'Old /= Idle then Status = Rejected);

   type Auth_Result is
     (-- Refused before the GuC request; phase unchanged.
      Rejected, Deadline_Passed, Clock_Unavailable, Invalid_MMIO,
      -- Faulted; phase Failed until Reset.
      Stale_Status, Transport_Failed, Signature_Refused, Status_Failed,
      Clock_Failed, Timed_Out,
      -- In progress or done.
      Pending, Authenticated);
   subtype Auth_Refused is Auth_Result range Rejected .. Invalid_MMIO;
   subtype Auth_Faulted is Auth_Result range Stale_Status .. Timed_Out;

   -- RSA_GGTT: 4 KiB page holding exactly the blob's RSA bytes, at or above
   -- Pin_Bias and below GUC_GGTT_TOP. GuC CT must be enabled.
   procedure Begin_Authentication
     (Object : in out Loader; RSA_GGTT, Pin_Bias, Deadline : Unsigned_64;
      Status : out Auth_Result)
   with Post =>
     Generation (Object) = Generation (Object)'Old and then
     Status in Auth_Refused | Auth_Faulted | Pending and then
     (if Status = Pending then
        Current (Object)'Old = Transferred and
        Current (Object) = Authenticating
      elsif Status in Auth_Refused then Current (Object) = Current (Object)'Old
      else Current (Object) = Failed) and then
     (if Current (Object)'Old /= Transferred then Status = Rejected);

   -- One status read; never waits. Suitable for a driver-loop turn.
   procedure Poll_Authentication (Object : in out Loader; Status : out Auth_Result)
   with Post =>
     Generation (Object) = Generation (Object)'Old and then
     (if Current (Object)'Old /= Authenticating then
        Status = Rejected and Current (Object) = Current (Object)'Old
      else Status in Pending | Authenticated | Status_Failed | Clock_Failed |
                     Timed_Out and then
        (case Status is
           when Pending => Current (Object) = Authenticating,
           when Authenticated => Current (Object) = Authenticated,
           when others => Current (Object) = Failed));

   -- Begin, then poll with Pause between reads, at most Poll_Limit reads.
   -- Exhausting Poll_Limit before the deadline is Timed_Out.
   procedure Authenticate
     (Object : in out Loader; RSA_GGTT, Pin_Bias, Deadline : Unsigned_64;
      Poll_Limit : Positive; Status : out Auth_Result)
   with Post =>
     Generation (Object) = Generation (Object)'Old and then
     Status /= Pending and then
     (if Status = Authenticated then
        Current (Object)'Old = Transferred and Current (Object) = Authenticated
      elsif Status in Auth_Refused then Current (Object) = Current (Object)'Old
      else Current (Object) = Failed);

   -- Re-read status while Authenticated; a cleared bit means the HuC was
   -- lost behind the driver's back (phase Failed, Reset required).
   type Check_Result is (Not_Authenticated, Still_Authenticated, Lost);
   procedure Check (Object : in out Loader; Status : out Check_Result)
   with Post =>
     Generation (Object) = Generation (Object)'Old and then
     (if Current (Object)'Old /= Authenticated then
        Status = Not_Authenticated and Current (Object) = Current (Object)'Old
      elsif Status = Still_Authenticated then Current (Object) = Authenticated
      else Status = Lost and Current (Object) = Failed);

   -- GT reset or GuC reload happened (or is about to): HuC state is gone.
   procedure Reset (Object : in out Loader)
   with Post => Current (Object) = Idle and
     Generation (Object) = Generation (Object)'Old + 1;
private
   type Loader is limited record
      State : Phase := Idle;
      Epoch : Load_Generation := 0;
      Deadline : Unsigned_64 := 0;
      Last_Stamp : Unsigned_64 := 0;
   end record;
   function Current (Object : Loader) return Phase is (Object.State);
   function Generation (Object : Loader) return Load_Generation is
     (Object.Epoch);
end Intel_GPU_HuC_Load;
