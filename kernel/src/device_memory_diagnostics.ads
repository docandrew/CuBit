pragma Ada_2022;
with Interfaces; use Interfaces;
with Device_Memory_Authority;
with Device_Memory_Publication;
with Process;

-------------------------------------------------------------------------------
-- Device_Memory_Diagnostics (CAP-001; process kills since 2026-10-11)
--
-- The kernel's side of Device_Memory_Publication: one frame holding every
-- refused device-memory request (a bounded ring numbered from 1; a full ring
-- overwrites the oldest, which readers count as lost) and the boot
-- classification. Written only here, mapped read-only into the registered
-- log service only, never charged to or freed with a process.
-------------------------------------------------------------------------------
package Device_Memory_Diagnostics with
   SPARK_Mode,
   Abstract_State => (Records with External),
   Initializes => Records
is

   -- After Physical_Range_Map.Seal_From_Boot, before the first process:
   -- allocates the frame and publishes the classification and the
   -- device-memory roots devmgr receives.
   procedure Publish_Classification
     with Global => (In_Out => Records);

   -- One refusal: the caller, what it asked for and why it was refused.
   -- Also printed on the kernel console.
   procedure Record_Denial
     (Caller  : Process.ProcessID;
      Request : Device_Memory_Publication.Operation;
      Base    : Unsigned_64;
      Bytes   : Unsigned_64;
      Mode    : Device_Memory_Authority.Access_Mode;
      Why     : Device_Memory_Authority.Explanation)
     with Global => (In_Out => Records);

   -- A user process the kernel is about to kill for a fault: who, what
   -- kind, the exception vector, the faulting address and instruction
   -- (0: unknown). Also printed on the kernel console. Never blocks.
   procedure Record_Kill
     (Victim  : Process.ProcessID;
      Kind    : Device_Memory_Publication.Kill_Kind;
      Vector  : Unsigned_64;
      Address : Unsigned_64;
      RIP     : Unsigned_64)
     with Global => (In_Out => Records);

   -- The IOMMU's state (IOMMU-001), as Device_Memory_Publication's state
   -- word. Written once by Iommu.Setup, after Publish_Classification and
   -- before the first process. Also printed on the kernel console.
   procedure Record_IOMMU_State (Word : Unsigned_64)
     with Global => (In_Out => Records);

   -- One DMA fault a remapping unit reported (IOMMU-001): the requester
   -- (segment 31:16, requester ID 15:0), the faulting page, the raw VT-d
   -- reason code, whether it was a write, and the unit (1-based). The
   -- page's class comes from the sealed physical classification. Also
   -- printed on the kernel console. Never blocks (interrupt path).
   procedure Record_DMA_Fault
     (Requester : Unsigned_64;
      Address   : Unsigned_64;
      Code      : Unsigned_64;
      Write     : Boolean;
      Unit      : Unsigned_64)
     with Global => (In_Out => Records);

   -- The registered log service reads the page (SYSCALL_REGISTER_DRIVER
   -- for the logstore role). Mapped is False only if the page could not be
   -- mapped; the registration is then refused, so a registered log
   -- service can always read it.
   procedure Map_Into (Reader : Process.ProcessID; Mapped : out Boolean)
     with Global => (In_Out => Records);

end Device_Memory_Diagnostics;
