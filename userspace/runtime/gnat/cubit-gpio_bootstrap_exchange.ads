------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The GPIO bootstrap admission (ACPI-POOL-001 step 1b): acpi.svc asks
--  devmgr, once per boot and before any device _INI, to adopt the memory
--  spans its identified GPIO controller's _CRS declares as platform-pool
--  members, because devmgr could not read them from the Intel P2SB.
--
--  @description
--  Ordering (acpi.svc's startup):
--    1. tables loaded, module-level code run, region handlers set up;
--    2. ONE bounded observation of the identified GPIO controller on the
--       startup evaluator: _HID, _STA, _CRS, copied, all handles released,
--       back at a quiet Field boundary; no GPIO pad access;
--    3. Admission_Request (this package) to devmgr, and its answer;
--    4. only then device _INI (the IdeaPad's TPD0._INI reads GPIO pad
--       registers): on Admitted the spans are pool members and AML maps
--       them like any other; on any refusal or timeout the GPIO-touching
--       devices are reported unavailable (typed) and _INI proceeds without
--       the GPIO spans. The request is never retried after _INI started.
--  The post-_INI platform-device report (Report_Platform_Device, 16#024F#)
--  stays facts only: it never adds pool members or mapping authority.
--
--  Transport: the firmware-data binding. A program approved with
--  (acpi-firmware-data approve-declared) holds devmgr's endpoint badged
--  CuBit.Firmware_Data_Exchange.Authority_Tag (startup name
--  Data_Label (Binding)); only over it is Admission_Request served. A
--  synchronous call DOWN the IPC graph: the requester calls devmgr with a
--  finite absolute deadline of its own (Now + at most Admission_Budget_
--  Milliseconds, on the clock it uses for Pool_Request); devmgr only
--  replies, never calls back, and does no blocking work while answering.
--
--  Request (a plain call; no capability is carried), Request_Words words:
--    Request_Word    the requester's request id (never zero)
--    Reference_Word  CuBit.Grant_References.Encode of a READ-ONLY grant to
--                    devmgr holding the payload (GPIO_Bootstrap_Payload)
--    Length_Word     the payload's exact byte length (at most
--                    GPIO_Bootstrap_Payload.Max_Payload_Bytes)
--  devmgr acquires the grant with the requester as the expected owner,
--  copies the bytes, returns the acquisition, and only then decodes and
--  decides (copy before validate). The requester revokes the grant after
--  the reply.
--
--  Reply, label Admission_Request, Reply_Words words:
--    Reply_Request_Word  the request id
--    Disposition_Word    Disposition'Enum_Rep
--    Detail_Word         Admitted: members added; Malformed: the first
--                        failing payload byte offset, or for a span rule
--                        Span_Detail_Base + the 1-based span ordinal; a
--                        span refusal (Not_Chipset_MMIO .. Straddles): the
--                        1-based span ordinal; Pool_Sealed/Kernel_Refused:
--                        the span ordinal the kernel refused; else 0
--    Members_Word        the pool's member count after the answer
--  A request id asked again (after a timeout) gets the original answer:
--  devmgr never admits twice. A distinct request after an Admitted one is
--  Already_Admitted. A refused request may be corrected and sent again
--  before _INI (each attempt is logged).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.GPIO_Bootstrap_Exchange with Pure, SPARK_Mode is

   --  After Data_Window_Request (16#024D#), Pool_Request (16#024E#) and the
   --  platform-device report's Report_Platform_Device (16#024F#) and
   --  Epoch_Complete (16#0250#).
   Admission_Request : constant Unsigned_32 := 16#0251#;

   Request_Words  : constant := 3;
   Request_Word   : constant := 0;
   Reference_Word : constant := 1;
   Length_Word    : constant := 2;

   Reply_Words        : constant := 4;
   Reply_Request_Word : constant := 0;
   Disposition_Word   : constant := 1;
   Detail_Word        : constant := 2;
   Members_Word       : constant := 3;

   --  The most a requester waits for the answer: it passes its clock's
   --  Now + at most this as the call's absolute deadline. devmgr answers
   --  without blocking, so a miss means devmgr is not serving: treat it as
   --  a refusal (GPIO unavailable) and never retry after _INI started.
   Admission_Budget_Milliseconds : constant := 1_000;

   --  Detail_Word for a Malformed span rule: this plus the span ordinal
   --  (payload byte offsets stay below it).
   Span_Detail_Base : constant := 16#1_0000#;

   --  How devmgr answered (the contract's table):
   --    No_Discovery_Failure: no P2SB discovery ran this boot (another
   --      platform), so there is nothing to fall back from;
   --    Unknown_Controller: _HID is not this platform's GPIO controller, or
   --      _STA says it is not present and enabled;
   --    Capacity: devmgr's records could not grow, or the pool has no room
   --      declared for the spans;
   --    Pool_Sealed: the kernel's pool is in use (the first mapping or
   --      delegation happened): its members are fixed.
   type Disposition is
     (Admitted,              --  the spans are pool members (Chipset_Shared)
      P2SB_Available,        --  devmgr read the P2SB this boot: use that
      No_Discovery_Failure,  --  no P2SB discovery ran this boot
      Already_Admitted,      --  admitted once this boot already
      Malformed,             --  words, payload or a span's shape is wrong
      Unknown_Controller,    --  not the platform's controller, or absent
      Not_Chipset_MMIO,      --  a span lies in no device-memory root
      Overlaps_RAM,          --  a span touches usable RAM
      Overlaps_BAR,          --  a span touches a PCI BAR or bridge window
      Overlaps_PCI_Config,   --  a span touches an MCFG (ECAM) allocation
      Overlaps_Pool_Member,  --  a span touches a member already in the pool
      Straddles,             --  a span is only partly inside one root
      Not_Approved,          --  not through the firmware-data badge
      Copy_Failed,           --  the payload grant could not be copied
      Capacity,              --  no records or no declared pool room
      Pool_Sealed,           --  the kernel's pool is in use
      Kernel_Refused);       --  the kernel refused an insert otherwise
   for Disposition use
     (Admitted             => 1,
      P2SB_Available       => 2,
      No_Discovery_Failure => 3,
      Already_Admitted     => 4,
      Malformed            => 5,
      Unknown_Controller   => 6,
      Not_Chipset_MMIO     => 7,
      Overlaps_RAM         => 8,
      Overlaps_BAR         => 9,
      Overlaps_PCI_Config  => 10,
      Overlaps_Pool_Member => 11,
      Straddles            => 12,
      Not_Approved         => 13,
      Copy_Failed          => 14,
      Capacity             => 15,
      Pool_Sealed          => 16,
      Kernel_Refused       => 17);

   function Name (D : Disposition) return String is
     (case D is
        when Admitted             => "ADMITTED",
        when P2SB_Available       => "P2SB_AVAILABLE",
        when No_Discovery_Failure => "NO_DISCOVERY_FAILURE",
        when Already_Admitted     => "ALREADY_ADMITTED",
        when Malformed            => "MALFORMED",
        when Unknown_Controller   => "UNKNOWN_CONTROLLER",
        when Not_Chipset_MMIO     => "NOT_CHIPSET_MMIO",
        when Overlaps_RAM         => "OVERLAPS_RAM",
        when Overlaps_BAR         => "OVERLAPS_BAR",
        when Overlaps_PCI_Config  => "OVERLAPS_PCI_CONFIG",
        when Overlaps_Pool_Member => "OVERLAPS_POOL_MEMBER",
        when Straddles            => "STRADDLES",
        when Not_Approved         => "NOT_APPROVED",
        when Copy_Failed          => "COPY_FAILED",
        when Capacity             => "CAPACITY",
        when Pool_Sealed          => "POOL_SEALED",
        when Kernel_Refused       => "KERNEL_REFUSED");

   function Valid_Disposition (Word : Unsigned_64) return Boolean is
     (for some D in Disposition => Disposition'Enum_Rep (D) = Word);

end CuBit.GPIO_Bootstrap_Exchange;
