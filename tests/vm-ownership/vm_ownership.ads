--  VMM-001 model (docs/hypervisor.md, section 5): a guest reaches only its
--  own frames, the frames that hold EPT/NPT tables and vCPU control state
--  are the kernel's and no guest translation names them, and nothing a VM
--  used is reused while any CPU may still translate it.
--
--  A hosted SPARK model, not kernel code, in the style of
--  tests/kernel-map-ownership (KERN-014) and tests/iommu (IOMMU-001). The
--  kernel adapter (EPT/NPT table writes, VMPTRLD/VMCLEAR, INVEPT and ASID
--  flushes) is outside the model; what it must meet is listed below.
--
--  State, one array per field:
--   * each frame: an owner (Free, Kernel, Process, Guest, Device), whether
--     the kernel's linear map covers it, the VM a Guest frame belongs to,
--     the VM a Device frame is granted to, the VM whose EPT a Kernel frame
--     is a table of (and whether that table is still linked, and when it
--     was unlinked), the vCPU whose control state a Kernel frame holds
--     (VMCS/VMCB, bitmaps, virtual-APIC, posted descriptor, XSAVE), the
--     one EPT entry that names it (Use_VM, Use_Page), and the monitor
--     process whose guest view maps it;
--   * each VM: its phase, its monitor, its flush round count, and the CPUs
--     it ran on since its last completed round (Ran_On);
--   * each EPT entry (VM, guest page): Absent, Mapped or Stale (cleared, a
--     CPU may still translate it), its target frame and stale stamp, and
--     whether an IOMMU guest domain mirrors it;
--   * each vCPU: its VM and the CPU its control state is active on; each
--     CPU: its current vCPU and whether it is in the guest;
--   * the CPUs' translation caches, abstractly: which (VM, page) entries
--     and which table frames each CPU may still hold.
--
--  Proved (gnatprove level 2, run.sh --prove): every operation keeps
--  Invariant, and the theorems below follow from it:
--   * Leaf_Owned: every live EPT entry names a frame the VM owns (Guest)
--     or was granted (Device), and no two entries name one frame;
--   * Tables_Private / Controls_Private: table and control frames are
--     Kernel-owned, so no entry, view or domain names them;
--   * Guest_Unmapped: guest frames are never in the kernel's map;
--   * Reuse_After_Flush: a frame leaves an EPT, and a table frame is freed,
--     only after a round of the VM that began after the unmap completed;
--     so no CPU translates a freed or reused frame for the VM (XSA-480);
--   * vCPU_Bound: a vCPU's VM never changes while it exists, its control
--     state is active on at most one CPU, and a CPU has one current vCPU;
--   * Views_Own_VM and Domain_Within_VM;
--   * Reclaimed: a VM that is Gone owns, maps, caches and names nothing.
--
--  Assumed (the kernel adapter):
--   * the EPT/NPT writer installs exactly the Mapped entries and clears an
--     entry before Unmap is recorded, and unlinks a table from its parent
--     (with no live entry below it) before Unlink_Table is recorded;
--   * Complete_Round is called only after INVEPT single-context (or the
--     ASID flush) has completed on every CPU in Ran_On, issued after every
--     Unmap and Unlink_Table recorded so far; the model clears exactly
--     those CPUs' caches and proves that this reaches every cached entry;
--   * Guest_Touch / Table_Walk abstract what the hardware may cache while
--     a CPU runs the guest: only Mapped entries and Linked tables of that
--     VM (the hardware's EPT walk honours present bits);
--   * Unmap_View and Unmap_Domain follow the debug-view and IOMMU ledgers'
--     own clear-then-flush order (proved there, not here).
pragma Ada_2022;

package VM_Ownership with SPARK_Mode is
   Frame_Count : constant := 48;   -- model sizes only
   VM_Count    : constant := 3;
   Page_Count  : constant := 8;    -- guest pages per VM
   VCPU_Count  : constant := 4;
   CPU_Count   : constant := 3;
   Process_Count : constant := 3;

   subtype Frame_Index is Natural range 0 .. Frame_Count - 1;
   subtype VM_Ref is Natural range 0 .. VM_Count;
   No_VM : constant VM_Ref := 0;
   subtype VM_Index is VM_Ref range 1 .. VM_Count;
   subtype Page_Index is Natural range 0 .. Page_Count - 1;
   subtype VCPU_Ref is Natural range 0 .. VCPU_Count;
   No_VCPU : constant VCPU_Ref := 0;
   subtype VCPU_Index is VCPU_Ref range 1 .. VCPU_Count;
   subtype CPU_Ref is Natural range 0 .. CPU_Count;
   No_CPU : constant CPU_Ref := 0;
   subtype CPU_Index is CPU_Ref range 1 .. CPU_Count;
   subtype Process_Ref is Natural range 0 .. Process_Count;
   No_Process : constant Process_Ref := 0;
   subtype Process_Index is Process_Ref range 1 .. Process_Count;
   subtype Round_Count is Natural range 0 .. 1_000_000;

   type Owner_Kind is (Free, Kernel, Process, Guest, Device);
   --  As in KERN-014: these never enter the kernel's map.
   subtype Process_Owner is Owner_Kind range Process .. Device;
   type Entry_Kind is (Absent, Mapped, Stale);
   type Phase_Kind is (Gone, Live, Closing);

   type Frame_Owners is array (Frame_Index) of Owner_Kind;
   type Frame_Flags is array (Frame_Index) of Boolean;
   type Frame_VMs is array (Frame_Index) of VM_Ref;
   type Frame_Pages is array (Frame_Index) of Page_Index;
   type Frame_Rounds is array (Frame_Index) of Round_Count;
   type Frame_VCPUs is array (Frame_Index) of VCPU_Ref;
   type Frame_Processes is array (Frame_Index) of Process_Ref;

   type Kind_Row is array (Page_Index) of Entry_Kind;
   type Target_Row is array (Page_Index) of Frame_Index;
   type Stamp_Row is array (Page_Index) of Round_Count;
   type Page_Flags is array (Page_Index) of Boolean;
   type CPU_Flags is array (CPU_Index) of Boolean;

   type VM_Kinds is array (VM_Index) of Kind_Row;
   type VM_Targets is array (VM_Index) of Target_Row;
   type VM_Stamps is array (VM_Index) of Stamp_Row;
   type VM_Page_Flags is array (VM_Index) of Page_Flags;
   type VM_Phases is array (VM_Index) of Phase_Kind;
   type VM_Rounds is array (VM_Index) of Round_Count;
   type VM_Processes is array (VM_Index) of Process_Ref;
   type VM_CPUs is array (VM_Index) of CPU_Flags;

   type VCPU_VMs is array (VCPU_Index) of VM_Ref;
   type VCPU_CPUs is array (VCPU_Index) of CPU_Ref;
   type CPU_VCPUs is array (CPU_Index) of VCPU_Ref;
   type Entry_Caches is array (CPU_Index) of VM_Page_Flags;
   type Table_Caches is array (CPU_Index) of Frame_Flags;

   --  Frames.
   Owners        : Frame_Owners := [others => Free];
   --  The lower half starts in the kernel pool, the upper in the process pool.
   Kernel_Mapped : Frame_Flags := [for F in Frame_Index => F < Frame_Count / 2];
   Guest_Of      : Frame_VMs := [others => No_VM];
   Device_Of     : Frame_VMs := [others => No_VM];
   Table_Of      : Frame_VMs := [others => No_VM];
   Table_Linked  : Frame_Flags := [others => False];
   Table_Stamp   : Frame_Rounds := [others => 0];
   Control_Of    : Frame_VCPUs := [others => No_VCPU];
   Use_VM        : Frame_VMs := [others => No_VM];
   Use_Page      : Frame_Pages := [others => 0];
   View_By       : Frame_Processes := [others => No_Process];
   --  VMs.
   Phase   : VM_Phases := [others => Gone];
   Monitor : VM_Processes := [others => No_Process];
   Round   : VM_Rounds := [others => 0];
   Ran_On  : VM_CPUs := [others => [others => False]];
   --  EPT entries.
   Kind   : VM_Kinds := [others => [others => Absent]];
   Target : VM_Targets := [others => [others => 0]];
   Stamp  : VM_Stamps := [others => [others => 0]];
   Dom    : VM_Page_Flags := [others => [others => False]];
   --  vCPUs and CPUs.
   VM_Of     : VCPU_VMs := [others => No_VM];
   Active_On : VCPU_CPUs := [others => No_CPU];
   Current   : CPU_VCPUs := [others => No_VCPU];
   Running   : CPU_Flags := [others => False];
   --  The vCPU's guest tag (VPID/ASID) must be flushed before it next
   --  enters: set at creation and by architectural changes to paging.
   type VCPU_Flags is array (VCPU_Index) of Boolean;
   Tag_Stale : VCPU_Flags := [others => True];
   --  The CPU the vCPU last ran on: a load elsewhere marks its tag stale
   --  (the tag is per vCPU; another CPU may hold an old stint's entries).
   type VCPU_Last is array (VCPU_Index) of CPU_Ref;
   Last_On : VCPU_Last := [others => No_CPU];
   --  Translation caches (abstract TLB and paging-structure caches).
   Cached  : Entry_Caches := [others => [others => [others => False]]];
   TCached : Table_Caches := [others => [others => False]];

   -- The invariant, per element ------------------------------------------

   --  Per frame, one clause per field (so an operation that changes one
   --  field re-proves only the clauses that read it: the KERN-014 lesson).
   function Pool_Ok (F : Frame_Index) return Boolean is
     ((if Owners (F) = Kernel then Kernel_Mapped (F))
      and then (if Owners (F) in Process_Owner then not Kernel_Mapped (F)))
     with Global => (Owners, Kernel_Mapped);

   function Guest_Frame_Ok (F : Frame_Index) return Boolean is
     (if Owners (F) = Guest then
        Guest_Of (F) /= No_VM and then Phase (Guest_Of (F)) /= Gone
      else Guest_Of (F) = No_VM)
     with Global => (Owners, Guest_Of, Phase);

   function Device_Frame_Ok (F : Frame_Index) return Boolean is
     (if Device_Of (F) /= No_VM then
        Owners (F) = Device and then Phase (Device_Of (F)) /= Gone)
     with Global => (Owners, Device_Of, Phase);

   function Table_Frame_Ok (F : Frame_Index) return Boolean is
     (if Table_Of (F) /= No_VM then
        Owners (F) = Kernel and then Control_Of (F) = No_VCPU
        and then Phase (Table_Of (F)) /= Gone
        and then (Table_Linked (F) or else Table_Stamp (F) <= Round (Table_Of (F)))
      else not Table_Linked (F))
     with Global => (Owners, Table_Of, Control_Of, Phase, Table_Linked,
                     Table_Stamp, Round);

   function Control_Frame_Ok (F : Frame_Index) return Boolean is
     (if Control_Of (F) /= No_VCPU then
        Owners (F) = Kernel and then VM_Of (Control_Of (F)) /= No_VM)
     with Global => (Owners, Control_Of, VM_Of);

   --  The reverse map: the one entry that names F.
   function Use_Ok (F : Frame_Index) return Boolean is
     (if Use_VM (F) /= No_VM then
        Kind (Use_VM (F)) (Use_Page (F)) /= Absent
        and then Target (Use_VM (F)) (Use_Page (F)) = F)
     with Global => (Use_VM, Use_Page, Kind, Target);

   function View_Ok (F : Frame_Index) return Boolean is
     (if View_By (F) /= No_Process then
        Owners (F) = Guest and then Guest_Of (F) /= No_VM
        and then View_By (F) = Monitor (Guest_Of (F)))
     with Global => (View_By, Owners, Guest_Of, Monitor);

   --  Leaf_Owned, per entry.
   function Target_Ok (V : VM_Index; G : Page_Index) return Boolean is
     (if Kind (V) (G) /= Absent then
        Phase (V) /= Gone
        and then Use_VM (Target (V) (G)) = V
        and then Use_Page (Target (V) (G)) = G
        and then ((Owners (Target (V) (G)) = Guest
                   and then Guest_Of (Target (V) (G)) = V)
                  or else (Owners (Target (V) (G)) = Device
                           and then Device_Of (Target (V) (G)) = V)))
     with Global => (Kind, Target, Phase, Use_VM, Use_Page, Owners, Guest_Of,
                     Device_Of);

   function Stamp_Ok (V : VM_Index; G : Page_Index) return Boolean is
     (if Kind (V) (G) = Stale then Stamp (V) (G) <= Round (V))
     with Global => (Kind, Stamp, Round);

   function Domain_Ok (V : VM_Index; G : Page_Index) return Boolean is
     (if Dom (V) (G) then Kind (V) (G) = Mapped)
     with Global => (Dom, Kind);

   function Frame_Ok (F : Frame_Index) return Boolean is
     (Pool_Ok (F) and then Guest_Frame_Ok (F) and then Device_Frame_Ok (F)
      and then Table_Frame_Ok (F) and then Control_Frame_Ok (F)
      and then Use_Ok (F) and then View_Ok (F));

   function Entry_Ok (V : VM_Index; G : Page_Index) return Boolean is
     (Target_Ok (V, G) and then Stamp_Ok (V, G) and then Domain_Ok (V, G));

   function VM_Ok (V : VM_Index) return Boolean is
     ((if Phase (V) = Gone then Monitor (V) = No_Process
      else Monitor (V) /= No_Process)
      and then (for all C in CPU_Index =>
                  (if Ran_On (V) (C) then Phase (V) /= Gone)))
     with Global => (Phase, Monitor, Ran_On);

   --  What a CPU may still translate for a VM: a live entry, unmapped no
   --  earlier than the VM's current round, on a CPU the next round reaches.
   function Cache_Ok (C : CPU_Index; V : VM_Index; G : Page_Index) return Boolean is
     (if Cached (C) (V) (G) then
        Ran_On (V) (C) and then Kind (V) (G) /= Absent
        and then (if Kind (V) (G) = Stale then Stamp (V) (G) = Round (V)))
     with Global => (Cached, Ran_On, Kind, Stamp, Round);

   function Table_Cache_Ok (C : CPU_Index; F : Frame_Index) return Boolean is
     (if TCached (C) (F) then
        Table_Of (F) /= No_VM and then Ran_On (Table_Of (F)) (C)
        and then (Table_Linked (F)
                  or else Table_Stamp (F) = Round (Table_Of (F))))
     with Global => (TCached, Table_Of, Table_Linked, Table_Stamp, Ran_On, Round);

   function VCPU_Ok (X : VCPU_Index) return Boolean is
     ((if VM_Of (X) /= No_VM then Phase (VM_Of (X)) /= Gone
       else Active_On (X) = No_CPU)
      and then (if Active_On (X) /= No_CPU then Current (Active_On (X)) = X))
     with Global => (VM_Of, Phase, Active_On, Current);

   function CPU_Ok (C : CPU_Index) return Boolean is
     ((if Current (C) /= No_VCPU then
         Active_On (Current (C)) = C
         and then VM_Of (Current (C)) /= No_VM
         and then Ran_On (VM_Of (Current (C))) (C))
      and then (if Running (C) then
                  Current (C) /= No_VCPU and then not Tag_Stale (Current (C))))
     with Global => (Current, Active_On, VM_Of, Ran_On, Running, Tag_Stale);

   --  One function per conjunct: an operation that leaves a conjunct's
   --  globals alone keeps it by congruence, without unfolding it.
   function Pools_Ok return Boolean is (for all F in Frame_Index => Pool_Ok (F));
   function Guests_Ok return Boolean is (for all F in Frame_Index => Guest_Frame_Ok (F));
   function Devices_Ok return Boolean is (for all F in Frame_Index => Device_Frame_Ok (F));
   function Tables_Ok return Boolean is (for all F in Frame_Index => Table_Frame_Ok (F));
   function Controls_Ok return Boolean is (for all F in Frame_Index => Control_Frame_Ok (F));
   function Uses_Ok return Boolean is (for all F in Frame_Index => Use_Ok (F));
   function Views_Ok return Boolean is (for all F in Frame_Index => View_Ok (F));
   function Targets_Ok return Boolean is
     (for all V in VM_Index => (for all G in Page_Index => Target_Ok (V, G)));
   function Stamps_Ok return Boolean is
     (for all V in VM_Index => (for all G in Page_Index => Stamp_Ok (V, G)));
   function Domains_Ok return Boolean is
     (for all V in VM_Index => (for all G in Page_Index => Domain_Ok (V, G)));
   function VMs_Ok return Boolean is
     (for all V in VM_Index => VM_Ok (V));
   function Caches_Ok return Boolean is
     (for all C in CPU_Index =>
        (for all V in VM_Index => (for all G in Page_Index => Cache_Ok (C, V, G))));
   function Table_Caches_Ok return Boolean is
     (for all C in CPU_Index => (for all F in Frame_Index => Table_Cache_Ok (C, F)));
   function CPUs_Ok return Boolean is
     (for all C in CPU_Index => CPU_Ok (C));
   function VCPUs_Ok return Boolean is
     (for all X in VCPU_Index => VCPU_Ok (X));

   function Frames_Ok return Boolean is
     (Pools_Ok and then Guests_Ok and then Devices_Ok and then Tables_Ok
      and then Controls_Ok and then Uses_Ok and then Views_Ok);
   function Entries_Ok return Boolean is
     (Targets_Ok and then Stamps_Ok and then Domains_Ok);

   function Invariant return Boolean is
     (Frames_Ok and then VMs_Ok and then Entries_Ok and then Caches_Ok
      and then Table_Caches_Ok and then CPUs_Ok and then VCPUs_Ok);

   -- Theorems (each follows from Invariant alone) --------------------------

   --  No live EPT entry names a kernel frame (tables, control state, the
   --  kernel heap) or a process's frame, and no two entries share a frame.
   procedure Leaf_Owned (V : VM_Index; G : Page_Index) with
     Ghost,
     Pre  => Invariant and then Kind (V) (G) /= Absent,
     Post => Owners (Target (V) (G)) in Guest | Device
             and then Table_Of (Target (V) (G)) = No_VM
             and then Control_Of (Target (V) (G)) = No_VCPU
             and then not Kernel_Mapped (Target (V) (G))
             and then (Guest_Of (Target (V) (G)) = V
                       or else Device_Of (Target (V) (G)) = V);

   procedure Leaves_Disjoint (V, W : VM_Index; G, H : Page_Index) with
     Ghost,
     Pre  => Invariant and then Kind (V) (G) /= Absent
             and then Kind (W) (H) /= Absent
             and then Target (V) (G) = Target (W) (H),
     Post => V = W and then G = H;

   --  A table or control frame is reachable from no entry, view or domain.
   procedure Tables_Private (F : Frame_Index) with
     Ghost,
     Pre  => Invariant and then (Table_Of (F) /= No_VM or else Control_Of (F) /= No_VCPU),
     Post => Owners (F) = Kernel and then Use_VM (F) = No_VM
             and then View_By (F) = No_Process
             and then (for all V in VM_Index =>
                         (for all G in Page_Index =>
                            (if Kind (V) (G) /= Absent then Target (V) (G) /= F)));

   --  Whatever a CPU may still translate for V is V's own frame.
   procedure Cached_Owned (C : CPU_Index; V : VM_Index; G : Page_Index) with
     Ghost,
     Pre  => Invariant and then Cached (C) (V) (G),
     Post => Owners (Target (V) (G)) in Guest | Device
             and then (Guest_Of (Target (V) (G)) = V
                       or else Device_Of (Target (V) (G)) = V);

   --  A VM that is Gone owns, maps, caches and names nothing.
   procedure Reclaimed (V : VM_Index) with
     Ghost,
     Pre  => Invariant and then Phase (V) = Gone,
     Post => Monitor (V) = No_Process
             and then (for all F in Frame_Index =>
                         Guest_Of (F) /= V and then Device_Of (F) /= V
                         and then Table_Of (F) /= V and then Use_VM (F) /= V)
             and then (for all X in VCPU_Index => VM_Of (X) /= V)
             and then (for all G in Page_Index => Kind (V) (G) = Absent)
             and then (for all C in CPU_Index =>
                         not Ran_On (V) (C)
                         and then (for all G in Page_Index => not Cached (C) (V) (G)));

   -- Lemmas the operations use ------------------------------------------------

   --  A frame no VM owns or was granted, or that no entry names in the
   --  reverse map, is the target of no entry.
   procedure Lemma_Not_Target (F : Frame_Index) with
     Ghost,
     Pre  => Invariant
             and then (Owners (F) not in Guest | Device or else Use_VM (F) = No_VM),
     Post => (for all V in VM_Index =>
                (for all G in Page_Index =>
                   (if Kind (V) (G) /= Absent then Target (V) (G) /= F)))
             and then Use_VM (F) = No_VM;

   --  A free frame carries nothing: no VM, table, vCPU, view, entry or cache.
   procedure Lemma_Free_Frame (F : Frame_Index) with
     Ghost,
     Pre  => Invariant and then Owners (F) = Free,
     Post => Guest_Of (F) = No_VM and then Device_Of (F) = No_VM
             and then Table_Of (F) = No_VM and then not Table_Linked (F)
             and then Control_Of (F) = No_VCPU and then View_By (F) = No_Process
             and then Use_VM (F) = No_VM
             and then (for all C in CPU_Index => not TCached (C) (F))
             and then (for all V in VM_Index =>
                         (for all G in Page_Index =>
                            (if Kind (V) (G) /= Absent then Target (V) (G) /= F)));

   -- Frames outside any VM -------------------------------------------------

   --  Kernel pool (mapped) and process pool (unmapped), as KERN-014 keeps
   --  them; only what the VM operations need.
   procedure Alloc_Kernel (F : Frame_Index) with
     Pre  => Invariant and then Owners (F) = Free and then Kernel_Mapped (F),
     Post => Invariant and then Owners (F) = Kernel;

   procedure Alloc_Process (F : Frame_Index) with
     Pre  => Invariant and then Owners (F) = Free and then not Kernel_Mapped (F),
     Post => Invariant and then Owners (F) = Process;

   --  devmgr's device memory (MMIO a driver or VM may be granted).
   procedure Make_Device (F : Frame_Index) with
     Pre  => Invariant and then Owners (F) = Free and then not Kernel_Mapped (F),
     Post => Invariant and then Owners (F) = Device;

   -- VMs -------------------------------------------------------------------

   procedure Create_VM (V : VM_Index; Root : Frame_Index; P : Process_Index) with
     Pre  => Invariant and then Phase (V) = Gone
             and then Owners (Root) = Free and then Kernel_Mapped (Root),
     Post => Invariant and then Phase (V) = Live and then Monitor (V) = P
             and then Table_Of (Root) = V;

   procedure Add_Table (V : VM_Index; F : Frame_Index) with
     Pre  => Invariant and then Phase (V) /= Gone
             and then Owners (F) = Free and then Kernel_Mapped (F),
     Post => Invariant and then Table_Of (F) = V and then Table_Linked (F);

   procedure Unlink_Table (F : Frame_Index) with
     Pre  => Invariant and then Table_Of (F) /= No_VM and then Table_Linked (F),
     Post => Invariant and then not Table_Linked (F);

   --  Only once a round of its VM began after the unlink (XSA-480).
   procedure Free_Table (F : Frame_Index) with
     Pre  => Invariant and then Table_Of (F) /= No_VM
             and then not Table_Linked (F)
             and then Table_Stamp (F) < Round (Table_Of (F)),
     Post => Invariant and then Owners (F) = Free and then Table_Of (F) = No_VM;

   --  From the process pool: never in the kernel's map.
   procedure Alloc_Guest (V : VM_Index; F : Frame_Index) with
     Pre  => Invariant and then Phase (V) = Live
             and then Owners (F) = Free and then not Kernel_Mapped (F),
     Post => Invariant and then Owners (F) = Guest and then Guest_Of (F) = V;

   --  KERN-014's Free_User: no entry, view or domain names it any more.
   procedure Free_Guest (F : Frame_Index) with
     Pre  => Invariant and then Owners (F) = Guest
             and then Use_VM (F) = No_VM and then View_By (F) = No_Process,
     Post => Invariant and then Owners (F) = Free;

   procedure Grant_Device (V : VM_Index; F : Frame_Index) with
     Pre  => Invariant and then Phase (V) = Live
             and then Owners (F) = Device and then Device_Of (F) = No_VM,
     Post => Invariant and then Device_Of (F) = V;

   procedure Revoke_Device (F : Frame_Index) with
     Pre  => Invariant and then Device_Of (F) /= No_VM and then Use_VM (F) = No_VM,
     Post => Invariant and then Device_Of (F) = No_VM;

   -- EPT entries -------------------------------------------------------------

   procedure Map (V : VM_Index; G : Page_Index; F : Frame_Index) with
     Pre  => Invariant and then Phase (V) = Live
             and then Kind (V) (G) = Absent
             and then Use_VM (F) = No_VM
             and then ((Owners (F) = Guest and then Guest_Of (F) = V)
                       or else (Owners (F) = Device and then Device_Of (F) = V)),
     Post => Invariant and then Kind (V) (G) = Mapped and then Target (V) (G) = F;

   --  After the writer cleared the entry (and any domain mirroring it).
   procedure Unmap (V : VM_Index; G : Page_Index) with
     Pre  => Invariant and then Kind (V) (G) = Mapped and then not Dom (V) (G),
     Post => Invariant and then Kind (V) (G) = Stale;

   --  The adapter's INVEPT / ASID round on every CPU in Ran_On (V).
   procedure Complete_Round (V : VM_Index) with
     Pre  => Invariant and then Phase (V) /= Gone
             and then Round (V) < Round_Count'Last,
     Post => Invariant and then Round (V) = Round'Old (V) + 1;

   --  Only once a round began after the unmap: then no CPU translates it.
   procedure Retire (V : VM_Index; G : Page_Index) with
     Pre  => Invariant and then Kind (V) (G) = Stale
             and then Stamp (V) (G) < Round (V),
     Post => Invariant and then Kind (V) (G) = Absent;

   -- IOMMU guest domains and monitor views ------------------------------------

   procedure Map_Domain (V : VM_Index; G : Page_Index) with
     Pre  => Invariant and then Kind (V) (G) = Mapped,
     Post => Invariant and then Dom (V) (G);

   procedure Unmap_Domain (V : VM_Index; G : Page_Index) with
     Pre  => Invariant and then Dom (V) (G),
     Post => Invariant and then not Dom (V) (G);

   procedure Map_View (P : Process_Index; V : VM_Index; G : Page_Index) with
     Pre  => Invariant and then Monitor (V) = P
             and then Kind (V) (G) = Mapped
             and then Owners (Target (V) (G)) = Guest
             and then View_By (Target (V) (G)) = No_Process,
     Post => Invariant and then View_By (Target (V) (G)) = P;

   procedure Unmap_View (F : Frame_Index) with
     Pre  => Invariant and then View_By (F) /= No_Process,
     Post => Invariant and then View_By (F) = No_Process;

   -- vCPUs -------------------------------------------------------------------

   procedure Create_VCPU (V : VM_Index; X : VCPU_Index; F : Frame_Index) with
     Pre  => Invariant and then Phase (V) = Live and then VM_Of (X) = No_VM
             and then Owners (F) = Free and then Kernel_Mapped (F),
     Post => Invariant and then VM_Of (X) = V and then Control_Of (F) = X
             and then Tag_Stale (X);

   --  Its last control frame, once its state is active on no CPU.
   procedure Destroy_VCPU (X : VCPU_Index; F : Frame_Index) with
     Pre  => Invariant and then VM_Of (X) /= No_VM and then Active_On (X) = No_CPU
             and then Control_Of (F) = X
             and then (for all E in Frame_Index =>
                         (if E /= F then Control_Of (E) /= X)),
     Post => Invariant and then VM_Of (X) = No_VM and then Owners (F) = Free;

   --  VMPTRLD (after VMCLEAR wherever it was active) / the VMCB's CPU.
   procedure Load (X : VCPU_Index; C : CPU_Index) with
     Pre  => Invariant and then VM_Of (X) /= No_VM
             and then Phase (VM_Of (X)) = Live
             and then Active_On (X) = No_CPU and then Current (C) = No_VCPU,
     Post => Invariant and then Active_On (X) = C and then Current (C) = X
             and then (if Last_On'Old (X) /= C then Tag_Stale (X));

   --  VMCLEAR: the CPU stays in Ran_On until the VM's next round.
   procedure Clear (X : VCPU_Index) with
     Pre  => Invariant and then Active_On (X) /= No_CPU
             and then not Running (Active_On (X)),
     Post => Invariant and then Active_On (X) = No_CPU;

   procedure Enter (C : CPU_Index) with
     Pre  => Invariant and then Current (C) /= No_VCPU
             and then Phase (VM_Of (Current (C))) = Live
             and then not Tag_Stale (Current (C)),
     Post => Invariant and then Running (C);

   procedure Leave (C : CPU_Index) with
     Pre  => Invariant and then Running (C),
     Post => Invariant and then not Running (C);

   --  What the hardware may cache while C runs the guest.
   procedure Guest_Touch (C : CPU_Index; G : Page_Index) with
     Pre  => Invariant and then Running (C)
             and then Kind (VM_Of (Current (C))) (G) = Mapped,
     Post => Invariant;

   procedure Table_Walk (C : CPU_Index; F : Frame_Index) with
     Pre  => Invariant and then Running (C)
             and then Table_Of (F) = VM_Of (Current (C))
             and then Table_Linked (F),
     Post => Invariant;

   -- Guest architectural state (docs/hypervisor.md 4.7a) ------------------------

   --  Set_State, INIT, SIPI, RESET, a CR0.PG / CR4 / EFER change: registers
   --  and control fields only, made by the monitor while the vCPU is not in
   --  the guest. EPT_Independent_Of_Arch_State is the postcondition: no
   --  entry, table, linkage, frame owner or cache entry of any VM changes.
   --  Flush_On_Arch_Change: a change to paging marks the tag stale, and
   --  Enter needs it fresh.
   procedure Set_Arch_State (X : VCPU_Index; Paging_Changed : Boolean) with
     Pre  => Invariant and then VM_Of (X) /= No_VM
             and then (Active_On (X) = No_CPU or else not Running (Active_On (X))),
     Post => Invariant
             and then Kind = Kind'Old and then Target = Target'Old
             and then Stamp = Stamp'Old and then Dom = Dom'Old
             and then Table_Of = Table_Of'Old and then Table_Linked = Table_Linked'Old
             and then Owners = Owners'Old and then Guest_Of = Guest_Of'Old
             and then Cached = Cached'Old and then TCached = TCached'Old
             and then (if Paging_Changed then Tag_Stale (X));

   --  INVVPID single-context / the ASID's TLB control, on X's CPU.
   procedure Flush_Tag (X : VCPU_Index) with
     Pre  => Invariant and then Active_On (X) /= No_CPU,
     Post => Invariant and then not Tag_Stale (X);

   -- Teardown ------------------------------------------------------------------

   procedure Destroy (V : VM_Index) with
     Pre  => Invariant and then Phase (V) = Live,
     Post => Invariant and then Phase (V) = Closing;

   --  Everything returned; a final round left no CPU holding it.
   procedure Finish_Destroy (V : VM_Index) with
     Pre  => Invariant and then Phase (V) = Closing
             and then (for all X in VCPU_Index => VM_Of (X) /= V)
             and then (for all G in Page_Index => Kind (V) (G) = Absent)
             and then (for all F in Frame_Index =>
                         Guest_Of (F) /= V and then Device_Of (F) /= V
                         and then Table_Of (F) /= V)
             and then (for all C in CPU_Index => not Ran_On (V) (C)),
     Post => Invariant and then Phase (V) = Gone;

end VM_Ownership;
