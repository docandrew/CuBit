with Interfaces; use Interfaces;
-- Media engine discovery for Gen12.0 (Alder Lake-P and its ADL-N
-- subplatform): decodes GEN11_GT_VEBOX_VDBOX_DISABLE into a typed engine set
-- pruned by the platform ceiling. Hardware facts only, from Linux v6.16:
--   i915/gt/intel_gt_regs.h:632-634   register 0x9140, VDBOX 7:0, VEBOX 19:16
--   i915/gt/intel_engine_cs.c:755-818 engine_mask_apply_media_fuses: below
--     media IP 12.55 the fields are DISABLE bits and i915 inverts them;
--     fused-on bits outside the platform ceiling are ignored
--   i915/gt/intel_engine_cs.c:722-749 gen11_vdbox_has_sfc (Gen12 rule)
--   i915/i915_pci.c:682-689,863       adl_p_info ceiling, ADL-N uses it
-- Media IP 12.0 reads no other fuse: HSW_PAVP_FUSE1 SFC enables apply only
-- from 12.55, and Gen12.0 has no compute engine to prune.
-- Pure decoding. A valid result is register evidence for one stable read
-- pair, not proof that an engine is powered, reset or usable.
package Intel_GPU_Media_Engines with SPARK_Mode is
   subtype Register_Offset is Unsigned_32;
   subtype Fuse_Word is Unsigned_32;

   Fuse_Register : constant Register_Offset := 16#9140#;
   -- An all-ones read is the PCI "no device / read failed" pattern.
   Unreadable : constant Fuse_Word := Fuse_Word'Last;
   VDBOX_Field_Shift : constant := 0;   -- GEN11_GT_VDBOX_DISABLE_MASK 7:0
   VEBOX_Field_Shift : constant := 16;  -- GEN11_GT_VEBOX_DISABLE_MASK 19:16

   Intel_Vendor : constant Unsigned_16 := 16#8086#;
   -- Admitted devices. Only the validated N95 (ADL-N GT1) for now; other
   -- ADL-P/ADL-N IDs share the ceiling but are not yet admitted.
   ADLN_N95_Device : constant Unsigned_16 := 16#46D2#;

   type VDBOX_Instance is range 0 .. 7;   -- I915_MAX_VCS
   type VEBOX_Instance is range 0 .. 3;   -- I915_MAX_VECS
   type VDBOX_Set is array (VDBOX_Instance) of Boolean;
   type VEBOX_Set is array (VEBOX_Instance) of Boolean;
   subtype VDBOX_Count is Natural range 0 .. 8;
   subtype VEBOX_Count is Natural range 0 .. 4;
   subtype VDBOX_Logical is VDBOX_Count range 0 .. 7;

   type Platform is (Alder_Lake_P);
   -- adl_p_info.platform_engine_mask: RCS0 BCS0 VCS0 VCS2 VECS0.
   ADLP_VDBOX_Ceiling : constant VDBOX_Set :=
     [0 | 2 => True, others => False];
   ADLP_VEBOX_Ceiling : constant VEBOX_Set := [0 => True, others => False];

   function VDBOX_Ceiling (Item : Platform) return VDBOX_Set is
     (case Item is when Alder_Lake_P => ADLP_VDBOX_Ceiling);
   function VEBOX_Ceiling (Item : Platform) return VEBOX_Set is
     (case Item is when Alder_Lake_P => ADLP_VEBOX_Ceiling);

   type Identity is record
      Known : Boolean := False;
      Item : Platform := Alder_Lake_P;
   end record;
   function Identify (Vendor, Device : Unsigned_16) return Identity is
     (if Vendor = Intel_Vendor and then Device = ADLN_N95_Device
      then (Known => True, Item => Alder_Lake_P)
      else (others => <>));

   -- Disable-bit semantics (media IP below 12.55).
   function VDBOX_Fused_Off (Fuse : Fuse_Word; I : VDBOX_Instance)
     return Boolean is
     ((Shift_Right (Fuse, VDBOX_Field_Shift + Natural (I)) and 1) = 1);
   function VEBOX_Fused_Off (Fuse : Fuse_Word; I : VEBOX_Instance)
     return Boolean is
     ((Shift_Right (Fuse, VEBOX_Field_Shift + Natural (I)) and 1) = 1);

   function Count (Set : VDBOX_Set) return VDBOX_Count;
   function Count (Set : VEBOX_Set) return VEBOX_Count;

   -- Gen12: an even physical VDBOX always reaches an SFC; an odd one only
   -- when its even neighbour is not enabled. (i915 also gates on sfc_mask,
   -- all ones below 12.55.)
   function Has_SFC (Enabled : VDBOX_Set; I : VDBOX_Instance) return Boolean is
     (Enabled (I) and then
        (I mod 2 = 0 or else not Enabled (I - 1)));

   type Engines is record
      Valid : Boolean := False;
      Render, Copy : Boolean := False;          -- not fusible on Gen12.0
      Video : VDBOX_Set := [others => False];    -- physical VCS instances
      Enhance : VEBOX_Set := [others => False];  -- physical VECS instances
      SFC : VDBOX_Set := [others => False];      -- VCS with SFC access
      Video_Count : VDBOX_Count := 0;
      Enhance_Count : VEBOX_Count := 0;
   end record;

   function Decode (Vendor, Device : Unsigned_16; Fuse : Fuse_Word)
     return Engines
   with Post =>
     Decode'Result.Valid =
       (Identify (Vendor, Device).Known and Fuse /= Unreadable) and then
     (if Decode'Result.Valid then
        Decode'Result.Render and Decode'Result.Copy and
        (for all I in VDBOX_Instance =>
           Decode'Result.Video (I) =
             (VDBOX_Ceiling (Identify (Vendor, Device).Item) (I) and
              not VDBOX_Fused_Off (Fuse, I))) and
        (for all I in VEBOX_Instance =>
           Decode'Result.Enhance (I) =
             (VEBOX_Ceiling (Identify (Vendor, Device).Item) (I) and
              not VEBOX_Fused_Off (Fuse, I))) and
        (for all I in VDBOX_Instance =>
           Decode'Result.SFC (I) = Has_SFC (Decode'Result.Video, I)) and
        Decode'Result.Video_Count = Count (Decode'Result.Video) and
        Decode'Result.Enhance_Count = Count (Decode'Result.Enhance)
      else Decode'Result = (Engines'(others => <>)));

   -- Two reads under one forcewake hold must agree; a changing value is
   -- rejected rather than guessed.
   function Decode_Stable (Vendor, Device : Unsigned_16;
                           First, Second : Fuse_Word) return Engines
   with Post =>
     (if First = Second then Decode_Stable'Result = Decode (Vendor, Device, First)
      else Decode_Stable'Result = (Engines'(others => <>)));

   -- Logical instance numbering: enabled physical instances in ascending
   -- order (i915 logical_vdbox; GuC ADS compacts the same way).
   function Logical (Set : Engines; I : VDBOX_Instance) return VDBOX_Logical
   with Pre => Set.Valid and then Set.Video (I);

   -- Video decode needs at least one VCS; HuC needs one too
   -- (i915/gt/uc/intel_huc.c:294-297 vcs_supported).
   function Has_Video (Set : Engines) return Boolean is
     (Set.Valid and then (for some I in VDBOX_Instance => Set.Video (I)));
end Intel_GPU_Media_Engines;
