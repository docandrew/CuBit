pragma Ada_2022;
with Interfaces; use Interfaces;
with Capabilities; use Capabilities;
with Device_Memory_Admission;
with Physical_Ranges;
with Device_Memory_Publication;

-------------------------------------------------------------------------------
-- Device_Memory_Authority (CAP-001)
--
-- The single decision every path that maps physical memory on the strength
-- of CAP_DEVICE_MEM makes (MAP_DEVICE, MAP_INTO device mappings, MAPFB), and
-- the kernel's bound on POLICY_MINT_CAPABILITY of CAP_DEVICE_MEM. Both
-- require the physical range to be device-mappable in the boot
-- classification, so RAM, kernel memory, boot modules, ACPI memory and
-- kernel-driven MMIO are refused whatever capability a process holds.
-------------------------------------------------------------------------------
package Device_Memory_Authority with SPARK_Mode is

   subtype Access_Mode is Device_Memory_Admission.Access_Mode;

   -- Slot S holds a CAP_DEVICE_MEM covering [Base, Base + Bytes) with the
   -- rights Mode needs.
   function Slot_Grants
     (Table : CapabilityTable; S : CapabilitySlot;
      Base, Bytes : Unsigned_64; Mode : Access_Mode) return Boolean
   is (Table (S).capType = CAP_DEVICE_MEM
       and then Device_Memory_Admission.Covers
                  (Table (S).object.ref, Table (S).object.param, Base, Bytes)
       and then Device_Memory_Admission.Allows
                  (Table (S).rights (RIGHT_READ), Table (S).rights (RIGHT_WRITE),
                   Mode));

   function Holds
     (Table : CapabilityTable; Base, Bytes : Unsigned_64; Mode : Access_Mode)
      return Boolean
   is (for some S in CapabilitySlot => Slot_Grants (Table, S, Base, Bytes, Mode));

   -- May a process with Table map [Base, Base + Bytes) in Mode?
   function Authorize
     (Table : CapabilityTable; M : Physical_Ranges.Map;
      Base, Bytes : Unsigned_64; Mode : Access_Mode) return Boolean
   with Pre  => Physical_Ranges.Well_Formed (M),
        Post => Authorize'Result =
                  (Physical_Ranges.Device_Bytes (M, Base, Bytes)
                   and then Holds (Table, Base, Bytes, Mode));

   -- May policy mint a CAP_DEVICE_MEM naming [Base, Base + Bytes)? Policy
   -- still chooses who gets which device; the kernel only refuses ranges
   -- that are not entirely device-mappable.
   function Mint_Admissible
     (M : Physical_Ranges.Map; Base, Bytes : Unsigned_64) return Boolean
   is (Physical_Ranges.Device_Bytes (M, Base, Bytes));

   -- Why a request was refused, for the kernel's denial record
   -- (Device_Memory_Publication). Page is a physical page address.
   package Pub renames Device_Memory_Publication;
   use type Pub.Reason;
   use type Physical_Ranges.Range_Class;
   type Explanation is record
      Why   : Pub.Reason := Pub.Invalid_Range;
      Page  : Unsigned_64 := 0;
      Class : Physical_Ranges.Range_Class := Physical_Ranges.Unreported;
   end record;

   function Valid_Range (Base, Bytes : Unsigned_64) return Boolean is
     (Bytes > 0 and then Base < Physical_Ranges.Address_Limit
      and then Bytes - 1 <= Physical_Ranges.Address_Limit - 1 - Base);

   -- The refusal of a policy mint: never No_Covering_Capability.
   function Explain_Mint
     (M : Physical_Ranges.Map; Base, Bytes : Unsigned_64) return Explanation
   with Pre  => Physical_Ranges.Well_Formed (M)
                and then not Mint_Admissible (M, Base, Bytes),
        Post => Explain_Mint'Result.Why /= Pub.No_Covering_Capability
                and then Explained (M, Base, Bytes, Explain_Mint'Result);

   -- The refusal of a mapping.
   function Explain
     (Table : CapabilityTable; M : Physical_Ranges.Map;
      Base, Bytes : Unsigned_64; Mode : Access_Mode) return Explanation
   with Pre  => Physical_Ranges.Well_Formed (M)
                and then not Authorize (Table, M, Base, Bytes, Mode),
        Post => Explained (M, Base, Bytes, Explain'Result)
                and then
                (if Explain'Result.Why = Pub.No_Covering_Capability then
                   Physical_Ranges.Device_Bytes (M, Base, Bytes)
                   and then not Holds (Table, Base, Bytes, Mode));

   -- What each reason states about the request.
   function Explained
     (M : Physical_Ranges.Map; Base, Bytes : Unsigned_64; E : Explanation)
      return Boolean
   is (case E.Why is
         when Pub.Invalid_Range => not Valid_Range (Base, Bytes),
         when Pub.Classification_Unavailable =>
           Valid_Range (Base, Bytes) and then not (M.Sealed and then M.Complete),
         when Pub.Blocked_Page =>
           Valid_Range (Base, Bytes) and then M.Sealed and then M.Complete
           and then E.Page mod Physical_Ranges.Page_Bytes = 0
           and then E.Page / Physical_Ranges.Page_Bytes in
                      Physical_Ranges.Page_Of (Base) ..
                      Physical_Ranges.Page_Of (Base + (Bytes - 1))
           and then not Physical_Ranges.Device_Mappable (E.Class)
           and then E.Class = Physical_Ranges.Class_Of
                                (M, E.Page / Physical_Ranges.Page_Bytes),
         when Pub.No_Covering_Capability =>
           Physical_Ranges.Device_Bytes (M, Base, Bytes),
         when Pub.Not_Boot_Module => False)
   with Pre => Physical_Ranges.Well_Formed (M);

   -- A mapping authorized here never includes a page of a non-mappable
   -- class: in particular never a RAM page.
   procedure Lemma_Authorized_Mapping_Excludes_RAM
     (Table : CapabilityTable; M : Physical_Ranges.Map;
      Base, Bytes : Unsigned_64; Mode : Access_Mode)
   with Ghost, Global => null,
        Pre  => Physical_Ranges.Well_Formed (M)
                and then Authorize (Table, M, Base, Bytes, Mode),
        Post => Bytes > 0 and then Base < Physical_Ranges.Address_Limit
                and then Bytes - 1 <= Physical_Ranges.Address_Limit - 1 - Base
                and then
                (for all P in Physical_Ranges.Page_Of (Base) ..
                              Physical_Ranges.Page_Of (Base + (Bytes - 1)) =>
                   Physical_Ranges.Device_Mappable
                     (Physical_Ranges.Class_Of (M, P)));

   -- A minted device capability likewise names only device-mappable pages.
   procedure Lemma_Minted_Range_Excludes_RAM
     (M : Physical_Ranges.Map; Base, Bytes : Unsigned_64)
   with Ghost, Global => null,
        Pre  => Physical_Ranges.Well_Formed (M)
                and then Mint_Admissible (M, Base, Bytes),
        Post => (for all P in Physical_Ranges.Page_Of (Base) ..
                              Physical_Ranges.Page_Of (Base + (Bytes - 1)) =>
                   Physical_Ranges.Device_Mappable
                     (Physical_Ranges.Class_Of (M, P)));

   -- Non-amplification: whatever an attenuated device capability grants,
   -- its parent grants too (range subset and rights subset).
   procedure Lemma_Attenuation_Does_Not_Amplify
     (Child, Parent : Capability; Base, Bytes : Unsigned_64; Mode : Access_Mode)
   with Ghost, Global => null,
        Pre  => Child.capType = CAP_DEVICE_MEM
                and then isAttenuationOf (Child, Parent)
                and then Device_Memory_Admission.Covers
                           (Child.object.ref, Child.object.param, Base, Bytes)
                and then Device_Memory_Admission.Allows
                           (Child.rights (RIGHT_READ), Child.rights (RIGHT_WRITE), Mode),
        Post => Parent.capType = CAP_DEVICE_MEM
                and then Device_Memory_Admission.Covers
                           (Parent.object.ref, Parent.object.param, Base, Bytes)
                and then Device_Memory_Admission.Allows
                           (Parent.rights (RIGHT_READ), Parent.rights (RIGHT_WRITE), Mode);

end Device_Memory_Authority;
