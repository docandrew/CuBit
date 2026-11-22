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
--
-- Firmware memory (the ACPI hardware grant, slice 2) is a separate,
-- narrower authority: a CAP_DEVICE_MEM tagged FIRMWARE_NVS_TAG,
-- FIRMWARE_CONTROL_TAG, FIRMWARE_PCI_CONFIG_TAG or FIRMWARE_DATA_TAG,
-- which only the kernel's firmware-memory roots carry, maps only firmware
-- pages of its own kind (Physical_Ranges.Firmware_Range: whole ACPI NVS
-- pages, the FACS page, PCI segment 0 bus 0's ECAM slice, or table-named
-- firmware data), and only through MAP_DEVICE into the holder itself: NVS
-- and the FACS page write-back, PCI configuration uncached and read-only
-- whatever rights a capability claims (ACPI-PCI-001; its kernel root has
-- no write right either), firmware data uncached and read-write. A
-- device-memory capability (untagged) never maps a firmware page through
-- this authority and a firmware-memory one never maps a device page. The
-- tag is what the holder reads back (INSPECT) as the memory's kind and
-- cache mode: no launcher word can claim it.
-------------------------------------------------------------------------------
package Device_Memory_Authority with SPARK_Mode is

   use type Physical_Ranges.Firmware_Kind;
   use type Device_Memory_Admission.Access_Mode;

   subtype Access_Mode is Device_Memory_Admission.Access_Mode;

   -- Cap is a CAP_DEVICE_MEM covering [Base, Base + Bytes) with the rights
   -- Mode needs. A system call that maps device memory names the one
   -- capability it relies on (its handle); nothing searches a table.
   function Holds
     (Cap : Capability; Base, Bytes : Unsigned_64; Mode : Access_Mode)
      return Boolean
   is (Cap.capType = CAP_DEVICE_MEM
       and then Cap.authorityTag = NO_AUTHORITY_TAG
       and then Device_Memory_Admission.Covers
                  (Cap.object.ref, Cap.object.param, Base, Bytes)
       and then Device_Memory_Admission.Allows
                  (Cap.rights (RIGHT_READ), Cap.rights (RIGHT_WRITE), Mode));

   -- May a process holding Cap map [Base, Base + Bytes) in Mode?
   function Authorize
     (Cap : Capability; M : Physical_Ranges.Map;
      Base, Bytes : Unsigned_64; Mode : Access_Mode) return Boolean
   with Pre  => Physical_Ranges.Well_Formed (M),
        Post => Authorize'Result =
                  (Physical_Ranges.Device_Bytes (M, Base, Bytes)
                   and then Holds (Cap, Base, Bytes, Mode));

   -- The firmware-memory tags, and the kind each names.
   function Firmware_Tag (Tag : Authority_Tag) return Boolean is
     (Tag = FIRMWARE_NVS_TAG or else Tag = FIRMWARE_CONTROL_TAG
      or else Tag = FIRMWARE_PCI_CONFIG_TAG or else Tag = FIRMWARE_DATA_TAG);
   function Kind_Of (Tag : Authority_Tag) return Physical_Ranges.Firmware_Kind is
     (if Tag = FIRMWARE_CONTROL_TAG then Physical_Ranges.Control_Memory
      elsif Tag = FIRMWARE_PCI_CONFIG_TAG then Physical_Ranges.PCI_Config_Memory
      elsif Tag = FIRMWARE_DATA_TAG then Physical_Ranges.Data_Memory
      else Physical_Ranges.NVS_Memory)
   with Pre => Firmware_Tag (Tag);
   function Tag_Of (Kind : Physical_Ranges.Firmware_Kind) return Authority_Tag is
     (case Kind is
         when Physical_Ranges.NVS_Memory        => FIRMWARE_NVS_TAG,
         when Physical_Ranges.Control_Memory    => FIRMWARE_CONTROL_TAG,
         when Physical_Ranges.PCI_Config_Memory => FIRMWARE_PCI_CONFIG_TAG,
         when Physical_Ranges.Data_Memory       => FIRMWARE_DATA_TAG)
   with Post => Firmware_Tag (Tag_Of'Result) and then Kind_Of (Tag_Of'Result) = Kind;

   -- Kind_Of and Tag_Of are inverse on the firmware tags.
   procedure Lemma_Tag_Round_Trip (Tag : Authority_Tag)
   with Ghost, Global => null,
        Pre  => Firmware_Tag (Tag),
        Post => Tag_Of (Kind_Of (Tag)) = Tag;

   -- The access modes a firmware kind may be mapped in: PCI configuration
   -- only read-only (configuration writes stay devmgr's), the others
   -- (firmware data included) either.
   function Firmware_Mode
     (Kind : Physical_Ranges.Firmware_Kind; Mode : Access_Mode) return Boolean
   is (Kind /= Physical_Ranges.PCI_Config_Memory
       or else Mode = Device_Memory_Admission.Read_Only);

   -- How the kernel maps a firmware kind: write-back for NVS and the FACS
   -- page (Physical_Ranges, "Firmware memory"), uncached for PCI
   -- configuration (device registers) and for firmware data (a reserved
   -- span may be firmware RAM or chipset MMIO; uncached is right for both).
   function Write_Back (Kind : Physical_Ranges.Firmware_Kind) return Boolean is
     (Kind in Physical_Ranges.NVS_Memory | Physical_Ranges.Control_Memory);

   -- Cap is a firmware-memory capability covering [Base, Base + Bytes) with
   -- the rights Mode needs.
   function Firmware_Holds
     (Cap : Capability; Base, Bytes : Unsigned_64; Mode : Access_Mode)
      return Boolean
   is (Cap.capType = CAP_DEVICE_MEM
       and then Firmware_Tag (Cap.authorityTag)
       and then Device_Memory_Admission.Covers
                  (Cap.object.ref, Cap.object.param, Base, Bytes)
       and then Device_Memory_Admission.Allows
                  (Cap.rights (RIGHT_READ), Cap.rights (RIGHT_WRITE), Mode));

   -- May a process holding Cap map the firmware pages [Base, Base + Bytes)
   -- of Cap's kind into itself in Mode? (Cache mode: Write_Back.)
   function Authorize_Firmware
     (Cap : Capability; M : Physical_Ranges.Map;
      Base, Bytes : Unsigned_64; Mode : Access_Mode) return Boolean
   with Pre  => Physical_Ranges.Well_Formed (M),
        Post => Authorize_Firmware'Result =
                  (Firmware_Holds (Cap, Base, Bytes, Mode)
                   and then Firmware_Mode (Kind_Of (Cap.authorityTag), Mode)
                   and then Physical_Ranges.Firmware_Bytes
                              (M, Base, Bytes, Kind_Of (Cap.authorityTag)));

   -- PCI configuration is never mapped writable, and is mapped uncached,
   -- whatever rights the capability carries.
   procedure Lemma_PCI_Config_Read_Only_Uncached
     (Cap : Capability; M : Physical_Ranges.Map;
      Base, Bytes : Unsigned_64; Mode : Access_Mode)
   with Ghost, Global => null,
        Pre  => Physical_Ranges.Well_Formed (M)
                and then Authorize_Firmware (Cap, M, Base, Bytes, Mode)
                and then Cap.authorityTag = FIRMWARE_PCI_CONFIG_TAG,
        Post => Mode = Device_Memory_Admission.Read_Only
                and then not Write_Back (Kind_Of (Cap.authorityTag))
                and then (for all P in Physical_Ranges.Page_Of (Base) ..
                                       Physical_Ranges.Page_Of (Base + (Bytes - 1)) =>
                            Physical_Ranges.In_PCI_Config (M, P));

   -- Firmware data is mapped uncached, may be mapped writable, and never
   -- aliases a page of another firmware kind (so never a write-back page:
   -- not NVS, not the FACS page) nor the PCI slice or an ACPI table's page.
   procedure Lemma_Data_Uncached_Alone
     (Cap : Capability; M : Physical_Ranges.Map;
      Base, Bytes : Unsigned_64; Mode : Access_Mode)
   with Ghost, Global => null,
        Pre  => Physical_Ranges.Well_Formed (M)
                and then Authorize_Firmware (Cap, M, Base, Bytes, Mode)
                and then Cap.authorityTag = FIRMWARE_DATA_TAG,
        Post => not Write_Back (Kind_Of (Cap.authorityTag))
                and then Firmware_Mode (Kind_Of (Cap.authorityTag), Device_Memory_Admission.Read_Write)
                and then (for all P in Physical_Ranges.Page_Of (Base) ..
                                       Physical_Ranges.Page_Of (Base + (Bytes - 1)) =>
                            Physical_Ranges.Data_Page (M, P)
                            and then not Physical_Ranges.NVS_Page (M, P)
                            and then not Physical_Ranges.Control_Page_Admitted (M, P)
                            and then not Physical_Ranges.In_PCI_Config (M, P)
                            and then not Physical_Ranges.Table_Page (M, P));

   -- The two authorities never overlap: no capability is both, and an NVS
   -- page is never device memory. (The FACS page may be firmware-reserved,
   -- which device memory also names; devmgr's ledger keeps it ACPI's.)
   procedure Lemma_Authorities_Disjoint
     (Cap : Capability; M : Physical_Ranges.Map;
      Base, Bytes : Unsigned_64; Mode : Access_Mode)
   with Ghost, Global => null,
        Pre  => Physical_Ranges.Well_Formed (M),
        Post => not (Holds (Cap, Base, Bytes, Mode)
                     and then Firmware_Holds (Cap, Base, Bytes, Mode))
                and then
                (if Physical_Ranges.Device_Bytes (M, Base, Bytes) then
                   not Physical_Ranges.NVS_Page (M, Physical_Ranges.Page_Of (Base)));

   -- A firmware mapping authorized here is ACPI NVS or firmware-reserved
   -- pages (NVS, the FACS page) or device-mappable ECAM pages only: never
   -- RAM, kernel memory, a boot module, ACPI tables or kernel MMIO.
   procedure Lemma_Firmware_Mapping_Excludes_RAM
     (Cap : Capability; M : Physical_Ranges.Map;
      Base, Bytes : Unsigned_64; Mode : Access_Mode)
   with Ghost, Global => null,
        Pre  => Physical_Ranges.Well_Formed (M)
                and then Authorize_Firmware (Cap, M, Base, Bytes, Mode),
        Post => (for all P in Physical_Ranges.Page_Of (Base) ..
                              Physical_Ranges.Page_Of (Base + (Bytes - 1)) =>
                   Physical_Ranges.Firmware_Class
                     (Kind_Of (Cap.authorityTag), Physical_Ranges.Class_Of (M, P)));

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
     (Cap : Capability; M : Physical_Ranges.Map;
      Base, Bytes : Unsigned_64; Mode : Access_Mode) return Explanation
   with Pre  => Physical_Ranges.Well_Formed (M)
                and then not Authorize (Cap, M, Base, Bytes, Mode),
        Post => Explained (M, Base, Bytes, Explain'Result)
                and then
                (if Explain'Result.Why = Pub.No_Covering_Capability then
                   Physical_Ranges.Device_Bytes (M, Base, Bytes)
                   and then not Holds (Cap, Base, Bytes, Mode));

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
     (Cap : Capability; M : Physical_Ranges.Map;
      Base, Bytes : Unsigned_64; Mode : Access_Mode)
   with Ghost, Global => null,
        Pre  => Physical_Ranges.Well_Formed (M)
                and then Authorize (Cap, M, Base, Bytes, Mode),
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
