pragma Ada_2022;
with Interfaces;

--  The DMA remapping facts the kernel drives VT-d from (IOMMU-001): one pass
--  over a copied ACPI DMAR table that either accepts the whole table and
--  returns its hardware units (DRHD), reserved memory regions (RMRR) and
--  their device scopes, or rejects it with a typed reason and the offset of
--  the first offending byte. Nothing here touches memory or hardware: every
--  address is an untrusted number that the kernel still has to admit.
--
--  Firmware_Tables.DMAR (the ACPI service's metadata reader) is separate and
--  unchanged; this unit is the kernel's narrower, proved admission.
--
--  Admission rules (ACPI DMAR, Intel VT-d specification chapter 8):
--  * the SDT header is accepted by Read_Table ("DMAR", checksum, extent) and
--    the declared extent is the whole input;
--  * the fixed part (host address width, flags, reserved) is present and the
--    host address width (field + 1) is 32 .. 64 bits;
--  * every remapping structure has at least its type's fixed size and fits;
--    unknown structure types are skipped by their length (the spec reserves
--    them for future use), never interpreted;
--  * every device scope inside a DRHD or RMRR has a valid length (scope
--    types the spec does not define are skipped, never stored), a path of
--    whole (device, function) pairs with device <= 31 and function <= 7, at
--    least one pair for a PCI endpoint or bridge, and at most
--    Maximum_Path_Hops of them;
--  * a DRHD's register base is nonzero and page aligned and its register set
--    (2 ** size pages) does not wrap; at most one DRHD per segment includes
--    all PCI devices, and that unit names no PCI endpoint or bridge scope;
--  * an RMRR's base is page aligned, its inclusive limit ends a page, base
--    <= limit, and it names at least one device scope;
--  * the table has at least one DRHD.
--  Ceilings (LIM-001: named, CAPACITY when exceeded): Maximum_Units,
--  Maximum_Regions and Maximum_Scopes.
package Firmware_Tables.Remapping with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_64;

   Fixed_Size : constant := Table_Header_Size + 12;
   Structure_Header_Size : constant := 4;
   Unit_Fixed_Size : constant := 16;
   Region_Fixed_Size : constant := 24;
   Scope_Header_Size : constant := 6;
   Path_Pair_Size : constant := 2;
   Page_Bytes : constant := 4096;

   Maximum_Units : constant := 8;
   Maximum_Regions : constant := 16;
   Maximum_Scopes : constant := 64;
   Maximum_Path_Hops : constant := 8;
   --  A DRHD size field above this is refused: 2 ** 4 pages is 64 KiB, four
   --  times any register set the VT-d specification lays out.
   Maximum_Register_Size_Field : constant := 4;

   Minimum_Host_Width : constant := 32;
   Maximum_Host_Width : constant := 64;
   subtype Host_Width is Natural range Minimum_Host_Width .. Maximum_Host_Width;

   subtype Unit_Count is Natural range 0 .. Maximum_Units;
   subtype Unit_Index is Positive range 1 .. Maximum_Units;
   subtype Region_Count is Natural range 0 .. Maximum_Regions;
   subtype Region_Index is Positive range 1 .. Maximum_Regions;
   subtype Scope_Count is Natural range 0 .. Maximum_Scopes;
   subtype Scope_Index is Positive range 1 .. Maximum_Scopes;
   subtype Hop_Count is Natural range 0 .. Maximum_Path_Hops;
   subtype Hop_Index is Positive range 1 .. Maximum_Path_Hops;
   subtype Register_Size_Field is Natural range 0 .. Maximum_Register_Size_Field;

   subtype Segment_Number is Interfaces.Unsigned_16;
   subtype Bus_Number is Interfaces.Unsigned_8;
   subtype Device_Number is Byte range 0 .. 31;
   subtype Function_Number is Byte range 0 .. 7;

   --  Device scope types (DMAR "Device Scope Structure", type field).
   type Scope_Kind is
     (PCI_Endpoint, PCI_Bridge, IOAPIC, HPET, Namespace_Device);

   type Hop is record
      Device : Device_Number := 0;
      Func   : Function_Number := 0;
   end record;
   type Hop_Array is array (Hop_Index) of Hop;

   --  Which structure a scope belongs to.
   type Scope_Owner is (Unit_Scope, Region_Scope);

   type Scope is record
      Kind           : Scope_Kind := PCI_Endpoint;
      Owner          : Scope_Owner := Unit_Scope;
      Owner_Index    : Positive := 1;   --  Unit_Index or Region_Index
      Enumeration_ID : Byte := 0;       --  IOAPIC/HPET/namespace number
      Start_Bus      : Bus_Number := 0;
      Hops           : Hop_Count := 0;
      Path           : Hop_Array := [others => (Device => 0, Func => 0)];
   end record;
   type Scope_Array is array (Scope_Index) of Scope;

   type Unit is record
      Segment        : Segment_Number := 0;
      Register_Base  : Address_Value := 0;          --  page aligned, nonzero
      Register_Pages : Positive := 1;               --  2 ** size field
      Include_All    : Boolean := False;            --  INCLUDE_PCI_ALL
      First_Scope    : Positive := 1;               --  its scopes, in order
      Scopes         : Scope_Count := 0;
   end record;
   type Unit_Array is array (Unit_Index) of Unit;

   type Region is record
      Segment     : Segment_Number := 0;
      Base        : Address_Value := 0;             --  page aligned
      Limit       : Address_Value := 0;             --  inclusive, ends a page
      First_Scope : Positive := 1;
      Scopes      : Scope_Count := 0;
   end record;
   type Region_Array is array (Region_Index) of Region;

   type Facts is record
      Revision            : Byte := 0;
      Width               : Host_Width := Minimum_Host_Width;
      Interrupt_Remapping : Boolean := False;   --  flags bit 0 (INTR_REMAP)
      X2APIC_Opt_Out      : Boolean := False;   --  flags bit 1
      DMA_Control_Opt_In  : Boolean := False;   --  flags bit 2
      Unit_Total          : Unit_Count := 0;
      Units               : Unit_Array;
      Region_Total        : Region_Count := 0;
      Regions             : Region_Array;
      Scope_Total         : Scope_Count := 0;
      Scopes              : Scope_Array;
   end record;

   type Rejection is
     (Header_Rejected,        --  Read_Table refused it, or extent /= input
      Fixed_Part_Truncated,   --  shorter than the fixed part
      Host_Width_Invalid,     --  outside Host_Width
      Structure_Truncated,    --  structure header or length past the end
      Structure_Too_Short,    --  length below its type's fixed size
      Scope_Truncated,        --  scope header or length past its structure
      Scope_Too_Short,        --  scope length below the header
      Scope_Path_Invalid,     --  odd path, empty PCI path, device/function out of range
      Path_Too_Long,          --  more than Maximum_Path_Hops (CAPACITY)
      Unit_Base_Invalid,      --  zero, unaligned or wrapping register set
      Unit_Size_Unsupported,  --  size field above the ceiling
      Include_All_Invalid,    --  a second INCLUDE_PCI_ALL unit, or one naming PCI scopes
      Region_Range_Invalid,   --  unaligned or reversed RMRR
      Region_Without_Scope,   --  an RMRR naming no device
      Too_Many_Units,         --  CAPACITY: Maximum_Units
      Too_Many_Regions,       --  CAPACITY: Maximum_Regions
      Too_Many_Scopes,        --  CAPACITY: Maximum_Scopes
      No_Units);              --  a DMAR without any DRHD

   type Outcome is (Accepted, Rejected);

   function Scope_Well_Formed (F : Facts; S : Scope_Index) return Boolean is
     ((case F.Scopes (S).Owner is
         when Unit_Scope => F.Scopes (S).Owner_Index <= F.Unit_Total,
         when Region_Scope => F.Scopes (S).Owner_Index <= F.Region_Total)
      and then (if F.Scopes (S).Kind in PCI_Endpoint | PCI_Bridge
                then F.Scopes (S).Hops >= 1));

   --  Well-formed facts: counts and scope ranges line up, every unit's
   --  register set is aligned and nonwrapping, every region ordered and
   --  naming a device. Consistent adds that there is a unit.
   function Well_Formed (F : Facts) return Boolean is
     ((for all U in 1 .. F.Unit_Total =>
         F.Units (U).Register_Base /= 0
         and then F.Units (U).Register_Base mod Page_Bytes = 0
         and then F.Units (U).Register_Pages <= 2 ** Maximum_Register_Size_Field
         and then F.Units (U).Register_Base
                    <= Address_Value'Last
                       - Address_Value (F.Units (U).Register_Pages) * Page_Bytes + 1
         and then F.Units (U).First_Scope - 1 <= F.Scope_Total
         and then F.Units (U).Scopes <= F.Scope_Total - (F.Units (U).First_Scope - 1))
      and then (for all R in 1 .. F.Region_Total =>
                  F.Regions (R).Base mod Page_Bytes = 0
                  and then F.Regions (R).Base <= F.Regions (R).Limit
                  and then F.Regions (R).Limit mod Page_Bytes = Page_Bytes - 1
                  and then F.Regions (R).Scopes >= 1
                  and then F.Regions (R).First_Scope - 1 <= F.Scope_Total
                  and then F.Regions (R).Scopes
                             <= F.Scope_Total - (F.Regions (R).First_Scope - 1))
      and then (for all S in 1 .. F.Scope_Total => Scope_Well_Formed (F, S)));

   function Consistent (F : Facts) return Boolean is
     (F.Unit_Total >= 1 and then Well_Formed (F));

   Empty_Facts : constant Facts := (others => <>);

   --  F is written in place (the kernel decodes into its static copy; no
   --  stack copy of the facts). On rejection F means nothing.
   procedure Decode
     (Data   : Bytes;
      F      : out Facts;
      Status : out Outcome;
      Why    : out Rejection;
      Offset : out Natural)     --  first offending byte (0-based)
     with Post => (if Status = Accepted then Consistent (F));

   function Rejection_Name (Why : Rejection) return String is
     (case Why is
        when Header_Rejected       => "header rejected",
        when Fixed_Part_Truncated  => "fixed part truncated",
        when Host_Width_Invalid    => "host address width invalid",
        when Structure_Truncated   => "structure truncated",
        when Structure_Too_Short   => "structure too short",
        when Scope_Truncated       => "device scope truncated",
        when Scope_Too_Short       => "device scope too short",
        when Scope_Path_Invalid    => "device scope path invalid",
        when Path_Too_Long         => "CAPACITY device scope path",
        when Unit_Base_Invalid     => "unit register base invalid",
        when Unit_Size_Unsupported => "unit register size unsupported",
        when Include_All_Invalid   => "INCLUDE_PCI_ALL invalid",
        when Region_Range_Invalid  => "reserved region range invalid",
        when Region_Without_Scope  => "reserved region names no device",
        when Too_Many_Units        => "CAPACITY remapping units",
        when Too_Many_Regions      => "CAPACITY reserved regions",
        when Too_Many_Scopes       => "CAPACITY device scopes",
        when No_Units              => "no remapping unit");
end Firmware_Tables.Remapping;
