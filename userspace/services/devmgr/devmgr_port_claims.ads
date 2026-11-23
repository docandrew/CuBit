------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Who holds which I/O ports: devmgr's record of the port ranges it has
--  handed out (or the kernel keeps), so the ACPI windows it grants for
--  acpi.svc and its drivers' ports never overlap
--  (CuBit.ACPI_Window_Exchange, coordination/acpi-region-boundary.md).
--
--  @description
--  The kernel enforces each I/O-port capability's own range but keeps no
--  record of exclusivity; this ledger is that record. It is the proved
--  core over an array the caller provides and grows (Devmgr_Claim_Ledgers
--  keeps it on the heap, LIM-001b): no fixed count of claims.
--
--  * An ACPI claim is refused if any port of it is held by devmgr or a
--    driver, or if it partly overlaps an earlier ACPI claim; the same
--    ACPI range again is granted (a restarted backend). ACPI windows stay
--    claimed for the rest of the boot.
--  * A driver or devmgr claim is refused if any port of it is in an ACPI
--    window; ranges of drivers and devmgr may repeat or overlap each other
--    (devmgr's own concern, as before). A driver's claims carry its process
--    identity and are released when it exits (Release_Owner); the same
--    range again by the same driver is not recorded twice.
--  * A claim that needs a new entry when the array is full is neither
--    granted nor refused: Needs_Room, and the caller grows the array and
--    asks again.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Devmgr_Port_Claims with Pure, SPARK_Mode is

   Port_Space : constant := 65_536;

   type Holder is (Kernel, Device_Manager, Driver, ACPI, Reserved);

   --  Whose driver claim it is: the driver's full process identity word
   --  (KERN-003); No_Owner for the kernel's, devmgr's and ACPI's claims.
   subtype Owner_Word is Unsigned_64;
   No_Owner : constant Owner_Word := 0;

   type Claim is record
      First  : Unsigned_32 := 0;   --  first port
      Ends   : Unsigned_32 := 0;   --  one past the last port
      By     : Holder := Kernel;
      Owner  : Owner_Word := No_Owner;
   end record;

   type Claim_Array is array (Positive range <>) of Claim;

   function Valid_Range (First, Count : Unsigned_64) return Boolean is
     (Count >= 1 and then First < Port_Space and then Count <= Port_Space - First);

   function Overlaps (Item : Claim; First, Ends : Unsigned_32) return Boolean is
     (First < Item.Ends and then Item.First < Ends);

   --  A claim this driver's exit gives back.
   function Owned_By (Item : Claim; Owner : Owner_Word) return Boolean is
     (Item.By = Driver and then Item.Owner = Owner);

   --  The ranges the kernel and devmgr itself use before any driver runs:
   --  the interrupt controllers, the interval timer, the CMOS clock, the
   --  PCI configuration ports and the kernel's serial console.
   Seed_Claims : constant := 6;
   procedure Initialize (Claims : in out Claim_Array; Count : out Natural)
   with Pre  => Claims'First = 1 and then Claims'Length >= Seed_Claims,
        Post => Count = Seed_Claims
                and then (for all K in 1 .. Count => Claims (K).By /= Driver);

   type Take_Result is (Granted, Refused, Needs_Room);

   procedure Take
     (Claims : in out Claim_Array; Count : in out Natural;
      First, Ports : Unsigned_64; By : Holder; Owner : Owner_Word;
      Result : out Take_Result)
   with Pre  => By not in Kernel | Reserved and then Claims'First = 1 and then Count <= Claims'Last
                and then (Owner = No_Owner) = (By /= Driver),
        Post => Count <= Claims'Last
                and then Count >= Count'Old
                and then (for all K in 1 .. Count'Old => Claims (K) = Claims'Old (K))
                and then (if Result /= Granted then Count = Count'Old)
                and then (if Result = Needs_Room then Count'Old = Claims'Last)
                and then
                (if Result = Granted then
                   Valid_Range (First, Ports) and then
                   (for all K in 1 .. Count'Old =>
                     (if Claims'Old (K).By = Reserved then
                       not Overlaps (Claims'Old (K), Unsigned_32 (First), Unsigned_32 (First + Ports)))))
                and then
                --  An ACPI claim never overlaps another holder's range, and a
                --  granted claim of another holder never overlaps an ACPI one.
                (if Result = Granted then
                   Valid_Range (First, Ports)
                   and then (for all K in 1 .. Count'Old =>
                               (if (By = ACPI) /= (Claims'Old (K).By = ACPI)
                                then not Overlaps (Claims'Old (K), Unsigned_32 (First),
                                                   Unsigned_32 (First + Ports))))
                   and then (for some K in 1 .. Count =>
                               Claims (K) = (First => Unsigned_32 (First),
                                             Ends  => Unsigned_32 (First + Ports),
                                             By    => By, Owner => Owner)));

   type CMOS_Handover_Result is
     (Transferred, Already_Held, Handover_Refused, Handover_Needs_Room);
   -- Reserve70..73 always. A validated Intel alias profile additionally
   -- reserves74..77. Only70/71 (or70..73 for an enabled extension) is
   -- grantable; a Reserved claim is denied even to direct ACPI requests.
   procedure Handover_CMOS
     (Claims : in out Claim_Array; Count : in out Natural;
      Extended, Intel_Aliases : Boolean; Result : out CMOS_Handover_Result)
     with Pre => Claims'First = 1 and then Count <= Claims'Last,
       Post => Count <= Claims'Last
         and then (if Result in Handover_Refused | Handover_Needs_Room then
           Count = Count'Old and then Claims = Claims'Old)
         and then (if Result = Already_Held then
           Count = Count'Old and then Claims = Claims'Old)
         and then (if Result in Transferred | Already_Held then
           (for some K in 1 .. Count =>
             Claims (K) = (First => 16#70#, Ends => (if Extended then 16#74# else 16#72#),
                            By => ACPI, Owner => No_Owner))
           and then (if not Extended or else Intel_Aliases then
             (for some K in 1 .. Count =>
               Claims (K) = (First => (if Extended then 16#74# else 16#72#),
                 Ends => (if Intel_Aliases then 16#78# else 16#74#),
                 By => Reserved, Owner => No_Owner)))
           and then (for all K in 1 .. Count =>
             (if Overlaps (Claims (K), 16#70#, (if Intel_Aliases then 16#78# else 16#74#)) then
                Claims (K).Owner = No_Owner
                and then ((Claims (K).By = ACPI and then Claims (K).First = 16#70#
                  and then Claims (K).Ends = (if Extended then 16#74# else 16#72#))
                or else (Claims (K).By = Reserved
                  and then Claims (K).First = (if Extended then 16#74# else 16#72#)
                  and then Claims (K).Ends = (if Intel_Aliases then 16#78# else 16#74#))))))
         and then (if Result = Transferred then
           Count = Count'Old + (if not Extended or else Intel_Aliases then 1 else 0)
           and then (for some K in 1 .. Count'Old =>
             Claims'Old (K) = (First => 16#70#, Ends => 16#72#,
                                By => Device_Manager, Owner => No_Owner)
             and then Claims (K) = (First => 16#70#, Ends => (if Extended then 16#74# else 16#72#),
                                     By => ACPI, Owner => No_Owner)
             and then (for all J in 1 .. Count'Old =>
               (if J /= K then Claims (J) = Claims'Old (J)))));

   --  The driver Owner exited: its claims are given back. Every other claim
   --  (the kernel's, devmgr's, every ACPI window, other drivers') stays.
   procedure Release_Owner
     (Claims : in out Claim_Array; Count : in out Natural; Owner : Owner_Word)
   with Pre  => Owner /= No_Owner and then Claims'First = 1 and then Count <= Claims'Last,
        Post => Count <= Count'Old
                and then (for all K in 1 .. Count => not Owned_By (Claims (K), Owner))
                and then (for all K in 1 .. Count'Old =>
                            (if not Owned_By (Claims'Old (K), Owner)
                             then (for some J in 1 .. Count => Claims (J) = Claims'Old (K))));

end Devmgr_Port_Claims;
