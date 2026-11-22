-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- What a VM or vCPU Call may do, decided from the request and the VM's
-- run ledger alone (VMM-001 slice 1, docs/hypervisor.md 4.5, 4.6, 5).
--
-- @description
-- Proved (gnatprove level 2, tests/vm-units):
--  * Add_RAM admits a range only when it is page-aligned, non-empty,
--    inside the 48-bit guest-physical space, overlaps no run the VM already
--    maps, leaves room in the run ledger, and keeps the VM's guest pages
--    under the named ceiling; an admitted range then joins the ledger with
--    every run still pairwise disjoint (Runs_Disjoint is kept);
--  * Injection_Valid admits exactly the events the architecture can
--    deliver (APM 15.20, SDM 26.2.1.3): an external interrupt with a vector
--    of 32 .. 255 and no error code; an NMI with no error code; a hardware
--    exception with a vector of 0 .. 31 other than 2 (NMI) and the
--    reserved 15 and 22 .. 27 and 31, carrying an error code exactly for
--    8, 10 .. 14, 17, 21, 29 and 30.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with VM_ABI;

package VM_Admission with
    SPARK_Mode => On,
    Pure
is
    Page_Bytes : constant := VM_ABI.Page_Bytes;
    subtype Run_Count is Natural range 0 .. VM_ABI.RAM_Run_Ceiling;
    subtype Run_Index is Positive range 1 .. VM_ABI.RAM_Run_Ceiling;
    subtype Page_Total is Unsigned_64 range 0 .. VM_ABI.Guest_Page_Ceiling;

    type Run is record
        First : Unsigned_64 := 0;  -- guest-physical, page-aligned
        Pages : Unsigned_64 := 0;
    end record;
    type Run_Table is array (Run_Index) of Run;

    function Run_Ok (R : Run) return Boolean is
      (R.Pages in 1 .. VM_ABI.Guest_Page_Ceiling
       and then R.First mod Page_Bytes = 0
       and then R.First < VM_ABI.Guest_Physical_Limit
       and then R.Pages <= (VM_ABI.Guest_Physical_Limit - R.First) / Page_Bytes);

    function Last_Byte (R : Run) return Unsigned_64 is
      (R.First + R.Pages * Page_Bytes - 1)
    with Pre => Run_Ok (R);

    function Overlap (A, B : Run) return Boolean is
      (A.First <= Last_Byte (B) and then B.First <= Last_Byte (A))
    with Pre => Run_Ok (A) and then Run_Ok (B);

    function Runs_Disjoint (T : Run_Table; Count : Run_Count) return Boolean is
      ((for all I in 1 .. Count => Run_Ok (T (I)))
       and then (for all I in 1 .. Count =>
                   (for all J in 1 .. Count =>
                      (if I /= J then not Overlap (T (I), T (J))))));

    type Range_Verdict is (Admitted, Bad_Range, Overlapping, Ledger_Full, Too_Large);

    function Range_Admission
      (T : Run_Table; Count : Run_Count; Mapped : Page_Total;
       First, Bytes : Unsigned_64) return Range_Verdict
    with Pre  => Runs_Disjoint (T, Count),
         Post => (if Range_Admission'Result = Admitted then
                    Count < VM_ABI.RAM_Run_Ceiling
                    and then Bytes mod Page_Bytes = 0
                    and then Run_Ok ((First, Bytes / Page_Bytes))
                    and then Mapped + Bytes / Page_Bytes <= VM_ABI.Guest_Page_Ceiling
                    and then (for all I in 1 .. Count =>
                                not Overlap (T (I), (First, Bytes / Page_Bytes))));

    -- Adding an admitted run keeps the ledger disjoint.
    procedure Lemma_Add_Keeps_Disjoint
      (T : Run_Table; Count : Run_Count; New_Run : Run) with
      Ghost,
      Pre  => Runs_Disjoint (T, Count) and then Count < VM_ABI.RAM_Run_Ceiling
              and then Run_Ok (New_Run)
              and then (for all I in 1 .. Count => not Overlap (T (I), New_Run)),
      Post => Runs_Disjoint ((T with delta Count + 1 => New_Run), Count + 1);

    -- Injection.
    function Error_Code_Vector (Vector : Unsigned_64) return Boolean is
      (Vector in 8 | 10 .. 14 | 17 | 21 | 29 | 30);

    function Exception_Vector (Vector : Unsigned_64) return Boolean is
      (Vector <= 31 and then Vector /= 2 and then Vector /= 15
       and then Vector not in 22 .. 27 and then Vector /= 31);

    function Injection_Valid
      (Kind : VM_ABI.Event_Kind; Vector : Unsigned_64; Error_Valid : Boolean)
       return Boolean is
      (case Kind is
         when VM_ABI.External => Vector in 32 .. 255 and then not Error_Valid,
         when VM_ABI.NMI      => not Error_Valid,
         when VM_ABI.Hardware_Exception =>
            Exception_Vector (Vector)
            and then Error_Valid = Error_Code_Vector (Vector));
end VM_Admission;
