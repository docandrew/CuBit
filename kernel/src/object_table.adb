with System.Address_To_Access_Conversions;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;

-- Adapter over the proved Id_Ledger and Quiescent_Reclamation rules: memory
-- mapping, publication and locking are not proved (see tests/object-table).
package body Object_Table with SPARK_Mode => Off is
    use type System.Address;
    use type Quiescent_Reclamation.Count;

    type Element_Array is array (0 .. Entries_Per_Page - 1) of aliased Element;
    package Conv is new System.Address_To_Access_Conversions (Element_Array);
    subtype Page_Ptr is Conv.Object_Pointer;
    use type Conv.Object_Pointer;

    Page_Bytes : constant Natural :=
      Natural ((Element_Array'Size + System.Storage_Unit - 1) / System.Storage_Unit);

    -- Published page per directory slot; null when the page is not live.
    Directory : array (Ledger.Page_Index) of Page_Ptr := (others => null)
      with Atomic_Components;

    L : Ledger.Ledger;

    -- A record is readable only once published: the ledger marks an ID used
    -- before its record is reset, so lock-free readers check this instead.
    -- Set after Reset, cleared before release.
    Published : array (Valid_Id) of Boolean := (others => False)
      with Atomic_Components;

    -- Quiescent-point counter per CPU, written only by that CPU.
    type Atomic_Counters is array (Quiescent_Reclamation.CPU_Index)
      of Quiescent_Reclamation.Count with Atomic_Components;
    Counts : Atomic_Counters := (others => 0);

    -- The record seen for unused IDs, and a reference copy to detect writes.
    Absent, Pristine : aliased Element;

    type Retired_Entry is record
        Page       : Page_Ptr := null;
        Retired_At : Quiescent_Reclamation.Counters := (others => 0);
    end record;
    Retired_Capacity : constant Positive := 2 * (Ledger.Page_Index'Last + 1);
    Retired : array (1 .. Retired_Capacity) of Retired_Entry;
    Retired_Count : Natural := 0;

    High : Id := 0 with Atomic;
    Live : Natural := 0;

    function Offset_Of (I : Valid_Id) return Natural is (I mod Entries_Per_Page);

    -- Full fence. x86 lets a later load complete before an earlier store is
    -- visible to other CPUs; the grace-period protocol needs both of these
    -- orders:
    -- * unlink store, then counter snapshot loads (Release);
    -- * counter store, then the CPU's next directory loads (Quiescent).
    -- Without them a reader can take a page pointer after its quiescent
    -- point while the snapshot records its old count, and the page is freed
    -- under it. This is required by the x86 memory model (a store-then-load
    -- pattern between two CPUs); the hosted stress test does not reach this
    -- window, so it is reasoned, not tested.
    procedure Full_Fence is
    begin
        System.Machine_Code.Asm ("mfence", Volatile => True, Clobber => "memory");
    end Full_Fence;

    function Present (I : Id) return Boolean is
      (I /= 0 and then Published (I) and then
       Directory (Ledger.Page_Of (I)) /= null);

    function Lookup (I : Id) return Element_Ref is
    begin
        if I /= 0 and then Published (I) then
            declare
                Page : constant Page_Ptr := Directory (Ledger.Page_Of (I));
            begin
                if Page /= null then
                    return (E => Page (Offset_Of (I))'Access);
                end if;
            end;
        end if;
        return (E => Absent'Access);
    end Lookup;

    function Generation_Of (I : Valid_Id) return Generation is
      (Ledger.Generation_Of (L, I));

    -- Map I's page if needed and reset I's record. Called with the lock held
    -- after the ledger marked I used. False if page memory is exhausted.
    function Materialize (I : Valid_Id) return Boolean is
        P : constant Ledger.Page_Index := Ledger.Page_Of (I);
        Addr : System.Address;
    begin
        if Directory (P) = null then
            Alloc_Page (Page_Bytes, Addr);
            if Addr = System.Null_Address then
                return False;
            end if;
            -- Only I's record is initialized; the others stay unreachable
            -- (Lookup returns Absent) until allocated and reset themselves.
            Reset (Conv.To_Pointer (Addr) (Offset_Of (I)));
            Directory (P) := Conv.To_Pointer (Addr);
            Live := Live + 1;
        else
            Reset (Directory (P) (Offset_Of (I)));
        end if;
        Published (I) := True;
        if I > High then
            High := I;
        end if;
        return True;
    end Materialize;

    procedure Allocate (I : out Id) is
        First, Last : Boolean;
    begin
        Lock;
        Ledger.Allocate (L, I, First);
        if I /= 0 and then not Materialize (I) then
            Ledger.Release (L, I, Last);
            I := 0;
        end if;
        Unlock;
    end Allocate;

    procedure Allocate_Specific (I : Valid_Id; Success : out Boolean) is
        First, Last : Boolean;
    begin
        Lock;
        Ledger.Allocate_Specific (L, I, Success, First);
        if Success and then not Materialize (I) then
            Ledger.Release (L, I, Last);
            Success := False;
        end if;
        Unlock;
    end Allocate_Specific;

    procedure Invalidate (I : Valid_Id; Saturated : out Boolean) is
    begin
        Lock;
        if Ledger.Used (L, I) then
            Ledger.Invalidate (L, I, Saturated);
        else
            Saturated := False;
        end if;
        Unlock;
    end Invalidate;

    procedure Release (I : Valid_Id; Advance : Boolean := True) is
        Last : Boolean;
        P : constant Ledger.Page_Index := Ledger.Page_Of (I);
    begin
        Lock;
        if Ledger.Used (L, I) then
            Published (I) := False;
            Ledger.Release (L, I, Last, Advance);
            -- Unlink the emptied page, then snapshot: readers that could have
            -- seen it are exactly those that have not yet passed a quiescent
            -- point. With the retired list full, keep the empty page mapped
            -- for reuse instead (fails safe: memory stays valid).
            if Last and then Directory (P) /= null and then
               Retired_Count < Retired_Capacity
            then
                Retired_Count := Retired_Count + 1;
                Retired (Retired_Count).Page := Directory (P);
                Directory (P) := null;
                Full_Fence;
                for C in Quiescent_Reclamation.CPU_Index loop
                    Retired (Retired_Count).Retired_At (C) := Counts (C);
                end loop;
                Live := Live - 1;
            end if;
        end if;
        Unlock;
    end Release;

    procedure Quiescent (CPU : Quiescent_Reclamation.CPU_Index) is
    begin
        Counts (CPU) := Quiescent_Reclamation.Next_Count (Counts (CPU));
        Full_Fence;
    end Quiescent;

    procedure Reclaim (Online : Quiescent_Reclamation.CPU_Set) is
        Now : Quiescent_Reclamation.Counters;
        K : Natural;
    begin
        Lock;
        for C in Quiescent_Reclamation.CPU_Index loop
            Now (C) := Counts (C);
        end loop;
        K := 1;
        while K <= Retired_Count loop
            if Quiescent_Reclamation.Grace_Elapsed
                 (Now, Retired (K).Retired_At, Online)
            then
                Free_Page (Page_Bytes, Conv.To_Address (Retired (K).Page));
                Retired (K) := Retired (Retired_Count);
                Retired_Count := Retired_Count - 1;
            else
                K := K + 1;
            end if;
        end loop;
        Unlock;
    end Reclaim;

    function High_Water return Id is (High);
    function Live_Pages return Natural is (Live);
    function Retired_Pages return Natural is (Retired_Count);

    function Absent_Is_Pristine return Boolean is
        Bytes : constant Storage_Offset :=
          Storage_Offset ((Element'Size + System.Storage_Unit - 1) / System.Storage_Unit);
        A : Storage_Array (1 .. Bytes) with Import, Address => Absent'Address;
        B : Storage_Array (1 .. Bytes) with Import, Address => Pristine'Address;
    begin
        return A = B;
    end Absent_Is_Pristine;

    procedure Initialize is
    begin
        Ledger.Initialize (L);
        Reset (Absent);
        Reset (Pristine);
    end Initialize;

end Object_Table;
