-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- The kernel log ring's writer (kernel_log.ads).
-------------------------------------------------------------------------------
with System.Machine_Code; use System.Machine_Code;
with Build;

package body Kernel_Log with
    SPARK_Mode => Off
is
    pragma Compile_Time_Error
      (Build.Kernel_Log_Records not in 1 .. KLP.Maximum_Records,
       "KERNEL_LOG_RECORDS must be in 1 .. Kernel_Log_Publication.Maximum_Records");

    Ring_Capacity : constant KLP.Record_Capacity := Build.Kernel_Log_Records;

    -- Never initialized by the program: it is bss, which boot zeroes before
    -- the first print, so it records from the first line on. (A default
    -- initialization would be elaboration code copying a 1 MiB aggregate
    -- over records already written.) All zero is the empty ring.
    Area : KLP.Ring_Area with Volatile, Alignment => KLP.Page_Bytes;
    pragma Suppress_Initialization (Area);

    Header_Version : Unsigned_64 with
        Import, Atomic, Address => Area.Head.Version'Address;
    Header_Total : Unsigned_64 with
        Import, Atomic, Address => Area.Head.Total'Address;

    -- The writing CPU's kernel GS base; Early_Owner for a CPU without one
    -- (the boot CPU before Enable_Locking, a CPU still being brought up);
    -- No_Owner when free.
    No_Owner    : constant Unsigned_64 := 0;
    Early_Owner : constant Unsigned_64 := 1;   -- never a GS base (aligned)
    Writer_Owner : aliased Unsigned_64 := No_Owner with Volatile;
    Locking : Boolean := False with Atomic;

    function Capacity return KLP.Record_Capacity is (Ring_Capacity);

    -- The kernel's millisecond counter (Use_Clock); records before it say 0.
    Clock : System.Address := System.Null_Address with Volatile;

    procedure Use_Clock (Milliseconds : System.Address) is
    begin
        Clock := Milliseconds;
    end Use_Clock;

    function now return Unsigned_64 is
        use type System.Address;
        Where : constant System.Address := Clock;
    begin
        if Where = System.Null_Address then
            return 0;
        end if;
        declare
            Ms : Unsigned_64 with Import, Volatile, Address => Where;
        begin
            return Ms;
        end;
    end now;

    function Area_Address return System.Address is (Area'Address);

    procedure Enable_Locking is
    begin
        Locking := True;
    end Enable_Locking;

    function compareAndSwap (Ptr : System.Address; Expected, Desired : Unsigned_64)
      return Unsigned_64
      with Import, Convention => Intrinsic,
           External_Name => "__sync_val_compare_and_swap_8";

    function kernelGSBase return Unsigned_64 is
        Value : Unsigned_64;
    begin
        Asm ("rdgsbase %0", Outputs => Unsigned_64'Asm_Output ("=r", Value),
             Volatile => True);
        return Value;
    end kernelGSBase;

    function saveFlagsAndDisable return Unsigned_64 is
        Flags : Unsigned_64;
    begin
        Asm ("pushfq; popq %0; cli",
             Outputs => Unsigned_64'Asm_Output ("=r", Flags),
             Volatile => True, Clobber => "memory");
        return Flags;
    end saveFlagsAndDisable;

    procedure restoreFlags (Flags : Unsigned_64) is
    begin
        Asm ("pushq %0; popfq",
             Inputs => Unsigned_64'Asm_Input ("r", Flags),
             Volatile => True, Clobber => "memory,cc");
    end restoreFlags;

    procedure countReentered is
        Count : Unsigned_64 with
            Import, Volatile, Address => Area.Head.Reentered'Address;
    begin
        if Count < Unsigned_64'Last then
            Count := Count + 1;
        end if;
    end countReentered;

    -- The writer's step, holding the lock: number the record, write it
    -- with Writing in its Sequence, then its number, then Total.
    procedure store (Item : KLP.Log_Record) is
        Total : constant Unsigned_64 := Header_Total;
    begin
        if Header_Version /= KLP.Layout_Version then
            Area.Head.Capacity := Ring_Capacity;
            Header_Version := KLP.Layout_Version;
        end if;
        if not KLP.Can_Number (Total) then
            countReentered;
            return;
        end if;
        declare
            Number : constant KLP.Record_Number := KLP.Next_Number (Total);
            Where  : constant KLP.Slot := KLP.Slot_Of (Number, Ring_Capacity);
            Sequence : Unsigned_64 with
                Import, Atomic, Address => Area.Records (Where).Sequence'Address;
        begin
            Sequence := KLP.Writing;
            Area.Records (Where) := Item;
            Sequence := Number;
            Header_Total := Number;
        end;
    end store;

    -- Take the writer lock; Taken False when this CPU is already writing
    -- (the record is then given up and counted).
    procedure lock (Flags : out Unsigned_64; Taken : out Boolean) is
        Me : Unsigned_64 := Early_Owner;
    begin
        Flags := saveFlagsAndDisable;
        Taken := False;
        if Locking then
            Me := kernelGSBase;
            if Me = No_Owner then
                Me := Early_Owner;
            end if;
        end if;
        if Writer_Owner = Me then
            restoreFlags (Flags);
            return;
        end if;
        while compareAndSwap (Writer_Owner'Address, No_Owner, Me) /= No_Owner loop
            Asm ("pause", Volatile => True);
        end loop;
        Taken := True;
    end lock;

    procedure unlock (Flags : Unsigned_64) is
    begin
        Writer_Owner := No_Owner;
        restoreFlags (Flags);
    end unlock;

    procedure append (Item : KLP.Log_Record) is
        Flags : Unsigned_64;
        Taken : Boolean;
    begin
        lock (Flags, Taken);
        if not Taken then
            countReentered;
            return;
        end if;
        -- The time under the lock: record order is time order.
        declare
            Timed : KLP.Log_Record := Item;
        begin
            Timed.Time_Ms := now;
            store (Timed);
        end;
        unlock (Flags);
    end append;

    procedure Append_Kernel_Line (Text : String; Truncated : Boolean) is
    begin
        append (KLP.Make_Line (0, Text, Truncated));
    end Append_Kernel_Line;

    procedure Append_Stop
      (Source : Unsigned_64;
       Name   : KLP.Name_Text;
       Fault  : Fault_Reports.Report) is
    begin
        append (KLP.Make_Stop (0, Source, Name, Fault, KLP.Everyone));
        if Fault_Reports."/=" (Fault_Reports.Kind_Of (Fault), Fault_Reports.Stopped_By_Process) then
            append (KLP.Make_Stop (0, Source, Name, Fault, KLP.Diagnostics));
        end if;
    end Append_Stop;
end Kernel_Log;
