-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Address-space tags on x86 (PCIDs): the trusted CR3/TLB adapter
-------------------------------------------------------------------------------
with System.Machine_Code; use System.Machine_Code;
with Build;
with cpuid;
with PerCPUData;
with TextIO; use TextIO;
with x86;

package body TLB_Context is
    use type Tags.Life;

    -- CR4.PCIDE (bit 17).
    CR4_PCIDE : constant Unsigned_64 := 2 ** 17;
    -- INVPCID type 2: every tag's translations, global ones included.
    INVPCID_All_Contexts : constant Unsigned_64 := 2;

    type Life_Table is array (Slot_Tag) of Tags.Life with Atomic_Components;
    -- Row CPU: the life this CPU last loaded each tag for. Only that CPU
    -- reads or writes its row.
    type Loaded_Table is array (CPU_Index, Slot_Tag) of Tags.Life;
    type CPU_Flags is array (CPU_Index) of Boolean with Atomic_Components;

    -- Decided once on the boot CPU, before any other CPU starts.
    Tagging : Boolean := False with Atomic;
    Lives : Life_Table := (others => Tags.No_Life);
    Loaded : Loaded_Table := (others => (others => Tags.No_Life));
    -- CPUs that have set CR4.PCIDE. Before that a CPU loads untagged CR3
    -- values only (bit 63 and the tag bits would be invalid there).
    Tagged_CPU : CPU_Flags := (others => False);

    Tag_Failure : exception;

    -- The INVPCID descriptor: tag in bits 11:0, then a linear address. Type
    -- 2 ignores both.
    type Descriptor is record
        PCID   : Unsigned_64;
        Linear : Unsigned_64;
    end record with Size => 128, Alignment => 16;

    procedure Invalidate_All_Contexts is
        Every : constant Descriptor := (PCID => 0, Linear => 0);
    begin
        Asm ("invpcid %0, %1",
             Inputs => (Descriptor'Asm_Input ("m", Every),
                        Unsigned_64'Asm_Input ("r", INVPCID_All_Contexts)),
             Volatile => True, Clobber => "memory");
    end Invalidate_All_Contexts;

    procedure Write_CR3 (Value : Unsigned_64) is
    begin
        Asm ("movq %0, %%cr3",
             Inputs => Unsigned_64'Asm_Input ("r", Value),
             Volatile => True, Clobber => "memory");
    end Write_CR3;

    function Current return Unsigned_64 is
        Value : Unsigned_64;
    begin
        Asm ("movq %%cr3, %0",
             Outputs => Unsigned_64'Asm_Output ("=r", Value),
             Volatile => True);
        return Value;
    end Current;

    function This_CPU return CPU_Index is (CPU_Index (PerCPUData.getCPUNumber));

    function Enabled return Boolean is (Tagging);

    procedure Setup_This_CPU (Boot_CPU : Boolean) is
    begin
        if Boot_CPU then
            Tagging := Build.Tagged_TLB and then cpuid.leaf1ecx.hasPCID
                and then cpuid.leaf7ebx.hasINVPCID;
            println (if Tagging then "Address-space tags (PCID) on"
                     else "Address-space tags (PCID) off");
        end if;
        if not Tagging then
            return;
        end if;
        -- CR4.PCIDE may be set only while CR3's tag bits are 0 (Intel SDM
        -- Vol. 3A, 4.10.1): the kernel's tables are loaded untagged here.
        if Tags.Tag_Of (Current) /= Tags.Kernel_Tag then
            raise Tag_Failure with "PCIDE needs an untagged CR3";
        end if;
        x86.setCR4 (x86.getCR4 or CR4_PCIDE);
        -- Translations cached so far carry tag 0 and are the kernel's; drop
        -- them anyway so every CPU starts from nothing.
        Invalidate_All_Contexts;
        Tagged_CPU (This_CPU) := True;
    end Setup_This_CPU;

    procedure Begin_Life (T : Slot_Tag) is
        Was : constant Tags.Life := Lives (T);
    begin
        if Was = Tags.Life'Last then
            raise Tag_Failure with "address-space tag out of lives";
        end if;
        Lives (T) := Tags.Next_Life (Was);
    end Begin_Life;

    procedure Load_Raw (CR3 : Unsigned_64) is
    begin
        Write_CR3 (CR3);
    end Load_Raw;

    procedure Load_Process (Root : Unsigned_64; T : Slot_Tag) is
        CPU : constant CPU_Index := This_CPU;
    begin
        if not Tagged_CPU (CPU) then
            Load_Raw (Root);
            return;
        end if;
        if not Tags.Valid_Root (Root) then
            raise Tag_Failure with "page-table root outside CR3's address bits";
        end if;
        declare
            Life : constant Tags.Life := Lives (T);
            Keep : constant Boolean := Tags.Keep (Loaded (CPU, T), Life);
        begin
            if Life = Tags.No_Life then
                raise Tag_Failure with "address space loaded before its life began";
            end if;
            Loaded (CPU, T) := Life;
            -- Already loaded with its translations kept: nothing to change.
            if Keep and then Current = Tags.CR3_Value (Root, T, False) then
                return;
            end if;
            Write_CR3 (Tags.CR3_Value (Root, T, Keep));
        end;
    end Load_Process;

    procedure Load_Kernel (Root : Unsigned_64) is
    begin
        if not Tagged_CPU (This_CPU) then
            -- Untagged, every load drops translations: skip a needless one.
            if Current /= Root then
                Load_Raw (Root);
            end if;
            return;
        end if;
        if not Tags.Valid_Root (Root) then
            raise Tag_Failure with "kernel root outside CR3's address bits";
        end if;
        -- One life, never replaced: its translations are always its own.
        -- Kernel mapping changes flush every tag (Flush_All).
        if Current /= Tags.CR3_Value (Root, Tags.Kernel_Tag, False) then
            Write_CR3 (Tags.CR3_Value (Root, Tags.Kernel_Tag, True));
        end if;
    end Load_Kernel;

    procedure Flush_All is
    begin
        if Tagging then
            -- INVPCID works with CR4.PCIDE still clear (a starting CPU).
            Invalidate_All_Contexts;
        else
            -- Untagged: reloading CR3 drops every non-global translation,
            -- and CR4.PGE is clear, so none is global.
            Write_CR3 (Current);
        end if;
    end Flush_All;
end TLB_Context;
