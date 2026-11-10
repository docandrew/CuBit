pragma Ada_2022;
with System;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;

with acpi;
with BuddyAllocator;
with CPU_Topology.Boot;
with Device_Memory_Diagnostics;
with Device_Memory_Publication;
with Firmware_Tables.Remapping;
with InterruptNumbers;
with IO_Page_Tables; use IO_Page_Tables;
with IO_Page_Walk;
with IOMMU_Domains;
with Mem_mgr;
with Spinlocks;
with TextIO; use TextIO;
with Time;
with Util;
with Virtmem;
with x86;

package body DMA_Remapping with SPARK_Mode => Off is

    package R renames Firmware_Tables.Remapping;
    package ID renames IOMMU_Domains;
    package Pub renames Device_Memory_Publication;
    use type R.Scope_Kind;
    use type acpi.DMAR_Status;
    use type ID.Domain_Status;
    use type ID.Bind_Status;
    use type ID.Map_Status;
    use type ID.Admit_Status;
    use type ID.Release_Status;

    ---------------------------------------------------------------------------
    -- VT-d registers (specification chapter 11) and their bits
    ---------------------------------------------------------------------------
    VER_Register     : constant := 16#00#;
    CAP_Register     : constant := 16#08#;
    ECAP_Register    : constant := 16#10#;
    GCMD_Register    : constant := 16#18#;
    GSTS_Register    : constant := 16#1C#;
    RTADDR_Register  : constant := 16#20#;
    CCMD_Register    : constant := 16#28#;
    FSTS_Register    : constant := 16#34#;
    FECTL_Register   : constant := 16#38#;
    FEDATA_Register  : constant := 16#3C#;
    FEADDR_Register  : constant := 16#40#;
    FEUADDR_Register : constant := 16#44#;
    PMEN_Register    : constant := 16#64#;

    -- Global command / status (GCMD, GSTS): same bit positions.
    Translation_Enable      : constant Unsigned_32 := 2 ** 31;   -- TE / TES
    Set_Root_Table_Pointer  : constant Unsigned_32 := 2 ** 30;   -- SRTP / RTPS
    Write_Buffer_Flush      : constant Unsigned_32 := 2 ** 27;   -- WBF / WBFS
    Queued_Invalidation     : constant Unsigned_32 := 2 ** 26;   -- QIE / QIES
    Interrupt_Remapping     : constant Unsigned_32 := 2 ** 25;   -- IRE / IRES
    -- The status bits that are persistent commands (TES, EAFLS, QIES,
    -- IRES, CFIS and the reserved low bits); a GCMD write repeats them and
    -- drops the one-shot ones (VT-d 11.4.4).
    Persistent_Commands     : constant Unsigned_32 := 16#96FF_FFFF#;

    -- CAP fields.
    CAP_Domains_Mask        : constant Unsigned_64 := 16#7#;        -- ND 2:0
    CAP_Write_Buffer_Flush  : constant Unsigned_64 := 2 ** 4;       -- RWBF
    CAP_Caching_Mode        : constant Unsigned_64 := 2 ** 7;       -- CM
    CAP_SAGAW_Shift         : constant := 8;
    CAP_SAGAW_Mask          : constant Unsigned_64 := 16#1F#;
    CAP_MGAW_Shift          : constant := 16;
    CAP_MGAW_Mask           : constant Unsigned_64 := 16#3F#;
    CAP_FRO_Shift           : constant := 24;
    CAP_FRO_Mask            : constant Unsigned_64 := 16#3FF#;
    CAP_NFR_Shift           : constant := 40;
    CAP_NFR_Mask            : constant Unsigned_64 := 16#FF#;
    -- ECAP fields.
    ECAP_Coherent           : constant Unsigned_64 := 2 ** 0;       -- C
    ECAP_Pass_Through       : constant Unsigned_64 := 2 ** 6;       -- PT
    ECAP_IRO_Shift          : constant := 8;
    ECAP_IRO_Mask           : constant Unsigned_64 := 16#3FF#;
    -- Register offsets in CAP/ECAP count 16-byte units.
    Offset_Unit             : constant := 16;

    -- Context command (CCMD).
    CCMD_Invalidate         : constant Unsigned_64 := 2 ** 63;      -- ICC
    CCMD_Global             : constant Unsigned_64 := 2 ** 61;      -- CIRG 01

    -- IOTLB invalidate register (at IRO * 16 + 8).
    IOTLB_Register_Offset   : constant := 8;
    IOTLB_Invalidate        : constant Unsigned_64 := 2 ** 63;      -- IVT
    IOTLB_Global            : constant Unsigned_64 := 2 ** 60;      -- IIRG 01
    IOTLB_Domain            : constant Unsigned_64 := 2 ** 61;      -- IIRG 10
    IOTLB_Drain_Reads       : constant Unsigned_64 := 2 ** 49;      -- DR
    IOTLB_Drain_Writes      : constant Unsigned_64 := 2 ** 48;      -- DW
    IOTLB_Domain_Shift      : constant := 32;                       -- DID 47:32

    -- Fault status, control and records.
    FSTS_Overflow           : constant Unsigned_32 := 2 ** 0;       -- PFO
    FSTS_Pending            : constant Unsigned_32 := 2 ** 1;       -- PPF
    FSTS_Index_Shift        : constant := 8;                        -- FRI 15:8
    FSTS_Index_Mask         : constant Unsigned_32 := 16#FF#;
    FSTS_Clear              : constant Unsigned_32 := 16#7F#;       -- RW1C bits 6:0
    FECTL_Interrupt_Mask    : constant Unsigned_32 := 2 ** 31;      -- IM
    Fault_Record_Bytes      : constant := 16;
    Fault_High_Word         : constant := 8;
    Fault_Flag_Dword        : constant := 12;   -- the dword holding F (bit 31)
    Fault_Flag              : constant Unsigned_64 := 2 ** 63;      -- F
    Fault_Clear             : constant Unsigned_32 := 2 ** 31;
    Fault_Type_Read         : constant Unsigned_64 := 2 ** 62;      -- T: 1 read
    Fault_Reason_Shift      : constant := 32;                       -- FR 39:32
    Fault_Reason_Mask       : constant Unsigned_64 := 16#FF#;
    Fault_Source_Mask       : constant Unsigned_64 := 16#FFFF#;     -- SID 15:0
    Fault_Address_Mask      : constant Unsigned_64 := not 16#FFF#;  -- FI 63:12

    -- Protected memory enable.
    PMEN_Enable             : constant Unsigned_32 := 2 ** 31;      -- EPM
    PMEN_Status             : constant Unsigned_32 := 2 ** 0;       -- PRS

    -- The fault event message: the local APIC interrupt window.
    Interrupt_Window_Base   : constant Unsigned_32 := 16#FEE0_0000#;
    APIC_Destination_Shift  : constant := 12;

    -- PCI configuration mechanism #1, for resolving DMAR scope paths.
    PCI_Config_Address        : constant x86.IOPort := 16#0CF8#;
    PCI_Config_Data           : constant x86.IOPort := 16#0CFC#;
    PCI_Config_Enable         : constant Unsigned_32 := 2 ** 31;
    PCI_Config_Bus_Shift      : constant := 16;
    PCI_Config_Device_Shift   : constant := 11;
    PCI_Config_Function_Shift : constant := 8;
    PCI_Config_Dword_Mask     : constant Unsigned_32 := 16#FC#;
    Config_Vendor           : constant := 16#00#;
    Config_Base_Class       : constant := 16#0B#;
    Config_Secondary_Bus    : constant := 16#19#;
    Config_Subordinate_Bus  : constant := 16#1A#;
    Display_Class           : constant Unsigned_8 := 16#03#;
    Intel_Vendor_Low        : constant Unsigned_8 := 16#86#;   -- 8086h
    Intel_Vendor_High       : constant Unsigned_8 := 16#80#;
    No_Device               : constant Unsigned_8 := 16#FF#;
    -- The integrated GPU's fixed location on Intel platforms.
    Graphics_Requester      : constant Requester_ID := Requester (0, 2, 0);
    -- The domain ID of the GPU's pass-through entry: above every ledger
    -- domain, so no translated domain shares its IOTLB tag.
    Pass_Through_Domain     : constant Domain_ID := IOMMU_Domains.Maximum_Domains + 1;

    -- Every wait on a status bit has a deadline. One second matches the
    -- order of Linux's DMAR_OPERATION_TIMEOUT; hardware answers in
    -- microseconds. The poll limit bounds the wait when the clock stalls.
    Command_Deadline_US      : constant := 1_000_000;
    Invalidation_Deadline_US : constant := 1_000_000;
    Wait_Poll_Limit          : constant := 100_000_000;

    -- At most this many fault records are printed per second; the rest are
    -- still counted (Suppressed) and reported when the second ends.
    Fault_Reports_Per_Second : constant := 32;
    Milliseconds_Per_Second  : constant := 1000;

    ---------------------------------------------------------------------------
    -- State
    ---------------------------------------------------------------------------
    subtype Unit_Index is Positive range 1 .. ID.Maximum_Units;

    type Unit_State is
      (Unused,                 -- not in the DMAR
       Skipped,                -- not driven (segment, scope, size)
       Graphics_Untranslated,  -- only display devices: left for slice 5
       Ready,                  -- prepared, translation not on yet
       Translating,
       Failed);                -- a step did not complete

    type Context_Table_Array is array (Bus_Number) of Unsigned_64;

    type Unit_Record is record
        State          : Unit_State := Unused;
        Physical       : Unsigned_64 := 0;
        Registers      : Integer_Address := 0;     -- linear-map address
        Register_Bytes : Unsigned_64 := 0;
        CAP, ECAP      : Unsigned_64 := 0;
        Width          : Address_Width := Width_39;
        Coherent       : Boolean := False;
        Caching_Mode   : Boolean := False;
        Flush_Writes   : Boolean := False;          -- CAP.RWBF
        Pass_Through   : Boolean := False;          -- ECAP.PT
        Domain_Limit   : Unsigned_64 := 0;          -- domain IDs below this
        Fault_Offset   : Unsigned_64 := 0;
        Fault_Records  : Unsigned_64 := 0;
        IOTLB_Offset   : Unsigned_64 := 0;
        Root_Table     : Unsigned_64 := 0;
        Context_Tables : Context_Table_Array := [others => 0];
        Graphics_Pass  : Boolean := False;          -- iGPU pass-through entry
    end record;

    Units : array (Unit_Index) of Unit_Record;
    Unit_Total : Natural := 0;

    -- Resolved DMAR scopes: which requester (or bus range) a unit names.
    type Scope_Entry is record
        Used        : Boolean := False;
        Unit        : Unit_Index := 1;
        Bridge      : Boolean := False;
        Who         : Requester_ID := 0;
        First_Bus   : Bus_Number := 0;    -- a bridge's secondary .. subordinate
        Last_Bus    : Bus_Number := 0;
    end record;
    Scopes : array (R.Scope_Index) of Scope_Entry;

    Ledger : ID.Ledger;
    Domain_Roots : array (ID.Domain_Index) of Unsigned_64 := [others => 0];
    Domain_Programmed : array (ID.Domain_Index) of Boolean := [others => False];

    Lock       : Spinlocks.Spinlock;
    Fault_Lock : Spinlocks.Spinlock;
    Lock_Name       : aliased constant String := "IOMMU";
    Fault_Lock_Name : aliased constant String := "IOMMU faults";

    Setup_Done   : Boolean := False;
    Any_Translating : Boolean := False with Volatile;

    Fault_Window      : Unsigned_64 := 0;
    Faults_In_Window  : Natural := 0;
    Suppressed        : Unsigned_64 := 0;

    -- The walker's table writes flush the cache line when the unit's page
    -- walks are not coherent. Set under Lock before each walk.
    Walk_Coherent : Boolean := True;

    ---------------------------------------------------------------------------
    -- Low-level helpers
    ---------------------------------------------------------------------------
    procedure Flush_Line (Addr : System.Address) is
    begin
        System.Machine_Code.Asm
          ("clflush (%0)", Inputs => System.Address'Asm_Input ("r", Addr),
           Clobber => "memory", Volatile => True);
    end Flush_Line;

    procedure Fence is
    begin
        System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
    end Fence;

    procedure Pause is
    begin
        System.Machine_Code.Asm ("pause", Volatile => True);
    end Pause;

    Cache_Line_Bytes : constant := 64;

    procedure Make_Visible (Addr : System.Address; Bytes : Storage_Count; Coherent : Boolean) is
    begin
        if not Coherent then
            for Offset in 0 .. (Bytes - 1) / Cache_Line_Bytes loop
                Flush_Line (Addr + Offset * Cache_Line_Bytes);
            end loop;
        end if;
        Fence;
    end Make_Visible;

    function Linear (Physical : Unsigned_64) return System.Address is
      (Virtmem.P2Va (Virtmem.PhysAddress (Physical)));

    function Read_32 (U : Unit_Index; Offset : Unsigned_64) return Unsigned_32 is
        Value : Unsigned_32 with Import, Volatile,
          Address => To_Address (Units (U).Registers + Integer_Address (Offset));
    begin
        return Value;
    end Read_32;

    procedure Write_32 (U : Unit_Index; Offset : Unsigned_64; Value : Unsigned_32) is
        Target : Unsigned_32 with Import, Volatile,
          Address => To_Address (Units (U).Registers + Integer_Address (Offset));
    begin
        Target := Value;
    end Write_32;

    function Read_64 (U : Unit_Index; Offset : Unsigned_64) return Unsigned_64 is
        Value : Unsigned_64 with Import, Volatile,
          Address => To_Address (Units (U).Registers + Integer_Address (Offset));
    begin
        return Value;
    end Read_64;

    procedure Write_64 (U : Unit_Index; Offset : Unsigned_64; Value : Unsigned_64) is
        Target : Unsigned_64 with Import, Volatile,
          Address => To_Address (Units (U).Registers + Integer_Address (Offset));
    begin
        Target := Value;
    end Write_64;

    -- Wait until (register and Mask) = Expect, within Deadline_US.
    procedure Wait_32 (U : Unit_Index; Offset : Unsigned_64; Mask, Expect : Unsigned_32;
                       Deadline_US : Unsigned_64; OK : out Boolean) is
        Start, Now : Unsigned_64;
        Clock : Boolean;
    begin
        Time.Read_Monotonic (Start, Clock);
        for Poll in 1 .. Wait_Poll_Limit loop
            if (Read_32 (U, Offset) and Mask) = Expect then
                OK := True;
                return;
            end if;
            if Clock then
                Time.Read_Monotonic (Now, Clock);
                exit when Clock and then Now >= Start and then Now - Start > Deadline_US;
            end if;
            Pause;
        end loop;
        OK := (Read_32 (U, Offset) and Mask) = Expect;
    end Wait_32;

    procedure Wait_64 (U : Unit_Index; Offset : Unsigned_64; Mask, Expect : Unsigned_64;
                       Deadline_US : Unsigned_64; OK : out Boolean) is
        Start, Now : Unsigned_64;
        Clock : Boolean;
    begin
        Time.Read_Monotonic (Start, Clock);
        for Poll in 1 .. Wait_Poll_Limit loop
            if (Read_64 (U, Offset) and Mask) = Expect then
                OK := True;
                return;
            end if;
            if Clock then
                Time.Read_Monotonic (Now, Clock);
                exit when Clock and then Now >= Start and then Now - Start > Deadline_US;
            end if;
            Pause;
        end loop;
        OK := (Read_64 (U, Offset) and Mask) = Expect;
    end Wait_64;

    -- Issue one global command (VT-d 11.4.4): the persistent status bits,
    -- with Command set (On) or cleared, then wait for its status bit.
    procedure Command (U : Unit_Index; Bit : Unsigned_32; On : Boolean; OK : out Boolean) is
        Current : constant Unsigned_32 := Read_32 (U, GSTS_Register) and Persistent_Commands;
    begin
        Write_32 (U, GCMD_Register, (if On then Current or Bit else Current and not Bit));
        Wait_32 (U, GSTS_Register, Bit, (if On then Bit else 0), Command_Deadline_US, OK);
    end Command;

    -- One-shot commands (SRTP, WBF) report completion differently: RTPS
    -- sets, WBFS clears.
    procedure One_Shot (U : Unit_Index; Bit : Unsigned_32; Done_When_Set : Boolean;
                        OK : out Boolean) is
        Current : constant Unsigned_32 := Read_32 (U, GSTS_Register) and Persistent_Commands;
    begin
        Write_32 (U, GCMD_Register, Current or Bit);
        Wait_32 (U, GSTS_Register, Bit, (if Done_When_Set then Bit else 0),
                 Command_Deadline_US, OK);
    end One_Shot;

    procedure Flush_Write_Buffer (U : Unit_Index; OK : out Boolean) is
    begin
        OK := True;
        if Units (U).Flush_Writes then
            One_Shot (U, Write_Buffer_Flush, False, OK);
        end if;
    end Flush_Write_Buffer;

    procedure Invalidate_Context_Global (U : Unit_Index; OK : out Boolean) is
    begin
        Flush_Write_Buffer (U, OK);
        if not OK then
            return;
        end if;
        Write_64 (U, CCMD_Register, CCMD_Invalidate or CCMD_Global);
        Wait_64 (U, CCMD_Register, CCMD_Invalidate, 0, Invalidation_Deadline_US, OK);
    end Invalidate_Context_Global;

    procedure Invalidate_IOTLB (U : Unit_Index; Domain : Unsigned_64; OK : out Boolean) is
        Register : constant Unsigned_64 := Units (U).IOTLB_Offset + IOTLB_Register_Offset;
        Granularity : constant Unsigned_64 :=
          (if Domain = 0 then IOTLB_Global
           else IOTLB_Domain or Shift_Left (Domain, IOTLB_Domain_Shift));
    begin
        Flush_Write_Buffer (U, OK);
        if not OK then
            return;
        end if;
        Write_64 (U, Register,
                  IOTLB_Invalidate or Granularity or IOTLB_Drain_Reads or IOTLB_Drain_Writes);
        Wait_64 (U, Register, IOTLB_Invalidate, 0, Invalidation_Deadline_US, OK);
    end Invalidate_IOTLB;

    ---------------------------------------------------------------------------
    -- Table storage (the walker's instance)
    ---------------------------------------------------------------------------
    procedure New_Page (Coherent : Boolean; Page : out Unsigned_64) is
        Frame : Virtmem.PhysAddress;
        Ignore : System.Address;
    begin
        BuddyAllocator.allocFrame (Frame);
        Page := Unsigned_64 (Frame);
        if Frame = 0 then
            return;
        end if;
        Ignore := Util.memset (Linear (Page), 0, IO_Page_Tables.Page_Bytes);
        Make_Visible (Linear (Page), IO_Page_Tables.Page_Bytes, Coherent);
    end New_Page;

    function Table_Read (Table : Page_Address; Index : Table_Index) return Unsigned_64 is
        Value : Unsigned_64 with Import, Volatile,
          Address => Linear (Table) + Storage_Offset (Index * Entry_Bytes);
    begin
        return Value;
    end Table_Read;

    procedure Table_Write (Table : Page_Address; Index : Table_Index; Value : Unsigned_64) is
        Target : Unsigned_64 with Import, Volatile,
          Address => Linear (Table) + Storage_Offset (Index * Entry_Bytes);
    begin
        Target := Value;
        Make_Visible (Target'Address, Entry_Bytes, Walk_Coherent);
    end Table_Write;

    procedure Table_New (Table : out Page_Address; Success : out Boolean) is
        Page : Unsigned_64;
    begin
        New_Page (Walk_Coherent, Page);
        Table := Page;
        Success := Page /= 0;
    end Table_New;

    package Walk is new IO_Page_Walk (Table_Read, Table_Write, Table_New);

    -- A 16-byte root or context entry: high word first, then the low word
    -- with the present bit, so the unit never sees half an entry.
    procedure Write_Wide (Table : Unsigned_64; Index : Natural; Low, High : Unsigned_64;
                          Coherent : Boolean) is
        Low_Word : Unsigned_64 with Import, Volatile,
          Address => Linear (Table) + Storage_Offset (Index * Wide_Entry_Bytes);
        High_Word : Unsigned_64 with Import, Volatile,
          Address => Linear (Table) + Storage_Offset (Index * Wide_Entry_Bytes + Entry_Bytes);
    begin
        High_Word := High;
        Fence;
        Low_Word := Low;
        Make_Visible (Low_Word'Address, Wide_Entry_Bytes, Coherent);
    end Write_Wide;

    ---------------------------------------------------------------------------
    -- PCI configuration reads (scope resolution at Setup only)
    ---------------------------------------------------------------------------
    -- Through the configuration mechanism (ports 0CF8h/0CFCh), at Setup
    -- only: no process exists yet, so nothing else uses the ports. (The
    -- ECAM window may sit inside a large linear-map page the kernel cannot
    -- remap a 4 KiB frame of.)
    function Config_Byte (Bus : Bus_Number; Device : Device_Number; Func : Function_Number;
                          Offset : Natural) return Unsigned_8 is
        Value : Unsigned_32;
    begin
        x86.out32 (PCI_Config_Address,
                   PCI_Config_Enable
                   or Shift_Left (Unsigned_32 (Bus), PCI_Config_Bus_Shift)
                   or Shift_Left (Unsigned_32 (Device), PCI_Config_Device_Shift)
                   or Shift_Left (Unsigned_32 (Func), PCI_Config_Function_Shift)
                   or (Unsigned_32 (Offset) and PCI_Config_Dword_Mask));
        x86.in32 (PCI_Config_Data, Value);
        return Unsigned_8 (Shift_Right (Value, 8 * (Offset mod 4)) and 16#FF#);
    end Config_Byte;

    function Is_Display (Who : Requester_ID) return Boolean is
      (Config_Byte (Bus_Of (Who), Device_Of (Who), Function_Of (Who), Config_Base_Class)
         = Display_Class);

    function Is_Intel_Graphics (Who : Requester_ID) return Boolean is
      (Config_Byte (Bus_Of (Who), Device_Of (Who), Function_Of (Who), Config_Vendor)
         = Intel_Vendor_Low
       and then Config_Byte (Bus_Of (Who), Device_Of (Who), Function_Of (Who),
                             Config_Vendor + 1) = Intel_Vendor_High
       and then Is_Display (Who));

    -- Follow a DMAR scope path through bridges (secondary bus numbers).
    procedure Resolve (S : R.Scope; Who : out Requester_ID; Found : out Boolean) is
        Bus : Unsigned_8 := S.Start_Bus;
    begin
        Who := 0;
        Found := False;
        for Hop in 1 .. S.Hops loop
            declare
                Device : constant Device_Number := Device_Number (S.Path (Hop).Device);
                Func : constant Function_Number := Function_Number (S.Path (Hop).Func);
            begin
                if Hop = S.Hops then
                    Who := Requester (Bus_Number (Bus), Device, Func);
                    Found := True;
                else
                    Bus := Config_Byte (Bus_Number (Bus), Device, Func, Config_Secondary_Bus);
                    if Bus = No_Device or else Bus = 0 then
                        return;
                    end if;
                end if;
            end;
        end loop;
    end Resolve;

    -- The unit a requester's DMA goes through: an explicit scope first,
    -- else the segment's INCLUDE_PCI_ALL unit; 0 if none.
    function Unit_For (Who : Requester_ID) return Natural is
    begin
        for S of Scopes loop
            if S.Used and then not S.Bridge and then S.Who = Who then
                return S.Unit;
            end if;
        end loop;
        for S of Scopes loop
            if S.Used and then S.Bridge
              and then Bus_Of (Who) in S.First_Bus .. S.Last_Bus
            then
                return S.Unit;
            end if;
        end loop;
        for U in 1 .. Unit_Total loop
            if acpi.dmarFacts.Units (U).Include_All
              and then acpi.dmarFacts.Units (U).Segment = 0
            then
                return U;
            end if;
        end loop;
        return 0;
    end Unit_For;

    -- "bb:dd.f", as lspci and devmgr print a PCI function.
    function Location (Who : Requester_ID) return String is
        Hex_Digits : constant String := "0123456789abcdef";
        function Two (V : Natural) return String is
          ([Hex_Digits (V / 16 + 1), Hex_Digits (V mod 16 + 1)]);
    begin
        return Two (Bus_Of (Who)) & ":" & Two (Device_Of (Who)) & "."
          & [Hex_Digits (Function_Of (Who) + 1)];
    end Location;

    ---------------------------------------------------------------------------
    -- Domains
    ---------------------------------------------------------------------------

    -- The context table for Bus on unit U (adding it, and its root entry).
    procedure Context_Table (U : Unit_Index; Bus : Bus_Number; Table : out Unsigned_64) is
    begin
        Table := Units (U).Context_Tables (Bus);
        if Table /= 0 then
            return;
        end if;
        New_Page (Units (U).Coherent, Table);
        if Table = 0 then
            return;
        end if;
        Units (U).Context_Tables (Bus) := Table;
        Write_Wide (Units (U).Root_Table, Bus, Root_Entry (Table), 0, Units (U).Coherent);
    end Context_Table;

    -- Point Who's context entry at domain D's tables (or pass-through).
    procedure Program_Context
      (U : Unit_Index; Who : Requester_ID; Low, High : Unsigned_64; OK : out Boolean)
    is
        Table : Unsigned_64;
    begin
        Context_Table (U, Bus_Of (Who), Table);
        OK := Table /= 0;
        if not OK then
            return;
        end if;
        Write_Wide (Table, Context_Of (Who), Low, High, Units (U).Coherent);
        if Units (U).State = Translating then
            Invalidate_Context_Global (U, OK);
            if OK then
                Invalidate_IOTLB (U, 0, OK);
            end if;
        end if;
    end Program_Context;

    -- The domain of (U, Who), with its tables and context entry. Under Lock.
    procedure Ensure_Domain (U : Unit_Index; Who : Requester_ID;
                             D : out ID.Domain_Index; OK : out Boolean) is
        Created : Boolean;
        Status : ID.Domain_Status;
    begin
        OK := False;
        D := ID.Domain_Index'First;
        if ID.Find_Domain (Ledger, U, Who) = 0
          and then Unsigned_64 (Ledger.Domain_Total) + 1 >= Units (U).Domain_Limit
        then
            println ("IOMMU: CAPACITY domain IDs on unit" & Natural'Image (U));
            return;
        end if;
        ID.Create_Domain (Ledger, U, Who, D, Created, Status);
        if Status /= ID.Ready then
            println ("IOMMU: CAPACITY domains (" & Natural'Image (ID.Maximum_Domains) & ")");
            return;
        end if;
        if Created then
            println ("IOMMU: domain" & Natural'Image (D) & " for requester " & Location (Who)
                     & " on unit" & Natural'Image (U));
        end if;
        if Domain_Roots (D) = 0 then
            Walk_Coherent := Units (U).Coherent;
            New_Page (Units (U).Coherent, Domain_Roots (D));
            if Domain_Roots (D) = 0 then
                println ("IOMMU: no frame for a domain's page-table root");
                return;
            end if;
        end if;
        if not Domain_Programmed (D) then
            Program_Context (U, Who, Context_Low (Domain_Roots (D)),
                             Context_High (Units (U).Width, Domain_ID (D)), OK);
            if not OK then
                println ("IOMMU: context entry not installed");
                return;
            end if;
            Domain_Programmed (D) := True;
        end if;
        OK := True;
    end Ensure_Domain;

    -- Map or unmap Pages identity pages from First in domain D. Under Lock.
    procedure Map_Pages (U : Unit_Index; D : ID.Domain_Index; First, Pages : Unsigned_64;
                         Writable : Boolean; Mapped : out Unsigned_64) is
        Done : Boolean;
    begin
        Walk_Coherent := Units (U).Coherent;
        Mapped := 0;
        while Mapped < Pages loop
            Walk.Map_Page (Domain_Roots (D), Units (U).Width,
                           First + Mapped * IO_Page_Tables.Page_Bytes,
                           First + Mapped * IO_Page_Tables.Page_Bytes, Writable, Done);
            exit when not Done;
            Mapped := Mapped + 1;
        end loop;
    end Map_Pages;

    procedure Unmap_Pages (U : Unit_Index; D : ID.Domain_Index; First, Pages : Unsigned_64) is
    begin
        Walk_Coherent := Units (U).Coherent;
        for P in 0 .. Pages - 1 loop
            Walk.Unmap_Page (Domain_Roots (D), Units (U).Width,
                             First + P * IO_Page_Tables.Page_Bytes);
        end loop;
    end Unmap_Pages;

    -- Unmap a ledger run, invalidate, and release it. Under Lock.
    procedure Retire_Run (M : ID.Mapping_Index; Released : out Boolean) is
        D : constant ID.Domain_Index := Ledger.Mappings (M).Domain;
        U : constant Unit_Index := Ledger.Domains (D).Unit;
        First : constant Unsigned_64 := Ledger.Mappings (M).First * IO_Page_Tables.Page_Bytes;
        Pages : constant Unsigned_64 := Ledger.Mappings (M).Pages;
        Stamp : Unsigned_64;
        OK : Boolean;
        Status : ID.Release_Status;
    begin
        Released := False;
        -- 1. Unmap: the page-table entries go.
        Unmap_Pages (U, D, First, Pages);
        ID.Begin_Unmap (Ledger, M, Stamp);
        -- 2. IOTLB invalidation for the domain, waited for.
        Invalidate_IOTLB (U, Unsigned_64 (D), OK);
        if not OK then
            println ("IOMMU: IOTLB invalidation timed out; frames quarantined");
            return;
        end if;
        ID.Complete_Flush (Ledger, D, Stamp);
        -- 3. Only now may the frames be reused.
        ID.Release (Ledger, M, Status);
        Released := Status = ID.Released;
    end Retire_Run;

    ---------------------------------------------------------------------------
    -- Setup
    ---------------------------------------------------------------------------
    function Field (Word : Unsigned_64; Shift : Natural; Mask : Unsigned_64)
      return Unsigned_64 is (Shift_Right (Word, Shift) and Mask);

    procedure Unit_Note (U : Unit_Index; Text : String) is
    begin
        print ("IOMMU: unit");
        print (Integer (U));
        print (" at ");
        print (Units (U).Physical);
        print (": ");
        println (Text);
    end Unit_Note;

    procedure Prepare_Unit (U : Unit_Index) is
        function mapIOFrame is new Mem_mgr.mapIOFrame (BuddyAllocator.allocFrame);
        F : R.Unit renames acpi.dmarFacts.Units (U);
        Choice : Width_Choice;
    begin
        Units (U).Physical := F.Register_Base;
        Units (U).Register_Bytes := Unsigned_64 (F.Register_Pages) * IO_Page_Tables.Page_Bytes;
        if F.Segment /= 0 then
            Units (U).State := Skipped;
            Unit_Note (U, "PCI segment above 0 not driven");
            return;
        end if;
        for P in 0 .. F.Register_Pages - 1 loop
            if not mapIOFrame (Virtmem.PhysAddress
                                 (F.Register_Base + Unsigned_64 (P) * IO_Page_Tables.Page_Bytes))
            then
                Units (U).State := Failed;
                Unit_Note (U, "registers could not be mapped");
                return;
            end if;
        end loop;
        Units (U).Registers := Virtmem.P2V (Virtmem.PhysAddress (F.Register_Base));
        Units (U).CAP := Read_64 (U, CAP_Register);
        Units (U).ECAP := Read_64 (U, ECAP_Register);
        if Units (U).CAP = Unsigned_64'Last or else Units (U).CAP = 0 then
            Units (U).State := Failed;
            Unit_Note (U, "not responding (CAP all ones or zero)");
            return;
        end if;
        declare
            CAP : constant Unsigned_64 := Units (U).CAP;
            ECAP : constant Unsigned_64 := Units (U).ECAP;
        begin
            Choice := Choose_Width
              (SAGAW_Field (Field (CAP, CAP_SAGAW_Shift, CAP_SAGAW_Mask)),
               Natural (Field (CAP, CAP_MGAW_Shift, CAP_MGAW_Mask)) + 1,
               acpi.dmarFacts.Width);
            if not Choice.Found then
                Units (U).State := Failed;
                Unit_Note (U, "no supported address width covers the host width");
                return;
            end if;
            Units (U).Width := Choice.Width;
            Units (U).Coherent := (ECAP and ECAP_Coherent) /= 0;
            Units (U).Caching_Mode := (CAP and CAP_Caching_Mode) /= 0;
            Units (U).Flush_Writes := (CAP and CAP_Write_Buffer_Flush) /= 0;
            Units (U).Pass_Through := (ECAP and ECAP_Pass_Through) /= 0;
            Units (U).Domain_Limit :=
              2 ** (4 + 2 * Natural (CAP and CAP_Domains_Mask));
            Units (U).Fault_Offset := Field (CAP, CAP_FRO_Shift, CAP_FRO_Mask) * Offset_Unit;
            Units (U).Fault_Records := Field (CAP, CAP_NFR_Shift, CAP_NFR_Mask) + 1;
            Units (U).IOTLB_Offset := Field (ECAP, ECAP_IRO_Shift, ECAP_IRO_Mask) * Offset_Unit;
            if Units (U).Fault_Offset + Units (U).Fault_Records * Fault_Record_Bytes
                 > Units (U).Register_Bytes
              or else Units (U).IOTLB_Offset + 2 * Entry_Bytes > Units (U).Register_Bytes
            then
                Units (U).State := Failed;
                Unit_Note (U, "fault or IOTLB registers outside the register set");
                return;
            end if;
        end;
        New_Page (Units (U).Coherent, Units (U).Root_Table);
        if Units (U).Root_Table = 0 then
            Units (U).State := Failed;
            Unit_Note (U, "no frame for the root table");
            return;
        end if;
        Units (U).State := Ready;
        print ("IOMMU: unit");
        print (Integer (U));
        print (" at ");
        print (Units (U).Physical);
        print (" version ");
        print (Read_32 (U, VER_Register));
        print (" CAP ");
        print (Units (U).CAP);
        print (" ECAP ");
        print (Units (U).ECAP);
        print (if Units (U).Width = Width_39 then " 39-bit" else " 48-bit");
        print (if Units (U).Coherent then " coherent" else " non-coherent");
        if Units (U).Caching_Mode then print (" caching-mode"); end if;
        println;
    end Prepare_Unit;

    -- Resolve every unit scope; a unit naming only display devices stays
    -- untranslated (the iGPU domain is slice 5).
    procedure Resolve_Scopes is
        F : R.Facts renames acpi.dmarFacts;
    begin
        for U in 1 .. Unit_Total loop
            declare
                PCI_Scopes, Display_Scopes : Natural := 0;
            begin
                for K in 0 .. F.Units (U).Scopes - 1 loop
                    declare
                        I : constant R.Scope_Index := F.Units (U).First_Scope + K;
                        S : R.Scope renames F.Scopes (I);
                        Who : Requester_ID;
                        Found : Boolean;
                    begin
                        if S.Kind in R.PCI_Endpoint | R.PCI_Bridge then
                            PCI_Scopes := PCI_Scopes + 1;
                            Resolve (S, Who, Found);
                            if not Found then
                                if Units (U).State = Ready then
                                    Units (U).State := Skipped;
                                    Unit_Note (U, "a device scope path could not be resolved");
                                end if;
                            elsif S.Kind = R.PCI_Endpoint then
                                Scopes (I) := (Used => True, Unit => U, Bridge => False,
                                               Who => Who, First_Bus => 0, Last_Bus => 0);
                                if Is_Display (Who) then
                                    Display_Scopes := Display_Scopes + 1;
                                end if;
                            else
                                declare
                                    Secondary : constant Unsigned_8 := Config_Byte
                                      (Bus_Of (Who), Device_Of (Who), Function_Of (Who),
                                       Config_Secondary_Bus);
                                    Subordinate : constant Unsigned_8 := Config_Byte
                                      (Bus_Of (Who), Device_Of (Who), Function_Of (Who),
                                       Config_Subordinate_Bus);
                                begin
                                    if Secondary = No_Device or else Secondary > Subordinate then
                                        if Units (U).State = Ready then
                                            Units (U).State := Skipped;
                                            Unit_Note (U, "a bridge scope's buses could not be read");
                                        end if;
                                    else
                                        Scopes (I) := (Used => True, Unit => U, Bridge => True,
                                                       Who => Who,
                                                       First_Bus => Bus_Number (Secondary),
                                                       Last_Bus => Bus_Number (Subordinate));
                                    end if;
                                end;
                            end if;
                        end if;
                    end;
                end loop;
                if not F.Units (U).Include_All and then PCI_Scopes > 0
                  and then Display_Scopes = PCI_Scopes and then Units (U).State = Ready
                then
                    Units (U).State := Graphics_Untranslated;
                    Unit_Note (U, "display devices only: left untranslated (iGPU domain is IOMMU-001 slice 5)");
                end if;
            end;
        end loop;
    end Resolve_Scopes;

    -- The integrated GPU behind an INCLUDE_PCI_ALL unit: pass-through.
    procedure Pass_Through_Graphics is
        U : constant Natural := Unit_For (Graphics_Requester);
        OK : Boolean;
    begin
        if U = 0 or else Units (U).State /= Ready
          or else not acpi.dmarFacts.Units (U).Include_All
          or else not Is_Intel_Graphics (Graphics_Requester)
        then
            return;
        end if;
        if not Units (U).Pass_Through then
            Units (U).State := Skipped;
            Unit_Note (U, "the integrated GPU needs pass-through, which the unit lacks");
            return;
        end if;
        -- Pass-through entries name the largest width the unit supports.
        Program_Context
          (U, Graphics_Requester, Pass_Through_Low,
           Context_High ((if Supports (SAGAW_Field (Field (Units (U).CAP, CAP_SAGAW_Shift,
                                                           CAP_SAGAW_Mask)), Width_48)
                          then Width_48 else Width_39),
                         Pass_Through_Domain), OK);
        if OK then
            Units (U).Graphics_Pass := True;
            Unit_Note (U, "integrated GPU 00:02.0 passes through (iGPU domain is IOMMU-001 slice 5)");
        else
            Units (U).State := Failed;
            Unit_Note (U, "integrated GPU context entry not installed");
        end if;
    end Pass_Through_Graphics;

    -- Every RMRR endpoint on a ready unit: its domain, the range identity
    -- mapped.
    procedure Map_Reserved_Regions is
        F : R.Facts renames acpi.dmarFacts;
    begin
        for Pass in 1 .. 2 loop
            for G in 1 .. F.Region_Total loop
                for K in 0 .. F.Regions (G).Scopes - 1 loop
                    declare
                        S : R.Scope renames F.Scopes (F.Regions (G).First_Scope + K);
                        Who : Requester_ID;
                        Found : Boolean;
                        Base : constant Unsigned_64 := F.Regions (G).Base;
                        Pages : constant Unsigned_64 :=
                          (F.Regions (G).Limit - Base + 1) / IO_Page_Tables.Page_Bytes;
                    begin
                        if S.Kind = R.PCI_Endpoint then
                            Resolve (S, Who, Found);
                            declare
                                U : constant Natural := (if Found then Unit_For (Who) else 0);
                            begin
                                if U /= 0 and then Units (U).State = Ready
                                  and then F.Regions (G).Segment = 0
                                  and then Pages <= ID.Maximum_Run_Pages
                                  and then Base / IO_Page_Tables.Page_Bytes < ID.Frame_Limit
                                  and then Run_In_Width (Base, Pages, Units (U).Width)
                                then
                                    if Pass = 1 then
                                        declare
                                            Status : ID.Admit_Status;
                                        begin
                                            ID.Admit_Reserved
                                              (Ledger, U, Who, Base / IO_Page_Tables.Page_Bytes,
                                               Pages, Status);
                                            if Status /= ID.Admitted then
                                                println ("IOMMU: reserved region not admitted");
                                            end if;
                                        end;
                                    else
                                        declare
                                            D : ID.Domain_Index;
                                            M : ID.Mapping_Index;
                                            MS : ID.Map_Status;
                                            OK : Boolean;
                                            Mapped : Unsigned_64;
                                        begin
                                            Ensure_Domain (U, Who, D, OK);
                                            if OK then
                                                ID.Map_Reserved
                                                  (Ledger, D, Base / IO_Page_Tables.Page_Bytes,
                                                   Pages, M, MS);
                                                if MS = ID.Added then
                                                    Map_Pages (U, D, Base, Pages, True, Mapped);
                                                    OK := Mapped = Pages;
                                                end if;
                                                OK := OK and then MS = ID.Added;
                                            end if;
                                            print ("IOMMU: reserved region ");
                                            print (Base);
                                            print (" .. ");
                                            print (F.Regions (G).Limit);
                                            print (" for requester ");
                                            print (Location (Who));
                                            println (if OK then " identity mapped"
                                                     else " NOT mapped");
                                        end;
                                    end if;
                                end if;
                            end;
                        end if;
                    end;
                end loop;
            end loop;
        end loop;
    end Map_Reserved_Regions;

    -- Bring one prepared unit's translation up, in the spec's order.
    procedure Enable_Unit (U : Unit_Index) is
        OK : Boolean;
        Status : constant Unsigned_32 := Read_32 (U, GSTS_Register);
        Destination : constant Unsigned_32 :=
          Unsigned_32 (CPU_Topology.Destination (CPU_Topology.Boot.CPUs, 0));

        procedure Fail (Stage : String) is
        begin
            Units (U).State := Failed;
            Unit_Note (U, Stage & " did not complete in time");
        end Fail;
    begin
        if (Status and Interrupt_Remapping) /= 0 then
            Units (U).State := Skipped;
            Unit_Note (U, "firmware left interrupt remapping on; unit not driven");
            return;
        end if;
        -- Firmware's translation and queued invalidation off first.
        if (Status and Translation_Enable) /= 0 then
            Command (U, Translation_Enable, False, OK);
            if not OK then Fail ("disabling firmware translation"); return; end if;
        end if;
        if (Status and Queued_Invalidation) /= 0 then
            Command (U, Queued_Invalidation, False, OK);
            if not OK then Fail ("disabling queued invalidation"); return; end if;
        end if;
        -- Root table (legacy mode: TTM 00).
        Make_Visible (Linear (Units (U).Root_Table), IO_Page_Tables.Page_Bytes,
                      Units (U).Coherent);
        Write_64 (U, RTADDR_Register, Units (U).Root_Table);
        One_Shot (U, Set_Root_Table_Pointer, True, OK);
        if not OK then Fail ("setting the root table pointer"); return; end if;
        Invalidate_Context_Global (U, OK);
        if not OK then Fail ("context-cache invalidation"); return; end if;
        Invalidate_IOTLB (U, 0, OK);
        if not OK then Fail ("IOTLB invalidation"); return; end if;
        -- Fault events to the boot CPU's local APIC, unmasked.
        Write_32 (U, FSTS_Register, FSTS_Clear);
        Write_32 (U, FEDATA_Register, Unsigned_32 (InterruptNumbers.IOMMU_FAULT));
        Write_32 (U, FEADDR_Register,
                  Interrupt_Window_Base or Shift_Left (Destination, APIC_Destination_Shift));
        Write_32 (U, FEUADDR_Register, 0);
        Write_32 (U, FECTL_Register, Read_32 (U, FECTL_Register) and not FECTL_Interrupt_Mask);
        -- Translation on.
        Command (U, Translation_Enable, True, OK);
        if not OK then Fail ("enabling translation"); return; end if;
        Units (U).State := Translating;
        -- The firmware's protected memory regions are no longer needed.
        if (Read_32 (U, PMEN_Register) and PMEN_Status) /= 0 then
            Write_32 (U, PMEN_Register, Read_32 (U, PMEN_Register) and not PMEN_Enable);
            Wait_32 (U, PMEN_Register, PMEN_Status, 0, Command_Deadline_US, OK);
            if not OK then
                Unit_Note (U, "protected memory regions still on");
            end if;
        end if;
        Unit_Note (U, "translating");
    end Enable_Unit;

    procedure Report_State is
        Status : Pub.IOMMU_Status := Pub.Active;
        Reason : Pub.IOMMU_Reason := Pub.No_Reason;
        Translating_Units, Units_Listed : Unsigned_64 := 0;
        Graphics : Boolean := False;
        Rejection : Unsigned_64 := 0;
        Segment_Problem : Boolean := False;
    begin
        case acpi.dmarStatus is
            when acpi.DMAR_Absent =>
                Status := Pub.Degraded;
                Reason := Pub.No_DMAR;
            when acpi.DMAR_Rejected =>
                Status := Pub.Degraded;
                Reason := Pub.DMAR_Rejected;
                Rejection := Unsigned_64 (R.Rejection'Pos (acpi.dmarRejection));
            when acpi.DMAR_Accepted =>
                Units_Listed := Unsigned_64 (Unit_Total);
                for U in 1 .. Unit_Total loop
                    case Units (U).State is
                        when Translating =>
                            Translating_Units := Translating_Units + 1;
                            Graphics := Graphics or else Units (U).Graphics_Pass;
                        when Graphics_Untranslated =>
                            Graphics := True;
                        when Skipped | Failed | Ready | Unused =>
                            if acpi.dmarFacts.Units (U).Segment /= 0 then
                                Segment_Problem := True;
                            end if;
                            Status := Pub.Degraded;
                            Reason := (if Segment_Problem then Pub.Segment_Unsupported
                                       else Pub.Unit_Failed);
                    end case;
                end loop;
                if Translating_Units = 0 then
                    Status := Pub.Degraded;
                    Reason := Pub.No_Unit_Translating;
                end if;
        end case;
        Device_Memory_Diagnostics.Record_IOMMU_State
          (Shift_Left (Unsigned_64 (Pub.IOMMU_Status'Pos (Status)), Pub.State_Status_Shift)
           or Shift_Left (Unsigned_64 (Pub.IOMMU_Reason'Pos (Reason)), Pub.State_Reason_Shift)
           or Shift_Left (Units_Listed, Pub.State_Units_Shift)
           or Shift_Left (Translating_Units, Pub.State_Translating_Shift)
           or Shift_Left ((if Graphics then 1 else 0), Pub.State_Graphics_Shift)
           or Shift_Left (Rejection, Pub.State_Rejection_Shift));
    end Report_State;

    procedure Setup is
    begin
        if Setup_Done then
            return;
        end if;
        Setup_Done := True;
        Spinlocks.Initialize (Lock, Lock_Name'Access);
        Spinlocks.Initialize (Fault_Lock, Fault_Lock_Name'Access);
        if acpi.dmarStatus = acpi.DMAR_Accepted then
            Unit_Total := acpi.dmarFacts.Unit_Total;
            for U in 1 .. Unit_Total loop
                Prepare_Unit (U);
            end loop;
            Resolve_Scopes;
            Pass_Through_Graphics;
            Map_Reserved_Regions;
            for U in 1 .. Unit_Total loop
                if Units (U).State = Ready then
                    Enable_Unit (U);
                end if;
                if Units (U).State = Translating then
                    Any_Translating := True;
                end if;
            end loop;
        end if;
        Report_State;
    end Setup;

    function Translating return Boolean is (Any_Translating);

    ---------------------------------------------------------------------------
    -- Grants
    ---------------------------------------------------------------------------
    procedure Map_Grant
      (Requester : Unsigned_16;
       Owner     : Unsigned_64;
       Physical  : Unsigned_64;
       Pages     : Unsigned_64;
       Writable  : Boolean;
       Domain    : out Unsigned_64;
       Result    : out Grant_Result)
    is
        Who : constant Requester_ID := Requester;
        U : Natural;
        D : ID.Domain_Index;
        M : ID.Mapping_Index;
        OK : Boolean;
        Bound : ID.Bind_Status;
        Status : ID.Map_Status;
        Mapped : Unsigned_64;
    begin
        Domain := 0;
        Result := Unconfined;
        if not Any_Translating then
            return;
        end if;
        U := Unit_For (Who);
        if U = 0 or else Units (U).State /= Translating
          or else (Units (U).Graphics_Pass and then Who = Graphics_Requester)
        then
            return;
        end if;
        Result := Refused;
        if Owner = 0 or else Pages = 0 or else Pages > ID.Maximum_Run_Pages
          or else Physical mod IO_Page_Tables.Page_Bytes /= 0
          or else Physical / IO_Page_Tables.Page_Bytes >= ID.Frame_Limit
          or else not Run_In_Width (Physical, Pages, Units (U).Width)
        then
            println ("IOMMU: grant outside the domain's address width refused");
            return;
        end if;
        Spinlocks.enterCriticalSection (Lock);
        Ensure_Domain (U, Who, D, OK);
        if not OK then
            Spinlocks.exitCriticalSection (Lock);
            return;
        end if;
        ID.Bind_Owner (Ledger, D, ID.Owner_Key (Owner), Bound);
        if Bound /= ID.Bound then
            Spinlocks.exitCriticalSection (Lock);
            println ("IOMMU: device domain is bound to another live driver");
            return;
        end if;
        ID.Map_Grant (Ledger, D, ID.Owner_Key (Owner), Physical / IO_Page_Tables.Page_Bytes,
                      Pages, Writable, M, Status);
        if Status /= ID.Added then
            Spinlocks.exitCriticalSection (Lock);
            println ("IOMMU: grant refused: " &
                     (case Status is
                        when ID.Not_Owner    => "domain bound to another owner",
                        when ID.Overlaps     => "frames already mapped or reserved",
                        when ID.Not_Reserved => "not reserved",
                        when ID.Capacity     => "CAPACITY mappings",
                        when ID.Added        => "added"));
            return;
        end if;
        Map_Pages (U, D, Physical, Pages, Writable, Mapped);
        if Mapped /= Pages then
            -- Roll back in order; the device was never told the address.
            declare
                Released : Boolean;
            begin
                Retire_Run (M, Released);
                Spinlocks.exitCriticalSection (Lock);
                println ("IOMMU: no frame for an I/O page table; grant refused");
                if not Released then
                    -- The caller must not free the block: report Refused
                    -- with the domain so it quarantines instead.
                    Domain := Unsigned_64 (D);
                end if;
                return;
            end;
        end if;
        -- Non-present to present needs no invalidation unless the unit
        -- caches non-present entries (caching mode).
        if Units (U).Caching_Mode then
            Invalidate_IOTLB (U, Unsigned_64 (D), OK);
        else
            Flush_Write_Buffer (U, OK);
        end if;
        Spinlocks.exitCriticalSection (Lock);
        Domain := Unsigned_64 (D);
        Result := Mapped_In_Domain;
    end Map_Grant;

    procedure Unmap_Grant
      (Domain   : Unsigned_64;
       Physical : Unsigned_64;
       Pages    : Unsigned_64;
       Released : out Boolean)
    is
        M : Natural;
    begin
        Released := False;
        if Domain not in 1 .. ID.Maximum_Domains or else Pages not in 1 .. ID.Maximum_Run_Pages
          or else Physical / IO_Page_Tables.Page_Bytes >= ID.Frame_Limit
        then
            return;
        end if;
        Spinlocks.enterCriticalSection (Lock);
        M := ID.Find_Grant (Ledger, ID.Domain_Index (Domain),
                            Physical / IO_Page_Tables.Page_Bytes, Pages);
        if M = 0 then
            -- Not mapped: reusable only if no live run covers it.
            Released := ID.Reusable (Ledger, Physical / IO_Page_Tables.Page_Bytes, Pages);
        elsif Ledger.Domains (ID.Domain_Index (Domain)).Epoch = Unsigned_64'Last then
            Released := False;
        else
            Retire_Run (M, Released);
        end if;
        Spinlocks.exitCriticalSection (Lock);
    end Unmap_Grant;

    procedure Release_Owner (Owner : Unsigned_64) is
    begin
        if Owner = 0 or else not Setup_Done then
            return;
        end if;
        Spinlocks.enterCriticalSection (Lock);
        ID.Release_Owner (Ledger, ID.Owner_Key (Owner));
        Spinlocks.exitCriticalSection (Lock);
    end Release_Owner;

    ---------------------------------------------------------------------------
    -- Faults
    ---------------------------------------------------------------------------
    procedure Drain_Unit (U : Unit_Index) is
        Status : constant Unsigned_32 := Read_32 (U, FSTS_Register);
        Index : Unsigned_64;
        Now_Window : constant Unsigned_64 := Time.msTicks / Milliseconds_Per_Second;
    begin
        if (Status and FSTS_Pending) = 0 and then (Status and FSTS_Overflow) = 0 then
            return;
        end if;
        Index := Unsigned_64 (Shift_Right (Status, FSTS_Index_Shift) and FSTS_Index_Mask);
        if Index >= Units (U).Fault_Records then
            Index := 0;
        end if;
        for Count in 1 .. Units (U).Fault_Records loop
            declare
                Base : constant Unsigned_64 := Units (U).Fault_Offset + Index * Fault_Record_Bytes;
                High : constant Unsigned_64 := Read_64 (U, Base + Fault_High_Word);
            begin
                exit when (High and Fault_Flag) = 0;
                declare
                    Low : constant Unsigned_64 := Read_64 (U, Base);
                begin
                    -- Clear the record before reporting: the unit may reuse it.
                    Write_32 (U, Base + Fault_Flag_Dword, Fault_Clear);
                    if Now_Window /= Fault_Window then
                        if Suppressed > 0 then
                            print ("kernel: DMA faults not printed in the last window: ");
                            println (Suppressed);
                        end if;
                        Fault_Window := Now_Window;
                        Faults_In_Window := 0;
                        Suppressed := 0;
                    end if;
                    if Faults_In_Window < Fault_Reports_Per_Second then
                        Faults_In_Window := Faults_In_Window + 1;
                        Device_Memory_Diagnostics.Record_DMA_Fault
                          (Requester => High and Fault_Source_Mask,
                           Address   => Low and Fault_Address_Mask,
                           Code      => Field (High, Fault_Reason_Shift, Fault_Reason_Mask),
                           Write     => (High and Fault_Type_Read) = 0,
                           Unit      => Unsigned_64 (U));
                    else
                        Suppressed := Suppressed + 1;
                    end if;
                end;
            end;
            Index := (Index + 1) mod Units (U).Fault_Records;
        end loop;
        Write_32 (U, FSTS_Register, FSTS_Overflow or FSTS_Pending);
    end Drain_Unit;

    procedure Service_Faults is
    begin
        if not Setup_Done then
            return;
        end if;
        Spinlocks.enterCriticalSection (Fault_Lock);
        for U in 1 .. Unit_Total loop
            if Units (U).State = Translating then
                Drain_Unit (U);
            end if;
        end loop;
        Spinlocks.exitCriticalSection (Fault_Lock);
    end Service_Faults;
end DMA_Remapping;
