-------------------------------------------------------------------------------
-- CubitOS
-- Copyright (C) 2019 Jon Andrew
--
-- @summary 
-- Setup of GDT and TSS segments on x86-64
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with x86;

package segment with
    SPARK_Mode => On
is
    -- GDT_SEGMENT_NULL            : constant := 0;
    -- GDT_SEGMENT_KERNEL_CODE     : constant := 1;
    -- GDT_SEGMENT_KERNEL_DATA     : constant := 2;
    -- GDT_SEGMENT_USER_CODE       : constant := 3;
    -- GDT_SEGMENT_USER_DATA       : constant := 4;
    -- GDT_SEGMENT_TSS_0           : constant := 5;
    -- GDT_SEGMENT_TSS_1           : constant := 6;
    -- GDT_SEGMENT_UNUSED          : constant := 7;

    -- Indexes into our GDT for various segment types used in Cubit
    -- Note that USER_DATA comes before USER_CODE here. This is because of a
    -- quirk in the way that SYSRET loads segments based on the value in the
    -- STAR MSR. (see: 6.1.1 in AMD System Programming Manual)
    type GDTIndex is (
        GDT_SEGMENT_NULL,
        GDT_SEGMENT_KERNEL_CODE,
        GDT_SEGMENT_KERNEL_DATA,
        GDT_SEGMENT_USER_DATA,
        GDT_SEGMENT_USER_CODE,
        GDT_SEGMENT_TSS_0,  
        GDT_SEGMENT_TSS_1,
        GDT_SEGMENT_UNUSED);

    -- -- Byte offsets for the indexes above
    type GDTOffset is (
        GDT_OFFSET_NULL,        -- 0
        GDT_OFFSET_KERNEL_CODE, -- 8
        GDT_OFFSET_KERNEL_DATA, -- 16
        GDT_OFFSET_USER_DATA,   -- 24
        GDT_OFFSET_USER_CODE,   -- 32
        GDT_OFFSET_TSS);        -- 40

    for GDTOffset use (
        GDT_OFFSET_NULL         => 0,
        GDT_OFFSET_KERNEL_CODE  => GDTIndex'Pos(GDT_SEGMENT_KERNEL_CODE) * 8,
        GDT_OFFSET_KERNEL_DATA  => GDTIndex'Pos(GDT_SEGMENT_KERNEL_DATA) * 8,
        GDT_OFFSET_USER_DATA    => GDTIndex'Pos(GDT_SEGMENT_USER_DATA) * 8,
        GDT_OFFSET_USER_CODE    => GDTIndex'Pos(GDT_SEGMENT_USER_CODE) * 8,
        GDT_OFFSET_TSS          => GDTIndex'Pos(GDT_SEGMENT_TSS_0) * 8);

    -- Code and Data segment types, bits 8-11 of the segment descriptor
    type SegmentType is (
        DATA_READ_ONLY,
        DATA_READ_ONLY_ACCESSED,
        DATA_READ_WRITE,
        DATA_READ_WRITE_ACCESSED,
        DATA_READ_ONLY_EXPAND,
        DATA_READ_ONLY_EXPAND_ACCESSED,
        DATA_READ_WRITE_EXPAND,
        DATA_READ_WRITE_EXPAND_ACCESSED,
        CODE_EXECUTE_ONLY,
        CODE_EXECUTE_ONLY_ACCESSED,            -- also 64-bit TSS  
        CODE_EXECUTE_READ,
        CODE_EXECUTE_READ_ACCESSED,
        CODE_EXECUTE_ONLY_CONFORMING,
        CODE_EXECUTE_ONLY_CONFORMING_ACCESSED,
        CODE_EXECUTE_READ_CONFORMING,
        CODE_EXECUTE_READ_CONFORMING_ACCESSED);

    for SegmentType use (
        DATA_READ_ONLY                          => 0,
        DATA_READ_ONLY_ACCESSED                 => 1,
        DATA_READ_WRITE                         => 2,
        DATA_READ_WRITE_ACCESSED                => 3,
        DATA_READ_ONLY_EXPAND                   => 4,
        DATA_READ_ONLY_EXPAND_ACCESSED          => 5,
        DATA_READ_WRITE_EXPAND                  => 6,
        DATA_READ_WRITE_EXPAND_ACCESSED         => 7,
        CODE_EXECUTE_ONLY                       => 8,
        CODE_EXECUTE_ONLY_ACCESSED              => 9,
        CODE_EXECUTE_READ                       => 10,
        CODE_EXECUTE_READ_ACCESSED              => 11,
        CODE_EXECUTE_ONLY_CONFORMING            => 12,
        CODE_EXECUTE_ONLY_CONFORMING_ACCESSED   => 13,
        CODE_EXECUTE_READ_CONFORMING            => 14,
        CODE_EXECUTE_READ_CONFORMING_ACCESSED   => 15);

    ---------------------------------------------------------------------------
    -- Descriptor for use in our GDT
    -- @field limit_0_15 is ignored in long mode
    -- @field base_0_15 is ignored in code segments, in data segments can be
    --   used for FS/GS thread-local addressing.
    -- @field base_16_23 (see base_0_15)
    -- @field typeField is a SegmentType. Accessed and Readable bits are
    --  ignored in long mode.
    -- @field systemSegment is False for system segments (TSS), True for
    --  code/data segments.
    -- @field dpl is 0 for kernel mode, 3 for user mode
    -- @field present is True if this descriptor is present
    -- @field limit_16_19 is ignored in long mode
    -- @field available is ignored in long mode
    -- @field longmode should be True for 64-bit code segments, ignored in data
    --  segments
    -- @field defaultOperand is ignored in long mode (unless compatibility-mode
    --  used)
    -- @field granularity is ignored in long mode
    -- @field base_24_31 is ignored in code segments
    ---------------------------------------------------------------------------
    type Descriptor is
    record
        limit_0_15      : Unsigned_16 := 0;
        base_0_15       : Unsigned_16 := 0;
        base_16_23      : Unsigned_8 := 0;
        typeField       : SegmentType;
        systemSegment   : Boolean := False;
        dpl             : x86.PrivilegeLevel := x86.DPL_KERNEL;
        present         : Boolean := False;
        limit_16_19     : Natural range 0..7 := 0;
        available       : Boolean := False;
        longmode        : Boolean := False;
        defaultOperand  : Boolean := False;
        granularity     : Boolean := False;
        base_24_31      : Unsigned_8 := 0;
    end record with Size => 64;

    for Descriptor use
    record
        limit_0_15      at 0 range 0..15;
        base_0_15       at 2 range 0..15;
        base_16_23      at 4 range 0..7;
        typeField       at 5 range 0..3;
        systemSegment   at 5 range 4..4;
        dpl             at 5 range 5..6;
        present         at 5 range 7..7;
        limit_16_19     at 6 range 0..3;
        available       at 6 range 4..4;
        longmode        at 6 range 5..5;
        defaultOperand  at 6 range 6..6;
        granularity     at 6 range 7..7;
        base_24_31      at 7 range 0..7;
    end record;

    ---------------------------------------------------------------------------
    -- TaskSwitchSegment
    -- @field rspN - stack pointers for privilege levels 0-2. The CPU will set
    --  RSP to this value during interrupts. For syscalls, our code needs to
    --  manually set the RSP during entry.
    -- @field istN - interrupt stack table pointers
    -- @field IOMapBaseOffset - 16-bit offset to the I/O permission bit map.
    --  If no IOPB is present, this should contain the size of the TSS without
    --  it.
    ---------------------------------------------------------------------------
    type TaskSwitchSegment is
    record
        reserved1       : Unsigned_32 := 0;
        rsp0            : Unsigned_64 := 0;
        rsp1            : Unsigned_64 := 0;
        rsp2            : Unsigned_64 := 0;
        reserved2       : Unsigned_64 := 0;
        ist1            : Unsigned_64 := 0;
        ist2            : Unsigned_64 := 0;
        ist3            : Unsigned_64 := 0;
        ist4            : Unsigned_64 := 0;
        ist5            : Unsigned_64 := 0;
        ist6            : Unsigned_64 := 0;
        ist7            : Unsigned_64 := 0;
        reserved3       : Unsigned_64 := 0;
        reserved4       : Unsigned_16 := 0;
        IOMapBaseOffset : Unsigned_16 := 104;
        -- TODO: put IOPB here
    end record with Size => 104*8;

    for TaskSwitchSegment use
    record
        reserved1       at 0   range 0..31;
        rsp0            at 4   range 0..63;
        rsp1            at 12  range 0..63;
        rsp2            at 20  range 0..63;
        reserved2       at 28  range 0..63;
        ist1            at 36  range 0..63;
        ist2            at 44  range 0..63;
        ist3            at 52  range 0..63;
        ist4            at 60  range 0..63;
        ist5            at 68  range 0..63;
        ist6            at 76  range 0..63;
        ist7            at 84  range 0..63;
        reserved3       at 92  range 0..63;
        reserved4       at 100 range 0..15;
        IOMapBaseOffset at 102 range 0..15;
        -- TODO: put IOPB here
    end record;

    ---------------------------------------------------------------------------
    -- Global Descriptor Table (GDT) - specify code and data segments for the
    -- system. Array of segment descriptors.
    ---------------------------------------------------------------------------
    type GDT is array (GDTIndex) of Descriptor
        with Alignment => 64, Convention => C;
    pragma Pack(GDT);

    ---------------------------------------------------------------------------
    -- GDT Pointer structure containing size of GDT and it's address.
    -- @field limit - the size of the table-1
    -- @field base - base _linear_ address of the GDT
    ---------------------------------------------------------------------------
    type GDTPointer is 
    record
        limit           : Unsigned_16 := 0;
        base            : Unsigned_64 := 0;
    end record with Size => 64 + 16;

    for GDTPointer use
    record
        limit           at 0 range 0..15;
        base            at 2 range 0..63;
    end record;

end segment;