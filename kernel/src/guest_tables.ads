-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Guest-physical page tables: the arithmetic and the entry encodings of
-- AMD nested page tables (NPT) (VMM-001 slice 1, docs/hypervisor.md 4.4).
--
-- @description
-- NPT uses the ordinary long-mode format (AMD APM Vol. 2, 15.25.5, 5.3):
-- four levels of 512 eight-byte entries translating a 48-bit
-- guest-physical address. Every guest access is a user access to the
-- nested walk, so every present entry has U/S set. This kernel maps guest
-- RAM write-back (PAT index 0), never sets the global or large-page bits
-- above level 1 (4 KiB leaves only in slice 1), and sets NX on every leaf
-- the guest may not execute from.
--
-- Proved (gnatprove level 2, tests/vm-units): an index is in its table
-- (hosted-tested: Compose of the indices is the address); a table or leaf entry
-- round-trips its address and rights; a leaf is present only with read;
-- no reserved bit (51..63 apart from NX, the PS bit in a leaf at level 1,
-- PWT/PCD/PAT) is ever set, so a nested walk never meets an encoding it
-- would reject.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Guest_Tables with
    SPARK_Mode => On,
    Pure
is
    Page_Bits     : constant := 12;
    Page_Bytes    : constant := 2 ** Page_Bits;
    Index_Bits    : constant := 9;
    Table_Entries : constant := 2 ** Index_Bits;
    Levels        : constant := 4;
    Address_Bits  : constant := Page_Bits + Index_Bits * Levels;  -- 48
    -- Physical frames below 2 ** 52 (the architectural maximum).
    Physical_Bits : constant := 52;

    subtype Table_Index is Natural range 0 .. Table_Entries - 1;
    subtype Level is Positive range 1 .. Levels;   -- 1 = leaf table
    subtype Entry_Value is Unsigned_64;

    Present_Bit  : constant Entry_Value := 2 ** 0;
    Writable_Bit : constant Entry_Value := 2 ** 1;
    User_Bit     : constant Entry_Value := 2 ** 2;
    No_Execute_Bit : constant Entry_Value := 2 ** 63;
    Address_Mask : constant Entry_Value := 16#000F_FFFF_FFFF_F000#;
    -- The CPU sets these in NPT entries as the guest touches memory (AMD
    -- APM 2, 15.25.5); they record use, not the translation. (This
    -- kernel's EPTP leaves EPT accessed/dirty flags off.)
    NPT_Accessed_Bit : constant Entry_Value := 2 ** 5;
    NPT_Dirty_Bit    : constant Entry_Value := 2 ** 6;

    -- A guest-physical address inside the 48-bit space.
    function In_Space (GPA : Unsigned_64) return Boolean is
      (GPA < 2 ** Address_Bits);

    function Page_Aligned (A : Unsigned_64) return Boolean is
      (A mod Page_Bytes = 0);

    -- A physical frame address an entry can name.
    function Frame_Address (A : Unsigned_64) return Boolean is
      (Page_Aligned (A) and then A < 2 ** Physical_Bits);

    function Index_Of (GPA : Unsigned_64; L : Level) return Table_Index is
      (Table_Index (Shift_Right (GPA, Page_Bits + Index_Bits * (L - 1))
                      and (Table_Entries - 1)));

    -- The page-aligned address the four indices name.
    function Compose (I4, I3, I2, I1 : Table_Index) return Unsigned_64 is
      (Shift_Left (Unsigned_64 (I4), Page_Bits + 3 * Index_Bits)
       or Shift_Left (Unsigned_64 (I3), Page_Bits + 2 * Index_Bits)
       or Shift_Left (Unsigned_64 (I2), Page_Bits + Index_Bits)
       or Shift_Left (Unsigned_64 (I1), Page_Bits));

    function Address (E : Entry_Value) return Unsigned_64 is (E and Address_Mask);
    function Present (E : Entry_Value) return Boolean is ((E and Present_Bit) /= 0);
    function Writable (E : Entry_Value) return Boolean is ((E and Writable_Bit) /= 0);
    function Executable (E : Entry_Value) return Boolean is ((E and No_Execute_Bit) = 0);

    -- The bits this kernel may set: P, R/W, U/S, the address and NX.
    function Well_Formed (E : Entry_Value) return Boolean is
      ((E and not (Present_Bit or Writable_Bit or User_Bit or Address_Mask
                   or No_Execute_Bit)) = 0);

    -- A non-leaf entry naming the next table.
    function Table_Entry (Table : Unsigned_64) return Entry_Value is
      (Table or Present_Bit or Writable_Bit or User_Bit)
    with Pre  => Frame_Address (Table),
         Post => Address (Table_Entry'Result) = Table
                 and then Present (Table_Entry'Result)
                 and then Writable (Table_Entry'Result)
                 and then Executable (Table_Entry'Result)
                 and then Well_Formed (Table_Entry'Result);

    -- A 4 KiB leaf: always readable; writable and executable as asked.
    function Leaf (Frame : Unsigned_64; Write, Execute : Boolean) return Entry_Value is
      (Frame or Present_Bit or User_Bit
       or (if Write then Writable_Bit else 0)
       or (if Execute then 0 else No_Execute_Bit))
    with Pre  => Frame_Address (Frame),
         Post => Address (Leaf'Result) = Frame
                 and then Present (Leaf'Result)
                 and then Writable (Leaf'Result) = Write
                 and then Executable (Leaf'Result) = Execute
                 and then Well_Formed (Leaf'Result);

    Not_Present : constant Entry_Value := 0;

    ---------------------------------------------------------------------------
    -- Intel EPT (SDM Vol. 3C, 29.3.2): the same four-level geometry, its own
    -- entry format. Read, write and execute are bits 0 .. 2; a leaf carries
    -- its memory type in bits 5 .. 3 (write-back, 6) and never sets
    -- "ignore PAT"; a non-leaf entry sets all three rights and nothing else.
    ---------------------------------------------------------------------------
    EPT_Read_Bit    : constant Entry_Value := 2 ** 0;
    EPT_Write_Bit   : constant Entry_Value := 2 ** 1;
    EPT_Execute_Bit : constant Entry_Value := 2 ** 2;
    EPT_Memory_Type_Shift : constant := 3;
    EPT_Write_Back  : constant Entry_Value := 6;
    EPT_Leaf_Type   : constant Entry_Value := EPT_Write_Back * 2 ** EPT_Memory_Type_Shift;

    function EPT_Present (E : Entry_Value) return Boolean is
      ((E and (EPT_Read_Bit or EPT_Write_Bit or EPT_Execute_Bit)) /= 0);

    function EPT_Well_Formed (E : Entry_Value; Leaf : Boolean) return Boolean is
      ((E and not (EPT_Read_Bit or EPT_Write_Bit or EPT_Execute_Bit or Address_Mask
                   or (if Leaf then EPT_Leaf_Type else 0))) = 0);

    function EPT_Table_Entry (Table : Unsigned_64) return Entry_Value is
      (Table or EPT_Read_Bit or EPT_Write_Bit or EPT_Execute_Bit)
    with Pre  => Frame_Address (Table),
         Post => Address (EPT_Table_Entry'Result) = Table
                 and then EPT_Present (EPT_Table_Entry'Result)
                 and then EPT_Well_Formed (EPT_Table_Entry'Result, Leaf => False);

    function EPT_Leaf (Frame : Unsigned_64; Write, Execute : Boolean) return Entry_Value is
      (Frame or EPT_Read_Bit or EPT_Leaf_Type
       or (if Write then EPT_Write_Bit else 0)
       or (if Execute then EPT_Execute_Bit else 0))
    with Pre  => Frame_Address (Frame),
         Post => Address (EPT_Leaf'Result) = Frame
                 and then EPT_Present (EPT_Leaf'Result)
                 and then ((EPT_Leaf'Result and EPT_Write_Bit) /= 0) = Write
                 and then ((EPT_Leaf'Result and EPT_Execute_Bit) /= 0) = Execute
                 and then EPT_Well_Formed (EPT_Leaf'Result, Leaf => True);
end Guest_Tables;
