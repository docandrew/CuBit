-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- VT-d second-level I/O page tables and context entries: the arithmetic and
-- the entry encodings (IOMMU-001; Intel VT-d specification 9.1-9.3, 9.8).
--
-- @description
-- Legacy (non-scalable) mode. A requester ID (bus, device, function) selects
-- a root entry (bus) and a context entry (device and function); the context
-- entry names the domain's second-level page-table root, its address width
-- and its domain ID. A second-level table has 512 eight-byte entries; each
-- level translates 9 bits of the I/O virtual address above the 12-bit page
-- offset. CuBit maps 4 KiB leaves only (no superpages yet).
--
-- Widths. A domain uses the 39-bit (3-level) AGAW when the unit supports it,
-- else the 48-bit (4-level) one, and never a width below the host address
-- width the DMAR reports (so every physical frame the platform can address
-- has an I/O address). CuBit's I/O addresses equal the physical address of
-- the granted frame (identity within the domain): a device still reaches
-- only what its domain maps.
--
-- Proved (gnatprove level 2, tests/iommu): every index is in its table,
-- Index_Of recomposes the address (Recompose), entries round-trip their
-- address and rights, a leaf is present only with read or write set, and
-- the selected width is supported by the unit and covers the host width.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package IO_Page_Tables with
    SPARK_Mode => On,
    Pure
is
    Page_Bits    : constant := 12;
    Page_Bytes   : constant := 2 ** Page_Bits;
    Index_Bits   : constant := 9;
    Table_Entries : constant := 2 ** Index_Bits;
    Entry_Bytes  : constant := 8;

    subtype Table_Index is Natural range 0 .. Table_Entries - 1;

    -- Level 1 is the leaf table (it holds page entries), the highest level
    -- is the domain's root table.
    Maximum_Levels : constant := 4;
    subtype Level is Positive range 1 .. Maximum_Levels;

    -- The adjusted guest address widths CuBit drives (CAP.SAGAW bits 1, 2).
    type Address_Width is (Width_39, Width_48);

    function Levels (W : Address_Width) return Level is
      (case W is when Width_39 => 3, when Width_48 => 4);

    function Width_Bits (W : Address_Width) return Positive is
      (Page_Bits + Index_Bits * Levels (W))
    with Post => Width_Bits'Result in 39 | 48;

    -- An address is in the domain's I/O address space.
    function In_Width (Address : Unsigned_64; W : Address_Width) return Boolean is
      (Address < 2 ** Width_Bits (W));

    function Page_Aligned (Address : Unsigned_64) return Boolean is
      (Address mod Page_Bytes = 0);

    -- The page-aligned addresses an entry can name (bits 12 .. 51).
    Address_Mask : constant Unsigned_64 := 16#000F_FFFF_FFFF_F000#;
    subtype Page_Address is Unsigned_64
      with Dynamic_Predicate => (Page_Address and not Address_Mask) = 0;

    -- Which entry of a level-L table translates Address.
    function Shift_Of (L : Level) return Natural is
      (Page_Bits + Index_Bits * (L - 1))
    with Post => Shift_Of'Result in 12 | 21 | 30 | 39;

    function Index_Of (Address : Unsigned_64; L : Level) return Table_Index is
      (Table_Index (Shift_Right (Address, Shift_Of (L)) and (Table_Entries - 1)))
    with Post => Unsigned_64 (Index_Of'Result)
                   = (Shift_Right (Address, Shift_Of (L)) and (Table_Entries - 1));

    -- An address in the width is exactly its indices and page offset: the
    -- walk covers every bit of it and no other (Lemma_Recompose).
    -- Index_Of (Address, L) in place: Shift_Right (Index_Part, Shift_Of (L))
    -- is that index (Index_Of's postcondition).
    function Index_Part (Address : Unsigned_64; L : Level) return Unsigned_64 is
      (Shift_Left (Shift_Right (Address, Shift_Of (L)) and (Table_Entries - 1),
                   Shift_Of (L)));

    function Compose (Address : Unsigned_64; W : Address_Width) return Unsigned_64 is
      ((Address and (Page_Bytes - 1))
       or Index_Part (Address, 1) or Index_Part (Address, 2) or Index_Part (Address, 3)
       or (case W is when Width_39 => 0, when Width_48 => Index_Part (Address, 4)));

    procedure Lemma_Recompose (Address : Unsigned_64; W : Address_Width)
    with Ghost, Global => null,
         Pre  => In_Width (Address, W),
         Post => Compose (Address, W) = Address;

    ---------------------------------------------------------------------------
    -- Second-level entries (VT-d 9.8). Read (bit 0) and write (bit 1); an
    -- entry with neither is not present. Non-leaf entries carry both.
    ---------------------------------------------------------------------------
    Read_Bit  : constant Unsigned_64 := 1;
    Write_Bit : constant Unsigned_64 := 2;
    Not_Present : constant Unsigned_64 := 0;

    function Present (E : Unsigned_64) return Boolean is
      ((E and (Read_Bit or Write_Bit)) /= 0);

    function Entry_Address (E : Unsigned_64) return Page_Address is
      (E and Address_Mask);

    function Leaf_Entry (Frame : Page_Address; Writable : Boolean) return Unsigned_64 is
      (Frame or Read_Bit or (if Writable then Write_Bit else 0))
    with Post => Present (Leaf_Entry'Result)
      and then Entry_Address (Leaf_Entry'Result) = Frame
      and then ((Leaf_Entry'Result and Write_Bit) /= 0) = Writable;

    function Table_Entry (Table : Page_Address) return Unsigned_64 is
      (Table or Read_Bit or Write_Bit)
    with Post => Present (Table_Entry'Result)
      and then Entry_Address (Table_Entry'Result) = Table;

    ---------------------------------------------------------------------------
    -- Requester IDs and the root/context tables (VT-d 9.1, 9.3).
    ---------------------------------------------------------------------------
    subtype Requester_ID is Unsigned_16;
    subtype Bus_Number is Natural range 0 .. 255;
    subtype Device_Number is Natural range 0 .. 31;
    subtype Function_Number is Natural range 0 .. 7;
    subtype Context_Index is Natural range 0 .. 255;   -- device * 8 + function

    function Requester (Bus : Bus_Number; Device : Device_Number;
                        Func : Function_Number) return Requester_ID is
      (Requester_ID (Bus * 256 + Device * 8 + Func));

    function Bus_Of (R : Requester_ID) return Bus_Number is
      (Bus_Number (R / 256));
    function Device_Of (R : Requester_ID) return Device_Number is
      (Device_Number ((R mod 256) / 8));
    function Function_Of (R : Requester_ID) return Function_Number is
      (Function_Number (R mod 8));
    function Context_Of (R : Requester_ID) return Context_Index is
      (Context_Index (R mod 256));

    -- Root and context entries are 16 bytes: two 64-bit words.
    Root_Entries    : constant := 256;
    Context_Entries : constant := 256;
    Wide_Entry_Bytes : constant := 16;

    Entry_Present : constant Unsigned_64 := 1;

    -- Root entry low word: present, context-table pointer. High word zero.
    function Root_Entry (Context_Table : Page_Address) return Unsigned_64 is
      (Context_Table or Entry_Present);

    -- Context entry low word: present, translation type 00 (untranslated
    -- requests use second-level translation), fault processing enabled,
    -- second-level page-table pointer.
    function Context_Low (Second_Level_Root : Page_Address) return Unsigned_64 is
      (Second_Level_Root or Entry_Present);

    -- Translation type 10: pass-through (the unit forwards untranslated
    -- requests unchanged). Used only for the integrated GPU, whose domain is
    -- later work (IOMMU-001), and only when ECAP.PT is set.
    Pass_Through_Type : constant Unsigned_64 := 2 * 4;   -- TT = 10b at bits 3:2
    function Pass_Through_Low return Unsigned_64 is
      (Pass_Through_Type or Entry_Present);

    -- Context entry high word: AW (bits 2:0; 1 = 39-bit, 2 = 48-bit) and
    -- the domain ID (bits 23:8).
    subtype Domain_ID is Unsigned_64 range 1 .. 2 ** 16 - 1;
    function Width_Code (W : Address_Width) return Unsigned_64 is
      (case W is when Width_39 => 1, when Width_48 => 2);
    Domain_ID_Shift : constant := 8;
    function Context_High (W : Address_Width; Domain : Domain_ID) return Unsigned_64 is
      (Width_Code (W) or Shift_Left (Domain, Domain_ID_Shift));

    ---------------------------------------------------------------------------
    -- Choosing the width. SAGAW is CAP bits 12:8 (bit 1 of the field: 39-bit,
    -- bit 2: 48-bit); MGAW is the unit's maximum guest address width in bits.
    ---------------------------------------------------------------------------
    subtype SAGAW_Field is Unsigned_64 range 0 .. 31;
    SAGAW_39 : constant SAGAW_Field := 2;
    SAGAW_48 : constant SAGAW_Field := 4;

    function Supports (SAGAW : SAGAW_Field; W : Address_Width) return Boolean is
      ((SAGAW and (case W is when Width_39 => SAGAW_39, when Width_48 => SAGAW_48)) /= 0);

    type Width_Choice (Found : Boolean := False) is record
        case Found is
            when True  => Width : Address_Width;
            when False => null;
        end case;
    end record;

    -- The narrowest supported width that covers the host width and the
    -- unit's MGAW allows.
    function Choose_Width
      (SAGAW : SAGAW_Field; MGAW_Bits, Host_Bits : Natural) return Width_Choice
    with Post =>
      (if Choose_Width'Result.Found then
         Supports (SAGAW, Choose_Width'Result.Width)
         and then Width_Bits (Choose_Width'Result.Width) >= Host_Bits
         and then Width_Bits (Choose_Width'Result.Width) <= MGAW_Bits
       else
         (for all W in Address_Width =>
            not (Supports (SAGAW, W) and then Width_Bits (W) >= Host_Bits
                 and then Width_Bits (W) <= MGAW_Bits)));

    -- A run of Pages frames from First lies wholly inside the width.
    function Run_In_Width (First : Unsigned_64; Pages : Unsigned_64;
                           W : Address_Width) return Boolean is
      (Pages >= 1 and then Pages <= 2 ** (Width_Bits (W) - Page_Bits)
       and then First <= 2 ** Width_Bits (W) - Pages * Page_Bytes);
end IO_Page_Tables;
