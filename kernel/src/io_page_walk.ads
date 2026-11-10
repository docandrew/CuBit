-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Walking and editing one domain's VT-d second-level page tables
-- (IOMMU-001), over table storage the instance supplies.
--
-- @description
-- The kernel instantiates it over physical pages reached through the linear
-- map (Iommu); the hosted model test (tests/iommu) over an array of tables,
-- where it checks after every operation that the leaves reachable from a
-- domain's root are exactly the ledger's (IOMMU_Domains) reachable runs.
-- Tables are only ever added, never freed (a domain's tables live as long
-- as the domain, which is until reboot).
--
-- Write must make the entry visible to the remapping unit (flush the cache
-- line when the unit's page walks are not coherent); New_Table returns a
-- zeroed, visible table.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with IO_Page_Tables; use IO_Page_Tables;

generic
    with function Read (Table : Page_Address; Index : Table_Index) return Unsigned_64;
    with procedure Write (Table : Page_Address; Index : Table_Index; Value : Unsigned_64);
    with procedure New_Table (Table : out Page_Address; Success : out Boolean);
package IO_Page_Walk with
    SPARK_Mode => On
is
    -- The leaf entry translating Address (Not_Present if none).
    function Lookup (Root : Page_Address; W : Address_Width; Address : Unsigned_64)
      return Unsigned_64
    with Pre => In_Width (Address, W);

    -- Map the page at Address to Frame, adding tables on the way. Fails
    -- (nothing mapped) when a table cannot be added or the page is already
    -- mapped. Tables added before a failure stay (empty of leaves).
    procedure Map_Page
      (Root : Page_Address; W : Address_Width; Address : Unsigned_64;
       Frame : Page_Address; Writable : Boolean; Success : out Boolean)
    with Pre => In_Width (Address, W) and then Page_Aligned (Address);

    -- Clear the leaf for Address (no change when not mapped). The caller
    -- invalidates the unit's IOTLB before the frame is reused.
    procedure Unmap_Page (Root : Page_Address; W : Address_Width; Address : Unsigned_64)
    with Pre => In_Width (Address, W) and then Page_Aligned (Address);
end IO_Page_Walk;
