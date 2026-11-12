------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The one shape every platform quirk table has: a row per quirk, each a
--  typed key (which platforms), a typed effect (what changes there) and
--  the source that justifies it. Selection is a table lookup; nobody tests
--  platform IDs with scattered ifs.
--
--  @description
--  Quirks are split by who acts on them:
--    * devmgr's Platform_Quirks: hardware discovery (the Intel P2SB
--      sideband window, chipset sideband layouts, GPIO communities, PCI
--      fixups), keyed by PCI identity and the firmware Platform_Identity;
--    * acpi.svc's firmware-interpretation table (the ACPI agent's), keyed
--      the same way.
--  Each instantiates this generic with its own enumeration of quirks, key,
--  effect and observed-platform types. A row's Source says where the fact
--  comes from (a Linux or coreboot file, an Intel datasheet, the
--  machine's own tables, a CuBit hardware log); the full citation text
--  sits beside the table as a case expression over the quirk enumeration
--  (Citation), so every row is documented where it is declared.
--
--  Pure and proved (tests/platform-quirks): ZFP-compatible.
------------------------------------------------------------------------------
pragma Ada_2022;

package Platform_Quirk_Tables with Pure, SPARK_Mode is

   --  Where a quirk's fact comes from.
   type Source_Kind is
     (Linux_Source,          --  a file in the Linux kernel tree
      Coreboot_Source,       --  a file in the coreboot tree
      Intel_Documentation,   --  an Intel datasheet or register database
      Firmware_Tables,       --  the machine's own ACPI tables (DSDT, SSDT, ...)
      CuBit_Hardware_Log);   --  a CuBit boot log from the machine itself

   generic
      type Quirk is (<>);          --  one literal per quirk (the table's rows)
      type Key is private;         --  which platforms a row applies to
      type Effect is private;      --  what the row changes there
      type Observed is private;    --  the platform as discovered this boot
      with function Matches (K : Key; Platform : Observed) return Boolean;
   package Tables is

      type Row is record
         Applies_To : Key;
         Changes    : Effect;
         Source     : Source_Kind;
      end record;

      type Table is array (Quirk) of Row;

      type Selection is array (Quirk) of Boolean;

      --  Every row whose key matches the platform.
      function Applicable (T : Table; Platform : Observed) return Selection is
        ([for Q in Quirk => Matches (T (Q).Applies_To, Platform)])
      with Post => (for all Q in Quirk =>
                      Applicable'Result (Q) = Matches (T (Q).Applies_To, Platform));

      function Applies (T : Table; Q : Quirk; Platform : Observed) return Boolean is
        (Matches (T (Q).Applies_To, Platform));

   end Tables;

end Platform_Quirk_Tables;
