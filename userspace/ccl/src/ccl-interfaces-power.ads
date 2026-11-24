with Interfaces;
with CCL.Binding_Blocks;
with CCL.Catalog;
with CCL.Objects;
with CuBit.Power_Sources;

--  The machine's power sources (PWR-004 slice 1): (power.sources) lists
--  every source power.svc reports (CuBit.Power_Sources: the AC adapter and
--  the batteries ACPI knows, later UPSes and battery banks) as typed
--  Power_Source records. Reading asks power.svc to read its sources again
--  first (query on read, within their freshness), then copies its
--  snapshot, page by page, against one generation. Read only: holding it
--  turns nothing off. An unknown quantity is Power_Quantity.Unknown,
--  never a zero: charge_per_mille, energy_*_mwh, rate_mw (a magnitude:
--  state says its direction), voltage_mv, and a UPS's mains side
--  (load_per_mille, input_mv, output_mv); runtime_s is how long to empty
--  or to full; conditions are low, replace, overload and bypass. The interface is
--  published only where the program holds power.svc's power-sources
--  endpoint (startup plan only). The types are CCL source (TYPE_SOURCE);
--  the keys are SHA-256 of it, then "#" and the type's name
--  (tests/ccl-console/check_interface_keys.py checks them).
package CCL.Interfaces.Power with SPARK_Mode is
   use Standard.Interfaces;
   package PS renames CuBit.Power_Sources;

   --  What Publish adds: one interface, two schemas (Power_Source and
   --  List<Power_Source>) and eleven types (TYPE_SOURCE's ten and the
   --  list).
   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 2;
   PUBLISHED_TYPES : constant := 11;

   --  Qualified member names (Power_Kind.Internal_Battery) stay within
   --  CCL.Types.Maximum_Name_Length.
   TYPE_SOURCE : constant String :=
     "(type Power_Kind (enum Internal_Battery AC_Adapter UPS External_Battery)) (type " &
     "Power_State (enum Unknown Online Offline On_Battery Charging Discharging Full " &
     "Not_Charging)) (type Power_Flag (enum Unknown Clear Set)) (type Power_Freshness (enum " &
     "Unknown Fresh Stale)) (type Power_Quantity (variant (Unknown) (Known Integer))) (type " &
     "Power_Runtime (variant (Unknown) (To_Empty Integer) (To_Full Integer))) (type " &
     "Power_Transfer (enum Unknown None_Recorded Input_Voltage Input_Frequency Self_Test " &
     "Other)) (type Power_Conditions (record (low Power_Flag) (replace Power_Flag) (overload " &
     "Power_Flag) (bypass Power_Flag))) (type Power_Mains (record (load_per_mille " &
     "Power_Quantity) (input_mv Power_Quantity) (output_mv Power_Quantity) (transfer " &
     "Power_Transfer))) (type Power_Source (record (name String) (kind Power_Kind) (state " &
     "Power_State) (fresh Power_Freshness) (charge_per_mille Power_Quantity) (runtime_s " &
     "Power_Runtime) (energy_now_mwh Power_Quantity) (energy_full_mwh Power_Quantity) " &
     "(energy_design_mwh Power_Quantity) (rate_mw Power_Quantity) (voltage_mv Power_Quantity) " &
     "(conditions Power_Conditions) (mains Power_Mains)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#3DB1_558C_BBF0_4D57#,
      16#95B3_058E_381D_4F7E#,
      16#1860_5A84_5C00_CB45#,
      16#1098_0073_F95E_1CE7#];
   SOURCE_KEY : constant CCL.Objects.Schema_Key :=
     [16#A587_B4DA_334B_8BE7#,
      16#933B_4D62_BA99_B353#,
      16#1E72_689F_E15B_8EA7#,
      16#5107_B2A7_E23D_C630#];
   SOURCES_KEY : constant CCL.Objects.Schema_Key :=
     [16#27BB_B647_7953_29CD#,
      16#0D9C_F68E_C343_F75E#,
      16#E345_F681_6079_E5D3#,
      16#F976_A8EF_C4DB_244D#];

   --  A Power_Source's fields (a record holds at most
   --  CCL.Types.Maximum_Components: the flags are Power_Conditions, a UPS's
   --  mains side is Power_Mains), and its cells: the product cell, a String
   --  cell, two cells for each of the ten scalar fields (a member and its
   --  payload), then each nested record's product cell and its four fields'
   --  two each; a listing's count cell comes first.
   SOURCE_FIELDS : constant := 13;
   NESTED_FIELDS : constant := 4;
   SOURCE_CELLS : constant := 1 + 1 + 2 * 10 + 2 * (1 + 2 * NESTED_FIELDS);
   MAXIMUM_LISTED : constant := (CCL.Objects.Maximum_Cells - 1) / SOURCE_CELLS;

   type Operation is (Sources);
   function Name (Item : Operation) return String is ("sources");
   FIRST_BINDING : constant Unsigned_32 := CCL.Binding_Blocks.First (CCL.Binding_Blocks.Power);
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));

   type Contracts is record
      Source, Sources : CCL.Objects.Binding;
   end record;
   --  Check TYPE_SOURCE and bind Power_Source and List<Power_Source>.
   procedure Define_Types (Bound : out Contracts; Accepted : out Boolean);
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);

   --  The List<Power_Source> of Sources (1 .. Count); Built False when it
   --  does not fit one result.
   procedure Sources_Value
     (Contract : CCL.Objects.Binding; Sources : PS.Source_Array; Count : PS.Source_Count;
      Result : out CCL.Objects.Image; Built : out Boolean);
end CCL.Interfaces.Power;
