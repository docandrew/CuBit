with Interfaces;
with CCL.Binding_Blocks;
with CCL.Catalog;
with CCL.Objects;
with CuBit.Graphics_Report;

--  The renderer report (GPU-014): (graphics.report) lists every entry of
--  devmgr's report (CuBit.Graphics_Report) as a typed Gfx_Entry: for each
--  GPU, output and GPU or Vulkan program, the path that renders it
--  (Hardware, Venus or Software) and, when Software, why (Gfx_Why, with its
--  stage, gap, setting, admission outcome, VkResult or fallback cause).
--  Stale marks an entry whose poster has exited (its last value). Read
--  only: one summary, then every entry's pages against one generation. The
--  interface is published only where the program holds devmgr's
--  graphics-observer endpoint (startup plan only). The types are CCL source
--  (TYPE_SOURCE); the keys are SHA-256 of it, then "#" and the type's name
--  (tests/ccl-console/check_interface_keys.py checks them).
package CCL.Interfaces.Graphics with SPARK_Mode is
   use Standard.Interfaces;
   package GR renames CuBit.Graphics_Report;

   --  What Publish adds: one interface, two schemas (Gfx_Entry and
   --  List<Gfx_Entry>) and ten types (TYPE_SOURCE's nine and the list).
   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 2;
   PUBLISHED_TYPES : constant := 10;

   TYPE_SOURCE : constant String :=
     "(type Gfx_Subject (enum Output Program GPU)) (type Gfx_Path (enum Hardware Venus " &
     "Software)) (type Gfx_Stage (enum Probe Firmware Init Modeset Render)) (type Gfx_Gap " &
     "(enum No_3D No_Capset No_Hostmem)) (type Gfx_Policy (enum Render_GPU_Off " &
     "Composition_CPU)) (type Gfx_Admission (enum Rejected Unanswered Uncertain)) (type " &
     "Gfx_Fallback (enum Device_Lost Scene_Stalled Surface_Declined Readback_Failed)) (type " &
     "Gfx_Why (variant (None) (No_GPU_Device) (No_Driver_Bound) (Driver_Failed Gfx_Stage) " &
     "(Venus_Not_Offered Gfx_Gap) (Manifest_Lacks_Render) (Policy Gfx_Policy) " &
     "(Admission_Refused Gfx_Admission) (ICD_Init_Failed Integer) (Startup_Timed_Out) " &
     "(Fell_Back Gfx_Fallback))) (type Gfx_Entry (record (kind Gfx_Subject) (id Integer) " &
     "(subject String) (path Gfx_Path) (why Gfx_Why) (device String) (since_ms Integer) (stale " &
     "Boolean)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#2DCA_0E04_A63E_3940#,
      16#6333_9484_C359_1345#,
      16#CC3F_C9DF_C7FB_2558#,
      16#0726_0096_1A6C_B1C9#];
   ENTRY_KEY : constant CCL.Objects.Schema_Key :=
     [16#04D6_3784_CEB1_6E49#,
      16#F7D6_B726_4808_7F25#,
      16#FAF8_7302_3A24_8C7C#,
      16#D067_6FC7_46FD_9EAB#];
   ENTRIES_KEY : constant CCL.Objects.Schema_Key :=
     [16#87DD_CB43_E462_18BD#,
      16#AC80_059F_5C87_D543#,
      16#E015_EC45_22E7_227C#,
      16#E89A_B132_03DE_22D2#];

   --  A Gfx_Entry's fields and its most cells: the product cell, two for
   --  each enumeration, one for each Integer, String and Boolean, and up to
   --  three for the reason (its variant cell and an enumeration payload);
   --  a listing's count cell comes first.
   ENTRY_FIELDS : constant := 8;
   ENTRY_CELLS : constant := 1 + 2 + 1 + 1 + 2 + 3 + 1 + 1 + 1;
   MAXIMUM_LISTED : constant := (CCL.Objects.Maximum_Cells - 1) / ENTRY_CELLS;

   type Entry_Array is array (Positive range <>) of GR.Report_Entry;

   type Operation is (Report);
   function Name (Item : Operation) return String is (case Item is when Report => "report");
   FIRST_BINDING : constant Unsigned_32 := CCL.Binding_Blocks.First (CCL.Binding_Blocks.Graphics);
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));

   type Contracts is record
      Single, Listing : CCL.Objects.Binding;
   end record;
   procedure Define_Types (Bound : out Contracts; Accepted : out Boolean);
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);

   --  The List<Gfx_Entry> of Entries; Built False when it does not fit one
   --  result.
   procedure Report_Value
     (Contract : CCL.Objects.Binding; Entries : Entry_Array;
      Result : out CCL.Objects.Image; Built : out Boolean)
     with Pre => Entries'Length <= MAXIMUM_LISTED;
end CCL.Interfaces.Graphics;
