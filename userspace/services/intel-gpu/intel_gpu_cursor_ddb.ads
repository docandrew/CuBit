with Interfaces;
--  The cursor's data-buffer (DDB) allocation and watermarks on the firmware
--  pipe, when the firmware left the cursor none (NUC, 2026-10-10:
--  CUR_BUF_CFG = 0). Pure planning, no MMIO.
--
--  Hardware facts (Linux v6.16 i915, facts only):
--  - PLANE_BUF_CFG / CUR_BUF_CFG: start bits 11:0, end bits 27:16, end
--    inclusive; zero means no allocation (skl_ddb_entry_init_from_hw,
--    skl_cursor_ddb_reg_val). Display version 13 has 4096 blocks in four
--    1024-block slices (xe_lpd dbuf.size).
--  - The cursor gets a fixed allocation at the end of the pipe's range:
--    max (32 with one active pipe, else 8; the cursor's min_ddb_alloc),
--    taken before the planes are sized (skl_crtc_allocate_plane_ddb,
--    skl_cursor_allocation). Here it is carved from the tail of plane 1's
--    range, so it stays inside the pipe's range and its DBUF slices.
--  - Watermarks (PLANE_WM / CUR_WM): enable bit 31, ignore-lines bit 30,
--    plane auto-min-alloc bit 29, lines bits 26:14, blocks bits 11:0.
--    Display 13 has six levels plus SAGV, SAGV-transition and transition
--    words (num_levels 6 with HW SAGV WM). A watermark is invalid unless
--    its blocks are below the allocation (min_ddb_alloc = blocks +
--    ceil (blocks / 10) + 1, skl_compute_plane_wm).
--  - A linear plane's blocks per line are ceil (bytes per line / 512) + 1:
--    3 for the 256-pixel ARGB cursor i915 sizes the allocation for.
--  - Method 2 watermarks are (lines of memory latency) x blocks per line,
--    plus one block; lines are the same for every plane on the pipe.
--
--  Cursor watermarks are derived from the firmware's plane-1 watermark at
--  the same level: its lines field already encodes this mode's memory
--  latency in lines, so the cursor's level is lines x 3 + 1 blocks
--  (method 2). The firmware's latency table itself is not readable here.
package Intel_GPU_Cursor_DDB with SPARK_Mode is
   use Interfaces;

   DBUF_Blocks : constant := 4_096;
   subtype Block is Natural range 0 .. DBUF_Blocks;

   --  [First, Last + 1); Stop = 0 means no allocation.
   type Entry_Range is record
      First, Stop : Block := 0;
   end record;
   function Empty (Item : Entry_Range) return Boolean is (Item.Stop = 0);
   function Size (Item : Entry_Range) return Block is
     (if Item.Stop > Item.First then Item.Stop - Item.First else 0);
   function Disjoint (A, B : Entry_Range) return Boolean is
     (Empty (A) or else Empty (B) or else A.Stop <= B.First or else B.Stop <= A.First);

   Start_Mask : constant Unsigned_32 := 16#FFF#;
   End_Shift  : constant := 16;
   End_Mask   : constant Unsigned_32 := 16#FFF#;
   Buffer_Spare_Bits : constant Unsigned_32 :=
     not (Start_Mask or Shift_Left (End_Mask, End_Shift));

   type Range_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Entry_Range;
         when False => null;
      end case;
   end record;
   function Decode (Word : Unsigned_32) return Range_Decoding
     with Post => (if Decode'Result.Valid then
                     (Empty (Decode'Result.Value) or else
                      Decode'Result.Value.First < Decode'Result.Value.Stop));
   function Encode (Item : Entry_Range) return Unsigned_32 is
     (if Empty (Item) then 0
      else Shift_Left (Unsigned_32 (Item.Stop - 1), End_Shift) or Unsigned_32 (Item.First))
     with Pre => Empty (Item) or else
                 (Item.First < Item.Stop and then Item.Stop <= DBUF_Blocks);

   --  Watermark words.
   Enable_Bit       : constant Unsigned_32 := 16#8000_0000#;
   Ignore_Lines_Bit : constant Unsigned_32 := 16#4000_0000#;
   Lines_Shift      : constant := 14;
   Lines_Mask       : constant Unsigned_32 := 16#1FFF#;
   Blocks_Mask      : constant Unsigned_32 := 16#FFF#;
   Max_Lines        : constant := 255;
   --  Field views written arithmetically (equal to the masks above).
   function Enabled (Word : Unsigned_32) return Boolean is (Word >= Enable_Bit);
   function Lines (Word : Unsigned_32) return Natural is
     (Natural (Word / 2 ** Lines_Shift mod (Lines_Mask + 1)));
   function Blocks (Word : Unsigned_32) return Natural is
     (Natural (Word mod (Blocks_Mask + 1)));
   function Fits (Word : Unsigned_32; Allocation : Block) return Boolean is
     (not Enabled (Word) or else Blocks (Word) < Allocation);

   --  Six levels, then SAGV, SAGV transition and transition: the order of
   --  the registers' i915 names; offsets are in Intel_GPU_Cursor_Plane.
   type Watermark_Slot is
     (Level_0, Level_1, Level_2, Level_3, Level_4, Level_5,
      SAGV, SAGV_Transition, Transition);
   type Watermarks is array (Watermark_Slot) of Unsigned_32;

   Cursor_Blocks_Per_Line : constant := 3;
   Single_Pipe_Minimum : constant := 32;
   Several_Pipe_Minimum : constant := 8;
   --  Upper bound on what is taken from plane 1; deeper levels that would
   --  need more stay disabled for the cursor (power, not correctness).
   Cursor_Limit : constant := 128;

   function Cursor_Watermark_Blocks (Plane_Word : Unsigned_32) return Positive is
     (Natural'Max (1, Natural'Min (Max_Lines, Lines (Plane_Word))) *
        Cursor_Blocks_Per_Line + 1)
     with Post => Cursor_Watermark_Blocks'Result <= Max_Lines * Cursor_Blocks_Per_Line + 1;
   function Minimum_Allocation (Blocks_Value : Positive) return Positive is
     (Blocks_Value + (Blocks_Value + 9) / 10 + 1)
     with Pre => Blocks_Value <= Max_Lines * Cursor_Blocks_Per_Line + 1;

   type Outcome is (Plane_Range_Invalid, Plane_Too_Small, Overlaps_Other_Plane,
                    Cursor_Already_Allocated, Ready);
   type Other_Planes is array (2 .. 5) of Unsigned_32;

   type Plan_Result is record
      State : Outcome := Plane_Range_Invalid;
      Plane_Before, Plane_After, Cursor : Entry_Range;
      Plane_Buffer, Cursor_Buffer : Unsigned_32 := 0;
      Plane_WM, Cursor_WM : Watermarks := [others => 0];
      Plane_Levels_Disabled : Natural := 0;
   end record;

   function Plan
     (Plane_Buffer : Unsigned_32; Other_Buffers : Other_Planes; Plane_WM : Watermarks;
      Cursor_Buffer : Unsigned_32; Single_Pipe : Boolean) return Plan_Result
     with Global => null,
          Post =>
            (if Plan'Result.State = Ready then
               --  Cursor = the tail of plane 1's old range, plane 1 keeps
               --  the head: disjoint, contiguous, never beyond the old range.
               Decode (Plane_Buffer).Valid and then
               Plan'Result.Plane_Before = Decode (Plane_Buffer).Value and then
               Plan'Result.Plane_After.First = Plan'Result.Plane_Before.First and then
               Plan'Result.Plane_After.Stop = Plan'Result.Cursor.First and then
               Plan'Result.Cursor.Stop = Plan'Result.Plane_Before.Stop and then
               Plan'Result.Plane_After.First < Plan'Result.Plane_After.Stop and then
               Plan'Result.Cursor.First < Plan'Result.Cursor.Stop and then
               Size (Plan'Result.Cursor) >=
                 (if Single_Pipe then Single_Pipe_Minimum else Several_Pipe_Minimum) and then
               Size (Plan'Result.Cursor) <= Cursor_Limit and then
               Disjoint (Plan'Result.Plane_After, Plan'Result.Cursor) and then
               (for all P in Other_Planes'Range =>
                  Decode (Other_Buffers (P)).Valid and then
                  Disjoint (Decode (Other_Buffers (P)).Value, Plan'Result.Cursor)) and then
               Plan'Result.Plane_Buffer = Encode (Plan'Result.Plane_After) and then
               Plan'Result.Cursor_Buffer = Encode (Plan'Result.Cursor) and then
               (for all S in Watermark_Slot =>
                  Fits (Plan'Result.Plane_WM (S), Size (Plan'Result.Plane_After)) and then
                  Fits (Plan'Result.Cursor_WM (S), Size (Plan'Result.Cursor))) and then
               Enabled (Plan'Result.Plane_WM (Level_0)));

   function Name (Item : Outcome) return String is
     (case Item is
        when Plane_Range_Invalid => "plane-1-range-invalid",
        when Plane_Too_Small => "plane-1-too-small",
        when Overlaps_Other_Plane => "overlaps-another-plane",
        when Cursor_Already_Allocated => "cursor-already-allocated",
        when Ready => "ready");
end Intel_GPU_Cursor_DDB;
