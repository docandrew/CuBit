-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Which frame a debug view may show for one page of its target (DBG-001,
-- docs/debugging.md 3.5): the walk of the target's page tables.
--
-- @description
-- A page is viewable when every level of the target's tables maps it
-- present and user-accessible, and the leaf maps it write-back (PWT, PCD
-- and PAT clear: PAT entry 0, write-back, PerCPUData's PAT). A 2 MiB leaf
-- gives the 4 KiB frame inside it; a 1 GiB leaf is refused. The kernel
-- adapter (Debug_Sessions) then pins the frame only if the target owns it
-- (BuddyAllocator.pinOwnedFrame), so device memory, kernel pages and
-- frames another process granted the target are refused there.
--
-- Why write-back only: x86 requires every mapping of a frame to agree on
-- its memory type, and the view maps write-back. Uncached and
-- write-combining pages (device buffers) are never viewed.
--
-- Proved (gnatprove level 2, tests/debug-sessions): a Viewable result names
-- a page-aligned RAM frame, the address is a user address, and the leaf
-- that gave it was present, user-accessible and write-back.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Debug_View_Walk with SPARK_Mode, Pure is
   type Level is (P4_Level, P3_Level, P2_Level, P1_Level);
   subtype Table_Index is Natural range 0 .. 511;

   Page_Size        : constant Unsigned_64 := 4096;
   Large_Page_Size  : constant Unsigned_64 := 16#20_0000#;
   User_Limit       : constant Unsigned_64 := 16#0000_8000_0000_0000#;
   Present_Bit      : constant Unsigned_64 := 2 ** 0;
   User_Bit         : constant Unsigned_64 := 2 ** 2;
   Write_Through_Bit : constant Unsigned_64 := 2 ** 3;
   Cache_Disable_Bit : constant Unsigned_64 := 2 ** 4;
   Large_Page_Bit   : constant Unsigned_64 := 2 ** 7;
   -- PAT selects the memory type with PCD and PWT: bit 7 in a 4 KiB leaf,
   -- bit 12 in a 2 MiB leaf (where bit 7 is the large-page bit).
   PAT_Small_Bit    : constant Unsigned_64 := 2 ** 7;
   PAT_Large_Bit    : constant Unsigned_64 := 2 ** 12;
   Frame_Mask       : constant Unsigned_64 := 16#000F_FFFF_FFFF_F000#;
   Large_Frame_Mask : constant Unsigned_64 := 16#000F_FFFF_FFE0_0000#;
   Large_Offset_Mask : constant Unsigned_64 := 16#1F_F000#;

   Required : constant Unsigned_64 := Present_Bit or User_Bit;

   type Verdict is (Viewable, Unmapped, Not_Write_Back);

   type Result is record
      Frame : Unsigned_64 := 0;
      Kind  : Verdict := Unmapped;
      Leaf  : Unsigned_64 := 0;     -- the leaf entry that gave Frame
      Large : Boolean := False;     -- that leaf maps 2 MiB
   end record;

   function RAM_Page (Frame, Physical_Last : Unsigned_64) return Boolean is
     (Frame /= 0 and then Frame mod Page_Size = 0 and then
      Frame <= Physical_Last and then Page_Size - 1 <= Physical_Last - Frame);

   -- A leaf word's memory type is write-back.
   function Write_Back (Word : Unsigned_64; Large : Boolean) return Boolean is
     ((Word and (Write_Through_Bit or Cache_Disable_Bit)) = 0
      and then (Word and (if Large then PAT_Large_Bit else PAT_Small_Bit)) = 0);

   -- The 4 KiB frame a leaf word gives for Address.
   function Leaf_Frame (Word, Address : Unsigned_64; Large : Boolean) return Unsigned_64 is
     (if Large then (Word and Large_Frame_Mask) + (Address and Large_Offset_Mask)
      else Word and Frame_Mask)
   with Post => Leaf_Frame'Result mod Page_Size = 0;

   generic
      -- Reads one coherent entry from an existing, lifetime-protected table.
      with procedure Read_Entry
        (Table_Frame : Unsigned_64; Index : Table_Index; Word : out Unsigned_64);
   function Walk (Root, Address, Physical_Last : Unsigned_64) return Result
     with Post =>
       (if Walk'Result.Kind = Viewable then
          Address < User_Limit and then RAM_Page (Walk'Result.Frame, Physical_Last)
          and then (Walk'Result.Leaf and Required) = Required
          and then Write_Back (Walk'Result.Leaf, Walk'Result.Large)
          and then Walk'Result.Frame = Leaf_Frame (Walk'Result.Leaf, Address, Walk'Result.Large)
        else Walk'Result.Frame = 0);
end Debug_View_Walk;
