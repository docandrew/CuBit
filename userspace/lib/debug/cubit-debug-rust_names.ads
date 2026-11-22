------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Rust type names as people write them (DBG-001): rustc's DWARF names
--  every type with full paths and the default allocator, as in
--  Option<alloc::boxed::Box<debug_demo_rs::Sensor, alloc::alloc::Global>>;
--  shown, that is Option<Box<Sensor>>.
------------------------------------------------------------------------------
pragma Ada_2022;

package CuBit.Debug.Rust_Names with SPARK_Mode is

   Name_Ceiling : constant := 512;

   --  Default generic arguments dropped, then path qualifiers dropped.
   --  Names come from DWARF strings, far shorter than the bound here.
   Raw_Ceiling : constant := 2 ** 20;

   procedure Simplify (Raw : String; Text : out String; Length : out Natural)
   with Pre  => Text'First = 1 and then Text'Length = Name_Ceiling
                and then Raw'Last < Raw_Ceiling,
        Post => Length <= Name_Ceiling;

   function Simplified (Raw : String) return String
   with Pre => Raw'Last < Raw_Ceiling;

end CuBit.Debug.Rust_Names;
