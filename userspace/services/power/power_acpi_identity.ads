------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The stable identity of an ACPI power source (PWR-004): its namespace
--  path as acpi.svc's observer page 23 gives it, and the source name
--  power.svc derives from it. Pure, without I/O.
--
--  @description
--  ACPI_Power_Identity_Protocol (shared/acpi; tests/power-sources checks
--  these numbers against it): Read_Metrics [23, index, byte offset,
--  expected family generation], index 0 the AC adapter page 21 measures,
--  1 .. 16 the battery slots of page 22; the expected generation is that
--  family's cache generation (page 21's summary, page 22's inventory).
--  Answered 16#F000# [family generation now, result, total length, seven
--  bytes little endian]. A path is canonical: a backslash, then four
--  character name segments joined by dots ("\_SB_.PCI0.LPCB.EC0_.BAT0").
--  It names a firmware location across slot reordering, not a physical
--  battery across a replacement, and grants nothing.
--
--  The name (CuBit.Power_Sources.Source_Name, sixteen characters): the
--  path's last segment without its padding underscores ("BAT0", "ADP1",
--  "AC"), as Linux names ACPI power supplies. Where two sources of one
--  snapshot share it, the last two segments ("EC0.BAT0"); where those are
--  shared too, or the path is unknown, the slot name (Power_Source_Mapping:
--  "battery-N", "ac-adapter") stays.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Power_Sources;

package Power_ACPI_Identity with Pure, SPARK_Mode is
   package PS renames CuBit.Power_Sources;
   use type PS.Source, PS.Source_Name;

   subtype Word is Unsigned_64;
   type Words is array (0 .. 3) of Word;

   --  ACPI_Power_Identity_Protocol.
   Identity_Page : constant Word := 23;
   Maximum_Battery_Index : constant := 16;
   Maximum_Path_Characters : constant := 320;
   Chunk_Characters : constant := 7;
   type Result_Code is (OK, Not_Ready, Stale, Index_Out_Of_Range, Too_Long, Bad_Offset);

   --  0: the AC adapter; 1 .. 16: a battery slot.
   AC_Index : constant := 0;
   subtype Source_Index is Natural range 0 .. Maximum_Battery_Index;

   subtype Path_Length is Natural range 0 .. Maximum_Path_Characters;
   subtype Path_Position is Positive range 1 .. Maximum_Path_Characters;
   --  A canonical path's characters.
   function Path_Character (C : Character) return Boolean is
     (C in '\' | '.' | '_' | 'A' .. 'Z' | '0' .. '9');
   type Path_Text is array (Path_Position) of Character;
   type Path is record
      Length : Path_Length := 0;   --  0: unknown
      Text   : Path_Text := [others => ' '];
   end record
     with Predicate => (for all I in 1 .. Path.Length => Path_Character (Path.Text (I)));
   Unknown_Path : constant Path := (Length => 0, Text => [others => ' ']);

   function Same (A, B : Path) return Boolean is
     (A.Length = B.Length and then (for all I in 1 .. A.Length => A.Text (I) = B.Text (I)));

   function Request (Index : Source_Index; Offset : Path_Length; Expected : Word) return Words is
     ([Identity_Page, Word (Index), Word (Offset), Expected]);

   --  An OK-labelled reply's words: Valid when they are words acpi.svc
   --  writes (Code then its result; Total the path's length, OK only).
   procedure Decode
     (Data : Words; Generation : out Word; Code : out Result_Code; Total : out Path_Length; Valid : out Boolean)
   with Post => (if Valid and then Code /= OK then Total = 0);

   --  The chunk at Offset (P.Length) of a path Total long appended to P.
   --  Valid False, and P unchanged, when a character is not a path's or a
   --  byte past Total is not zero.
   procedure Append
     (P : in out Path; Total : Path_Length; Chunk : Word; Valid : out Boolean)
   with Pre  => P.Length < Total,
        Post => (if Valid then P.Length = Natural'Min (P.Length'Old + Chunk_Characters, Total)
                 else P = P'Old);

   --  The names a path gives (Length 0: none).
   function Short_Name (P : Path) return PS.Source_Name;
   function Long_Name (P : Path) return PS.Source_Name;

   --  Each source's path, in the snapshot's order (Unknown_Path: none).
   type Path_Array is array (PS.Source_Index) of Path;

   --  Each source with a path named from it where the name is its alone
   --  (see the description); every other field, and every source without
   --  a path, unchanged.
   procedure Name_Sources
     (Sources : in out PS.Source_Array; Count : PS.Source_Count; Paths : Path_Array)
   with Post => (for all I in PS.Source_Index =>
                   Sources (I) = (Sources'Old (I) with delta Name => Sources (I).Name)
                   and then (if I > Count or else Paths (I).Length = 0
                             then Sources (I).Name = Sources'Old (I).Name));
end Power_ACPI_Identity;
