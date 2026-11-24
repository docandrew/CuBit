------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  FAT32 names (FS-030): CuBit's names (UTF-8, at most 255 bytes) to and
--  from VFAT long names (UTF-16, at most 255 units), short names (8.3) for
--  display, case-insensitive matching, the refusal of names Windows cannot
--  hold, and short-name ("alias") generation with numeric tails. Pure and
--  proved (SPARK).
--
--  @description
--  Rules follow FATGEN 1.03 (Microsoft's FAT32 File System Specification):
--  section 6.1 for short-name characters, section 7 for long names, the
--  basis-name generation and numeric tails. A name is refused when Windows
--  could not create or open it: control characters, " * / : < > ? \ |, a
--  trailing dot or space (Windows strips them), and the device names CON,
--  PRN, AUX, NUL, COM1-COM9 and LPT1-LPT9 with or without an extension.
--
--  Matching ignores case as FAT does, by simple upper-casing of ASCII,
--  Latin-1, basic Greek and basic Cyrillic letters. Windows upper-cases by
--  a fuller table; names differing only in the case of other letters are
--  distinct here.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Fat_Format; use Fat_Format;

package Fat_Names with Pure, SPARK_Mode is

   --  CuBit names: UTF-8, as the directory pages carry them.
   Maximum_Name_Bytes : constant := 255;
   subtype Name_Length is Natural range 0 .. Maximum_Name_Bytes;
   subtype Name_Text is String (1 .. Maximum_Name_Bytes);

   type Name_Check is
     (Name_Valid,
      Name_Empty,
      Name_Dots,               --  "." or ".."
      Name_Not_UTF8,           --  malformed, overlong or a surrogate
      Name_Control,            --  a character below U+0020
      Name_Reserved_Character, --  " * / : < > ? \ |
      Name_Trailing,           --  ends in a dot or a space
      Name_Too_Long,           --  more than 255 UTF-16 units
      Name_Device);            --  CON, PRN, AUX, NUL, COM1-9, LPT1-9

   --  Name's UTF-16 units. Strict (a name to create): every rule above.
   --  Otherwise (a name to look up): only well-formed UTF-8 and the length.
   procedure To_Units
     (Name : String; Strict : Boolean; Units : out Long_Units; Length : out Long_Length;
      Result : out Name_Check)
     with Pre  => Name'First = 1 and then Name'Length <= Maximum_Name_Bytes,
          Post => (if Result = Name_Valid then Length >= 1);

   --  The CuBit name of long-name units: False when it holds an unpaired
   --  surrogate, U+0000, a control character or "/", or needs more than
   --  255 bytes. Such an entry is then known by its short name.
   procedure From_Units
     (Units : Long_Units; Length : Long_Length; Text : out Name_Text; Text_Length : out Name_Length;
      OK : out Boolean)
     with Post => (if OK then Text_Length >= 1);

   --  The simple upper case of a UTF-16 unit (see the description).
   function Fold (Unit : Unsigned_16) return Unsigned_16;

   --  A and B are the same name, ignoring case.
   function Same_Name (A : Long_Units; A_Length : Long_Length; B : Long_Units; B_Length : Long_Length)
     return Boolean;

   --  A short entry's name as units: base, then "." and the extension if
   --  any, lower-cased as DIR_NTRes asks, high bytes as code page 437.
   --  False when the base is empty or a byte is a control character or "/".
   Maximum_Short_Units : constant := Base_Bytes + 1 + Extension_Bytes;
   procedure Short_Units
     (Name : Short_Name; Case_Flags : Unsigned_8; Units : out Long_Units; Length : out Long_Length;
      OK : out Boolean)
     with Post => Length <= Maximum_Short_Units and then (if OK then Length >= 1);

   --  The name is a short name as it stands (upper case, 8.3, legal
   --  characters): stored without a long name. Its encoding is Short.
   procedure Exact_Short (Units : Long_Units; Length : Long_Length; Short : out Short_Name; Exact : out Boolean);

   --  The basis of an alias (FATGEN section 7, "basis-name generation"):
   --  upper case, illegal characters as "_", spaces and dots dropped, the
   --  base cut to 8 and the extension (after the last dot) to 3. Lossy
   --  when anything but case changed: then the alias needs a numeric tail.
   subtype Basis_Length is Natural range 1 .. Base_Bytes;
   subtype Basis_Bytes is Byte_Array (0 .. Base_Bytes - 1);
   subtype Extension is Byte_Array (0 .. Extension_Bytes - 1);
   type Basis is record
      Base   : Basis_Bytes := [others => Space];
      Length : Basis_Length := 1;
      Ext    : Extension := [others => Space];
      Lossy  : Boolean := True;
   end record;

   function Make_Basis (Units : Long_Units; Length : Long_Length) return Basis
     with Pre => Length >= 1;

   --  The alias without a tail: the basis as it stands.
   function Plain_Alias (B : Basis) return Short_Name;

   --  The alias with numeric tail N ("BASIS~N.EXT"): the base is cut so
   --  that it and the tail fit in eight bytes.
   Maximum_Tail : constant := 999_999;
   subtype Tail_Number is Positive range 1 .. Maximum_Tail;
   function Alias (B : Basis; N : Tail_Number) return Short_Name;

   --  N when Name is Alias (B, N), else 0. Lets one scan of a directory
   --  find every tail in use for a basis.
   function Tail_Of (Name : Short_Name; B : Basis) return Natural
     with Post => Tail_Of'Result <= Maximum_Tail;

   --  Code page 437's upper half as Unicode (the default OEM code page of
   --  FAT short names). Generated with Python's cp437 codec:
   --    [ord(bytes([b]).decode('cp437')) for b in range(128, 256)]
   type Code_Page_Half is array (Unsigned_8 range 128 .. 255) of Unsigned_16;
   Code_Page_437 : constant Code_Page_Half :=
     [16#00C7#, 16#00FC#, 16#00E9#, 16#00E2#, 16#00E4#, 16#00E0#, 16#00E5#, 16#00E7#,
      16#00EA#, 16#00EB#, 16#00E8#, 16#00EF#, 16#00EE#, 16#00EC#, 16#00C4#, 16#00C5#,
      16#00C9#, 16#00E6#, 16#00C6#, 16#00F4#, 16#00F6#, 16#00F2#, 16#00FB#, 16#00F9#,
      16#00FF#, 16#00D6#, 16#00DC#, 16#00A2#, 16#00A3#, 16#00A5#, 16#20A7#, 16#0192#,
      16#00E1#, 16#00ED#, 16#00F3#, 16#00FA#, 16#00F1#, 16#00D1#, 16#00AA#, 16#00BA#,
      16#00BF#, 16#2310#, 16#00AC#, 16#00BD#, 16#00BC#, 16#00A1#, 16#00AB#, 16#00BB#,
      16#2591#, 16#2592#, 16#2593#, 16#2502#, 16#2524#, 16#2561#, 16#2562#, 16#2556#,
      16#2555#, 16#2563#, 16#2551#, 16#2557#, 16#255D#, 16#255C#, 16#255B#, 16#2510#,
      16#2514#, 16#2534#, 16#252C#, 16#251C#, 16#2500#, 16#253C#, 16#255E#, 16#255F#,
      16#255A#, 16#2554#, 16#2569#, 16#2566#, 16#2560#, 16#2550#, 16#256C#, 16#2567#,
      16#2568#, 16#2564#, 16#2565#, 16#2559#, 16#2558#, 16#2552#, 16#2553#, 16#256B#,
      16#256A#, 16#2518#, 16#250C#, 16#2588#, 16#2584#, 16#258C#, 16#2590#, 16#2580#,
      16#03B1#, 16#00DF#, 16#0393#, 16#03C0#, 16#03A3#, 16#03C3#, 16#00B5#, 16#03C4#,
      16#03A6#, 16#0398#, 16#03A9#, 16#03B4#, 16#221E#, 16#03C6#, 16#03B5#, 16#2229#,
      16#2261#, 16#00B1#, 16#2265#, 16#2264#, 16#2320#, 16#2321#, 16#00F7#, 16#2248#,
      16#00B0#, 16#2219#, 16#00B7#, 16#221A#, 16#207F#, 16#00B2#, 16#25A0#, 16#00A0#];

end Fat_Names;
