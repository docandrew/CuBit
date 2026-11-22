------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The libc's diagnostics (docs/c-removal.md): one console line the first
--  time a program uses something the libc does not support, by what it was
--  and its value. Later uses are counted out (Seen).
--
--  @description
--  Proved (tests/libc-ada): lines fit their buffer whatever the input, and
--  every Integer_64 value prints, its most negative one included.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Libc_Reports with Pure, SPARK_Mode is

   --  The longest Prefix, and the longest What kept (longer text is cut).
   What_Bytes : constant := 32;
   --  "cubit-libc: ", Prefix, ' ', What, ' ', '-', 20 digits, line feed.
   Line_Bytes : constant := 12 + What_Bytes + 1 + What_Bytes + 1 + 1 + 20 + 1;
   subtype Line_Length is Natural range 0 .. Line_Bytes;
   subtype Line is String (1 .. Line_Bytes);

   --  "cubit-libc: <Prefix> <What> <Value>" and a line feed.
   procedure Format
     (Prefix, What : String; Value : Integer_64;
      Result : out Line; Length : out Line_Length)
   with Pre => Prefix'Length <= What_Bytes and then What'Last < Positive'Last,
        Post => Length >= 1;

   --  The (What, Value) pairs already reported.
   Maximum_Seen : constant := 32;
   type Seen_Entry is record
      What   : String (1 .. What_Bytes) := [others => ' '];
      Length : Natural range 0 .. What_Bytes := 0;
      Value  : Integer_64 := 0;
   end record;
   type Seen_Entries is array (1 .. Maximum_Seen) of Seen_Entry;
   type Seen_Table is record
      Entries : Seen_Entries;
      Count   : Natural range 0 .. Maximum_Seen := 0;
   end record;

   --  Whether (What, Value) is new; it is then remembered (while room
   --  lasts: past that, every report is new).
   procedure First_Time
     (Seen : in out Seen_Table; What : String; Value : Integer_64;
      Is_New : out Boolean)
   with Pre => What'Last < Positive'Last;

end CuBit.Libc_Reports;
