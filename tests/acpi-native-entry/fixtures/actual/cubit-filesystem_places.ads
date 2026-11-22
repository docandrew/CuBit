------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Queue_List_Places (CuBit.Filesystem_Queues): which of a caller's granted
--  scopes are places, that is, name a volume that is present now. A
--  manifest grants "@nvme:0/" whether or not this machine has an NVMe
--  disk; a browser shows only what can be opened.
--
--  @description
--  The service lists its present volumes' roots ("@mem:0/", "@boot/") and
--  this unit selects, in profile order, every volume-qualified scope whose
--  volume is one of them; the bootstrap wildcard (an empty prefix) becomes
--  one entry per present root, with the wildcard's rights. The output is
--  CuBit.File_Access wire entries (rights, prefix length, reserved zeroes,
--  zero-padded prefix), so a client decodes it with File_Access.Decode.
--  Unqualified scopes ("apps/") name no volume and are never places.
--
--  Proved (tests/files-app, run.sh --prove, level 2): no run-time errors;
--  Select_Places writes only whole entries inside its output. Tested
--  (tests/files-app): absent volumes are left out, profile order is kept,
--  the wildcard expands, Count_Places is the number Select_Places writes.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.File_Access;
with CuBit.Volume_Descriptions;

package CuBit.Filesystem_Places with SPARK_Mode is
   package FA renames CuBit.File_Access;

   --  A volume's root: '@', its name, '/'.
   Volume_Mark : constant Character := '@';
   Root_End    : constant Character := '/';
   Maximum_Root_Bytes : constant :=
     CuBit.Volume_Descriptions.Maximum_Name_Bytes + 2;
   subtype Root_Length is Natural range 0 .. Maximum_Root_Bytes;
   subtype Volume_Name_Length is
     Natural range 1 .. CuBit.Volume_Descriptions.Maximum_Name_Bytes;
   type Volume_Root is record
      Text   : String (1 .. Maximum_Root_Bytes) := [others => ' '];
      Length : Root_Length := 0;
   end record;
   type Root_Table is array (Positive range <>) of Volume_Root;

   --  "@" & Name & "/".
   function Make_Root (Name : String) return Volume_Root
     with Pre  => Name'Length in Volume_Name_Length,
          Post => Make_Root'Result.Length = Name'Length + 2;

   --  The scope Prefix is on Root's volume: Root itself, Root without its
   --  slash, or a path below it.
   function On_Root (Root : Volume_Root; Prefix : String) return Boolean;

   --  Wire entry fields (offsets within one entry, as File_Access.Decode
   --  reads them).
   Rights_At        : constant := 0;
   Prefix_Length_At : constant := 1;   --  u16, little-endian
   Prefix_At        : constant := FA.Wire_Header_Bytes;
   Byte_Radix       : constant := 256;

   subtype Scope_Count is Natural range 0 .. FA.Maximum_Entries;
   --  The output: whole wire entries from Places'First.
   type Place_Bytes is array (Positive range <>) of Unsigned_8;

   --  Most present roots a caller may pass: what keeps every count and
   --  byte offset below in range (not a policy bound).
   Maximum_Roots : constant :=
     Natural'Last / (FA.Maximum_Entries * FA.Wire_Entry_Bytes);

   --  How many places Select_Places lists for Scopes (the first Count
   --  entries) and Present.
   function Count_Places
     (Scopes : FA.Wire_Bytes; Count : Scope_Count; Present : Root_Table)
      return Natural
     with Pre  => Scopes'First = 1
                  and then Count * FA.Wire_Entry_Bytes <= Scopes'Length
                  and then Present'Length <= Maximum_Roots,
          Post => Count_Places'Result <=
                    Count * Natural'Max (1, Present'Length);

   --  The places, as wire entries from Places'First; Written of them. Only
   --  whole entries are written, and only inside Places; bytes after the
   --  last are zero.
   procedure Select_Places
     (Scopes  : FA.Wire_Bytes;
      Count   : Scope_Count;
      Present : Root_Table;
      Places  : out Place_Bytes;
      Written : out Natural)
     with Pre  => Scopes'First = 1 and then Places'First = 1
                  and then Count * FA.Wire_Entry_Bytes <= Scopes'Length
                  and then Present'Length <= Maximum_Roots,
          Post => Written <= Places'Length / FA.Wire_Entry_Bytes;
end CuBit.Filesystem_Places;
