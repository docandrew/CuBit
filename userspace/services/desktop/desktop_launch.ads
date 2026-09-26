------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Apps (launch) menu's entries, read from Config.
--
--  Each `desktop.launch.<key>` setting is one entry, in key order (so keys
--  like "10-workbench", "40-browser" order the menu). A value is CCL:
--
--    (launch v1 (label "Servo") (program "cubitshell.app") (icon files))
--    (launch v1 (label "DOOM") (program "doom.elf") (icon doom)
--      (single-instance))
--    (launch v1 (label "Settings") (internal settings))
--
--  Labels and program names are bounded; an invalid entry is skipped and
--  reported. Without any valid entry the built-in list is used. The menu
--  names programs only; what a launched program may do is decided by
--  procmgr's launch policy and the program's manifest, never by this list.
------------------------------------------------------------------------------
with Desktop_Icons;

package Desktop_Launch is
   Maximum_Entries : constant := 14;
   Maximum_Label   : constant := 32;
   Maximum_Program : constant := 64;

   type Entry_Kind is (Launch_Program, Internal_Settings);

   type Entry_Info is record
      Kind            : Entry_Kind := Launch_Program;
      Label           : String (1 .. Maximum_Label) := [others => ' '];
      Label_Length    : Natural range 0 .. Maximum_Label := 0;
      Program         : String (1 .. Maximum_Program) := [others => ' '];
      Program_Length  : Natural range 0 .. Maximum_Program := 0;
      Icon            : Desktop_Icons.Icon_ID := Desktop_Icons.Files;
      Single_Instance : Boolean := False;
   end record;

   subtype Entry_Index is Positive range 1 .. Maximum_Entries;
   type Entry_Array is array (Entry_Index) of Entry_Info;

   type Menu is record
      Entries : Entry_Array;
      Count   : Natural range 0 .. Maximum_Entries := 0;
   end record;

   --  One entry from its CCL value. Success is False for anything outside
   --  the grammar above or its bounds.
   --  (Tested on the host: tests/desktop-launch; not SPARK-proved.)
   procedure Parse (Source : String; Item : out Entry_Info; Success : out Boolean)
     with Pre => Source'Last < Natural'Last;

   --  The built-in list (the menu before Config held it).
   function Defaults return Menu;

   --  Add an entry if there is room.
   procedure Append (Items : in out Menu; Item : Entry_Info);

   function Label_Of (Item : Entry_Info) return String is
     (Item.Label (1 .. Item.Label_Length));

   function Program_Of (Item : Entry_Info) return String is
     (Item.Program (1 .. Item.Program_Length));
end Desktop_Launch;
