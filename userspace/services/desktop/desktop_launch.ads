------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Apps (launch) menu's entries, read from Config.
--
--  Each `desktop.launch.<key>` setting is one entry, in key order (so keys
--  like "10-workbench", "40-browser" order the menu). Its value is a typed
--  CCL Launch_Entry (CCL.Interfaces.Desktop_Launch), checked when the
--  configuration is compiled and stored as its canonical source, e.g.
--
--    (Launch_Entry label => "Penny" action => (Launch_Action.Program
--      "cubitshell.app") icon => Launch_Icon.Penny category => App_Category.Web
--      single_instance => false)
--
--  Decode reads it back through the same typed path (CCL.Typed_Settings);
--  anything but the canonical, schema-valid form is refused. Labels and
--  program names are bounded; an invalid entry is skipped and reported. Without any valid entry the built-in list is used. The menu
--  names programs only; what a launched program may do is decided by
--  procmgr's launch policy and the program's manifest, never by this list.
------------------------------------------------------------------------------
with CCL.Interfaces.Desktop_Launch;
with Desktop_Icons;

package Desktop_Launch is
   Maximum_Entries : constant := 14;
   Maximum_Label   : constant := 32;
   Maximum_Program : constant := 64;

   type Entry_Kind is (Launch_Program, Internal_Settings);
   subtype Category is CCL.Interfaces.Desktop_Launch.App_Category;

   type Entry_Info is record
      Kind            : Entry_Kind := Launch_Program;
      Label           : String (1 .. Maximum_Label) := [others => ' '];
      Label_Length    : Natural range 0 .. Maximum_Label := 0;
      Program         : String (1 .. Maximum_Program) := [others => ' '];
      Program_Length  : Natural range 0 .. Maximum_Program := 0;
      Icon            : Desktop_Icons.Icon_ID := Desktop_Icons.Files;
      Single_Instance : Boolean := False;
      Group           : Category := CCL.Interfaces.Desktop_Launch.Tools;
   end record;

   subtype Entry_Index is Positive range 1 .. Maximum_Entries;
   type Entry_Array is array (Entry_Index) of Entry_Info;

   type Menu is record
      Entries : Entry_Array;
      Count   : Natural range 0 .. Maximum_Entries := 0;
   end record;

   --  One entry from its stored value (a canonical Launch_Entry). Success
   --  is False for anything else, or a label or program out of bounds.
   --  (Tested on the host: tests/desktop-launch.)
   procedure Decode (Source : String; Item : out Entry_Info; Success : out Boolean);

   --  The built-in list (the menu before Config held it).
   function Defaults return Menu;

   --  Add an entry if there is room.
   procedure Append (Items : in out Menu; Item : Entry_Info);

   function Label_Of (Item : Entry_Info) return String is
     (Item.Label (1 .. Item.Label_Length));

   function Program_Of (Item : Entry_Info) return String is
     (Item.Program (1 .. Item.Program_Length));
end Desktop_Launch;
