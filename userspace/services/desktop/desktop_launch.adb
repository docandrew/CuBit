------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with Interfaces;
with CCL.Objects.Views;
with CCL.Typed_Settings;

package body Desktop_Launch is
   package DL renames CCL.Interfaces.Desktop_Launch;
   --  The schema's category names (enumeration 'Image is numeric natively).
   function Name_Of (Member : DL.App_Category) return String is
     (case Member is
         when DL.System => "System", when DL.Development => "Development", when DL.Web => "Web",
         when DL.Games => "Games", when DL.Media => "Media", when DL.Tools => "Tools");

   --  The built-in list's app colours (CCL App_Color.RGB in system.ccl).
   Workbench_Color : constant RGB := 16#2F4F9E#;
   Doom_Color      : constant RGB := 16#B3261A#;
   Devices_Color   : constant RGB := 16#5FB848#;
   Penny_Color     : constant RGB := 16#B86A3A#;
   Files_Color     : constant RGB := 16#E0A21F#;
   SameBoy_Color   : constant RGB := 16#74749A#;
   Settings_Color  : constant RGB := 16#3D7CC9#;
   Inspector_Color : constant RGB := 16#7B3FD6#;

   procedure Append (Items : in out Menu; Item : Entry_Info) is
   begin
      if Items.Count < Maximum_Entries then
         Items.Count := Items.Count + 1;
         Items.Entries (Items.Count) := Item;
      end if;
   end Append;

   function Make
     (Label   : String;
      Program : String;
      Icon    : Desktop_Icons.Icon_ID;
      Group   : Category;
      Color   : RGB;
      Kind    : Entry_Kind := Launch_Program;
      Single  : Boolean := False) return Entry_Info
   is
      Item : Entry_Info;
   begin
      Item.Kind := Kind;
      Item.Label (1 .. Label'Length) := Label;
      Item.Label_Length := Label'Length;
      Item.Program (1 .. Program'Length) := Program;
      Item.Program_Length := Program'Length;
      Item.Icon := Icon;
      Item.Single_Instance := Single;
      Item.Group := Group;
      Item.Color := (Declared => True, Value => Color);
      return Item;
   end Make;

   function Defaults return Menu is
      use Desktop_Icons;
      Items : Menu;
   begin
      --  The same colours as system.ccl's entries.
      Append (Items, Make ("CCL Workbench", "ccl-workbench.app", Workbench, DL.Development, Workbench_Color));
      Append (Items, Make ("DOOM", "doom.elf", Doom, DL.Games, Doom_Color, Single => True));
      Append (Items, Make ("Devices", "devices.app", Devices, DL.System, Devices_Color));
      Append (Items, Make ("Penny", "cubitshell.app", Penny, DL.Web, Penny_Color));
      Append (Items, Make ("Files", "files.app", Files, DL.Tools, Files_Color));
      Append (Items, Make ("SameBoy", "sameboy.app", Gameboy, DL.Games, SameBoy_Color));
      Append (Items, Make ("Settings", "", Settings, DL.System, Settings_Color, Kind => Internal_Settings));
      Append (Items, Make ("Config Inspector", "config-inspector.app", Inspector, DL.System, Inspector_Color));
      return Items;
   end Defaults;

   type Snapshot_Access is access CCL.Objects.Views.Snapshot;
   Work : Snapshot_Access;

   procedure Decode (Source : String; Item : out Entry_Info; Success : out Boolean) is
      package Views renames CCL.Objects.Views;
      package TS renames CCL.Typed_Settings;
      Read : Boolean;
   begin
      Item := (others => <>);
      Success := False;
      if Work = null then
         Work := new Views.Snapshot;
      end if;
      TS.Read (TS.Launch_Entry_Setting, Source, Work.all, Read);
      if not Read then
         return;
      end if;
      declare
         Root : constant Views.Cursor := Views.Root (Work.all);
         Label : constant String := Views.Text (Work.all, TS.Named (Work.all, Root, "label"));
         Action : constant Views.Cursor := TS.Named (Work.all, Root, "action");
         Icon : constant String :=
           Views.Text (Work.all, TS.Named (Work.all, TS.Named (Work.all, Root, "icon"), "name"));
         Icon_Found : Boolean := False;
         Group : constant String := TS.Alternative_Name (Work.all, TS.Named (Work.all, Root, "category"));
         Color : constant Views.Cursor := TS.Named (Work.all, Root, "color");
         --  A Color_Channel: 0 .. 255 by its CCL range type, checked again.
         function Channel (Name : String) return Interfaces.Unsigned_64 is
           (Views.Scalar (Work.all, TS.Named (Work.all, Views.Payload (Work.all, Color), Name)).First);
      begin
         if Label'Length not in 1 .. Maximum_Label then
            return;
         end if;
         Item.Label (1 .. Label'Length) := Label;
         Item.Label_Length := Label'Length;
         if TS.Alternative_Name (Work.all, Action) = "Program" then
            declare
               Program : constant String := Views.Text (Work.all, Views.Payload (Work.all, Action));
            begin
               --  A program name, not a path: procmgr resolves it.
               if Program'Length not in 1 .. Maximum_Program
                 or else (for some C of Program => C not in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.')
               then
                  return;
               end if;
               Item.Program (1 .. Program'Length) := Program;
               Item.Program_Length := Program'Length;
               Item.Kind := Launch_Program;
            end;
         else
            Item.Kind := Internal_Settings;
         end if;
         --  The icon is an atlas icon by its name (the checker refused any
         --  other); the category's members map one to one onto App_Category.
         for Member in Desktop_Icons.Icon_ID loop
            if Desktop_Icons.Name_Of (Member) = Icon then
               Item.Icon := Member;
               Icon_Found := True;
            end if;
         end loop;
         if not Icon_Found then
            return;
         end if;
         for Member in DL.App_Category loop
            if Name_Of (Member) = Group then
               Item.Group := Member;
            end if;
         end loop;
         Item.Single_Instance := Interfaces."=" (Views.Scalar (Work.all, TS.Named (Work.all, Root, "single_instance")).First, 1);
         if TS.Alternative_Name (Work.all, Color) = "RGB" then
            declare
               use type Interfaces.Unsigned_64;
               Red : constant Interfaces.Unsigned_64 := Channel ("red");
               Green : constant Interfaces.Unsigned_64 := Channel ("green");
               Blue : constant Interfaces.Unsigned_64 := Channel ("blue");
            begin
               if Red > 255 or else Green > 255 or else Blue > 255 then
                  return;
               end if;
               Item.Color := (Declared => True, Value => RGB (Red * 16#1_0000# + Green * 16#100# + Blue));
            end;
         end if;
         Success := True;
      end;
   end Decode;
end Desktop_Launch;
