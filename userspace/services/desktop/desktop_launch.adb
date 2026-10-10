------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with Interfaces;
with CCL.Objects.Views;
with CCL.Typed_Settings;

package body Desktop_Launch is
   package DL renames CCL.Interfaces.Desktop_Launch;
   --  The schema's member names (enumeration 'Image is numeric natively).
   function Name_Of (Member : DL.Launch_Icon) return String is
     (case Member is
         when DL.Workbench => "Workbench", when DL.Console => "Console", when DL.Logs => "Logs",
         when DL.Trace => "Trace", when DL.Doom => "Doom", when DL.Devices => "Devices",
         when DL.Penny => "Penny", when DL.Files => "Files", when DL.Gameboy => "Gameboy",
         when DL.Settings => "Settings", when DL.Inspector => "Inspector", when DL.Mesa => "Mesa",
         when DL.Boot => "Boot");
   function Name_Of (Member : DL.App_Category) return String is
     (case Member is
         when DL.System => "System", when DL.Development => "Development", when DL.Web => "Web",
         when DL.Games => "Games", when DL.Media => "Media", when DL.Tools => "Tools");

   function Icon_Of (Member : CCL.Interfaces.Desktop_Launch.Launch_Icon) return Desktop_Icons.Icon_ID is
     (case Member is
         when DL.Workbench => Desktop_Icons.Workbench, when DL.Console => Desktop_Icons.Console,
         when DL.Logs => Desktop_Icons.Logs, when DL.Trace => Desktop_Icons.Trace,
         when DL.Doom => Desktop_Icons.Doom, when DL.Devices => Desktop_Icons.Devices,
         when DL.Penny => Desktop_Icons.Penny, when DL.Files => Desktop_Icons.Files,
         when DL.Gameboy => Desktop_Icons.Gameboy, when DL.Settings => Desktop_Icons.Settings,
         when DL.Inspector => Desktop_Icons.Inspector, when DL.Mesa => Desktop_Icons.Mesa,
         when DL.Boot => Desktop_Icons.Boot);

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
      return Item;
   end Make;

   function Defaults return Menu is
      use Desktop_Icons;
      Items : Menu;
   begin
      Append (Items, Make ("CCL Workbench", "ccl-workbench.app", Workbench, DL.Development));
      Append (Items, Make ("DOOM", "doom.elf", Doom, DL.Games, Single => True));
      Append (Items, Make ("Devices", "devices.app", Devices, DL.System));
      Append (Items, Make ("Penny", "cubitshell.app", Penny, DL.Web));
      Append (Items, Make ("Files", "files.app", Files, DL.Tools));
      Append (Items, Make ("SameBoy", "sameboy.app", Gameboy, DL.Games));
      Append (Items, Make ("Settings", "", Settings, DL.System, Kind => Internal_Settings));
      Append (Items, Make ("Config Inspector", "config-inspector.app", Inspector, DL.System));
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
         Icon : constant String := TS.Alternative_Name (Work.all, TS.Named (Work.all, Root, "icon"));
         Group : constant String := TS.Alternative_Name (Work.all, TS.Named (Work.all, Root, "category"));
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
         --  The schema's members map one to one onto these enums.
         for Member in DL.Launch_Icon loop
            if Name_Of (Member) = Icon then
               Item.Icon := Icon_Of (Member);
            end if;
         end loop;
         for Member in DL.App_Category loop
            if Name_Of (Member) = Group then
               Item.Group := Member;
            end if;
         end loop;
         Item.Single_Instance := Interfaces."=" (Views.Scalar (Work.all, TS.Named (Work.all, Root, "single_instance")).First, 1);
         Success := True;
      end;
   end Decode;
end Desktop_Launch;
