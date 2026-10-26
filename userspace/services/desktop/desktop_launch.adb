------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with CuBit.Canonical_Readers;
with CCL.Setting_Codecs.Launch_Entry;

package body Desktop_Launch is
   package DL renames CCL.Interfaces.Desktop_Launch;
   package Codec renames CCL.Setting_Codecs.Launch_Entry;
   use type Codec.Launch_Action_Choice;
   use type Codec.App_Color_Choice;
   use type Codec.Color_Channel_Value;

   --  The schema's categories are App_Category's, one to one.
   function Group_Of (Member : Codec.App_Category_Value) return DL.App_Category is
     (case Member is
         when Codec.System => DL.System, when Codec.Development => DL.Development, when Codec.Web => DL.Web,
         when Codec.Games => DL.Games, when Codec.Media => DL.Media, when Codec.Tools => DL.Tools);

   --  An RGB value's channels (Color_Channel: 0 .. 255, by its range type).
   Channel_Span : constant := 16#100#;
   pragma Compile_Time_Error
     (Codec.Color_Channel_Value'First /= 0 or else Codec.Color_Channel_Value'Last /= Channel_Span - 1
      or else RGB'Last /= Channel_Span ** 3 - 1,
      "an App_Color channel must be one byte of an RGB");

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

   procedure Decode (Source : String; Item : out Entry_Info; Success : out Boolean) is
      Setting : Codec.Launch_Entry_Value;
      Icon_Found : Boolean := False;
   begin
      Item := (others => <>);
      Codec.Decode (Source, Setting, Success);
      if not Success then
         return;
      end if;
      Success := False;
      declare
         Label : constant String := CuBit.Canonical_Readers.Image (Setting.label);
         Icon : constant String := CuBit.Canonical_Readers.Image (Setting.icon.name);
      begin
         if Label'Length not in 1 .. Maximum_Label then
            return;
         end if;
         Item.Label (1 .. Label'Length) := Label;
         Item.Label_Length := Label'Length;
         if Setting.action.Choice = Codec.Program then
            declare
               Program : constant String := CuBit.Canonical_Readers.Image (Setting.action.Program_Payload);
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
         --  The icon is an atlas icon by its name (the type cannot say so;
         --  the configuration's checker refused any other, and so does this).
         for Member in Desktop_Icons.Icon_ID loop
            if Desktop_Icons.Name_Of (Member) = Icon then
               Item.Icon := Member;
               Icon_Found := True;
            end if;
         end loop;
         if not Icon_Found then
            return;
         end if;
      end;
      Item.Group := Group_Of (Setting.category);
      Item.Single_Instance := Setting.single_instance;
      if Setting.color.Choice = Codec.RGB then
         Item.Color :=
           (Declared => True,
            Value => RGB ((Natural (Setting.color.RGB_Payload.red) * Channel_Span
                           + Natural (Setting.color.RGB_Payload.green)) * Channel_Span
                          + Natural (Setting.color.RGB_Payload.blue)));
      end if;
      Success := True;
   end Decode;
end Desktop_Launch;
