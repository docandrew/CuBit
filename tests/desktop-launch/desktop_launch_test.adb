--  Hosted tests for the desktop Apps menu model (Desktop_Launch) and its
--  typed settings: entries are CCL Launch_Entry values, checked when the
--  configuration compiles (CCL.Configurations, CCL.Typed_Settings) and read
--  back only in their canonical form.
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;
with CCL.Configurations;
with CCL.Interfaces.Desktop_Launch;
with Desktop_Icons;
with Desktop_Launch; use Desktop_Launch;
with Desktop_Launch_Menus;

procedure Desktop_Launch_Test is
   use type Desktop_Icons.Icon_ID;
   use type CCL.Configurations.Diagnostic_Code;
   use type CCL.Interfaces.Desktop_Launch.App_Category;
   Failures : Natural := 0;

   procedure Check (OK : Boolean; Name : String) is
   begin
      Put_Line ((if OK then "PASS " else "FAIL ") & Name);
      if not OK then Failures := Failures + 1; end if;
   end Check;

   --  A one-setting profile, compiled: the stored (canonical) value, or "".
   function Stored (Value : String; Key : String := "desktop.launch.10-x") return String is
      Result : CCL.Configurations.Compilation_Result;
   begin
      CCL.Configurations.Compile ("(system-config v1 (setting """ & Key & """ " & Value & "))", Result);
      if not Result.Success then
         Put_Line ("  refused: " & CCL.Configurations.Diagnostic_Name (Result.Diagnostic) & " "
                   & Result.Typed_Message (1 .. Result.Typed_Message_Length));
         return "";
      end if;
      return Result.Plan.Settings (1).Value.Data (1 .. Result.Plan.Settings (1).Value.Length);
   end Stored;

   function Decodes (Source : String; Item : out Entry_Info) return Boolean is
      OK : Boolean;
   begin
      Decode (Source, Item, OK);
      return OK;
   end Decodes;

   Item : Entry_Info;
   Items : Menu;
   Penny : constant String :=
     Stored ("(Launch_Entry label => ""Penny"" action => (Launch_Action.Program ""cubitshell.app"") " &
             "icon => Launch_Icon.Penny category => App_Category.Web)");
begin
   Check (Penny = "(Launch_Entry label => ""Penny"" action => (Launch_Action.Program ""cubitshell.app"") "
                  & "icon => Launch_Icon.Penny category => App_Category.Web single_instance => false)",
          "a typed entry compiles to its canonical form: " & Penny);
   Check (Decodes (Penny, Item) and then Label_Of (Item) = "Penny" and then Program_Of (Item) = "cubitshell.app"
          and then Item.Kind = Launch_Program and then Item.Icon = Desktop_Icons.Penny
          and then Item.Group = CCL.Interfaces.Desktop_Launch.Web and then not Item.Single_Instance,
          "the desktop reads it back");
   declare
      Doom : constant String :=
        Stored ("(Launch_Entry ""DOOM"" (Launch_Action.Program ""doom.elf"") Launch_Icon.Doom App_Category.Games true)");
   begin
      Check (Decodes (Doom, Item) and then Item.Single_Instance and then Item.Icon = Desktop_Icons.Doom
             and then Item.Group = CCL.Interfaces.Desktop_Launch.Games, "positional fields, single instance");
   end;
   declare
      Settings : constant String := Stored ("(Launch_Entry label => ""Settings"" action => Launch_Action.Settings)");
   begin
      Check (Decodes (Settings, Item) and then Item.Kind = Internal_Settings
             and then Item.Group = CCL.Interfaces.Desktop_Launch.Tools and then Item.Icon = Desktop_Icons.Files,
             "defaults: Tools, the Files icon; internal settings");
   end;
   --  Refused when compiling.
   Check (Stored ("(Launch_Entry label => ""X"" action => Launch_Action.Settings category => App_Category.Toys)") = "",
          "an undeclared category is refused");
   Check (Stored ("(Launch_Entry label => ""X"")") = "", "a missing action is refused");
   Check (Stored ("(Launch_Entry label => ""X"" action => Launch_Action.Settings color => 3)") = "",
          "an unknown field is refused");
   Check (Stored ("""(launch v1 (label \""X\"") (internal settings))""") = "",
          "the old string form is refused");
   Check (Stored ("42") = "", "a number is refused");
   --  Refused when reading: anything but the canonical form.
   Check (not Decodes ("(Launch_Entry label => ""Penny"" action => (Launch_Action.Program ""cubitshell.app""))", Item),
          "a non-canonical spelling is refused by the reader");
   Check (not Decodes ("(launch v1 (label ""Servo"") (program ""cubitshell.app"") (icon files))", Item),
          "the old format is refused by the reader");
   Check (not Decodes (Stored ("(Launch_Entry label => ""X"" action => (Launch_Action.Program ""a/b.app""))"), Item),
          "a path as program is refused");
   Check (not Decodes (Stored ("(Launch_Entry label => """ & [1 .. 40 => 'x'] & """ action => Launch_Action.Settings)"),
                       Item), "an overlong label is refused");
   Check (not Decodes ("", Item), "an empty value is refused");
   --  Untyped settings are unchanged.
   Check (Stored ("""UTC""", "clock.time-zone") = "UTC", "untyped settings still store their text");

   Items := Defaults;
   Check (Items.Count = 8 and then Label_Of (Items.Entries (1)) = "CCL Workbench"
          and then Items.Entries (7).Kind = Internal_Settings, "built-in list");
   Check (Items.Entries (4).Group = CCL.Interfaces.Desktop_Launch.Web, "built-in entries have categories");
   --  The two-level menu over the built-in list: System (Devices, Settings,
   --  Config Inspector), Development (Workbench), Web (Penny), Games (DOOM,
   --  SameBoy), Tools (Files).
   declare
      package LM renames Desktop_Launch_Menus;
      S : LM.Menu_State;
      Result : LM.Choice;
      Chosen : Natural;
      Changed : Boolean;
      use type LM.Choice;
   begin
      Check (LM.Rows (Items) = 5 and then LM.Category_Name (LM.Category_Of (Items, 3)) = "Web",
             "categories with entries, in declared order");
      Check (LM.Items (Items, 1) = 3 and then Label_Of (Items.Entries (LM.Entry_Of (Items, 1, 2))) = "Settings",
             "a category's entries in key order");
      LM.Reset (S);
      LM.Press (S, Items, LM.Down, Result, Chosen);
      LM.Press (S, Items, LM.Down, Result, Chosen);
      LM.Press (S, Items, LM.Right, Result, Chosen);
      Check (S.In_Submenu and then S.Open = 3 and then S.Item = 1, "Down, Down, Right opens Web");
      LM.Press (S, Items, LM.Enter, Result, Chosen);
      Check (Result = LM.Launch and then Label_Of (Items.Entries (Chosen)) = "Penny", "Enter launches Penny");
      LM.Press (S, Items, LM.Left, Result, Chosen);
      Check (not S.In_Submenu and then S.Open = 0, "Left closes the submenu");
      LM.Press (S, Items, LM.Up, Result, Chosen);
      LM.Press (S, Items, LM.Up, Result, Chosen);
      LM.Press (S, Items, LM.Up, Result, Chosen);
      Check (S.Row = 5, "Up wraps among the categories (Power takes no keys)");
      LM.Press (S, Items, LM.Escape, Result, Chosen);
      Check (Result = LM.Close, "Escape at the top closes the menu");
      LM.Reset (S);
      LM.Hover_Row (S, Items, 4);
      Check (S.Open = 4 and then not S.In_Submenu, "hovering Games opens it at once");
      LM.Hover_Row (S, Items, LM.Power_Row (Items));
      Check (S.Open = 0, "hovering Power closes the submenu");
      LM.Hover_Row (S, Items, 4);
      LM.Hover_Item (S, 2);
      LM.Press (S, Items, LM.Enter, Result, Chosen);
      Check (Result = LM.Launch and then Label_Of (Items.Entries (Chosen)) = "SameBoy", "hover then Enter");
      declare
         Row, Index : Natural;
      begin
         LM.Locate (Items, 2, Row, Index);
         Check (Row = 4 and then Index = 1, "DOOM is Games' first entry");
      end;
      LM.Click_Row (S, Items, LM.Power_Row (Items), Result);
      Check (Result = LM.Power, "a click on Power chooses it");
   end;
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Desktop_Launch_Test;
