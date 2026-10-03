--  Hosted tests for the desktop Apps menu model (Desktop_Launch).
with Ada.Text_IO; use Ada.Text_IO;
with Desktop_Icons;
with Desktop_Launch; use Desktop_Launch;

procedure Main is
   use type Desktop_Icons.Icon_ID;
   Failures : Natural := 0;

   procedure Check (OK : Boolean; Name : String) is
   begin
      Put_Line ((if OK then "PASS " else "FAIL ") & Name);
      if not OK then Failures := Failures + 1; end if;
   end Check;

   function Parses (Source : String; Item : out Entry_Info) return Boolean is
      OK : Boolean;
   begin
      Parse (Source, Item, OK);
      return OK;
   end Parses;

   Item : Entry_Info;
   Items : Menu;
begin
   Check (Parses ("(launch v1 (label ""Servo"") (program ""cubitshell.app"") (icon files))", Item)
          and then Label_Of (Item) = "Servo" and then Program_Of (Item) = "cubitshell.app"
          and then Item.Kind = Launch_Program and then Item.Icon = Desktop_Icons.Files
          and then not Item.Single_Instance, "program entry");
   Check (Parses ("(launch v1 (label ""DOOM"") (program ""doom.elf"") (icon doom) (single-instance))", Item)
          and then Item.Single_Instance and then Item.Icon = Desktop_Icons.Doom,
          "single-instance entry");
   Check (Parses ("  (launch v1" & ASCII.LF & "  (label ""Settings"") (internal settings))  ", Item)
          and then Item.Kind = Internal_Settings, "internal entry, whitespace");
   Check (not Parses ("(launch v1 (label ""X""))", Item), "no program or internal is rejected");
   Check (not Parses ("(launch v1 (label ""X"") (program ""a.app"") (internal settings))", Item),
          "program and internal together are rejected");
   Check (not Parses ("(launch v1 (label ""X"") (program ""a/b.app""))", Item),
          "paths are rejected");
   Check (not Parses ("(launch v1 (label ""X"") (program ""@nvme:0/a.app""))", Item),
          "volume names are rejected");
   Check (not Parses ("(launch v2 (label ""X"") (program ""a.app""))", Item), "other versions are rejected");
   Check (not Parses ("(launch v1 (label ""X"") (program ""a.app"") (color red))", Item),
          "unknown clauses are rejected");
   Check (not Parses ("(launch v1 (label """ & [1 .. 40 => 'x'] & """) (program ""a.app""))", Item),
          "overlong labels are rejected");
   Check (not Parses ("(launch v1 (label ""X"") (program ""a.app"")", Item), "unclosed entry is rejected");
   Check (not Parses ("(launch v1 (label ""X"") (program ""a.app"")) trailing", Item),
          "trailing text is rejected");
   Check (not Parses ("(launch v1 (label ""a\""b"") (program ""a.app""))", Item), "escapes are rejected");
   Check (not Parses ("", Item), "empty value is rejected");

   Check (Parses ("(launch v1 (label ""Penny"") (program ""cubitshell.app"") (icon penny))", Item)
          and then Item.Icon = Desktop_Icons.Penny, "Penny icon configuration");
   Items := Defaults;
   Check (Items.Count = 9 and then Label_Of (Items.Entries (1)) = "CCL Workbench"
          and then Items.Entries (8).Kind = Internal_Settings, "built-in list");
   Check (Label_Of (Items.Entries (4)) = "Penny" and then
          Program_Of (Items.Entries (4)) = "cubitshell.app" and then
          Items.Entries (4).Icon = Desktop_Icons.Penny and then
          Program_Of (Items.Entries (5)) = "netsurf.app", "Penny and NetSurf separate entries");
   declare
      First : constant Entry_Info := Items.Entries (1);
   begin
      for I in 1 .. 20 loop
         Append (Items, First);
      end loop;
   end;
   Check (Items.Count = Maximum_Entries, "append stops at the bound");

   Put_Line (if Failures = 0 then "DESKTOP-LAUNCH: PASS" else "DESKTOP-LAUNCH: FAIL");
end Main;
