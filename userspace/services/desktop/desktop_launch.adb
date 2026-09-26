------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body Desktop_Launch is

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
      return Item;
   end Make;

   function Defaults return Menu is
      use Desktop_Icons;
      Items : Menu;
   begin
      Append (Items, Make ("CCL Workbench", "ccl-workbench.app", UILab));
      Append (Items, Make ("DOOM", "doom.elf", Doom, Single => True));
      Append (Items, Make ("Devices", "devices.app", Files));
      Append (Items, Make ("NetSurf", "netsurf.app", Files));
      Append (Items, Make ("Files", "files.app", Files));
      Append (Items, Make ("SameBoy", "sameboy.app", Doom));
      Append (Items, Make ("Settings", "", UILab, Kind => Internal_Settings));
      Append (Items, Make ("Config Inspector", "config-inspector.app", Files));
      return Items;
   end Defaults;

   procedure Parse (Source : String; Item : out Entry_Info; Success : out Boolean) is
      Pos : Natural := Source'First;
      Has_Label, Has_Program, Has_Internal : Boolean := False;

      function At_End return Boolean is (Pos > Source'Last);

      procedure Skip_Space is
      begin
         while not At_End and then
           Source (Pos) in ' ' | ASCII.HT | ASCII.LF | ASCII.CR
         loop
            Pos := Pos + 1;
         end loop;
      end Skip_Space;

      function Accept_Char (C : Character) return Boolean is
      begin
         Skip_Space;
         if not At_End and then Source (Pos) = C then
            Pos := Pos + 1;
            return True;
         end if;
         return False;
      end Accept_Char;

      --  A bare word: letters, digits, '-', '_'.
      procedure Word (First, Last : out Natural) is
      begin
         Skip_Space;
         First := Pos;
         while not At_End and then
           Source (Pos) in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_'
         loop
            Pos := Pos + 1;
         end loop;
         Last := Pos - 1;
      end Word;

      --  A double-quoted string without escapes or control characters.
      procedure Quoted (First, Last : out Natural; OK : out Boolean) is
      begin
         OK := False;
         First := 1;
         Last := 0;
         if not Accept_Char ('"') then
            return;
         end if;
         First := Pos;
         while not At_End and then Source (Pos) /= '"' loop
            if Source (Pos) < ' ' or else Source (Pos) = '\' then
               return;
            end if;
            Pos := Pos + 1;
         end loop;
         if At_End then
            return;
         end if;
         Last := Pos - 1;
         Pos := Pos + 1;
         OK := True;
      end Quoted;

      function Is_Word (First, Last : Natural; Text : String) return Boolean is
        (Last - First + 1 = Text'Length and then Source (First .. Last) = Text);

      F, L : Natural;
      OK : Boolean;
   begin
      Item := (others => <>);
      Success := False;
      if not Accept_Char ('(') then return; end if;
      Word (F, L);
      if not Is_Word (F, L, "launch") then return; end if;
      Word (F, L);
      if not Is_Word (F, L, "v1") then return; end if;

      loop
         if Accept_Char (')') then
            exit;
         end if;
         if not Accept_Char ('(') then return; end if;
         Word (F, L);
         if Is_Word (F, L, "label") then
            Quoted (F, L, OK);
            if not OK or else Has_Label or else L < F or else
              L - F + 1 > Maximum_Label
            then
               return;
            end if;
            Item.Label (1 .. L - F + 1) := Source (F .. L);
            Item.Label_Length := L - F + 1;
            Has_Label := True;
         elsif Is_Word (F, L, "program") then
            Quoted (F, L, OK);
            if not OK or else Has_Program or else L < F or else
              L - F + 1 > Maximum_Program
            then
               return;
            end if;
            --  A program name, not a path: procmgr resolves it.
            for C of Source (F .. L) loop
               if C not in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.'
               then
                  return;
               end if;
            end loop;
            Item.Program (1 .. L - F + 1) := Source (F .. L);
            Item.Program_Length := L - F + 1;
            Has_Program := True;
         elsif Is_Word (F, L, "icon") then
            Word (F, L);
            if Is_Word (F, L, "start") then Item.Icon := Desktop_Icons.Start;
            elsif Is_Word (F, L, "console") then Item.Icon := Desktop_Icons.Console;
            elsif Is_Word (F, L, "uilab") then Item.Icon := Desktop_Icons.UILab;
            elsif Is_Word (F, L, "doom") then Item.Icon := Desktop_Icons.Doom;
            elsif Is_Word (F, L, "security") then Item.Icon := Desktop_Icons.Security;
            elsif Is_Word (F, L, "files") then Item.Icon := Desktop_Icons.Files;
            else return;
            end if;
         elsif Is_Word (F, L, "single-instance") then
            Item.Single_Instance := True;
         elsif Is_Word (F, L, "internal") then
            Word (F, L);
            if not Is_Word (F, L, "settings") or else Has_Internal then
               return;
            end if;
            Item.Kind := Internal_Settings;
            Has_Internal := True;
         else
            return;
         end if;
         if not Accept_Char (')') then return; end if;
      end loop;

      Skip_Space;
      Success := At_End and then Has_Label and then
        (Has_Internal xor Has_Program);
   end Parse;
end Desktop_Launch;
