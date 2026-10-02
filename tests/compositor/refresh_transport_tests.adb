with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with Desktop_Launch;
with Desktop_Launch_Refresh;
procedure Refresh_Transport_Tests is
   package R renames Desktop_Launch_Refresh;
   Mode : constant String := Argument (1);
   Sequence : Unsigned_64 := 100;
   C : CompletionEntry;
   Menu : Desktop_Launch.Menu;
   Updated : Boolean;
   procedure Reply (Label : Unsigned_32; Value : Unsigned_64) is
   begin
      C := (token => Last_Token, msg => NULL_MESSAGE, others => <>);
      C.msg.tag := (label => Label, length => 1, flags => 0, reserved => 0);
      C.msg.words (0) := Value;
   end Reply;
   procedure Assert_Held is
      Saved : constant String := Storage;
      Calls : constant Natural := Submissions;
   begin
      for I in 1 .. 100 loop R.Request; R.Pump (Sequence); end loop;
      pragma Assert (Storage = Saved and Submissions = Calls);
      R.Take (False, Menu, Updated);
      pragma Assert (not Updated);
   end Assert_Held;
   procedure Write (Text : String) is
   begin Storage := [others => ASCII.NUL]; Storage (1 .. Text'Length) := Text; end Write;
begin
   Fail_Allocate := Mode = "allocation";
   Fail_Submit := Mode = "submit";
   R.Initialize;
   R.Initialize;
   R.Request;
   R.Pump (Sequence);
   if Mode in "allocation" | "submit" then Assert_Held;
   else
      pragma Assert (Submissions = 1 and Last_Message.tag.label = 16#0603#);
      Assert_Held;
      Reply (16#F000#, 2);
      if Mode = "generic-error" then C.msg.tag.label := 16#F001#; C.msg.words (0) := 0;
      elsif Mode = "stale" then C.token := Last_Token - 1;
      elsif Mode = "envelope" then C.msg.words (3) := 1;
      elsif Mode = "count" then C.msg.words (0) := 257;
      elsif Mode = "kernel" then C.valid := False;
      end if;
      Write ("desktop.launch.20-b" & ASCII.NUL & "desktop.launch.10-a" & ASCII.NUL);
      R.Collect (C);
      if Mode /= "normal" then
         Assert_Held;
         Reply (16#F000#, 0);
         R.Collect (C);
         Assert_Held;
      else
         R.Pump (Sequence);
         pragma Assert (Submissions = 2 and Last_Message.tag.label = 16#0600#);
         pragma Assert (Storage (1 .. 19) = "desktop.launch.10-a");
         Assert_Held;
         declare Value : constant String := "(launch v1 (label ""A"") (program ""a.app"") (icon files))";
         begin Write (Value); Reply (16#F000#, Value'Length); R.Collect (C); end;
         R.Pump (Sequence);
         pragma Assert (Submissions = 3 and Storage (1 .. 19) = "desktop.launch.20-b");
         declare Value : constant String := "(launch v1 (label ""B"") (program ""b.app"") (icon files))";
         begin Write (Value); Reply (16#F000#, Value'Length); R.Collect (C); end;
         for I in 1 .. 100 loop
            R.Take (True, Menu, Updated);
            pragma Assert (not Updated);
            R.Pump (Sequence);
            pragma Assert (Submissions = 3);
         end loop;
         R.Take (False, Menu, Updated);
         pragma Assert (Updated and Menu.Count = 2);
         pragma Assert (Desktop_Launch.Program_Of (Menu.Entries (1)) = "a.app" and
                        Desktop_Launch.Program_Of (Menu.Entries (2)) = "b.app");
         R.Pump (Sequence);
         pragma Assert (Submissions = 4 and Last_Message.tag.label = 16#0603#);
         -- An empty successful refresh preserves the last usable menu.
         Reply (16#F000#, 0); R.Collect (C); R.Take (False, Menu, Updated);
         pragma Assert (not Updated);
         R.Pump (Sequence);
         pragma Assert (Submissions = 4);
      end if;
   end if;
   Put_Line ("refresh transport: PASS " & Mode);
end Refresh_Transport_Tests;
