with Ada.Text_IO; use Ada.Text_IO;
with Boot_Panel; use Boot_Panel;
with Boot_Font;
procedure Main is
   Visits : Natural := 0;
   procedure Walk (Before : State; Depth : Natural) is
      S : State;
      Changed : Boolean;
   begin
      if Depth = 0 then return; end if;
      for Op in 0 .. 7 loop
         S := Before;
         case Op is
            when 0 => Initialize (S);
            when 1 => Begin_Step (S, "PCI discovery");
            when 2 => Complete_Step (S, "ACPI ready");
            when 3 => Fail (S, "Controller reset timed out");
            when 4 => Append (S, 'x', Changed);
            when 5 => Append (S, ASCII.LF, Changed);
            when 6 => Retire (S);
            when 7 => Set_Evidence (S, Timing_Evidence, "span=00123456");
            when others => null;
         end case;
         Visits := Visits + 1;
         if Lifecycle (Before) = Retired then pragma Assert (S = Before); end if;
         if Failed (Before) then
            pragma Assert (Failed (S));
            pragma Assert (Content (S, First_Error) = Content (Before, First_Error));
            pragma Assert (Content (S, Timing_Evidence) = Content (Before, Timing_Evidence));
         end if;
         Walk (S, Depth - 1);
      end loop;
   end Walk;
   S : State;
   Changed : Boolean;
begin
   Walk (S, 6);
   Initialize (S);
   Set_Evidence (S, Timer_Evidence, "PIT mode");
   pragma Assert (Content (S, Timer_Evidence) = Fit ("PIT mode"));
   for I in 1 .. 10_000 loop Append (S, 'A', Changed); end loop;
   Append (S, ASCII.CR, Changed);
   pragma Assert (not Changed);
   Append (S, ASCII.LF, Changed);
   pragma Assert (Changed and Content (S, Latest_Detail) = Boot_Panel.Line'(others => 'A'));
   Append (S, 'B', Changed);
   Append (S, ASCII.ESC, Changed);
   Append (S, ASCII.LF, Changed);
   pragma Assert (Content (S, Latest_Detail) = Fit ("B?"));
   pragma Assert (Fit (String'(Positive'Last - 1 => 'O', Positive'Last => 'K')) = Fit ("OK"));
   pragma Assert (Fit ("") = Boot_Panel.Line'(others => ' '));
   for C in Character loop
      for X in Boot_Font.Column loop
         for Y in Boot_Font.Row loop
            if C not in ' ' .. '~' then
               pragma Assert (not Boot_Font.Pixel (C, X, Y));
            else
               Changed := Boot_Font.Pixel (C, X, Y);
            end if;
         end loop;
      end loop;
   end loop;
   Put_Line ("PASS boot panel:" & Visits'Image & " transitions; bounded text, sticky failure, retirement, glyphs");
end Main;
