with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Glyph_Renderer;
with Mesa_Cache;
procedure Glyph_Fallback_Tests is
   package R renames Compositor_Glyph_Renderer;
   package G renames R.P.G;
   package SW renames R.Software;
   use type SW.Pixels;
   procedure Reset with Import, Convention => C, External_Name => "glyph_mock_reset";
   procedure Fault (Value : Unsigned_32) with Import, Convention => C, External_Name => "glyph_mock_fault";
   function Stat (Index : Unsigned_32) return Unsigned_32 with Import, Convention => C, External_Name => "glyph_mock_stat";
   Target : aliased SW.Pixels (0 .. 84 * 72 - 1);
   Expected : SW.Pixels (Target'Range);
   Mask : SW.Bytes (0 .. R.P.L.Maximum_Bytes - 1) := (others => 127);
   Screen : G.Output := (80, 72, G.Clockwise_90, (5, 4), 0, 0);
   OK : Boolean;
   procedure Start (Views : in out Mesa_Cache.State) is
   begin
      Reset;
      Mesa_Cache.Initialize (Views, True);
      Mesa_Cache.Ensure (Views, 0, (Target'Address, 80, 72, 336, 1), Target'Length * 4, OK);
      pragma Assert (OK);
   end Start;
   procedure Queue (S : in out R.State; Views : in out Mesa_Cache.State) is
   begin
      R.Queue (S, Views, 0, (0, 65, Screen.Scale), Screen, (2, 3),
               (0, 0, 80, 72), 16#8031_AF07#, OK);
   end Queue;
   procedure Paint (S : in out R.State; Views : in out Mesa_Cache.State; Code : Natural := 65) is
   begin
      R.Paint (S, Views, (0, Code, Screen.Scale), Screen, (2, 3),
               (7, 9, 73, 64), Target, 84, 16#8031_AF07#, OK);
   end Paint;
begin
   for Failure in 0 .. 6 loop
      declare S : R.State; Views : Mesa_Cache.State; Charge : Natural; Before : Unsigned_32; begin
         Start (Views);
         for I in 1 .. 32 loop Queue (S, Views); pragma Assert (OK); end loop;
         Charge := R.Charged (S); Before := Stat (0);
         if Failure in 3 .. 5 then
            Fault (Unsigned_32 (Failure)); R.Flush (S, Views, OK); pragma Assert (not OK);
         elsif Failure = 6 then Fault (6);
         end if;
         R.Use_Software (S, Views, OK);
         Target := (others => 16#8012_3456#); Expected := Target;
         if Failure in 5 .. 6 then
            pragma Assert (not OK and not R.Software_Active (S) and R.Charged (S) = Charge);
            Paint (S, Views); pragma Assert (not OK and Target = Expected);
         else
            pragma Assert (OK and R.Software_Active (S) and R.Queued (S) = 0 and
              R.Charged (S) = Charge and Stat (0) = Before and Stat (4) = 1);
            Fault (0); Mesa_Cache.Shutdown (Views); pragma Assert (Mesa_Cache.Views_Clear (Views) and Stat (4) = 0);
            SW.Paint (Screen, (2, 3), (7, 9, 73, 64), Mask, Expected, 84, 16#8031_AF07#);
            Paint (S, Views); pragma Assert (OK and Target = Expected and Stat (0) = Before and Stat (4) = 0);
            Screen.Scale := (10, 8); Target := (others => 16#8012_3456#);
            Paint (S, Views); pragma Assert (OK and Target = Expected and Stat (0) = Before);
            Screen.Scale := (5, 4);
            Queue (S, Views); pragma Assert (not OK and R.Queued (S) = 0);
            if Failure = 1 then
               Fault (1); Expected := Target;
               R.Paint (S, Views, (0, 66, Screen.Scale), Screen, (4000, 4000),
                        (0, 0, 80, 72), Target, 84, 16#FFFF_FFFF#, OK);
               pragma Assert (OK and Target = Expected and Stat (0) = Before);
               Paint (S, Views, 66);
               pragma Assert (not OK and Target = Expected and R.Charged (S) = Charge);
               Fault (0);
            end if;
            for I in 0 .. 249 loop
               Screen.Scale := (G.Scale_Component (1 + I / 95), 1);
               Paint (S, Views, 32 + I mod 95); pragma Assert (OK and R.Charged (S) <= 524_288 and Stat (4) = 0);
            end loop;
            R.Shutdown (S, Views, OK); pragma Assert (OK and R.Charged (S) = 0);
         end if;
         Screen.Scale := (5, 4);
      end;
   end loop;
   -- Transition with every read lease holding a different imported mask.
   declare S : R.State; Views : Mesa_Cache.State; Before : Natural; begin
      Start (Views);
      for Code in 32 .. 63 loop
         R.Queue (S, Views, 0, (0, Code, Screen.Scale), Screen, (2, 3), (0, 0, 80, 72), 16#FFFF_FFFF#, OK);
         pragma Assert (OK);
      end loop;
      Before := R.Charged (S); pragma Assert (Stat (0) = 32 and Stat (4) = 33);
      R.Use_Software (S, Views, OK);
      pragma Assert (OK and R.Charged (S) = Before and R.Queued (S) = 0 and Stat (4) = 1 and Stat (3) = 0);
      Mesa_Cache.Shutdown (Views);
      for Code in 32 .. 63 loop Paint (S, Views, Code); pragma Assert (OK and Stat (0) = 32); end loop;
      R.Shutdown (S, Views, OK); pragma Assert (OK and R.Charged (S) = 0);
   end;
   -- Software mode can start without ever creating a Mesa context.
   declare S : R.State; Views : Mesa_Cache.State; begin
      Reset; R.Use_Software (S, Views, OK); pragma Assert (OK);
      Target := (others => 0); Paint (S, Views); pragma Assert (OK and Stat (4) = 0 and Stat (1) = 0);
      R.Shutdown (S, Views, OK); pragma Assert (OK and R.Charged (S) = 0);
   end;
   Ada.Text_IO.Put_Line ("GLYPH-FALLBACK: PASS retained masks, no second cache/context, software eviction, quiescent transitions and unknown-completion refusal");
end Glyph_Fallback_Tests;
