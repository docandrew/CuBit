with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Glyph_Renderer;
with Mesa_Cache;
procedure Glyph_Renderer_Tests is
   package R renames Compositor_Glyph_Renderer;
   package G renames R.P.G;
   procedure Reset with Import, Convention => C, External_Name => "glyph_mock_reset";
   procedure Fault (Value : Unsigned_32) with Import, Convention => C, External_Name => "glyph_mock_fault";
   function Stat (Index : Unsigned_32) return Unsigned_32 with Import, Convention => C, External_Name => "glyph_mock_stat";
   type Pixels is array (Natural range <>) of Unsigned_32;
   Target : aliased Pixels (0 .. 80 * 72 - 1);
   Screen : G.Output := (80, 72, G.Unrotated, (1, 1), 0, 0);
   OK : Boolean;
   procedure Start (Views : in out Mesa_Cache.State) is
   begin
      Reset;
      Mesa_Cache.Initialize (Views, True);
      Mesa_Cache.Ensure (Views, 0, (Target'Address, 80, 72, 320, 1), Target'Length * 4, OK);
      pragma Assert (OK);
   end Start;
   procedure Add (S : in out R.State; Views : in out Mesa_Cache.State; N : Natural := 0) is
   begin
      R.Queue (S, Views, 0, (N / 95 mod 2, 32 + N mod 95, Screen.Scale), Screen,
               (0, 0), (0, 0, 80, 72), 16#FFFF_FFFF#, OK);
   end Add;
begin
   declare S : R.State; Views : Mesa_Cache.State; begin
      Start (Views);
      for I in 1 .. 32 loop Add (S, Views); pragma Assert (OK and R.Queued (S) = I); end loop;
      pragma Assert (Stat (0) = 1 and Stat (1) = 1 and Stat (3) = 0);
      Add (S, Views); pragma Assert (OK and R.Queued (S) = 1 and Stat (3) = 1);
      R.Flush (S, Views, OK); pragma Assert (OK and R.Queued (S) = 0 and Stat (5) = 33);
      R.Shutdown (S, Views, OK); pragma Assert (OK and R.Charged (S) = 0 and Stat (4) = 1);
      Mesa_Cache.Shutdown (Views); pragma Assert (Stat (4) = 0);
   end;
   declare S : R.State; Views : Mesa_Cache.State; begin
      Start (Views);
      for I in 0 .. 249 loop
         Screen.Scale := (G.Scale_Component (1 + I / 190), 1);
         Add (S, Views, I); pragma Assert (OK);
         R.Flush (S, Views, OK); pragma Assert (OK);
      end loop;
      pragma Assert (Stat (0) = 250 and Stat (2) >= 122 and R.Charged (S) <= 524_288);
      R.Shutdown (S, Views, OK); pragma Assert (OK and Stat (4) = 1);
      Mesa_Cache.Shutdown (Views);
   end;
   declare S : R.State; Views : Mesa_Cache.State; begin
      Start (Views); Screen.Scale := (16, 1);
      for I in 0 .. 2 loop Add (S, Views, I); pragma Assert (OK); end loop;
      Add (S, Views, 3); pragma Assert (not OK and R.Queued (S) = 3 and R.Charged (S) = 3 * 139_264);
      R.Flush (S, Views, OK); pragma Assert (OK);
      Add (S, Views, 3); pragma Assert (OK);
      R.Shutdown (S, Views, OK); pragma Assert (OK and R.Queued (S) = 0 and R.Charged (S) = 0);
      Mesa_Cache.Shutdown (Views);
   end;
   -- Fill all 4096 arena cells while payload charge remains below budget.
   -- This must exercise backing allocation refusal, not just cache admission.
   declare
      S : R.State; Views : Mesa_Cache.State;
      Densities : constant array (1 .. 7) of G.UI_Scale :=
        ((16, 1), (16, 1), (16, 1), (13, 1), (5, 1), (1, 1), (3, 16));
   begin
      Start (Views);
      for I in Densities'Range loop
         Screen.Scale := Densities (I); Add (S, Views, I); pragma Assert (OK);
      end loop;
      pragma Assert (R.Queued (S) = 7 and R.Charged (S) = 523_936);
      Add (S, Views, 90);
      pragma Assert (not OK and R.Queued (S) = 7 and R.Charged (S) = 523_936);
      R.Flush (S, Views, OK); pragma Assert (OK);
      Add (S, Views, 90); pragma Assert (OK);
      R.Shutdown (S, Views, OK); pragma Assert (OK and R.Charged (S) = 0);
      Mesa_Cache.Shutdown (Views);
   end;
   Screen.Scale := (1, 1);
   declare S : R.State; Views : Mesa_Cache.State; begin
      Start (Views); Add (S, Views); pragma Assert (OK);
      Mesa_Cache.Ensure (Views, 1, (Target'Address, 64, 40, 320, 1), Target'Length * 4, OK);
      pragma Assert (OK);
      Screen.Width := 64; Screen.Height := 40;
      R.Queue (S, Views, 1, (0, 65, Screen.Scale), Screen, (0, 0), (0, 0, 64, 40), 16#FFFF_FFFF#, OK);
      pragma Assert (OK and Stat (3) = 1 and R.Queued (S) = 1);
      R.Queue (S, Views, 1, (0, 65, (5, 4)), Screen, (0, 0), (0, 0, 64, 40), 16#FFFF_FFFF#, OK);
      pragma Assert (not OK and R.Queued (S) = 1 and Stat (3) = 1);
      R.Shutdown (S, Views, OK); pragma Assert (OK and Stat (4) = 2);
      Mesa_Cache.Shutdown (Views);
      Screen.Width := 80; Screen.Height := 72;
   end;
   for Failure in 1 .. 6 loop
      declare S : R.State; Views : Mesa_Cache.State; Before : Unsigned_32; begin
         Start (Views);
         if Failure <= 2 then
            Fault (Unsigned_32 (Failure)); Add (S, Views);
            pragma Assert (not OK and R.Charged (S) = 0 and R.Queued (S) = 0);
            Fault (0); R.Shutdown (S, Views, OK); pragma Assert (OK); Mesa_Cache.Shutdown (Views);
         else
            Add (S, Views); pragma Assert (OK);
            Fault (Unsigned_32 (Failure)); Before := Stat (2);
            if Failure = 6 then R.Shutdown (S, Views, OK); else R.Flush (S, Views, OK); end if;
            pragma Assert (not OK);
            if Failure in 5 .. 6 then
               pragma Assert (not Mesa_Cache.Can_Retire (Views) and R.Charged (S) > 0 and Stat (2) = Before);
               R.Shutdown (S, Views, OK); pragma Assert (not OK and Stat (2) = Before);
            else
               pragma Assert (R.Queued (S) = 0 and not R.Enabled (S));
               Fault (0); R.Shutdown (S, Views, OK); pragma Assert (OK); Mesa_Cache.Shutdown (Views);
            end if;
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("GLYPH-RENDERER: PASS cache hits, batch32 rollover, 250 keys/eviction, pinned budget/rounded-arena pressure, target rollover, density rejection, build/draw/release faults and quarantine");
end Glyph_Renderer_Tests;
