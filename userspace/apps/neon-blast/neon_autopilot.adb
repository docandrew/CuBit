------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with Neon_Math; use Neon_Math;

package body Neon_Autopilot is
   use Neon_Sim;

   Start_Delay : constant Samples := 2 * Sample_Rate;    --  admire the title first
   Danger_Radius : constant := 3.2;

   function Drive (W : World; Now : Samples) return Controls is
      C : Controls;
      P : constant Vec2 := W.Player.Position;
      Nearest : Float := Float'Last;
      Target : Vec2 := (0.0, 6.0);
      Threat : Vec2 := (0.0, 0.0);
      Seconds : constant Float := Neon_Time.Seconds (Sample_Offset (Now));
   begin
      case W.Phase is
         when Title =>
            C.Confirm := Now >= Start_Delay and then Now mod (Sample_Rate / 2) < Tick_Samples;
            C.Demo := True;
            return C;
         when Game_Over =>
            C.Confirm := W.Phase_Time > 3.0 and then Now mod (Sample_Rate / 2) < Tick_Samples;
            C.Demo := True;
            return C;
         when Calibrating | Dying =>
            return C;
         when Playing =>
            null;
      end case;
      for E of W.Enemies loop
         if E.Alive then
            declare
               D : constant Float := Length (E.Position - P);
            begin
               if E.Spawn_Left = 0.0 and then D < Nearest then
                  Nearest := D;
                  Target := E.Position;
               end if;
               if D < Danger_Radius then
                  Threat := Threat + (1.0 / Max (D, 0.3)) * (P - E.Position);
               end if;
               --  A strobe's mark: where it lands on the downbeat.
               if E.Kind = Strobe and then E.Armed and then Length (E.Hop_To - P) < Danger_Radius then
                  Threat := Threat + (1.0 / Max (Length (E.Hop_To - P), 0.3)) * (P - E.Hop_To);
               end if;
            end;
         end if;
      end loop;
      for O of W.Orbs loop
         if O.Alive and then Length (O.Position - P) < Danger_Radius then
            Threat := Threat + (1.0 / Max (Length (O.Position - P), 0.3)) * (P - O.Position);
         end if;
      end loop;
      --  Circle the arena's middle, pushed away from threats.
      C.Move := (0.6 * Cos_Turns (Seconds * 0.11) - 0.35 * P.X / Half_Width,
                 0.6 * Sin_Turns (Seconds * 0.13) - 0.35 * P.Y / Half_Depth);
      C.Move := C.Move + 0.8 * Threat;
      C.Move := (Clamp (C.Move.X, -1.0, 1.0), Clamp (C.Move.Y, -1.0, 1.0));
      C.Aim := Target;
      C.Fire := Nearest < 30.0;
      C.Dash := Length (Threat) > 1.5 and then W.Player.Dash_Cooldown = 0.0;
      return C;
   end Drive;
end Neon_Autopilot;
