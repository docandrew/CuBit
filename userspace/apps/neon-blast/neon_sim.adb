------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body Neon_Sim is
   use Neon_Composer;

   Dt : constant Float := Tick_Seconds;

   --  Player.
   Player_Speed : constant := 10.0;
   Player_Response : constant := 14.0;     --  1/s: how fast velocity follows the keys
   Player_Radius : constant := 0.55;
   Dash_Speed : constant := 32.0;
   Dash_Seconds : constant := 0.13;
   Dash_Cooldown_Seconds : constant := 0.6;
   Dash_Shield_Seconds : constant := 0.3;
   Hit_Shield_Seconds : constant := 2.0;
   Starting_Lives : constant := 3;
   Shot_Speed : constant := 30.0;
   Shot_Radius : constant := 0.25;
   Shot_Lifetime : constant := 1.6;
   Shot_Degrees : constant := 8;           --  shots walk the chord up and down
   Trail_Interval : constant := 1.0 / 60.0;

   --  Enemies.
   Hop_Seconds : constant := 0.22;
   Hop_Distance : constant := 1.9;
   Hop_Height : constant := 0.9;
   Charge_Speed : constant := 13.0;
   Charge_Seconds : constant := 0.45;
   Orb_Speed : constant := 4.5;
   Orb_Radius : constant := 0.3;
   Orbs_Per_Ring : constant := 8;
   Shard_Speed : constant := 5.5;
   Splinter_Speed : constant := 7.5;
   Warning_Seconds : constant := 0.6;      --  seconds before a spawn is solid
   Boss_Warning_Seconds : constant := 1.5;
   Strobe_Mark_Spread : constant := 2.0;   --  how far off the player a strobe marks
   Strobe_Mark_Step : constant := 12;      --  the bar's last beat: it marks; the downbeat: it jumps
   Strobe_Lift : constant := 0.6;
   Dropper_Hover : constant := 6.5;        --  the distance a dropper circles the player at
   Dropper_Orbs : constant := 12;
   Dropper_Strike_Speed : constant := 15.0;
   Dropper_Strike_Seconds : constant := 0.7;
   Dropper_Chase_Speed : constant := 6.5;
   Bolt_Speed : constant := 9.0;
   Sentry_Range : constant := 7.0;
   Warden_Charge_Speed : constant := 9.0;
   Warden_Charge_Seconds : constant := 0.5;
   Warden_Orbs : constant := 4;
   Warden_Turn : constant := 1.0 / 16.0;   --  its cross turns this much a beat
   Totem_Spiral_Orbs : constant := 3;
   Totem_Spiral_Turn : constant := 0.07;
   Totem_Ring_Orbs : constant := 16;
   Totem_Spread : constant := 0.04;        --  turns between the snare triple's bolts
   Totem_Home : constant Vec2 := (0.0, 3.5);
   Warden_Health_Base : constant := 50;
   Warden_Health_Stage : constant := 20;
   Totem_Health_Base : constant := 100;
   Totem_Health_Stage : constant := 50;
   Crowd_Base : constant := 12;
   Crowd_Per_Stage : constant := 4;

   --  Scoring.
   Combo_Window : constant := 1.6;         --  seconds between kills to keep a combo
   Kills_Per_Multiplier : constant := 4;
   Maximum_Multiplier : constant := 8;
   On_Beat_Fraction : constant := 0.12;    --  of a beat, either side
   On_Beat_Bonus_Percent : constant := 50;
   Stage_Clear_Bonus : constant := 5000;
   Banner_Seconds : constant := 2.2;
   Stage_Banner_Seconds : constant := 3.2;
   Dying_Seconds : constant := 1.8;
   Restart_Delay : constant := 1.0;
   Shield_Blast_Radius : constant := 4.5;

   function Base_Score (Kind : Enemy_Kind) return Natural is
     (case Kind is
        when Cube => 100, when Pyramid => 150, when Torus => 400, when Shard => 200,
        when Splinter => 50, when Strobe => 250, when Dropper => 300, when Sentry => 350,
        when Warden => 3000, when Totem => 8000);
   function Base_Health (Kind : Enemy_Kind; Stage : Natural) return Natural is
     (case Kind is
        when Cube => 2, when Pyramid => 1, when Torus => 6, when Shard => 2, when Splinter => 1,
        when Strobe => 3, when Dropper => 6, when Sentry => 4,
        when Warden => Warden_Health_Base + Warden_Health_Stage * Stage,
        when Totem => Totem_Health_Base + Totem_Health_Stage * Stage);

   function Pan_Of (X : Float) return Float is (Clamp (X / Half_Width, -1.0, 1.0));

   function Uniform (W : in out World) return Float is
      Value : Float;
   begin
      Neon_Random.Uniform (W.Random, Value);
      return Value;
   end Uniform;

   function Between (W : in out World; Low, High : Float) return Float is
     (Low + (High - Low) * Uniform (W));

   procedure Request (Sounds : in out Sound_List; Item : Sound_Request) is
   begin
      if Sounds.Count < Sound_Ceiling then
         Sounds.Count := Sounds.Count + 1;
         Sounds.Items (Sounds.Count) := Item;
      end if;
   end Request;

   ---------------------------------------------------------------------------
   --  Effects
   ---------------------------------------------------------------------------

   procedure Emit (W : in out World; Position, Velocity : Vec3; Color : RGB; Life, Size : Float;
                   Drag : Float := 1.5; Gravity : Float := 0.0) is
   begin
      W.Particles (W.Next_Particle) :=
        (Position => Position, Velocity => Velocity, Color => Color, Life => Life,
         Lifetime => Life, Size => Size, Drag => Drag, Gravity => Gravity);
      W.Next_Particle := (if W.Next_Particle = Particle_Index'Last then 0 else W.Next_Particle + 1);
   end Emit;

   procedure Burst (W : in out World; At_Point : Vec3; Color : RGB; Count : Natural; Speed : Float;
                    Life : Float := 0.9) is
      Angle, Lift, Pace, Whiten, Span, Size : Float;
   begin
      --  One random draw per statement: the order of the draws is part of
      --  the replay, and Ada leaves the order inside one expression open.
      for I in 1 .. Count loop
         Angle := Uniform (W);
         Lift := Between (W, 0.5, 4.0);
         Pace := Speed * Between (W, 0.25, 1.0);
         Whiten := Between (W, 0.0, 0.35);
         Span := Life * Between (W, 0.5, 1.0);
         Size := Between (W, 0.05, 0.13);
         Emit (W, At_Point,
               (Pace * Cos_Turns (Angle), Lift, Pace * Sin_Turns (Angle)),
               Mix (Color, White, Whiten), Span, Size, Drag => 1.8, Gravity => 9.0);
      end loop;
   end Burst;

   procedure Add_Ripple (W : in out World; Center : Vec2; Strength : Float; Color : RGB) is
   begin
      W.Ripples (W.Next_Ripple) := (Center => Center, Age => 0.0, Strength => Strength, Color => Color);
      W.Next_Ripple := (if W.Next_Ripple = Ripple_Ceiling then 1 else W.Next_Ripple + 1);
   end Add_Ripple;

   procedure Add_Flash (W : in out World; Position : Vec3; Color : RGB; At_Time : Samples;
                        Duration : Float; Burst : Natural) is
      Oldest : Positive := 1;
   begin
      for I in W.Flashes'Range loop
         if W.Flashes (I).Duration = 0.0 then
            Oldest := I;
            exit;
         elsif W.Flashes (I).At_Time < W.Flashes (Oldest).At_Time then
            Oldest := I;
         end if;
      end loop;
      W.Flashes (Oldest) := (Position => Position, Color => Color, At_Time => At_Time,
                             Duration => Duration, Burst => Burst);
   end Add_Flash;

   ---------------------------------------------------------------------------
   --  Judgements and scores
   ---------------------------------------------------------------------------

   procedure Record_Judgement (J : in out Judgement; Error_Ms : Float) is
   begin
      J.Count := J.Count + 1;
      J.Sum := J.Sum + Error_Ms;
      J.Sum_Squares := J.Sum_Squares + Error_Ms * Error_Ms;
      J.Last := Error_Ms;
   end Record_Judgement;

   function Mean (J : Judgement) return Float is
     (if J.Count = 0 then 0.0 else J.Sum / Float (J.Count));

   function Spread (J : Judgement) return Float is
     (if J.Count < 2 then 0.0
      else Sqrt (Max (0.0, J.Sum_Squares / Float (J.Count) - Mean (J) * Mean (J))));

   procedure Place_Score (Table : in out Score_Table; Score, Stage : Natural; Playlist : Unsigned_64;
                          Rank : out Natural) is
   begin
      Rank := 0;
      if Score = 0 then
         return;
      end if;
      for I in Table'Range loop
         if Score > Table (I).Score then
            Rank := I;
            exit;
         end if;
      end loop;
      if Rank /= 0 then
         for I in reverse Rank + 1 .. Table'Last loop
            Table (I) := Table (I - 1);
         end loop;
         Table (Rank) := (Score => Score, Stage => Stage, Playlist => Playlist);
      end if;
   end Place_Score;

   ---------------------------------------------------------------------------

   function Is_Favourite (W : World; Seed : Unsigned_64) return Boolean is
     (for some I in 1 .. W.Favourites_Kept => W.Favourites (I) = Seed);

   Playlist_Span : constant := Neon_Style.Playlist_Seed'Last;

   procedure Reset (W : out World; Seed : Unsigned_64) is
   begin
      W := (Random => Neon_Random.Seeded (Seed, 16#AE0B#), others => <>);
      W.Player.Lives := Starting_Lives;
      --  The title opens on the lo-fi preset (the seed drives the game's
      --  own draws; the playlist is the player's to pick).
      W.Playlist := Neon_Style.Presets (Neon_Style.Lo_Fi);
   end Reset;

   function Living_Enemies (W : World) return Natural is
      Count : Natural := 0;
   begin
      for E of W.Enemies loop
         if E.Alive then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Living_Enemies;

   function Mood (W : World) return Neon_Composer.Mood is
      Arp : constant Boolean := W.Rhythm_Streak >= 4;
      Lead : constant Boolean := W.Rhythm_Streak >= 8;
   begin
      return (Scene => (case W.Phase is
                          when Title => Attract, when Calibrating => Calibrate,
                          when Dying | Game_Over => Over, when Playing => Play),
              Playlist => W.Playlist, Run => W.Run, Start_Stage => Choice_Stage (W.Choice),
              Boss_Practice => Choice_Is_Boss (W.Choice), Defeated_Stage => W.Defeated_Stage,
              Arp_On => Arp, Lead_On => Lead);
   end Mood;

   procedure Start_Game (W : in out World) is
      Keep : constant World := W;
   begin
      W := (others => <>);
      W.Random := Keep.Random;
      W.Scores := Keep.Scores;
      W.Calibration_Offset_Ms := Keep.Calibration_Offset_Ms;
      W.Clock := Keep.Clock;
      W.Last_Serial := Keep.Last_Serial;
      W.Particles := Keep.Particles;
      W.Press_Timing := Keep.Press_Timing;
      W.Playlist := Keep.Playlist;
      W.Choice := Keep.Choice;
      W.Favourites := Keep.Favourites;
      W.Favourites_Kept := Keep.Favourites_Kept;
      W.Run := Keep.Run + 1;
      W.Phase := Playing;
      W.Player.Lives := Starting_Lives;
      W.Player.Position := (0.0, -4.0);
   end Start_Game;

   ---------------------------------------------------------------------------
   --  Spawning
   ---------------------------------------------------------------------------

   procedure Spawn (W : in out World; Kind : Enemy_Kind; At_Point : Vec2; Sounds : in out Sound_List;
                    Slot : out Natural) is
      Spin : constant Float := Uniform (W);
      Orbit : constant Float := Uniform (W);
   begin
      Slot := 0;
      for I in W.Enemies'Range loop
         if not W.Enemies (I).Alive then
            W.Enemies (I) :=
              (Alive => True, Kind => Kind, Position => At_Point, Velocity => (0.0, 0.0),
               Height => 0.0, Health => Base_Health (Kind, W.Stage), Spin => Spin,
               Spawn_Left => (if Kind in Boss_Kind then Boss_Warning_Seconds else Warning_Seconds),
               Hop_From => At_Point, Hop_To => At_Point, Hop_Time => 1.0, Charge_Left => 0.0,
               Hit_Flash => 0.0, Age => 0.0, Orbit => Orbit, Power => 0.0, Armed => False,
               Volley => 0);
            Request (Sounds, (Kind => (if Kind in Boss_Kind then Boss_Arrive else Spawn_Warning),
                              Pan => Pan_Of (At_Point.X), X => At_Point.X, Z => At_Point.Y,
                              Color => Enemy_Color (Kind), others => <>));
            Slot := I;
            return;
         end if;
      end loop;
      W.Spawn_Refusals := W.Spawn_Refusals + 1;
   end Spawn;

   --  A point along one arena edge, away from the player.
   function Edge_Point (W : in out World) return Vec2 is
      Side : Natural;
      Along : constant Float := Between (W, -0.85, 0.85);
      Inset : constant Float := 1.2;
      Candidate : Vec2;
   begin
      for Attempt in 1 .. 4 loop
         Neon_Random.Pick (W.Random, 4, Side);
         Candidate :=
           (case Side is
              when 0 => (-Half_Width + Inset, Along * Half_Depth),
              when 1 => (Half_Width - Inset, Along * Half_Depth),
              when 2 => (Along * Half_Width, Half_Depth - Inset),
              when others => (Along * Half_Width, -Half_Depth + Inset));
         exit when Length (Candidate - W.Player.Position) > 7.0;
      end loop;
      return Candidate;
   end Edge_Point;

   procedure Spawn_Group (W : in out World; Kind : Enemy_Kind; Count : Natural;
                          Sounds : in out Sound_List) is
      Center : constant Vec2 := Edge_Point (W);
      Spread_X, Spread_Z : Float;
      Slot : Natural;
   begin
      for I in 1 .. Count loop
         Spread_X := Between (W, -2.5, 2.5);
         Spread_Z := Between (W, -2.0, 2.0);
         Spawn (W, Kind,
                (Clamp (Center.X + Spread_X, -Half_Width + 1.0, Half_Width - 1.0),
                 Clamp (Center.Y + Spread_Z, -Half_Depth + 1.0, Half_Depth - 1.0)),
                Sounds, Slot);
      end loop;
   end Spawn_Group;

   procedure Spawn_Boss (W : in out World; Kind : Boss_Kind; Sounds : in out Sound_List) is
      Slot : Natural;
   begin
      Spawn (W, Kind, Totem_Home, Sounds, Slot);
      if Slot /= 0 then
         W.Boss := Slot;
         W.Boss_Health := Natural'Max (1, W.Enemies (Slot).Health);
         W.Shake := Max (W.Shake, 0.6);
      end if;
   end Spawn_Boss;

   function Boss_Alive (W : World) return Boolean is
     (W.Boss /= 0 and then W.Enemies (W.Boss).Alive);

   --  Each section spawns its own mix, on its beats.
   procedure On_Beat_Spawns (W : in out World; Current : Step_Record; Sounds : in out Sound_List) is
      Stage : constant Natural := Current.Stage;
      Bar : constant Natural := Current.Section_Bar;
      Beat : constant Natural := Beat_In_Bar (Current);
      Crowd_Limit : constant Natural :=
        Natural'Min (Enemy_Ceiling - 8, Crowd_Base + Crowd_Per_Stage * Stage + 2 * Current.Energy);
   begin
      if Stage = 0 or else Current.Run /= W.Run then
         return;
      end if;
      if Current.Section = Peak and then Bar = 0 and then Beat = 0 and then W.Totem_Stage < Stage then
         W.Totem_Stage := Stage;
         Spawn_Boss (W, Totem, Sounds);
         return;
      end if;
      if Current.Section = Drop and then Bar = 0 and then Beat = 0 and then Stage >= 2
        and then W.Warden_Stage < Stage
      then
         W.Warden_Stage := Stage;
         Spawn_Boss (W, Warden, Sounds);
      end if;
      if Living_Enemies (W) >= Crowd_Limit then
         return;
      end if;
      case Current.Section is
         when Intro =>
            if Beat in 0 | 2 then
               Spawn_Group (W, Cube, 1 + Beat / 2, Sounds);
            end if;
         when Groove =>
            case Beat is
               when 0 =>
                  Spawn_Group (W, Cube, 2 + Stage / 2, Sounds);
                  if Stage >= 2 and then Bar = 4 then
                     Spawn_Group (W, Torus, 1, Sounds);
                  end if;
               when 1 =>
                  if Bar mod 2 = 1 then
                     Spawn_Group (W, Pyramid, 1 + Stage / 3, Sounds);
                  end if;
                  if Bar in 3 | 6 then
                     Spawn_Group (W, Strobe, 1 + Stage / 3, Sounds);
                  end if;
               when 2 =>
                  Spawn_Group (W, Cube, 1 + Stage / 2, Sounds);
               when others =>
                  if Stage >= 2 and then Bar in 2 | 5 then
                     Spawn_Group (W, Sentry, 1, Sounds);
                  end if;
                  if Stage >= 3 and then Bar in 1 | 5 then
                     Spawn_Group (W, Shard, 1, Sounds);
                  end if;
            end case;
         when Build =>
            if Beat = 0 and then Bar in 0 | 2 then
               Spawn_Group (W, Dropper, 1 + Stage / 2, Sounds);
            elsif Beat = 2 then
               Spawn_Group (W, Cube, 1 + Stage / 3, Sounds);
            end if;
         when Drop =>
            case Beat is
               when 0 =>
                  Spawn_Group (W, Cube, 2 + Stage / 2, Sounds);
               when 1 =>
                  Spawn_Group (W, Pyramid, 1 + Stage / 3, Sounds);
                  if Bar in 2 | 5 then
                     Spawn_Group (W, Strobe, 1 + Stage / 3, Sounds);
                  end if;
               when 2 =>
                  Spawn_Group (W, Cube, 2, Sounds);
               when others =>
                  if Stage >= 2 and then Bar in 1 | 4 then
                     Spawn_Group (W, Sentry, 1, Sounds);
                  end if;
                  if Bar = 3 then
                     Spawn_Group (W, (if Stage >= 2 then Torus else Strobe), 1, Sounds);
                  end if;
                  if Bar = 6 then
                     Spawn_Group (W, (if Stage >= 3 then Shard else Strobe), 1 + Stage / 3, Sounds);
                  end if;
            end case;
         when Breakdown =>
            if Beat = 0 then
               Spawn_Group (W, Cube, 1, Sounds);
            elsif Beat = 2 and then Stage >= 2 and then Bar in 1 | 3 then
               Spawn_Group (W, (if Bar = 1 then Torus else Sentry), 1, Sounds);
            end if;
         when Peak =>
            if Boss_Alive (W) then
               if Beat = 2 then
                  Spawn_Group (W, Cube, 1 + Stage / 2, Sounds);
               elsif Beat = 0 and then Bar mod 2 = 1 then
                  Spawn_Group (W, Pyramid, 1, Sounds);
               elsif Beat = 1 and then Current.Feel = Double_Time then
                  Spawn_Group (W, Strobe, 1, Sounds);
               end if;
            end if;
         when Title_Theme | Outro | Finale | Metronome =>
            null;
      end case;
   end On_Beat_Spawns;

   --  The song's form moved on: a new stage or section shows its banner.
   procedure On_Section (W : in out World; Current : Step_Record; Sounds : in out Sound_List) is
   begin
      if Current.Stage = 0 or else Current.Run /= W.Run then
         return;
      end if;
      if Current.Stage /= W.Stage then
         W.Stage := Current.Stage;
         W.Banner_Kind := Intro;
         W.Banner := Stage_Banner_Seconds;
         Request (Sounds, (Kind => Wave_Chime, Color => Cyan, others => <>));
      elsif Current.Section /= W.Section and then Current.Section in Build | Drop | Breakdown | Peak then
         W.Banner_Kind := Current.Section;
         W.Banner := Banner_Seconds;
      end if;
      W.Section := Current.Section;
   end On_Section;

   ---------------------------------------------------------------------------
   --  Combat
   ---------------------------------------------------------------------------

   procedure Kill (W : in out World; Index : Positive; Current : Step_Record; Scored : Boolean;
                   Sounds : in out Sound_List);
   procedure Kill (W : in out World; Index : Positive; Current : Step_Record; Scored : Boolean;
                   Sounds : in out Sound_List) is
      E : constant Enemy := W.Enemies (Index);
      Color : constant RGB := Enemy_Color (E.Kind);
      Beat_Length : constant Float := Float (Current.Length * Steps_Per_Beat);
      Into_Beat : constant Float :=
        Float (Long_Long_Integer (W.Clock) - Long_Long_Integer (Current.Start))
        + Float (Current.Step mod Steps_Per_Beat) * Float (Current.Length);
      Error : constant Float :=
        (if Into_Beat > Beat_Length / 2.0 then Into_Beat - Beat_Length else Into_Beat);
      Error_Ms : constant Float := Error * 1000.0 / Float (Sample_Rate) - W.Calibration_Offset_Ms;
      On_Beat : constant Boolean :=
        abs Error_Ms <= On_Beat_Fraction * Beat_Length * 1000.0 / Float (Sample_Rate);
      Points : Natural;
      Size : constant Float :=
        (case E.Kind is
           when Torus => 2.0, when Splinter => 0.6, when Warden => 2.5, when Totem => 3.2,
           when others => 1.0);
   begin
      W.Enemies (Index).Alive := False;
      W.Kills := W.Kills + 1;
      Burst (W, (E.Position.X, 0.6 + E.Height, E.Position.Y), Color,
             Natural (40.0 * Size), 9.0 * Size);
      Add_Ripple (W, E.Position, 0.8 * Size, Color);
      W.Shake := Max (W.Shake, 0.18 * Size);
      Request (Sounds, (Kind => (if E.Kind in Boss_Kind then Boss_Down else Explosion),
                        Pan => Pan_Of (E.Position.X), Size => Size,
                        X => E.Position.X, Z => E.Position.Y, Color => Color, others => <>));
      if Index = W.Boss then
         W.Boss := 0;
      end if;
      if E.Kind = Totem then
         --  The stage's boss is down: the music may move on, and the rest
         --  of the stage's enemies go with it.
         W.Defeated_Stage := Natural'Max (W.Defeated_Stage, W.Stage);
         W.Stages_Cleared := W.Stages_Cleared + 1;
         W.Stage_Clear := Stage_Banner_Seconds;
         W.Screen_Flash := 1.0;
         W.Shake := 1.0;
         if Scored then
            W.Score := W.Score + Stage_Clear_Bonus * Natural'Max (1, W.Stage);
         end if;
         for O of W.Orbs loop
            O.Alive := False;
         end loop;
         for I in W.Enemies'Range loop
            if W.Enemies (I).Alive then
               Kill (W, I, Current, Scored => Scored, Sounds => Sounds);
            end if;
         end loop;
      end if;
      if E.Kind = Shard then
         for Side in 0 .. 1 loop
            declare
               Spin : constant Float := Uniform (W);
            begin
            for S of W.Enemies loop
               if not S.Alive then
                  S := (Alive => True, Kind => Splinter, Position => E.Position,
                        Velocity => Splinter_Speed * Rotate (Normalize (E.Velocity, (1.0, 0.0)),
                                                             (if Side = 0 then 0.25 else -0.25)),
                        Health => 1, Spin => Spin, others => <>);
                  exit;
               end if;
            end loop;
            end;
         end loop;
      end if;
      if not Scored then
         return;
      end if;
      Record_Judgement (W.Kill_Timing, Error_Ms);
      W.Combo := W.Combo + 1;
      W.Combo_Timer := Combo_Window;
      W.Multiplier := Positive (Natural'Min (Maximum_Multiplier, 1 + W.Combo / Kills_Per_Multiplier));
      Points := Base_Score (E.Kind) * W.Multiplier;
      if On_Beat then
         Points := Points + Points * On_Beat_Bonus_Percent / 100;
         W.Rhythm_Streak := W.Rhythm_Streak + 1;
         W.Best_Streak := Natural'Max (W.Best_Streak, W.Rhythm_Streak);
         W.On_Beat_Kills := W.On_Beat_Kills + 1;
         W.Last_Kill_On_Beat := 0.6;
      elsif W.Rhythm_Streak > 0 then
         W.Rhythm_Streak := W.Rhythm_Streak - 1;
      end if;
      W.Score := W.Score + Points;
      if W.Combo mod 5 = 0 then
         Request (Sounds, (Kind => Combo_Chime, Degree => W.Combo / 5, Color => Gold, others => <>));
      end if;
   end Kill;

   procedure Hit_Player (W : in out World; Current : Step_Record; Sounds : in out Sound_List) is
      P : constant Vec2 := W.Player.Position;
   begin
      W.Player.Lives := (if W.Player.Lives > 0 then W.Player.Lives - 1 else 0);
      W.Player.Invulnerable := Hit_Shield_Seconds;
      W.Combo := 0;
      W.Combo_Timer := 0.0;
      W.Multiplier := 1;
      W.Rhythm_Streak := 0;
      W.Shake := 1.0;
      W.Hurt_Flash := 1.0;
      for O of W.Orbs loop
         O.Alive := False;
      end loop;
      for I in W.Enemies'Range loop
         if W.Enemies (I).Alive and then W.Enemies (I).Kind not in Boss_Kind
           and then Length (W.Enemies (I).Position - P) < Shield_Blast_Radius
         then
            Kill (W, I, Current, Scored => False, Sounds => Sounds);
         end if;
      end loop;
      Burst (W, (P.X, 0.5, P.Y), Hot_Pink, 90, 14.0, 1.2);
      Add_Ripple (W, P, 2.0, Hot_Pink);
      Request (Sounds, (Kind => Hurt_Sound, Pan => Pan_Of (P.X), Color => Hot_Pink, others => <>));
      if W.Player.Lives = 0 then
         W.Phase := Dying;
         W.Phase_Time := 0.0;
         Request (Sounds, (Kind => Death_Sound, Pan => Pan_Of (P.X), Color => Hot_Pink, others => <>));
      end if;
   end Hit_Player;

   procedure Fire_Shot (W : in out World; Sounds : in out Sound_List) is
      P : Player_State renames W.Player;
      Hue : constant RGB := (if P.Shot_Degree mod 2 = 0 then Cyan else Hot_Pink);
   begin
      for S of W.Shots loop
         if not S.Alive then
            S := (Alive => True, Position => P.Position + 0.9 * P.Facing,
                  Velocity => Shot_Speed * P.Facing + 0.3 * P.Velocity, Hue => Hue, Age => 0.0);
            exit;
         end if;
      end loop;
      W.Shots_Fired := W.Shots_Fired + 1;
      Request (Sounds, (Kind => Shot, Pan => Pan_Of (P.Position.X), Degree => P.Shot_Degree,
                        Color => Hue, others => <>));
      Emit (W, (P.Position.X + 0.9 * P.Facing.X, 0.5, P.Position.Y + 0.9 * P.Facing.Y),
            (6.0 * P.Facing.X, 0.5, 6.0 * P.Facing.Y), Hue, 0.12, 0.12, Drag => 6.0);
      if P.Degree_Up then
         if P.Shot_Degree + 1 >= Shot_Degrees then
            P.Degree_Up := False;
            P.Shot_Degree := P.Shot_Degree - 1;
         else
            P.Shot_Degree := P.Shot_Degree + 1;
         end if;
      else
         if P.Shot_Degree = 0 then
            P.Degree_Up := True;
            P.Shot_Degree := 1;
         else
            P.Shot_Degree := P.Shot_Degree - 1;
         end if;
      end if;
   end Fire_Shot;

   --  The next 32nd-note boundary strictly after Now.
   function Next_Thirty_Second (Now : Samples; Current : Step_Record) return Samples is
      Interval : constant Samples := Samples (Natural'Max (1, Current.Length / 2));
      Into : constant Samples := (if Now >= Current.Start then Now - Current.Start else 0);
   begin
      return Current.Start + (Into / Interval + 1) * Interval;
   end Next_Thirty_Second;

   procedure Update_Player (W : in out World; Input : Controls; Current : Step_Record;
                            Sounds : in out Sound_List) is
      P : Player_State renames W.Player;
      Wanted : Vec2 := Input.Move;
      Response : constant Float := 1.0 - Exp (-Player_Response * Dt);
   begin
      if Length (Wanted) > 1.0 then
         Wanted := Normalize (Wanted, (0.0, 0.0));
      end if;
      P.Invulnerable := Max (0.0, P.Invulnerable - Dt);
      P.Dash_Cooldown := Max (0.0, P.Dash_Cooldown - Dt);
      if Input.Dash and then P.Dash_Cooldown = 0.0 then
         P.Dash_Direction := Normalize (Wanted, P.Facing);
         P.Dash_Left := Dash_Seconds;
         P.Dash_Cooldown := Dash_Cooldown_Seconds;
         P.Invulnerable := Max (P.Invulnerable, Dash_Shield_Seconds);
         Add_Ripple (W, P.Position, 0.5, Cyan);
         Request (Sounds, (Kind => Dash_Sound, Pan => Pan_Of (P.Position.X), Color => Cyan,
                           others => <>));
      end if;
      if P.Dash_Left > 0.0 then
         P.Dash_Left := Max (0.0, P.Dash_Left - Dt);
         P.Velocity := Dash_Speed * P.Dash_Direction;
         declare
            VX : constant Float := Between (W, -1.0, 1.0);
            VY : constant Float := Between (W, 0.0, 1.0);
            VZ : constant Float := Between (W, -1.0, 1.0);
         begin
            Emit (W, (P.Position.X, 0.35, P.Position.Y), (VX, VY, VZ),
                  Mix (Cyan, White, 0.3), 0.35, 0.16, Drag => 4.0);
         end;
      else
         P.Velocity := P.Velocity + Response * (Player_Speed * Wanted - P.Velocity);
      end if;
      P.Position := P.Position + Dt * P.Velocity;
      P.Position := (Clamp (P.Position.X, -Half_Width + Player_Radius, Half_Width - Player_Radius),
                     Clamp (P.Position.Y, -Half_Depth + Player_Radius, Half_Depth - Player_Radius));
      P.Facing := Normalize (Input.Aim - P.Position, P.Facing);

      --  Thruster: a stream behind the ship, stronger at speed.
      if Length (P.Velocity) > 1.0 and then Uniform (W) < 0.85 then
         declare
            Back : constant Vec2 := Normalize (P.Velocity, P.Facing);
            VX : constant Float := Between (W, -0.6, 0.6);
            VY : constant Float := Between (W, 0.0, 0.6);
            VZ : constant Float := Between (W, -0.6, 0.6);
            Hue : constant Float := Uniform (W);
         begin
            Emit (W, (P.Position.X - 0.5 * Back.X, 0.3, P.Position.Y - 0.5 * Back.Y),
                  (-2.0 * Back.X + VX, VY, -2.0 * Back.Y + VZ),
                  Mix (Hot_Pink, Sunset, Hue), 0.45, 0.1, Drag => 2.5);
         end;
      end if;
      P.Trail_Clock := P.Trail_Clock + Dt;
      if P.Trail_Clock >= Trail_Interval then
         P.Trail_Clock := 0.0;
         P.Trail_Head := (P.Trail_Head + 1) mod Trail_Length;
         P.Trail (P.Trail_Head) := P.Position;
      end if;

      --  Fire on the 32nd-note grid while held; the first shot at once.
      if Input.Fire then
         if not P.Fire_Was_Down then
            Fire_Shot (W, Sounds);
            P.Next_Shot := Next_Thirty_Second (W.Clock, Current);
         elsif W.Clock >= P.Next_Shot then
            Fire_Shot (W, Sounds);
            P.Next_Shot := Next_Thirty_Second (W.Clock, Current);
         end if;
      end if;
      P.Fire_Was_Down := Input.Fire;
   end Update_Player;

   procedure Bounce (E : in out Enemy) is
      Limit_X : constant Float := Half_Width - Enemy_Radius (E.Kind);
      Limit_Z : constant Float := Half_Depth - Enemy_Radius (E.Kind);
   begin
      if abs E.Position.X > Limit_X then
         E.Position.X := Clamp (E.Position.X, -Limit_X, Limit_X);
         E.Velocity.X := -E.Velocity.X;
      end if;
      if abs E.Position.Y > Limit_Z then
         E.Position.Y := Clamp (E.Position.Y, -Limit_Z, Limit_Z);
         E.Velocity.Y := -E.Velocity.Y;
      end if;
   end Bounce;

   procedure Fire_Orb (W : in out World; From, Velocity : Vec2; Hue : RGB) is
   begin
      for O of W.Orbs loop
         if not O.Alive then
            O := (Alive => True, Position => From, Velocity => Velocity, Age => 0.0, Hue => Hue);
            return;
         end if;
      end loop;
   end Fire_Orb;

   procedure Update_Enemies (W : in out World; Current : Step_Record; New_Step : Boolean;
                             Sounds : in out Sound_List) is
      Player : constant Vec2 := W.Player.Position;
      On_Beat : constant Boolean := New_Step and then Is_Beat (Current);
      Downbeat : constant Boolean := New_Step and then Current.Step = 0;
      Beat_Seconds : constant Float :=
        Float (Steps_Per_Beat * Current.Length) / Float (Sample_Rate);
      Build_Progress : constant Float :=
        (if Current.Section = Build and then Current.Section_Bars > 0
         then Float (Current.Section_Bar * Steps_Per_Bar + Current.Step)
              / Float (Current.Section_Bars * Steps_Per_Bar)
         else 0.0);
   begin
      for I in W.Enemies'Range loop
         if W.Enemies (I).Alive then
            declare
               E : Enemy renames W.Enemies (I);
               To_Player : constant Vec2 := Normalize (Player - E.Position, (0.0, 1.0));
            begin
               E.Age := E.Age + Dt;
               E.Hit_Flash := Max (0.0, E.Hit_Flash - 4.0 * Dt);
               if E.Spawn_Left > 0.0 then
                  E.Spawn_Left := Max (0.0, E.Spawn_Left - Dt);
                  if E.Spawn_Left = 0.0 then
                     Burst (W, (E.Position.X, 0.3, E.Position.Y), Enemy_Color (E.Kind), 18, 5.0, 0.6);
                     Add_Ripple (W, E.Position, 0.35, Enemy_Color (E.Kind));
                  end if;
               else
                  case E.Kind is
                     when Cube =>
                        if On_Beat then
                           declare
                              Wobble : constant Float := Between (W, -0.06, 0.06);
                              Reach : constant Float :=
                                Hop_Distance + 0.12 * Float (Natural'Min (3 * W.Stage, 10));
                           begin
                              E.Hop_From := E.Position;
                              E.Hop_To := E.Position + Reach * Rotate (To_Player, Wobble);
                           end;
                           E.Hop_Time := 0.0;
                        end if;
                        if E.Hop_Time < 1.0 then
                           E.Hop_Time := Min (1.0, E.Hop_Time + Dt / Hop_Seconds);
                           E.Position := E.Hop_From
                             + Smooth_Step (0.0, 1.0, E.Hop_Time) * (E.Hop_To - E.Hop_From);
                           E.Height := Hop_Height * Sin_Turns (0.5 * E.Hop_Time);
                           E.Spin := E.Spin + Dt * 1.1;
                        else
                           E.Height := 0.0;
                        end if;
                     when Pyramid =>
                        if On_Beat and then Current.Step mod (2 * Steps_Per_Beat) = 0 then
                           E.Velocity := Charge_Speed * To_Player;
                           E.Charge_Left := Charge_Seconds;
                        end if;
                        if E.Charge_Left > 0.0 then
                           E.Charge_Left := Max (0.0, E.Charge_Left - Dt);
                        else
                           E.Velocity := Exp (-4.0 * Dt) * E.Velocity;
                        end if;
                        E.Position := E.Position + Dt * E.Velocity;
                        E.Spin := E.Spin + Dt * (0.3 + 0.1 * Length (E.Velocity));
                     when Torus =>
                        E.Orbit := Fraction (E.Orbit + Dt * 0.04);
                        declare
                           Target : constant Vec2 :=
                             (0.6 * Half_Width * Cos_Turns (E.Orbit), 0.55 * Half_Depth * Sin_Turns (E.Orbit));
                        begin
                           E.Velocity := E.Velocity + (Dt * 1.5) * (Target - E.Position - E.Velocity);
                        end;
                        E.Position := E.Position + Dt * E.Velocity;
                        E.Spin := E.Spin + Dt * 0.25;
                        if Downbeat or else (On_Beat and then Current.Step = 8 and then Current.Energy >= 4) then
                           for K in 0 .. Orbs_Per_Ring - 1 loop
                              for O of W.Orbs loop
                                 if not O.Alive then
                                    O := (Alive => True, Position => E.Position,
                                          Velocity => Orb_Speed * Rotate ((1.0, 0.0),
                                            E.Spin + Float (K) / Float (Orbs_Per_Ring)),
                                          Age => 0.0, Hue => Hot_Pink);
                                    exit;
                                 end if;
                              end loop;
                           end loop;
                           Add_Ripple (W, E.Position, 0.4, Hot_Pink);
                        end if;
                     when Shard | Splinter =>
                        declare
                           Speed : constant Float := (if E.Kind = Shard then Shard_Speed else Splinter_Speed);
                           Side : constant Vec2 := (-To_Player.Y, To_Player.X);
                           Wanted : constant Vec2 :=
                             Speed * To_Player
                             + (if E.Kind = Shard then 3.0 * Sin_Turns (E.Age * 0.7) else 0.0) * Side;
                        begin
                           E.Velocity := E.Velocity + (Dt * 3.0) * (Wanted - E.Velocity);
                           E.Position := E.Position + Dt * E.Velocity;
                           E.Spin := E.Spin + Dt * 0.8;
                        end;
                     when Strobe =>
                        --  Marks a spot by the player on the bar's last beat,
                        --  jumps there on the downbeat, in one beat.
                        if New_Step and then Current.Step = Strobe_Mark_Step and then E.Hop_Time >= 1.0 then
                           declare
                              DX : constant Float := Between (W, -Strobe_Mark_Spread, Strobe_Mark_Spread);
                              DZ : constant Float := Between (W, -Strobe_Mark_Spread, Strobe_Mark_Spread);
                              Limit_X : constant Float := Half_Width - Enemy_Radius (Strobe);
                              Limit_Z : constant Float := Half_Depth - Enemy_Radius (Strobe);
                           begin
                              E.Hop_To := (Clamp (Player.X + DX, -Limit_X, Limit_X),
                                           Clamp (Player.Y + DZ, -Limit_Z, Limit_Z));
                              E.Armed := True;
                           end;
                        elsif Downbeat and then E.Armed then
                           E.Armed := False;
                           E.Hop_From := E.Position;
                           E.Hop_Time := 0.0;
                           E.Charge_Left := Beat_Seconds;
                           Request (Sounds, (Kind => Warp, Pan => Pan_Of (E.Position.X), Color => Ember,
                                             others => <>));
                        end if;
                        if E.Hop_Time < 1.0 then
                           E.Hop_Time := Min (1.0, E.Hop_Time + Dt / Max (E.Charge_Left, Dt));
                           E.Position := E.Hop_From
                             + Smooth_Step (0.0, 1.0, E.Hop_Time) * (E.Hop_To - E.Hop_From);
                           E.Height := Strobe_Lift * Sin_Turns (0.5 * E.Hop_Time);
                           E.Spin := E.Spin + Dt * 2.0;
                        else
                           E.Height := 0.0;
                           E.Spin := E.Spin + Dt * 0.3;
                        end if;
                     when Dropper =>
                        if not E.Armed and then New_Step and then Current.Impact then
                           --  The drop: it strikes.
                           E.Armed := True;
                           E.Power := 1.0;
                           E.Velocity := Dropper_Strike_Speed * To_Player;
                           E.Charge_Left := Dropper_Strike_Seconds;
                           for K in 0 .. Dropper_Orbs - 1 loop
                              Fire_Orb (W, E.Position,
                                        Orb_Speed * Rotate ((1.0, 0.0), E.Spin + Float (K) / Float (Dropper_Orbs)),
                                        Orchid);
                           end loop;
                           Add_Ripple (W, E.Position, 1.0, Orchid);
                           W.Shake := Max (W.Shake, 0.35);
                           Request (Sounds, (Kind => Burst_Sound, Pan => Pan_Of (E.Position.X),
                                             X => E.Position.X, Z => E.Position.Y, Color => Orchid,
                                             others => <>));
                        elsif not E.Armed then
                           --  Charging with the build, circling at a distance.
                           E.Power := Max (E.Power, Build_Progress);
                           declare
                              Away : constant Vec2 := Normalize (E.Position - Player, (1.0, 0.0));
                              Target : constant Vec2 := Player + Dropper_Hover * Rotate (Away, Dt * 0.08);
                           begin
                              E.Velocity := E.Velocity + (Dt * 2.0) * (1.5 * (Target - E.Position) - E.Velocity);
                           end;
                        elsif E.Charge_Left > 0.0 then
                           E.Charge_Left := Max (0.0, E.Charge_Left - Dt);
                        else
                           E.Velocity := E.Velocity + (Dt * 2.5) * (Dropper_Chase_Speed * To_Player - E.Velocity);
                        end if;
                        E.Position := E.Position + Dt * E.Velocity;
                        E.Spin := E.Spin + Dt * (0.2 + 1.5 * E.Power);
                     when Sentry =>
                        --  Keeps its range, circling; fires on every snare.
                        E.Orbit := Fraction (E.Orbit + Dt * 0.05);
                        declare
                           Target : constant Vec2 :=
                             Player + Sentry_Range * (Cos_Turns (E.Orbit), 0.6 * Sin_Turns (E.Orbit));
                        begin
                           E.Velocity := E.Velocity + (Dt * 1.2) * (Target - E.Position - E.Velocity);
                        end;
                        E.Position := E.Position + Dt * E.Velocity;
                        E.Spin := E.Spin + Dt * 0.6;
                        if New_Step and then Current.Snare then
                           Fire_Orb (W, E.Position, Bolt_Speed * To_Player, Coral);
                           Request (Sounds, (Kind => Enemy_Fire, Pan => Pan_Of (E.Position.X), Color => Coral,
                                             others => <>));
                        end if;
                     when Warden =>
                        --  Charges on the bar; a turning cross of bolts on every beat.
                        if Downbeat then
                           E.Velocity := Warden_Charge_Speed * To_Player;
                           E.Charge_Left := Warden_Charge_Seconds;
                           Request (Sounds, (Kind => Enemy_Fire, Pan => Pan_Of (E.Position.X), Color => Magenta,
                                             others => <>));
                        end if;
                        if E.Charge_Left > 0.0 then
                           E.Charge_Left := Max (0.0, E.Charge_Left - Dt);
                        else
                           E.Velocity := Exp (-3.0 * Dt) * E.Velocity;
                        end if;
                        E.Position := E.Position + Dt * E.Velocity;
                        E.Spin := E.Spin + Dt * 0.35;
                        if On_Beat then
                           declare
                              Count : constant Positive :=
                                (if 2 * E.Health < W.Boss_Health then 2 * Warden_Orbs else Warden_Orbs);
                           begin
                              for K in 0 .. Count - 1 loop
                                 Fire_Orb (W, E.Position,
                                           Orb_Speed * Rotate ((1.0, 0.0), Warden_Turn * Float (E.Volley)
                                                               + Float (K) / Float (Count)),
                                           Magenta);
                              end loop;
                              E.Volley := E.Volley + 1;
                           end;
                        end if;
                     when Totem =>
                        --  Drifts across the far half; a spiral on every beat,
                        --  an aimed triple on the snare, a ring on the chase's bars.
                        E.Orbit := Fraction (E.Orbit + Dt * 0.03);
                        declare
                           Target : constant Vec2 :=
                             (0.45 * Half_Width * Sin_Turns (E.Orbit),
                              Totem_Home.Y + 2.0 * Cos_Turns (2.0 * E.Orbit));
                        begin
                           E.Velocity := E.Velocity + (Dt * 1.0) * (Target - E.Position - E.Velocity);
                        end;
                        E.Position := E.Position + Dt * E.Velocity;
                        E.Spin := E.Spin + Dt * 0.2;
                        if On_Beat then
                           for K in 0 .. Totem_Spiral_Orbs - 1 loop
                              Fire_Orb (W, E.Position,
                                        Orb_Speed * Rotate ((1.0, 0.0), Totem_Spiral_Turn * Float (E.Volley)
                                                            + Float (K) / Float (Totem_Spiral_Orbs)),
                                        (if E.Volley mod 2 = 0 then Gold else Hot_Pink));
                           end loop;
                           E.Volley := E.Volley + 1;
                        end if;
                        if New_Step and then Current.Snare then
                           for K in -1 .. 1 loop
                              Fire_Orb (W, E.Position, Bolt_Speed * Rotate (To_Player, Totem_Spread * Float (K)),
                                        Cyan);
                           end loop;
                           Request (Sounds, (Kind => Enemy_Fire, Pan => Pan_Of (E.Position.X), Color => Cyan,
                                             others => <>));
                        end if;
                        if Downbeat and then Current.Feel = Double_Time then
                           for K in 0 .. Totem_Ring_Orbs - 1 loop
                              Fire_Orb (W, E.Position,
                                        Orb_Speed * Rotate ((1.0, 0.0), Float (K) / Float (Totem_Ring_Orbs)),
                                        Hot_Pink);
                           end loop;
                           Add_Ripple (W, E.Position, 0.8, Hot_Pink);
                        end if;
                  end case;
                  Bounce (E);
               end if;
            end;
         end if;
      end loop;
   end Update_Enemies;

   procedure Collide (W : in out World; Current : Step_Record; Sounds : in out Sound_List) is
      P : constant Vec2 := W.Player.Position;
      Vulnerable : constant Boolean := W.Player.Invulnerable = 0.0 and then W.Phase = Playing;
   begin
      for S of W.Shots loop
         if S.Alive then
            for I in W.Enemies'Range loop
               if W.Enemies (I).Alive and then W.Enemies (I).Spawn_Left = 0.0
                 and then Length (S.Position - W.Enemies (I).Position)
                          < Enemy_Radius (W.Enemies (I).Kind) + Shot_Radius
               then
                  S.Alive := False;
                  if W.Enemies (I).Kind in Boss_Kind and then W.Enemies (I).Hit_Flash < 0.5 then
                     Request (Sounds, (Kind => Boss_Hit, Pan => Pan_Of (S.Position.X),
                                       Color => Enemy_Color (W.Enemies (I).Kind), others => <>));
                  end if;
                  W.Enemies (I).Hit_Flash := 1.0;
                  Burst (W, (S.Position.X, 0.5, S.Position.Y), S.Hue, 6, 6.0, 0.35);
                  if W.Enemies (I).Health > 1 then
                     W.Enemies (I).Health := W.Enemies (I).Health - 1;
                  else
                     Kill (W, I, Current, Scored => True, Sounds => Sounds);
                  end if;
                  exit;
               end if;
            end loop;
         end if;
      end loop;
      if not Vulnerable then
         return;
      end if;
      for O of W.Orbs loop
         if O.Alive and then Length (O.Position - P) < Orb_Radius + Player_Radius then
            Hit_Player (W, Current, Sounds);
            return;
         end if;
      end loop;
      for E of W.Enemies loop
         if E.Alive and then E.Spawn_Left = 0.0
           and then Length (E.Position - P) < Enemy_Radius (E.Kind) + Player_Radius
         then
            Hit_Player (W, Current, Sounds);
            return;
         end if;
      end loop;
   end Collide;

   procedure Update_Projectiles (W : in out World) is
   begin
      for S of W.Shots loop
         if S.Alive then
            S.Age := S.Age + Dt;
            S.Position := S.Position + Dt * S.Velocity;
            if S.Age > Shot_Lifetime or else abs S.Position.X > Half_Width
              or else abs S.Position.Y > Half_Depth
            then
               S.Alive := False;
               Burst (W, (Clamp (S.Position.X, -Half_Width, Half_Width), 0.3,
                          Clamp (S.Position.Y, -Half_Depth, Half_Depth)), S.Hue, 4, 3.0, 0.3);
            end if;
         end if;
      end loop;
      for O of W.Orbs loop
         if O.Alive then
            O.Age := O.Age + Dt;
            O.Position := O.Position + Dt * O.Velocity;
            if abs O.Position.X > Half_Width or else abs O.Position.Y > Half_Depth then
               O.Alive := False;
            end if;
         end if;
      end loop;
   end Update_Projectiles;

   procedure Update_Effects (W : in out World) is
   begin
      for P of W.Particles loop
         if P.Life > 0.0 then
            P.Life := Max (0.0, P.Life - Dt);
            P.Velocity := Exp (-P.Drag * Dt) * P.Velocity;
            P.Velocity.Y := P.Velocity.Y - P.Gravity * Dt;
            P.Position := P.Position + Dt * P.Velocity;
            if P.Position.Y < 0.02 then
               P.Position.Y := 0.02;
               P.Velocity.Y := -0.45 * P.Velocity.Y;
            end if;
         end if;
      end loop;
      for R of W.Ripples loop
         if R.Age < 100.0 then
            R.Age := R.Age + Dt;
         end if;
      end loop;
      for I in W.Flashes'Range loop
         declare
            F : Flash renames W.Flashes (I);
         begin
            if F.Duration > 0.0 and then W.Clock >= F.At_Time then
               if F.Burst > 0 then
                  Burst (W, F.Position, F.Color, F.Burst, 7.0, 0.7);
                  Add_Ripple (W, (F.Position.X, F.Position.Z), 0.5, F.Color);
                  F.Burst := 0;
               end if;
               if Seconds (Sample_Offset (W.Clock - F.At_Time)) > F.Duration then
                  F.Duration := 0.0;
               end if;
            end if;
         end;
      end loop;
      W.Shake := Max (0.0, W.Shake - 2.5 * Dt);
      W.Screen_Flash := Max (0.0, W.Screen_Flash - 3.0 * Dt);
      W.Hurt_Flash := Max (0.0, W.Hurt_Flash - 1.5 * Dt);
      W.Banner := Max (0.0, W.Banner - Dt);
      W.Stage_Clear := Max (0.0, W.Stage_Clear - Dt);
      W.Last_Kill_On_Beat := Max (0.0, W.Last_Kill_On_Beat - Dt);
      if W.Combo_Timer > 0.0 then
         W.Combo_Timer := Max (0.0, W.Combo_Timer - Dt);
         if W.Combo_Timer = 0.0 then
            W.Combo := 0;
            W.Multiplier := 1;
         end if;
      end if;
   end Update_Effects;

   procedure Finish_Calibration (W : in out World) is
      Sorted : Tap_Array := W.Taps;
      Swap : Float;
   begin
      for I in 2 .. Calibration_Taps loop
         for J in reverse 2 .. I loop
            if Sorted (J) < Sorted (J - 1) then
               Swap := Sorted (J);
               Sorted (J) := Sorted (J - 1);
               Sorted (J - 1) := Swap;
            end if;
         end loop;
      end loop;
      W.Calibration_Offset_Ms :=
        Clamp (0.5 * (Sorted (Calibration_Taps / 2) + Sorted (Calibration_Taps / 2 + 1)), -250.0, 250.0);
      W.Phase := Title;
      W.Phase_Time := 0.0;
   end Finish_Calibration;

   --  The title's pickers: the playlist's seed, a random one, keeping a
   --  favourite, and where the run starts.
   procedure Title_Pickers (W : in out World; Input : Controls; Sounds : in out Sound_List) is
      Moved : Boolean := False;
      Value : Natural;
   begin
      if Input.Seed_Step /= 0 then
         W.Playlist := Neon_Style.Playlist_Seed
           (1 + (Unsigned_64 (Integer (W.Playlist) - 1 + Playlist_Span + Input.Seed_Step)) mod Playlist_Span);
         Moved := True;
      end if;
      if Input.Seed_Random then
         Neon_Random.Pick (W.Random, Playlist_Span, Value);
         W.Playlist := Neon_Style.Playlist_Seed (1 + Value);
         Moved := True;
      end if;
      if Input.Choice_Step /= 0 then
         W.Choice := (W.Choice + Choice_Count + Input.Choice_Step) mod Choice_Count;
         Moved := True;
      end if;
      if Input.Keep then
         if Is_Favourite (W, W.Playlist) then
            declare
               Kept : Favourite_Count := 0;
               Old : constant Favourite_Array := W.Favourites;
            begin
               for I in 1 .. W.Favourites_Kept loop
                  if Old (I) /= W.Playlist then
                     Kept := Kept + 1;
                     W.Favourites (Kept) := Old (I);
                  end if;
               end loop;
               W.Favourites_Kept := Kept;
            end;
         else
            --  Newest first; the oldest drops off a full list.
            W.Favourites (2 .. Favourite_Ceiling) := W.Favourites (1 .. Favourite_Ceiling - 1);
            W.Favourites (1) := W.Playlist;
            W.Favourites_Kept := Natural'Min (Favourite_Ceiling, W.Favourites_Kept + 1);
         end if;
         Moved := True;
      end if;
      if Input.Bestiary then
         W.Bestiary := not W.Bestiary;
         Moved := True;
      end if;
      if Moved then
         Request (Sounds, (Kind => Title_Tick, Color => Cyan, others => <>));
      end if;
   end Title_Pickers;

   procedure Step (W : in out World; Input : Controls; Now : Samples;
                   Current : Neon_Composer.Step_Record; New_Step : Boolean;
                   Sounds : in out Sound_List) is
   begin
      W.Clock := Now;
      W.Steps_Taken := W.Steps_Taken + 1;
      W.Phase_Time := W.Phase_Time + Dt;
      if New_Step then
         W.Last_Serial := Current.Serial;
         if W.Phase = Playing then
            On_Section (W, Current, Sounds);
            if Is_Beat (Current) then
               On_Beat_Spawns (W, Current, Sounds);
            end if;
         end if;
         if Current.Kick then
            W.Screen_Flash := Max (W.Screen_Flash, 0.12);
         end if;
      end if;

      if Input.Press_Judged and then W.Phase = Playing then
         Record_Judgement (W.Press_Timing, Input.Press_Error_Ms - W.Calibration_Offset_Ms);
      end if;

      case W.Phase is
         when Title =>
            Title_Pickers (W, Input, Sounds);
            if Input.Confirm then
               Start_Game (W);
               W.Demo := Input.Demo;
            elsif Input.Calibrate then
               W.Phase := Calibrating;
               W.Phase_Time := 0.0;
               W.Tap_Count := 0;
            end if;
         when Calibrating =>
            if Input.Calibrate then
               W.Phase := Title;
               W.Phase_Time := 0.0;
            elsif Input.Tap and then Input.Press_Judged then
               W.Tap_Count := W.Tap_Count + 1;
               W.Taps (W.Tap_Count) := Input.Press_Error_Ms;
               Add_Ripple (W, (0.0, 0.0), 0.6, Cyan);
               if W.Tap_Count = Calibration_Taps then
                  Finish_Calibration (W);
               end if;
            end if;
         when Playing =>
            Update_Player (W, Input, Current, Sounds);
            Update_Enemies (W, Current, New_Step, Sounds);
            Update_Projectiles (W);
            Collide (W, Current, Sounds);
         when Dying =>
            Update_Enemies (W, Current, New_Step, Sounds);
            Update_Projectiles (W);
            if W.Phase_Time < 1.0 and then Uniform (W) < 0.3 then
               declare
                  DX : constant Float := Between (W, -1.0, 1.0);
                  DZ : constant Float := Between (W, -1.0, 1.0);
                  Hue : constant Float := Uniform (W);
               begin
                  Burst (W, (W.Player.Position.X + DX, 0.5, W.Player.Position.Y + DZ),
                         Mix (Hot_Pink, Gold, Hue), 6, 10.0);
               end;
            end if;
            if W.Phase_Time >= Dying_Seconds then
               W.Phase := Game_Over;
               W.Phase_Time := 0.0;
               if W.Demo then
                  W.New_High_Rank := 0;
               else
                  Place_Score (W.Scores, W.Score, W.Stage, W.Playlist, W.New_High_Rank);
               end if;
               for E of W.Enemies loop
                  if E.Alive then
                     Burst (W, (E.Position.X, 0.5, E.Position.Y), Enemy_Color (E.Kind), 12, 6.0);
                     E.Alive := False;
                  end if;
               end loop;
            end if;
         when Game_Over =>
            if Input.Confirm and then W.Phase_Time > Restart_Delay then
               Start_Game (W);
               W.Demo := Input.Demo;
            end if;
      end case;
      Update_Effects (W);
   end Step;

end Neon_Sim;
