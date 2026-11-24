------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast's game: the arena, the player, the enemies and their waves,
--  shots, particles, score and combos, stepped 240 times a song second.
--
--  @description
--  The simulation knows nothing of windows, pixels or speakers. Each step
--  takes the step's controls and the composer's steps that began in it
--  (the beat grid), and leaves sound requests for the audio side. Its only
--  randomness is its own seeded generator, so a seed and the same controls
--  replay the same game exactly.
--
--  The arena is the floor plane: X right (-Half_Width .. Half_Width) and Z
--  away from the camera (-Half_Depth .. Half_Depth); Y is height.
--
--  Enemies move on the music: cubes hop a step toward the player on every
--  beat, pyramids charge on every other beat, tori fire rings of orbs on
--  downbeats, shards weave and split, strobes jump on the bar to where
--  they marked a beat before, sentries fire on the snare, droppers charge
--  through a build and only strike on the drop. A warden (the mini-boss)
--  arrives on a stage's first drop, and the totem (the boss) at its peak.
--
--  The game follows the song's form, not the other way round: a stage is
--  the composer's stage (Neon_Composer), its sections are the waves, and
--  each section spawns its own mix. Felling the totem lets the music move
--  on, through an outro, into the next stage's style, palette and grid.
--  The title picks the playlist's seed (any seed is a new style), the
--  start (a stage, or a stage's boss) and keeps favourite seeds.
--
--  Every pool below is a named design ceiling of what one screen holds;
--  reaching one drops the oldest particle or refuses the spawn, and the
--  overlay shows the counts.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Neon_Math; use Neon_Math;
with Neon_Time; use Neon_Time;
with Neon_Random;
with Neon_Style;
with Neon_Composer;

package Neon_Sim is

   Half_Width : constant := 16.0;
   Half_Depth : constant := 9.0;

   type Phase_Kind is (Title, Playing, Dying, Game_Over, Calibrating);

   --  One step's controls: what the player is doing in this 1/240 s.
   type Controls is record
      Move : Vec2 := (0.0, 0.0);      --  WASD, each axis -1 .. 1 (Y: away)
      Aim : Vec2 := (0.0, 1.0);       --  the floor point under the pointer
      Fire : Boolean := False;        --  held
      Dash : Boolean := False;        --  pressed in this step
      Confirm : Boolean := False;     --  start / restart (pressed in this step)
      Calibrate : Boolean := False;   --  open or close calibration
      Tap : Boolean := False;         --  a calibration tap (pressed in this step)
      Demo : Boolean := False;        --  Confirm starts an attract-mode demo game
      --  The title's pickers (pressed in this step): the seed back or on,
      --  a random seed, keep the seed, and the start choice back or on.
      Seed_Step : Integer range -1 .. 1 := 0;
      Seed_Random : Boolean := False;
      Keep : Boolean := False;
      Choice_Step : Integer range -1 .. 1 := 0;
      Bestiary : Boolean := False;    --  show or hide the enemy guide
      --  The newest press among Fire, Dash and Tap in this step, judged
      --  against the beat by the caller (who has the hardware-side clock):
      --  signed milliseconds, late positive.
      Press_Judged : Boolean := False;
      Press_Error_Ms : Float := 0.0;
   end record;

   type Enemy_Kind is
     (Cube, Pyramid, Torus, Shard, Splinter,
      Strobe,     --  jumps on the bar to the mark it set a beat before
      Dropper,    --  charges through a build, bursts and strikes on the drop
      Sentry,     --  fires an aimed bolt on every snare
      Warden,     --  mini-boss: the first drop's
      Totem);     --  boss: the stage's peak
   subtype Boss_Kind is Enemy_Kind range Warden .. Totem;

   ---------------------------------------------------------------------------
   --  What the audio side is asked to play (resolved against the beat grid
   --  there: shots take the chord, explosions their next sixteenth).
   type Sound_Kind is
     (Shot, Explosion, Dash_Sound, Hurt_Sound, Combo_Chime, Spawn_Warning, Wave_Chime,
      Death_Sound, Tap_Sound,
      Enemy_Fire,     --  a sentry's or a boss's bolt: a falling zap on the chord
      Warp,           --  a strobe's jump: a rising zap
      Burst_Sound,    --  a dropper strikes
      Boss_Arrive,    --  growl and impact
      Boss_Hit,       --  a clank
      Boss_Down,      --  the boss falls
      Title_Tick);    --  a title picker moved
   type Sound_Request is record
      Kind : Sound_Kind := Shot;
      Pan : Float := 0.0;         --  -1 .. 1 from the arena X
      Degree : Natural := 0;      --  shots: the chord tone to play
      Size : Float := 1.0;        --  explosions: how big
      X, Z : Float := 0.0;        --  where (explosions flash there)
      Color : RGB;
   end record;
   Sound_Ceiling : constant := 64;
   type Sound_Array is array (1 .. Sound_Ceiling) of Sound_Request;
   type Sound_List is record
      Count : Natural range 0 .. Sound_Ceiling := 0;
      Items : Sound_Array;
   end record;

   ---------------------------------------------------------------------------
   Enemy_Ceiling : constant := 96;
   Shot_Ceiling : constant := 96;
   Orb_Ceiling : constant := 160;
   Particle_Ceiling : constant := 3072;
   Ripple_Ceiling : constant := 16;
   Flash_Ceiling : constant := 24;
   Trail_Length : constant := 24;
   Score_Table_Size : constant := 8;

   type Enemy is record
      Alive : Boolean := False;
      Kind : Enemy_Kind := Cube;
      Power : Float := 0.0;           --  a dropper's charge, 0 .. 1
      Armed : Boolean := False;       --  a strobe has its mark; a dropper has struck
      Volley : Natural := 0;          --  a boss's pattern counter
      Position, Velocity : Vec2;
      Height : Float := 0.0;          --  hops lift cubes off the floor
      Health : Natural := 1;
      Spin : Float := 0.0;            --  turns
      Spawn_Left : Float := 0.0;      --  seconds of warning before it is solid
      Hop_From, Hop_To : Vec2;
      Hop_Time : Float := 1.0;        --  0 .. 1 through the current hop
      Charge_Left : Float := 0.0;
      Hit_Flash : Float := 0.0;
      Age : Float := 0.0;
      Orbit : Float := 0.0;
   end record;
   type Enemy_Array is array (1 .. Enemy_Ceiling) of Enemy;

   type Bolt is record
      Alive : Boolean := False;
      Position, Velocity : Vec2;
      Hue : RGB;
      Age : Float := 0.0;
   end record;
   type Shot_Array is array (1 .. Shot_Ceiling) of Bolt;

   type Orb is record
      Alive : Boolean := False;
      Position, Velocity : Vec2;
      Age : Float := 0.0;
      Hue : RGB;
   end record;
   type Orb_Array is array (1 .. Orb_Ceiling) of Orb;

   type Particle is record
      Position, Velocity : Vec3;
      Color : RGB;
      Life, Lifetime : Float := 0.0;   --  seconds left, seconds total
      Size : Float := 0.1;
      Drag : Float := 0.0;
      Gravity : Float := 0.0;
   end record;
   subtype Particle_Index is Natural range 0 .. Particle_Ceiling - 1;
   type Particle_Array is array (Particle_Index) of Particle;

   type Ripple is record
      Center : Vec2;
      Age : Float := 100.0;      --  seconds since it began; old ones are idle
      Strength : Float := 0.0;
      Color : RGB;
   end record;
   type Ripple_Array is array (1 .. Ripple_Ceiling) of Ripple;

   --  A light that blooms at a song time (explosion flashes land on the
   --  explosion's sixteenth).
   type Flash is record
      Position : Vec3;
      Color : RGB;
      At_Time : Samples := 0;
      Duration : Float := 0.0;    --  seconds; 0 = idle
      Burst : Natural := 0;       --  particles to throw when it lands
   end record;
   type Flash_Array is array (1 .. Flash_Ceiling) of Flash;

   type Trail_Array is array (0 .. Trail_Length - 1) of Vec2;

   type Score_Entry is record
      Score : Natural := 0;
      Stage : Natural := 0;
      Playlist : Unsigned_64 := 0;
   end record;
   type Score_Table is array (1 .. Score_Table_Size) of Score_Entry;

   type Player_State is record
      Position, Velocity : Vec2;
      Facing : Vec2 := (0.0, 1.0);
      Lives : Natural := 3;
      Invulnerable : Float := 0.0;
      Dash_Left : Float := 0.0;
      Dash_Cooldown : Float := 0.0;
      Dash_Direction : Vec2 := (0.0, 1.0);
      Next_Shot : Samples := 0;
      Shot_Degree : Natural := 0;
      Degree_Up : Boolean := True;
      Trail : Trail_Array;
      Trail_Head : Natural := 0;
      Trail_Clock : Float := 0.0;
      Fire_Was_Down : Boolean := False;
   end record;

   --  Timing judgement statistics (song samples), for the overlay.
   type Judgement is record
      Count : Natural := 0;
      Sum, Sum_Squares : Float := 0.0;   --  of errors in milliseconds
      Last : Float := 0.0;
   end record;

   Calibration_Taps : constant := 16;
   Favourite_Ceiling : constant := 6;
   subtype Favourite_Count is Natural range 0 .. Favourite_Ceiling;
   type Favourite_Array is array (1 .. Favourite_Ceiling) of Unsigned_64;
   --  The title's start choices: a stage from its intro, or a stage's boss.
   Choice_Count : constant := 2 * Neon_Composer.Stages_Per_Run;
   subtype Start_Choice is Natural range 0 .. Choice_Count - 1;
   function Choice_Stage (C : Start_Choice) return Positive is
     (1 + C mod Neon_Composer.Stages_Per_Run);
   function Choice_Is_Boss (C : Start_Choice) return Boolean is
     (C >= Neon_Composer.Stages_Per_Run);
   type Tap_Array is array (1 .. Calibration_Taps) of Float;

   type World is record
      Random : Neon_Random.Generator;
      Phase : Phase_Kind := Title;
      Phase_Time : Float := 0.0;
      Clock : Samples := 0;            --  song time of the current step
      Steps_Taken : Natural := 0;
      Player : Player_State;
      Enemies : Enemy_Array;
      Shots : Shot_Array;
      Orbs : Orb_Array;
      Particles : Particle_Array;
      Next_Particle : Particle_Index := 0;
      Ripples : Ripple_Array;
      Next_Ripple : Positive range 1 .. Ripple_Ceiling := 1;
      Flashes : Flash_Array;
      Score : Natural := 0;
      Combo : Natural := 0;
      Combo_Timer : Float := 0.0;
      Multiplier : Positive := 1;
      Rhythm_Streak : Natural := 0;
      Best_Streak : Natural := 0;
      On_Beat_Kills : Natural := 0;
      Last_Kill_On_Beat : Float := 0.0;   --  seconds the "ON BEAT" banner shows
      Kill_Timing : Judgement;
      Press_Timing : Judgement;
      --  The song's form as the game last saw it.
      Run : Natural := 0;                 --  games started (the composer starts a run on a new one)
      Stage : Natural := 0;
      Section : Neon_Composer.Section_Kind := Neon_Composer.Title_Theme;
      Defeated_Stage : Natural := 0;      --  the newest stage whose totem is down
      Warden_Stage : Natural := 0;        --  the newest stage a warden came to
      Totem_Stage : Natural := 0;         --  the newest stage a totem came to
      Boss : Natural := 0;                --  the boss's enemy slot (0: none)
      Boss_Health : Natural := 1;         --  its starting health, for the bar
      Banner : Float := 0.0;              --  seconds the section banner shows
      Banner_Kind : Neon_Composer.Section_Kind := Neon_Composer.Intro;
      Stage_Clear : Float := 0.0;         --  seconds the stage-clear banner shows
      Stages_Cleared : Natural := 0;
      --  The title's pickers.
      Playlist : Neon_Style.Playlist_Seed := 1;
      Choice : Start_Choice := 0;
      Favourites : Favourite_Array := [others => 0];
      Favourites_Kept : Favourite_Count := 0;
      Bestiary : Boolean := False;        --  the title shows the enemy guide
      Shake : Float := 0.0;
      Screen_Flash : Float := 0.0;
      Hurt_Flash : Float := 0.0;
      Last_Serial : Natural := 0;         --  composer steps consumed
      Scores : Score_Table;
      New_High_Rank : Natural := 0;       --  1 .. Score_Table_Size, 0 = none
      Calibration_Offset_Ms : Float := 0.0;
      Taps : Tap_Array := [others => 0.0];
      Tap_Count : Natural := 0;
      Kills : Natural := 0;
      Shots_Fired : Natural := 0;
      Spawn_Refusals : Natural := 0;
      Demo : Boolean := False;            --  the autopilot plays; scores are not kept
   end record;

   procedure Reset (W : out World; Seed : Unsigned_64);

   --  The mood the soundtrack should take now.
   function Mood (W : World) return Neon_Composer.Mood;

   --  One step at song time Now. Current is the composer step playing at
   --  Now; New_Step says it began in this step (at most one does: steps are
   --  4500 samples or more, simulation steps 200).
   procedure Step (W : in out World; Input : Controls; Now : Samples;
                   Current : Neon_Composer.Step_Record; New_Step : Boolean;
                   Sounds : in out Sound_List);

   --  Schedule a flash at song time At (from the audio side: an explosion's
   --  sixteenth).
   procedure Add_Flash (W : in out World; Position : Vec3; Color : RGB; At_Time : Samples;
                        Duration : Float; Burst : Natural);

   --  Judge a timed event against the beat: Error_Ms is signed (late > 0).
   procedure Record_Judgement (J : in out Judgement; Error_Ms : Float);
   function Mean (J : Judgement) return Float;
   function Spread (J : Judgement) return Float;

   --  Insert Score into the table; Rank is its place (0: not placed).
   procedure Place_Score (Table : in out Score_Table; Score, Stage : Natural; Playlist : Unsigned_64;
                          Rank : out Natural);
   --  The seed is among the kept favourites.
   function Is_Favourite (W : World; Seed : Unsigned_64) return Boolean;

   --  Colours of the palette (also used by the renderers).
   Hot_Pink : constant RGB := From_Hex (16#FF2BD6#);
   Cyan : constant RGB := From_Hex (16#18F0FF#);
   Violet : constant RGB := From_Hex (16#8A4BFF#);
   Sunset : constant RGB := From_Hex (16#FF8A3D#);
   Lime : constant RGB := From_Hex (16#9DFF6A#);
   Gold : constant RGB := From_Hex (16#FFD84A#);
   White : constant RGB := (1.0, 1.0, 1.0);
   Ember : constant RGB := From_Hex (16#FF6A2B#);     --  strobes
   Orchid : constant RGB := From_Hex (16#B05CFF#);    --  droppers
   Coral : constant RGB := From_Hex (16#FF3D5A#);     --  sentries
   Magenta : constant RGB := From_Hex (16#FF4FD8#);   --  the warden

   function Enemy_Color (Kind : Enemy_Kind) return RGB is
     (case Kind is
        when Cube => Cyan,
        when Pyramid => Gold,
        when Torus => Hot_Pink,
        when Shard => Lime,
        when Splinter => Lime,
        when Strobe => Ember,
        when Dropper => Orchid,
        when Sentry => Coral,
        when Warden => Magenta,
        when Totem => Gold);
   function Enemy_Radius (Kind : Enemy_Kind) return Float is
     (case Kind is
        when Cube => 0.9, when Pyramid => 0.85, when Torus => 1.3, when Shard => 0.8,
        when Splinter => 0.5, when Strobe => 0.9, when Dropper => 0.8, when Sentry => 0.9,
        when Warden => 1.6, when Totem => 2.6);

end Neon_Sim;
