with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Display_Topology;
with Intel_GPU_Plane_Decode;
with Intel_GPU_Firmware_Output;
with Intel_GPU_Cursor_Program;
with Intel_GPU_Cursor_Surfaces;
with Intel_GPU_Cursor_Plane;
with Intel_GPU_Cursor_DDB;
--  Hosted (Linux) tests of the DISP-003 cursor path: firmware-output
--  selection, surface choice and fitting, and the cursor plane service
--  driven against a register model of one pipe's cursor registers whose
--  behaviour follows i915 v6.16 facts: CURCNTR/CURPOS/CUR_FBC_CTL are
--  double-buffered and armed by the CURBASE write, the armed values latch
--  at the next vblank, and CURSURFLIVE reports the base being scanned.
--  The model counts every CPU write into a surface that is armed or live
--  (writes_to_live), which must stay zero under random latch timing.
procedure Cursor_Plane_Tests is
   package Topology renames Intel_GPU_Display_Topology;
   package FO renames Intel_GPU_Firmware_Output;
   package CP renames Intel_GPU_Cursor_Program;
   package CS renames Intel_GPU_Cursor_Surfaces;
   package CDDB renames Intel_GPU_Cursor_DDB;
   use type CDDB.Entry_Range, CDDB.Outcome;
   use type FO.Outcome, FO.Geometry, Topology.Pipe, CP.Cursor_Width, CP.Field,
     CP.Position, CS.Readiness;

   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Ada.Text_IO.Put_Line ("FAIL " & Name);
         raise Program_Error with Name;
      end if;
      Checks := Checks + 1;
   end Check;

   --  8 MiB GGTT table: a 4 GiB aperture.
   Table_Bytes : constant Unsigned_64 := 8 * 1024 * 1024;

   function Primary (Width, Height, Pitch : Natural; Surface : Unsigned_32;
                     Offset : Unsigned_32 := 0) return FO.Primary is
      S : constant Intel_GPU_Plane_Decode.Sample :=
        (Control => 16#8400_0000#,
         Stride => Unsigned_32 (Pitch / 64),
         Size => Shift_Left (Unsigned_32 (Height - 1), 16) or Unsigned_32 (Width - 1),
         Offset => Offset, Surface => Surface, Live_Surface => Surface);
   begin
      return (True, S, S);
   end Primary;

   procedure Selection_Cases is
      Planes : FO.Primaries;
      R : FO.Selection;
   begin
      R := FO.Select_Output (Planes, Table_Bytes, 1920, 1080, 7680);
      Check (R.State = FO.No_Pipe, "no live primary");
      Planes (Topology.B) := Primary (1920, 1080, 7680, 16#0010_0000#);
      R := FO.Select_Output (Planes, Table_Bytes, 1920, 1080, 7680);
      Check (R.State = FO.Selected and then R.Pipe = Topology.B and then
             R.Surface = 16#0010_0000# and then R.Shape = (1920, 1080, 7680),
             "pipe B selected");
      Check (FO.Select_Output (Planes, Table_Bytes, 1920, 1080, 8192).State = FO.No_Pipe,
             "pitch must match");
      Check (FO.Select_Output (Planes, Table_Bytes, 1280, 1080, 7680).State = FO.No_Pipe,
             "width must match");
      Check (FO.Select_Output (Planes, Table_Bytes, 1920, 1080, 4000).State =
               FO.Invalid_Framebuffer, "pitch below width");
      Check (FO.Select_Output (Planes, Table_Bytes, 0, 1080, 7680).State =
               FO.Invalid_Framebuffer, "zero width");
      Planes (Topology.A) := Primary (1920, 1080, 7680, 16#0300_0000#);
      Check (FO.Select_Output (Planes, Table_Bytes, 1920, 1080, 7680).State =
               FO.Several_Pipes, "cloned firmware mode selects nothing");
      Planes (Topology.A) := Primary (1920, 1080, 7680, 16#0300_0000#, Offset => 16);
      R := FO.Select_Output (Planes, Table_Bytes, 1920, 1080, 7680);
      Check (R.State = FO.Selected and then R.Pipe = Topology.B,
             "panned plane does not match");
      Planes (Topology.A) := Primary (1920, 1080, 7680, 16#0300_0000#);
      Planes (Topology.A).After.Surface := 16#0400_0000#;
      R := FO.Select_Output (Planes, Table_Bytes, 1920, 1080, 7680);
      Check (R.State = FO.Selected and then R.Pipe = Topology.B,
             "changing plane does not match");
      Planes (Topology.B).Before.Control := 16#8400_0400#;  -- X-tiled
      Planes (Topology.B).After := Planes (Topology.B).Before;
      Check (FO.Select_Output (Planes, Table_Bytes, 1920, 1080, 7680).State = FO.No_Pipe,
             "tiled plane does not match");
   end Selection_Cases;

   procedure Surface_Cases is
      B : constant CS.Bases := [16#0100_0000#, 16#0104_0000#, 16#0108_0000#];
      Values : constant array (1 .. 5) of Unsigned_32 :=
        [0, B (1), B (2), B (3), 16#FFFF_F000#];
      I : CS.Surface_Index;
      F : CP.Image;
   begin
      Check (CS.Valid_Bases (B), "valid bases");
      for P of Values loop
         for L of Values loop
            I := CS.Pick (B, P, L);
            Check (B (I) /= P and then B (I) /= L, "pick avoids armed and live");
         end loop;
      end loop;
      for W in 1 .. CS.Edge loop
         for H in 1 .. CS.Edge loop
            F := CS.Fit ((W, H));
            Check (CP.Pixels (F.Width) >= W and then CP.Pixels (F.Width) >= H and then
                   F.Height = Integer'Max (8, H) and then
                   CP.Valid_Image ((F.Width, F.Height, 16#1_0000#), True),
                   "fit holds the image");
            Check ((if Integer'Max (W, H) <= 64 then F.Width = CP.Width_64
                    elsif Integer'Max (W, H) <= 128 then F.Width = CP.Width_128
                    else F.Width = CP.Width_256), "fit is smallest");
         end loop;
      end loop;
      Check (CS.Allocation (0) = 0, "no allocation");
      Check (CS.Allocation (Shift_Left (Unsigned_32 (2047), 16) or 2016) = 32,
             "32-block cursor allocation (end inclusive)");
      Check (CS.Allocation (Shift_Left (Unsigned_32 (5), 16) or 9) = 0,
             "inverted allocation is none");
      Check (CS.Classify (Shift_Left (Unsigned_32 (2047), 16) or 2016, 16#8000_0010#) =
               CS.Ready_Firmware_Watermark, "firmware watermark kept");
      Check (CS.Classify (Shift_Left (Unsigned_32 (2047), 16) or 2016, 0) =
               CS.Ready_Fallback_Watermark, "fallback watermark");
      Check (CS.Classify (0, 16#8000_0010#) = CS.No_Allocation, "no DDB: composited");
      Check (CS.Fallback_Watermark (32) = 16#C000_001F#, "fallback below allocation");
   end Surface_Cases;

   ---------------------------------------------------------------------------
   --  Register model of one pipe's cursor.
   ---------------------------------------------------------------------------
   type Field_Log is array (1 .. 8) of CP.Field;
   --  Pipe page registers outside the four cursor fields: written values
   --  (what a read returns) and, for the double-buffered data-buffer and
   --  watermark registers, the values latched at the last vblank after an
   --  arm (PLANE_SURF arms plane 1's, CURBASE the cursor's).
   type Page_Words is array (Unsigned_32 range 0 .. 16#3FF#) of Unsigned_32;
   Plane_Start : constant := 0;
   Plane_End_Inclusive : constant := 2047;   --  one 2048-block pipe range
   Firmware_Plane_Buffer : constant Unsigned_32 :=
     Shift_Left (Unsigned_32 (Plane_End_Inclusive), 16) or Plane_Start;
   type Model is record
      Armed, Live : CP.Register_Values;
      Written : Page_Words := [others => 0];
      Latched : Page_Words := [others => 0];
      Plane_Armed, Cursor_Armed : Boolean := False;
      Watermark_Writes : Natural := 0;
      Last : Field_Log;
      Last_Count : Natural := 0;
      Writes_To_Live : Natural := 0;
      Vblanks : Natural := 0;
      Frame : Unsigned_32 := 16#FFFF_FFF0#;   --  wraps during the tests
      Clock : Unsigned_64 := 1_000;
      Latch_Probability : Natural := 50;
      Overlap_Seen, Outside_Seen : Natural := 0;
   end record;
   M : Model;
   Readable : Boolean := True;
   function Index (Offset : Unsigned_32) return Unsigned_32 is (Offset / 4);
   Plane_Buffer_Offset : constant Unsigned_32 := 16#27C#;
   Plane_Surface_Offset : constant Unsigned_32 := 16#19C#;
   Frame_Offset : constant Unsigned_32 := 16#040#;
   function Plane_Register (I : Unsigned_32) return Boolean is
     (I * 4 in 16#240# .. 16#27C#);
   function Cursor_Register (I : Unsigned_32) return Boolean is
     (I * 4 in 16#140# .. 16#17C#);

   procedure Reset_Model (Cursor_Buffer, Cursor_WM0 : Unsigned_32) is
   begin
      M.Written := [others => 0];
      M.Written (Index (Plane_Buffer_Offset)) := Firmware_Plane_Buffer;
      for L in Unsigned_32 range 0 .. 5 loop
         --  Firmware plane-1 watermarks: enabled, lines 1 + L, 40 + 20 L blocks.
         M.Written (Index (16#240#) + L) :=
           CDDB.Enable_Bit or Shift_Left (1 + L, 14) or (40 + 20 * L);
      end loop;
      M.Written (Index (16#258#)) := CDDB.Enable_Bit or Shift_Left (2, 14) or 60;
      M.Written (Index (16#17C#)) := Cursor_Buffer;
      M.Written (Index (16#140#)) := Cursor_WM0;
      M.Written (Index (Plane_Surface_Offset)) := 16#0010_0000#;
      M.Latched := M.Written;
      M.Plane_Armed := False;
      M.Cursor_Armed := False;
      M.Armed := CP.Disabled;
      M.Live := CP.Disabled;
   end Reset_Model;

   --  The latched plane-1 and cursor ranges never overlap and never leave
   --  the pipe's range.
   procedure Check_Ranges is
      P : constant CDDB.Range_Decoding := CDDB.Decode (M.Latched (Index (Plane_Buffer_Offset)));
      C : constant CDDB.Range_Decoding := CDDB.Decode (M.Latched (Index (16#17C#)));
   begin
      Check (P.Valid and then C.Valid, "latched ranges decode");
      if not CDDB.Disjoint (P.Value, C.Value) then M.Overlap_Seen := M.Overlap_Seen + 1; end if;
      if (not CDDB.Empty (C.Value) and then
            (C.Value.First < Plane_Start or else C.Value.Stop > Plane_End_Inclusive + 1)) or else
         P.Value.Stop > Plane_End_Inclusive + 1
      then
         M.Outside_Seen := M.Outside_Seen + 1;
      end if;
   end Check_Ranges;

   procedure Write_Fields (Values : CP.Register_Values; Order : CP.Write_Sequence) is
   begin
      M.Last_Count := 0;
      for F of Order loop
         M.Last_Count := M.Last_Count + 1;
         M.Last (M.Last_Count) := F;
         case F is
            when CP.FBC_Control => M.Armed.FBC_Control := Values.FBC_Control;
            when CP.Control => M.Armed.Control := Values.Control;
            when CP.Position_Field => M.Armed.Position_Word := Values.Position_Word;
            when CP.Base =>
               M.Armed.Base := Values.Base;
               M.Cursor_Armed := True;
         end case;
      end loop;
   end Write_Fields;
   function Model_Program (Values : CP.Register_Values) return Boolean is
   begin
      Write_Fields (Values, CP.Full_Update);
      return True;
   end Model_Program;
   function Model_Move (Values : CP.Register_Values) return Boolean is
   begin
      Write_Fields (Values, CP.Move_Update);
      return True;
   end Model_Move;
   procedure Vblank is
   begin
      M.Live := M.Armed;
      for I in M.Written'Range loop
         if (M.Plane_Armed and then Plane_Register (I)) or else
           (M.Cursor_Armed and then Cursor_Register (I))
         then
            M.Latched (I) := M.Written (I);
         end if;
      end loop;
      M.Plane_Armed := False;
      M.Cursor_Armed := False;
      M.Vblanks := M.Vblanks + 1;
      M.Frame := M.Frame + 1;
      Check_Ranges;
   end Vblank;
   function Model_Read (Offset : Unsigned_32) return Unsigned_32 is
   begin
      if not Readable then return Unsigned_32'Last; end if;
      if Offset = CS.Live_Base_Offset then return M.Live.Base;
      elsif Offset = Frame_Offset then return M.Frame;
      else return M.Written (Index (Offset));
      end if;
   end Model_Read;
   procedure Model_Write (Offset, Value : Unsigned_32; Written : out Boolean) is
   begin
      Written := True;
      M.Written (Index (Offset)) := Value;
      if Offset = CS.Watermark_Offset then
         M.Watermark_Writes := M.Watermark_Writes + 1;
      end if;
      if Offset = Plane_Surface_Offset then
         M.Plane_Armed := True;
      end if;
      Check_Ranges;
   end Model_Write;
   function Model_Now return Unsigned_64 is (M.Clock);

   subtype Percent is Natural range 0 .. 99;
   package Coin is new Ada.Numerics.Discrete_Random (Percent);
   Coin_State : Coin.Generator;
   procedure Model_Pause is
   begin
      M.Clock := M.Clock + 1;
      if Coin.Random (Coin_State) < M.Latch_Probability then
         Vblank;
      end if;
   end Model_Pause;

   --  CPU memory: staging, then the three surfaces (GGTT base Surface_GGTT).
   type Pixels is array (Natural range <>) of Unsigned_32;
   Staging : Pixels (0 .. CS.Edge * CS.Edge - 1) := [others => 0];
   Surfaces : Pixels (0 .. CS.Surface_Count * CS.Edge * CS.Edge - 1) := [others => 0];
   Surface_GGTT : constant Unsigned_32 := 16#0200_0000#;
   Surface_CPU : constant Unsigned_64 :=
     Unsigned_64 (To_Integer (Surfaces (0)'Address));

   function GGTT_Of (Address : Unsigned_64) return Unsigned_32 is
     (Surface_GGTT + Unsigned_32 ((Address - Surface_CPU) / CS.Surface_Bytes) * CS.Surface_Bytes);
   Flushed_Base : Unsigned_32 := 0;
   function Model_Flush (Address, Bytes : Unsigned_64) return Boolean is
      Base : constant Unsigned_32 := GGTT_Of (Address);
   begin
      Check (Address >= Surface_CPU and then
             (Address - Surface_CPU) mod CS.Surface_Bytes = 0 and then
             Bytes <= CS.Surface_Bytes, "flush covers one surface");
      --  The copy just wrote this surface: it must not be armed or live.
      if Base = M.Armed.Base or else Base = M.Live.Base then
         M.Writes_To_Live := M.Writes_To_Live + 1;
      end if;
      Flushed_Base := Base;
      return True;
   end Model_Flush;

   package Plane is new Intel_GPU_Cursor_Plane
     (Model_Program, Model_Move, Model_Read, Model_Write, Model_Flush,
      Model_Now, Model_Pause);
   use type Plane.Status;

   procedure Stage (Width, Height : Natural; Seed : Unsigned_32) is
   begin
      Staging := [others => 0];
      for Y in 0 .. Height - 1 loop
         for X in 0 .. Width - 1 loop
            Staging (Y * CS.Edge + X) := 16#FF00_0000# or Seed or Unsigned_32 (X + Y);
         end loop;
      end loop;
   end Stage;

   procedure DDB_Cases is
      R : CS.Readiness;
      Plan : CDDB.Plan_Result;
      WM : CDDB.Watermarks := [others => 0];
      Others_Free : constant CDDB.Other_Planes := [others => 0];
      Staged : constant Unsigned_64 := Unsigned_64 (To_Integer (Staging (0)'Address));
   begin
      --  Pure planner: one active pipe, plane 1 owns 0 .. 2047.
      WM (CDDB.Level_0) := CDDB.Enable_Bit or Shift_Left (Unsigned_32 (1), 14) or 40;
      Plan := CDDB.Plan (Firmware_Plane_Buffer, Others_Free, WM, 0, True);
      Check (Plan.State = CDDB.Ready and then Plan.Cursor = (2016, 2048) and then
             Plan.Plane_After = (0, 2016) and then
             Plan.Cursor_Buffer = (Shift_Left (Unsigned_32 (2047), 16) or 2016) and then
             Plan.Plane_Buffer = (Shift_Left (Unsigned_32 (2015), 16) or 0),
             "32-block cursor at the end of the pipe's range");
      Check (Plan.Cursor_WM (CDDB.Level_0) = (CDDB.Enable_Bit or Shift_Left (Unsigned_32 (1), 14) or 4),
             "cursor level 0: one line x 3 blocks + 1");
      Check (CDDB.Plan (Firmware_Plane_Buffer, Others_Free, WM, 0, False).Cursor = (2040, 2048),
             "8 blocks with several pipes");
      Check (CDDB.Plan (Firmware_Plane_Buffer, Others_Free, WM, 16#07FF_07E0#, True).State =
               CDDB.Cursor_Already_Allocated, "firmware allocation kept");
      Check (CDDB.Plan (Shift_Left (Unsigned_32 (20), 16), Others_Free, WM, 0, True).State =
               CDDB.Plane_Too_Small, "small plane range refused");
      Check (CDDB.Plan (Firmware_Plane_Buffer, [16#07FF_07F0#, 0, 0, 0], WM, 0, True).State =
               CDDB.Overlaps_Other_Plane, "another plane in the tail refused");
      Check (CDDB.Plan (Firmware_Plane_Buffer, Others_Free, [others => 0], 0, True).State =
               CDDB.Plane_Too_Small, "plane without level 0 refused");
      WM (CDDB.Level_5) := CDDB.Enable_Bit or Shift_Left (Unsigned_32 (30), 14) or 2030;
      Plan := CDDB.Plan (Firmware_Plane_Buffer, Others_Free, WM, 0, True);
      Check (Plan.State = CDDB.Ready and then CDDB.Size (Plan.Cursor) = 102 and then
             not CDDB.Enabled (Plan.Plane_WM (CDDB.Level_5)) and then
             Plan.Plane_Levels_Disabled = 1 and then
             CDDB.Enabled (Plan.Cursor_WM (CDDB.Level_5)),
             "deep cursor level sizes the allocation; plane level not fitting is disabled");
      for Blocks in Unsigned_32 range 1 .. 4095 loop
         Plan := CDDB.Plan (Blocks * 2 ** 16, Others_Free, WM, 0, Blocks mod 2 = 0);
         if Plan.State = CDDB.Ready then
            Check (CDDB.Disjoint (Plan.Plane_After, Plan.Cursor) and then
                   Plan.Cursor.Stop = Natural (Blocks) + 1, "sweep: disjoint tail");
         end if;
      end loop;
      --  Native sequence against the register model, random latch timing.
      Coin.Reset (Coin_State, 11);
      for Run in 1 .. 200 loop
         Reset_Model (0, 16#0000_4007#);
         M.Latch_Probability := 10 + Run mod 80;
         Plane.Configure (Staged, Surface_CPU, Surface_GGTT, 1920, 1080, True, R);
         if R /= CS.Ready_Driver_Allocation then
            Ada.Text_IO.Put_Line (CS.Readiness_Name (R) & " " & CDDB.Name (Plane.Plan_State));
         end if;
         Check (R = CS.Ready_Driver_Allocation and then Plane.Ready, "driver allocation");
         Check (M.Latched (Index (16#17C#)) = (Shift_Left (Unsigned_32 (2047), 16) or 2016) and then
                M.Latched (Index (Plane_Buffer_Offset)) = (Shift_Left (Unsigned_32 (2015), 16) or 0),
                "both changes latched");
         Check (Plane.Before.Cursor_Buffer = 0 and then
                Plane.After.Cursor_Buffer = (Shift_Left (Unsigned_32 (2047), 16) or 2016),
                "before/after snapshots");
      end loop;
      Check (M.Overlap_Seen = 0, "plane 1 and cursor ranges never overlap");
      Check (M.Outside_Seen = 0, "never beyond the pipe's range");
      --  No vblank ever: the first step times out, the cursor stays off.
      Reset_Model (0, 0);
      M.Latch_Probability := 0;
      Plane.Configure (Staged, Surface_CPU, Surface_GGTT, 1920, 1080, True, R);
      Check (R = CS.Latch_Timeout and then not Plane.Ready and then
             M.Written (Index (16#17C#)) = 0, "latch timeout before the cursor is touched");
      M.Latch_Probability := 50;
      Vblank;
      Check (M.Overlap_Seen = 0, "timeout leaves no overlap");
   end DDB_Cases;

   procedure Plane_Cases is
      R : CS.Readiness;
      S : Plane.Status;
      Base_Index : Natural;
      C : Plane.Counters;
   begin
      Reset_Model (Shift_Left (Unsigned_32 (2047), 16) or 2016, 0);
      M.Watermark_Writes := 0;
      Plane.Configure (Unsigned_64 (To_Integer (Staging (0)'Address)), Surface_CPU,
                       Surface_GGTT, 1920, 1080, True, R);
      Check (R = CS.Ready_Fallback_Watermark and then Plane.Ready and then
             M.Written (Index (CS.Watermark_Offset)) = CS.Fallback_Watermark (32) and then
             M.Watermark_Writes = 1,
             "configure programs the fallback watermark");
      Plane.Move_To (10, 10, S);
      Check (S = Plane.Bad_State, "move before show refused");
      Stage (24, 30, 16#00AB_0000#);
      Plane.Show (100, 200, S);
      Check (S = Plane.Accepted and then Plane.Enabled and then
             Plane.Image.Width = CP.Width_64 and then Plane.Image.Height = 30,
             "show fits a 24x30 image in a 64-wide cursor");
      Check (M.Last_Count = 4 and then M.Last (1) = CP.FBC_Control and then
             M.Last (2) = CP.Control and then M.Last (3) = CP.Position_Field and then
             M.Last (4) = CP.Base, "i915 order: FBC, control, position, base");
      Check (M.Armed.Control = 16#1000_0027# and then
             M.Armed.FBC_Control = (CP.FBC_Enable or 29) and then
             M.Armed.Position_Word = CP.Encode_Position (100, 200) and then
             M.Armed.Base = Flushed_Base, "armed registers");
      Base_Index := Natural ((M.Armed.Base - Surface_GGTT) / CS.Surface_Bytes);
      for Y in 0 .. 29 loop
         for X in 0 .. 63 loop
            Check (Surfaces (Base_Index * CS.Edge * CS.Edge + Y * 64 + X) =
                     Staging (Y * CS.Edge + X), "image packed at pitch 64");
         end loop;
      end loop;
      Plane.Move_To (101, 199, S);
      Check (S = Plane.Accepted and then M.Last_Count = 2 and then
             M.Last (1) = CP.Position_Field and then M.Last (2) = CP.Base and then
             M.Armed.Position_Word = CP.Encode_Position (101, 199),
             "move writes position then base");
      Plane.Move_To (-63, -29, S);
      Check (S = Plane.Accepted and then Plane.Enabled and then
             M.Armed.Position_Word = CP.Encode_Position (-63, -29), "negative edge");
      Plane.Move_To (-64, 0, S);
      Check (S = Plane.Accepted and then not Plane.Enabled and then M.Armed.Control = 0,
             "entirely off the pipe disables");
      Plane.Move_To (1919, 1079, S);
      Check (S = Plane.Accepted and then Plane.Enabled and then M.Last_Count = 4,
             "back on the pipe re-arms fully");
      Plane.Move_To (1920, 0, S);
      Check (not Plane.Enabled, "right edge off the pipe");
      Plane.Hide (S);
      Check (S = Plane.Accepted and then M.Armed.Control = 0 and then M.Armed.Base = 0,
             "hide disables");
      Plane.Move_To (5, 5, S);
      Check (S = Plane.Bad_State, "move after hide refused");
      Stage (0, 0, 0);
      Plane.Show (5, 5, S);
      Check (S = Plane.Accepted and then not Plane.Enabled, "empty image shows nothing");
      Stage (200, 100, 16#0011_0000#);
      Plane.Show (5, 5, S);
      Check (S = Plane.Accepted and then Plane.Image.Width = CP.Width_256 and then
             Plane.Image.Height = 100, "large cursor");
      Readable := False;
      Plane.Show (5, 5, S);
      Check (S = Plane.Bad_State, "unreadable CURSURFLIVE refuses a show");
      Readable := True;
      Plane.Take (C);
      Check (C.Failures = 3 and then C.Shows = 3 and then C.Hides = 1, "counters");
   end Plane_Cases;

   --  Random shows, moves, hides and vblanks: no write ever reaches an armed
   --  or live surface, and every move's armed position is the newest.
   procedure Random_Cases is
      subtype Choice is Natural range 0 .. 99;
      package Random is new Ada.Numerics.Discrete_Random (Choice);
      G : Random.Generator;
      S : Plane.Status;
      Shown : Boolean := Plane.Ready;  -- Plane_Cases left an image defined
      X, Y : Integer;
   begin
      Random.Reset (G, 7);
      for Step in 1 .. 40_000 loop
         case Random.Random (G) is
            when 0 .. 9 =>
               Stage (1 + Random.Random (G) mod 64, 1 + Random.Random (G) mod 64,
                      Unsigned_32 (Step mod 256) * 16#100#);
               Plane.Show (Random.Random (G) * 19, Random.Random (G) * 10, S);
               Check (S = Plane.Accepted, "random show");
               Shown := True;
            when 10 .. 69 =>
               X := Random.Random (G) * 21 - 100;
               Y := Random.Random (G) * 12 - 100;
               Plane.Move_To (X, Y, S);
               Check (S = (if Shown then Plane.Accepted else Plane.Bad_State), "random move");
               if Shown and then Plane.Enabled then
                  Check (M.Armed.Position_Word = CP.Encode_Position
                           (CP.Position (X), CP.Position (Y)), "newest position armed");
               end if;
            when 70 .. 72 =>
               Plane.Hide (S);
               Shown := False;
            when others =>
               Vblank;
         end case;
      end loop;
      Check (M.Writes_To_Live = 0, "writes_to_live = 0");
   end Random_Cases;
begin
   Selection_Cases;
   Surface_Cases;
   DDB_Cases;
   Plane_Cases;
   Random_Cases;
   Ada.Text_IO.Put_Line ("PASS cursor plane:" & Natural'Image (Checks) & " checks, vblanks" &
                         Natural'Image (M.Vblanks));
end Cursor_Plane_Tests;
