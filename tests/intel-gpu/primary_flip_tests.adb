with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Flip_State;
with Intel_GPU_Vblank_Clock;
with Intel_GPU_Primary_Flip;
--  Hosted (Linux) tests of DISP-000a primary-plane flips against a register
--  model of one pipe whose behaviour follows i915 v6.16 facts: PLANE_SURF
--  is double-buffered and latches only at a vblank, PLANE_SURFLIVE reports
--  the latched surface, PIPE_FRMCOUNT counts vblanks and wraps. Vblanks
--  arrive at random points. The model counts every CPU write (seen as the
--  cache flush that follows it) into a surface that is armed or live
--  (writes_to_live), which must stay zero.
procedure Primary_Flip_Tests is
   package FS renames Intel_GPU_Flip_State;
   package VC renames Intel_GPU_Vblank_Clock;
   use type FS.Source;

   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Ada.Text_IO.Put_Line ("FAIL " & Name);
         raise Program_Error with Name;
      end if;
      Checks := Checks + 1;
   end Check;

   --  A small frame keeps the run fast: 64 x 16, pitch 256.
   Frame_W : constant := 64;
   Frame_H : constant := 16;
   Frame_P : constant := 256;
   Firmware_Surface : constant Unsigned_32 := 16#0010_0000#;
   Bases : constant array (0 .. 2) of Unsigned_32 :=
     [16#0200_0000#, 16#0210_0000#, 16#0220_0000#];
   type Pixels is array (0 .. 3 * Frame_P / 4 * Frame_H - 1) of Unsigned_32;
   Memory : Pixels := [others => 0];
   Memory_First : constant Unsigned_64 := Unsigned_64 (To_Integer (Memory (0)'Address));
   Frame_Bytes : constant Unsigned_64 := Frame_P * Frame_H;

   type Model is record
      Surf, Live : Unsigned_32 := Firmware_Surface;
      Frame : Unsigned_32 := 16#FFFF_FFFA#;
      Clock : Unsigned_64 := 10_000;
      Writes_To_Live, Vblanks, Surf_Writes : Natural := 0;
      Latch_Percent : Natural := 30;
      Bogus_Live : Boolean := False;
   end record;
   M : Model;

   subtype Percent is Natural range 0 .. 99;
   package Coin is new Ada.Numerics.Discrete_Random (Percent);
   G : Coin.Generator;

   procedure Vblank is
   begin
      M.Live := M.Surf;
      M.Frame := M.Frame + 1;
      M.Vblanks := M.Vblanks + 1;
   end Vblank;

   function Model_Read (Offset : Unsigned_32) return Unsigned_32 is
   begin
      if Offset = 16#1AC# then
         return (if M.Bogus_Live then 16#0FFF_F000# else M.Live);
      elsif Offset = 16#19C# then return M.Surf;
      elsif Offset = 16#040# then return M.Frame;
      else return 0;
      end if;
   end Model_Read;
   procedure Model_Write (Offset, Value : Unsigned_32; Written : out Boolean) is
   begin
      Written := Offset = 16#19C#;
      if Written then
         M.Surf := Value;
         M.Surf_Writes := M.Surf_Writes + 1;
      end if;
   end Model_Write;
   function Model_Flush (Address, Bytes : Unsigned_64) return Boolean is
      Buffer : constant Natural := Natural ((Address - Memory_First) / Frame_Bytes);
      Last : constant Natural := Natural ((Address + Bytes - 1 - Memory_First) / Frame_Bytes);
   begin
      Check (Address >= Memory_First and then Buffer = Last and then Buffer <= 2,
             "flush inside one buffer");
      if Bases (Buffer) = M.Surf or else Bases (Buffer) = M.Live then
         M.Writes_To_Live := M.Writes_To_Live + 1;
      end if;
      return True;
   end Model_Flush;
   function Model_Now return Unsigned_64 is (M.Clock);
   procedure Model_Pause is
   begin
      M.Clock := M.Clock + 100;
      if Coin.Random (G) < M.Latch_Percent then
         Vblank;
      end if;
   end Model_Pause;

   package Flip is new Intel_GPU_Primary_Flip
     (Model_Read, Model_Write, Model_Flush, Model_Now, Model_Pause);
   use type Flip.Takeover_Result, Flip.Submit_Result, Flip.Configure_Problem;

   procedure Configure (Firmware : Unsigned_32 := Firmware_Surface) is
   begin
      Flip.Configure
        ([Bases (0), Bases (1), Bases (2)],
         [Memory_First, Memory_First + Frame_Bytes, Memory_First + 2 * Frame_Bytes],
         Frame_W, Frame_H, Frame_P, Firmware);
   end Configure;

   procedure Pure_Cases is
      S : FS.State;
      Superseded, Latched : Boolean;
   begin
      Check (FS.Writable (S, FS.Buffer_1) and then FS.Writable (S, FS.Buffer_3),
             "firmware live: all buffers writable");
      FS.Arm (S, FS.Buffer_1, Superseded);
      Check (not Superseded and then not FS.Writable (S, FS.Buffer_1), "armed not writable");
      FS.Arm (S, FS.Buffer_2, Superseded);
      Check (Superseded and then S.Maybe (FS.Buffer_1) and then
             not FS.Writable (S, FS.Buffer_1), "replaced buffer may have latched");
      FS.Observe (S, FS.Buffer_1, Latched);
      Check (not Latched and then S.Owed and then S.Seen = FS.Buffer_1 and then
             not FS.Writable (S, FS.Buffer_1) and then FS.Writable (S, FS.Buffer_3),
             "replaced buffer seen live stays live");
      FS.Observe (S, FS.Buffer_2, Latched);
      Check (Latched and then not S.Owed and then FS.Writable (S, FS.Buffer_1),
             "latch frees the old live buffer");
      FS.Observe (S, FS.Buffer_3, Latched);
      Check (S.Faulted and then not FS.Writable (S, FS.Buffer_1),
             "impossible PLANE_SURFLIVE faults");
      Check (VC.Late (16#FFFF_FFFF#, 1) and then not VC.Late (16#FFFF_FFFF#, 0),
             "late latch across the wrap");
      Check (VC.Measure (50_000, 3) = 16_666 and then VC.Measure (50_000, 0) = VC.Default_Period_Us
             and then VC.Measure (1_000, 1) = VC.Default_Period_Us, "period measure");
      Check (VC.Next_Vblank (1_000, 5_000, 16_667) = 17_667 and then
             VC.Next_Vblank (1_000, 50_000, 16_667) = 66_667, "next vblank");
   end Pure_Cases;

   procedure Takeover_Cases is
      R : Flip.Takeover_Result;
      Ok : Boolean;
   begin
      --  No vblank ever: timeout, firmware surface armed again, flips off.
      M := (others => <>);
      M.Latch_Percent := 0;
      Configure;
      Flip.Take_Over (16#0013_1518#, R);
      Check (R = Flip.Latch_Timeout and then not Flip.Ready and then
             M.Surf = Firmware_Surface, "takeover timeout restores the firmware surface");
      --  A surface the driver never armed: refused.
      M := (others => <>);
      M.Bogus_Live := True;
      Configure;
      Flip.Take_Over (16#0013_1518#, R);
      Check (R = Flip.Unexpected_Surface and then not Flip.Ready and then
             M.Surf = Firmware_Surface, "unexpected PLANE_SURFLIVE abandons");
      --  GOP framebuffers commonly sit at GGTT offset 0: a valid surface.
      M := (others => <>);
      M.Surf := 0;
      M.Live := 0;
      Configure (Firmware => 0);
      Check (Flip.Last_Problem = Flip.Ready_To_Flip, "firmware surface at GGTT 0 configures");
      Flip.Take_Over (16#0013_1518#, R);
      Check (R = Flip.Taken and then M.Live = Bases (0), "takeover from a surface at GGTT 0");
      --  Each refused configuration names its reason.
      Flip.Configure ([Bases (0), Bases (0), Bases (2)],
                      [Memory_First, Memory_First + Frame_Bytes, Memory_First + 2 * Frame_Bytes],
                      Frame_W, Frame_H, Frame_P, Firmware_Surface);
      Check (Flip.Last_Problem = Flip.Buffers_Not_Distinct and then not Flip.Ready,
             "duplicate buffers refused by name");
      Flip.Configure ([Bases (0), Bases (1), Firmware_Surface],
                      [Memory_First, Memory_First + Frame_Bytes, Memory_First + 2 * Frame_Bytes],
                      Frame_W, Frame_H, Frame_P, Firmware_Surface);
      Check (Flip.Last_Problem = Flip.Buffer_Is_Firmware_Surface, "buffer on the firmware surface refused");
      Flip.Configure ([Bases (0), Bases (1), Bases (2)],
                      [Memory_First, Memory_First + Frame_Bytes, Memory_First + 2 * Frame_Bytes],
                      Frame_W, Frame_H, Frame_P, Firmware_Surface + 16);
      Check (Flip.Last_Problem = Flip.Firmware_Surface_Unaligned, "unaligned firmware surface refused");
      --  Normal takeover.
      M := (others => <>);
      Configure;
      Flip.Take_Over (16#0013_1518#, R);
      Check (R = Flip.Taken and then Flip.Ready and then M.Live = Bases (0),
             "takeover latches buffer 0");
      Check (Memory (0) = 16#0013_1518# and then Memory (Memory'Last) = 16#0013_1518#,
             "buffers filled");
      Flip.Clear (16#00FF_0000#, Ok);
      Check (Ok and then M.Live = Bases (0) and then Memory (0) = 16#00FF_0000# and then
             Memory (Memory'Last) = 16#00FF_0000#, "clear with buffer 0 live ends on buffer 0");
      Check (M.Writes_To_Live = 0, "takeover and clear never write a live buffer");
      --  Watchdog view: an armed flip that never latches ages and stays owed.
      declare
         Result : Flip.Submit_Result;
         Superseded : Boolean;
         Latched : Boolean;
         View : Flip.Stall_View;
      begin
         M.Latch_Percent := 0;
         Flip.Submit (1, 0, Frame_H, Result, Superseded);
         Check (Result = Flip.Armed, "watchdog: armed");
         M.Clock := M.Clock + 1_500_000;
         Flip.Service (Latched);
         View := Flip.Stall;
         Check (not Latched and then View.Owed and then
                Flip."=" (View.Armed, 1) and then View.Owed_Us >= 1_500_000 and then View.Surface_Word = Bases (1) and then
                View.Live_Word = Bases (0), "watchdog sees an unlatched flip and its age");
         Vblank;
         Flip.Service (Latched);
         Check (Latched and then not Flip.Stall.Owed, "a late latch still completes");
         M.Latch_Percent := 30;
      end;
   end Takeover_Cases;

   --  display.svc's pattern (one frame at a time, rotating over three) plus
   --  mailbox submissions, random vblanks, long run.
   procedure Random_Cases is
      R : Flip.Takeover_Result;
      Result : Flip.Submit_Result;
      Superseded, Latched : Boolean;
      Active : Natural := 0;     --  last latched buffer (display's view)
      In_Flight : Integer := -1; --  owed buffer
      Armed_Frames, Answered : Natural := 0;
      C : Flip.Counters;
      Total_Latched, Total_Superseded, Total_Rejected : Natural := 0;
   begin
      M := (others => <>);
      Coin.Reset (G, 3);
      Configure;
      Flip.Take_Over (0, R);
      Check (R = Flip.Taken, "random: takeover");
      Flip.Take (C);
      for Step in 1 .. 200_000 loop
         M.Clock := M.Clock + 50;
         case Coin.Random (G) is
            when 0 .. 29 =>
               --  FIFO present into the next buffer after the live one.
               if In_Flight < 0 then
                  Flip.Submit (Flip.Index ((Active + 1) mod 3), Step mod Frame_H, 1,
                               Result, Superseded);
                  Check (Result = Flip.Armed and then not Superseded, "fifo present armed");
                  In_Flight := (Active + 1) mod 3;
                  Armed_Frames := Armed_Frames + 1;
               end if;
            when 30 .. 34 =>
               --  Mailbox: present while one is owed, into any buffer.
               declare
                  B : constant Natural := Coin.Random (G) mod 3;
               begin
                  Flip.Submit (Flip.Index (B), 0, Frame_H, Result, Superseded);
                  if Result = Flip.Armed then
                     Armed_Frames := Armed_Frames + 1;
                     if Superseded then Answered := Answered + 1; end if;
                     In_Flight := B;
                  else
                     Check (Result = Flip.Not_Writable, "refusal is not-writable");
                     Total_Rejected := Total_Rejected + 1;
                  end if;
               end;
            when 35 .. 74 =>
               Flip.Service (Latched);
               if Latched then
                  Answered := Answered + 1;
                  Active := In_Flight;
                  In_Flight := -1;
               end if;
            when others =>
               Vblank;
         end case;
         if Step mod 1_000 = 0 then
            Flip.Take (C);
            Total_Latched := Total_Latched + C.Latched;
            Total_Superseded := Total_Superseded + C.Superseded;
            Check (C.Faults = 0, "no faults");
         end if;
      end loop;
      --  Drain: the owed frame latches at a later vblank.
      while Flip.Owed loop
         Vblank;
         Flip.Service (Latched);
         if Latched then Answered := Answered + 1; end if;
      end loop;
      Flip.Take (C);
      Total_Latched := Total_Latched + C.Latched;
      Total_Superseded := Total_Superseded + C.Superseded;
      Check (Answered = Armed_Frames, "every frame answered: latched or superseded");
      Check (Total_Latched + Total_Superseded = Armed_Frames, "counters");
      Check (M.Writes_To_Live = 0, "writes_to_live = 0");
      Ada.Text_IO.Put_Line ("random run: frames" & Natural'Image (Armed_Frames) &
        " latched" & Natural'Image (Total_Latched) & " superseded" &
        Natural'Image (Total_Superseded) & " refused" & Natural'Image (Total_Rejected) &
        " vblanks" & Natural'Image (M.Vblanks));
   end Random_Cases;
begin
   Pure_Cases;
   Takeover_Cases;
   Random_Cases;
   Ada.Text_IO.Put_Line ("PASS primary flip:" & Natural'Image (Checks) & " checks");
end Primary_Flip_Tests;
