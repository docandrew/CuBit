------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Where a CPU frame's drawing time goes, by path (timing builds): pixels
--  and time per path each frame, so the per-pixel cost of each path can be
--  compared with the machine's copy speed (Desktop_Memory_Bench).
--
--  @description
--  Each drawing primitive brackets itself with Enter and Leave. The time
--  between them is the path's own: a path entered inside another (a fill
--  inside the window chrome, the client's pixels inside a window) pauses
--  the outer one, so every TSC tick of the frame's drawing is charged to
--  exactly one path, and whatever no path claims is Other. The cost is two
--  TSC reads per primitive call, never anything per pixel; pixels are the
--  call's area as the primitive clipped it. Overdraw is the pixels written
--  by all paths beyond the frame's merged damage.
--
--  Off (not Desktop_Timing_Policy.Enabled), Enter and Leave return at once.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Desktop_Path_Timing is

   type Path is
     (Client_Row_Copy,   --  a client's 1:1 rows, memcpy (Compositor_Row_Copy)
      Client_Sampler,    --  a client's pixels through Compositor_Sampling
      Fill,              --  solid rectangles
      Backdrop,          --  the wallpaper and backdrop layers (row copies)
      Backdrop_Build,    --  (re)building a retained wallpaper layer: resampling
                         --  the image at the output's size, once per change
      Blend,             --  shadows and other blended rectangles
      Text,              --  text runs: layout, clipping, batching
      Text_Glyphs,       --  the runs' glyphs: cache lookups and blending
      Icon,              --  icons
      Chrome,            --  window frames, titles, buttons
      Deskbar,           --  the Deskbar and its menus, panels, toasts, tooltips
      Cursor);           --  the pointer overlay

   --  Why a client's pixels took the sampler instead of the row copy.
   type Sampler_Reason is (Scaled, Size_Differs, Rotated, Other_Reason);

   procedure Enter (P : Path) with Inline;
   procedure Leave (Pixels : Unsigned_64 := 0) with Inline;
   procedure Sampler_Because (Why : Sampler_Reason; Pixels : Unsigned_64);
   --  Count N items of path P this frame (text runs, glyphs).
   procedure Add_Items (P : Path; N : Unsigned_64);

   --  The frame begins and ends (the scene's drawing for one output).
   procedure Begin_Frame;
   procedure End_Frame (Merged_Pixels : Unsigned_64);

   type Path_Counts is array (Path) of Unsigned_64;
   type Reason_Counts is array (Sampler_Reason) of Unsigned_64;
   type Frame_Paths is record
      Pixels, Micros, Items : Path_Counts := [others => 0];
      Sampler_Pixels : Reason_Counts := [others => 0];
      Other_Micros, Total_Micros, Overdraw_Pixels : Unsigned_64 := 0;
   end record;

   --  The last finished frame (for the metrics series), and the totals
   --  since the last Take_Period (for the timing report).
   function Last_Frame return Frame_Paths;
   procedure Take_Period (Totals : out Frame_Paths; Frames : out Unsigned_64);

   function Name (P : Path) return String;
   function Name (R : Sampler_Reason) return String;

end Desktop_Path_Timing;
