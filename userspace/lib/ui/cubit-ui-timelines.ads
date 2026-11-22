------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Timeline strip: lanes over a ruled track, markers on them, a playhead
------------------------------------------------------------------------------
--  One horizontal strip, a lane per thread or process (docs/debugging.md
--  8.2: the replay timeline), markers for events (system calls, IPC, faults,
--  writes, bookmarks) and a playhead. Positions are per mille of the track,
--  so the caller maps its own time onto it.
--
--  A strip that is not Enabled is drawn greyed with its Notice over the
--  track (what is not built yet, said plainly), takes no scrubbing, and
--  its playhead stands at the end ("now").
with Interfaces;
with CuBit.UI.Instrument;

package CuBit.UI.Timelines is
   MAX_LANES   : constant := 16;
   MAX_MARKERS : constant := 256;
   NAME_CAPACITY : constant := 16;
   NOTICE_CAPACITY : constant := 96;
   subtype Permille is Natural range 0 .. 1_000;
   subtype Lane_Count is Natural range 0 .. MAX_LANES;
   subtype Lane_Index is Positive range 1 .. MAX_LANES;
   subtype Marker_Count is Natural range 0 .. MAX_MARKERS;
   subtype Marker_Index is Positive range 1 .. MAX_MARKERS;

   type Lane is record
      Name   : String (1 .. NAME_CAPACITY) := [others => ' '];
      Length : Natural range 0 .. NAME_CAPACITY := 0;
   end record;
   type Lane_Array is array (Lane_Index) of Lane;
   function Lane_Of (Name : String) return Lane;

   type Marker_Kind is (Event_Mark, Write_Mark, Fault_Mark, Bookmark_Mark);
   type Marker is record
      Lane : Lane_Index := 1;
      At_Position : Permille := 0;
      Kind : Marker_Kind := Event_Mark;
   end record;
   type Marker_Array is array (Marker_Index) of Marker;

   type Strip is record
      Title    : String (1 .. NAME_CAPACITY) := [others => ' '];
      Title_Length : Natural range 0 .. NAME_CAPACITY := 0;
      --  The badge under the title ("coming", "recording", ...).
      Badge    : String (1 .. NAME_CAPACITY) := [others => ' '];
      Badge_Length : Natural range 0 .. NAME_CAPACITY := 0;
      Lanes    : Lane_Array;
      Lane_Total : Lane_Count := 0;
      Markers  : Marker_Array;
      Marker_Total : Marker_Count := 0;
      Playhead : Permille := Permille'Last;
      Enabled  : Boolean := False;
      Notice   : String (1 .. NOTICE_CAPACITY) := [others => ' '];
      Notice_Length : Natural range 0 .. NOTICE_CAPACITY := 0;
      --  The notice shows while the strip is disabled; the keys, dim, at
      --  its right.
      Keys     : String (1 .. NOTICE_CAPACITY) := [others => ' '];
      Keys_Length : Natural range 0 .. NOTICE_CAPACITY := 0;
   end record;
   procedure Set_Title (S : in out Strip; Title, Badge : String);
   procedure Set_Notice (S : in out Strip; Notice, Keys : String);

   LABEL_WIDTH : constant := 170;
   procedure Draw (C : Canvas; Area : Rect; S : Strip; Colors : CuBit.UI.Instrument.Palette);

   --  Scrubbing: the track position under X (Enabled strips only).
   procedure Position_At (S : Strip; Area : Rect; X : Natural; Position : out Permille; Inside : out Boolean);
   --  J, K, L, ",", "." (docs/debugging.md 8.2); No_Scrub when disabled.
   type Scrub is (No_Scrub, Reverse_Play, Pause, Forward_Play, Step_Back, Step_Forward);
   function Scrub_Of (S : Strip; Code : Interfaces.Unsigned_64) return Scrub;
end CuBit.UI.Timelines;
