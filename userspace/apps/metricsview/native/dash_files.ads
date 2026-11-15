------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Saved dashboards on CuBit (MET-002's app, PLAN-metricsview.md "Saved
--  dashboards"): a dashboard is a file holding its CCL value, read at launch
--  and written in the background after edits.
--
--  @description
--  The filesystem queue (CuBit.Filesystem_Sessions) behind the manifest's
--  metricsview-filesystem endpoint, granted the dashboards' place only.
--  Calls wait for their answers until a deadline the caller passes: the UI
--  thread loads once at launch, and only the data thread saves (never under
--  Dash_Sync's lock). A save is atomic: the bytes go to a hidden sibling,
--  flushed, closed and renamed over the file, so a crash leaves the old
--  value or the new one, never part of one.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Handles;

package Dash_Files is
   type Outcome is (Done, Not_Found, Not_Granted, Too_Large, Unavailable, Late, Failed);

   procedure Open (Endpoint : CuBit.Handles.Endpoint; Ready : out Boolean);
   function Is_Open return Boolean;

   --  The file Name's bytes into Into (Got of them; Too_Large when it holds
   --  more), waiting until Deadline_Ms (monotonic milliseconds).
   procedure Load (Name : String; Into : out String; Got : out Natural; Deadline_Ms : Unsigned_64;
                   Result : out Outcome);

   --  Bytes as the file Name, replacing it atomically.
   procedure Save (Name, Bytes : String; Deadline_Ms : Unsigned_64; Result : out Outcome);

   --  The filesystem's reply to the last call that failed (diagnostics).
   function Last_Reply return Unsigned_32;
end Dash_Files;
