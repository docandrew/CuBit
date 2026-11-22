------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The dashboards folder's work (MET-002's app), on the data thread: the
--  window's requests (a listing, a dashboard opened, the dashboard saved as
--  a new file) done with the lock let go, and answered. The window checks
--  a dashboard opened against its type (Dash_View.End_Open).
--
--  @description
--  Platform-free: CuBit's filesystem queue (native/dash_runner) and the
--  hosted tests' folder in memory are the Worker's formal subprograms.
------------------------------------------------------------------------------
pragma Ada_2022;
with Dash_View;

package Dash_Jobs is
   --  How a file operation ended.
   type File_Result is (File_Done, File_Missing, File_Refused, File_Too_Large, File_Unavailable, File_Late, File_Failed);
   --  In plain words, for the status bar and the dialog.
   function Reason_For (Result : File_Result) return String;

   --  The largest dashboard file read (a checked value is far smaller:
   --  CCL.Types.Maximum_Value_Cells).
   Maximum_File_Bytes : constant := 262_144;

   generic
      --  The folder's .ccl files and when each changed.
      with procedure List (Into : out Dash_View.Folder_Listing);
      --  File Leaf's bytes (Got of them).
      with procedure Read (Leaf : String; Into : out String; Got : out Natural; Result : out File_Result);
      --  Bytes as the file Leaf (replacing it atomically).
      with procedure Write (Leaf, Bytes : String; Result : out File_Result);
      --  Whether Leaf is a file in the folder.
      with function Exists (Leaf : String) return Boolean;
   package Worker is
      --  One job, if the window asked for one (the lock is taken here).
      procedure Step (V : in out Dash_View.View);
   end Worker;
end Dash_Jobs;
