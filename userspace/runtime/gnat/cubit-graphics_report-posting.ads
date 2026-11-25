------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Posting an entry to the renderer report (GPU-014): the two calls of
--  CuBit.Graphics_Report's Post_Label and Post_End_Label to devmgr, each
--  acknowledged at once (devmgr only files it) and bounded by
--  Post_Deadline_Ms. The calls go down the IPC graph: devmgr never calls
--  its posters. A post that cannot be delivered is reported
--  back (Delivered False), so the poster can log it: the report then lacks
--  the entry, and the poster's own log says why.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Handles;

package CuBit.Graphics_Report.Posting is

   --  Each send waits at most this long for devmgr to take it.
   Post_Deadline_Ms : constant := 100;

   procedure Post (Target : CuBit.Handles.Endpoint; Item : Report_Entry; Delivered : out Boolean);

   --  procmgr only (devmgr takes it from its parent alone): Program ended.
   procedure Gone (Target : CuBit.Handles.Endpoint; Program : Subject_Id; Delivered : out Boolean);

end CuBit.Graphics_Report.Posting;
