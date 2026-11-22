------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Debugger (DBG-001): the session's snapshots and view, at library level
------------------------------------------------------------------------------
--  They are large (megabytes with the source pool and graph), so they live
--  here, never on a stack. Before is the previous read, for change marks.
with Debugger_Model;
with Debugger_View;

package Debugger_State is
   Current, Before : Debugger_Model.Snapshot;
   View : Debugger_View.View_State;
end Debugger_State;
