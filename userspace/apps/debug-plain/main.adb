------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  The plain debugger target (DBG-001): no stub, nothing offered. It says it
--  started, then counts rounds until it is stopped.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Messages; use CuBit.Messages;
with Plain_Work;

procedure Main is
begin
   debugPrint ("debug-plain: running" & ASCII.LF);
   Plain_Work.Run;
end Main;
