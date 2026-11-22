------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The cooperative debug stub (DBG-001 preview) a debuggable program links.
--
--  @description
--  Start registers the debug-target service role and runs the stub's server
--  thread (CuBit.Debug_Protocol). The program's observed threads are made
--  with Spawn, so the stub knows their stacks. Each calls Checkpoint where
--  it may be stopped: Checkpoint records the thread's registers on the
--  state page and, while a debugger has asked for a pause, parks there
--  until it is resumed. That is a cooperative stop: a thread that never
--  reaches a checkpoint is never stopped.
--
--  What the stub shares, it shares READ-ONLY, of its own pages, only to the
--  debugger whose call it is answering. A debugger can never write to the
--  program through it. Not named CuBit.Debug.Stub: a child unit would link
--  the whole engine into every debuggable program.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Handles;

package CuBit.Debug_Stub is

   type Thread_Body is access procedure;

   --  Registration: the program's startup handle for
   --  (notification "debug-target" Notify_Access.Manage "registration").
   procedure Start
     (Program      : String;
      Registration : CuBit.Handles.Notification;
      Started      : out Boolean);

   --  Run Run on a new observed thread named Name (at most
   --  CuBit.Debug_Protocol.T_Name_Bytes shown), on a stack of the stub's.
   procedure Spawn (Name : String; Run : not null Thread_Body; Started : out Boolean);

   --  Record this thread's registers for the debugger, and park while a
   --  pause is asked. A thread Spawn did not make returns at once.
   procedure Checkpoint with No_Inline;

end CuBit.Debug_Stub;
