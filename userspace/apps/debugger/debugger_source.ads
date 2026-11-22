------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Debugger (DBG-001): where the snapshot's data comes from
------------------------------------------------------------------------------
--  One spec, two bodies: native/debugger_source.adb reads a real target
--  through the engine (CuBit.Debug); tests/debugger-gui/double has a test
--  double for the hosted tests, which says so in every snapshot it makes
--  (Target_Name "test double") and is never linked into the app.
with Debugger_Model; use Debugger_Model;

package Debugger_Source is
   type Attach_Result is (Attached, No_Target, Refused, Engine_Failed);

   procedure Attach (S : in out Snapshot; Result : out Attach_Result);
   --  Read again everything the views show. Before is the previous
   --  snapshot: its selections, expanded locals and graph are kept.
   procedure Read (Before : Snapshot; S : in out Snapshot);
   --  F5: let the target run to its next checkpoint, then read.
   procedure Continue (Before : Snapshot; S : in out Snapshot);
   --  Live (L): On lets the target run and Read then re-reads what can be
   --  read while it runs (threads, globals, the graph, memory) through the
   --  grants; frames and frame locals stay as at the last stop. Off parks
   --  it again and reads everything.
   procedure Set_Live (Before : Snapshot; S : in out Snapshot; On : Boolean);
   procedure Load_Children (S : in out Snapshot; I : Local_Index)
     with Pre => I <= S.Local_Total;
   --  Draw local I in the graph (as a new root).
   procedure Graph_Local (S : in out Snapshot; I : Local_Index)
     with Pre => I <= S.Local_Total;
   --  Expand node N's pointer field F into the graph.
   procedure Follow (S : in out Snapshot; N : Node_Index; F : Field_Index)
     with Pre => N <= S.Node_Total;
   procedure Memory_At (S : in out Snapshot; Address : Target_Address);
   procedure Select_Thread (Before : Snapshot; S : in out Snapshot; T : Thread_Index);
   procedure Select_Frame (Before : Snapshot; S : in out Snapshot; F : Frame_Index);
   --  Attach to process...: what runs now (procmgr's process-observer
   --  listing), into S.Processes.
   procedure List_Processes (S : in out Snapshot);
   --  Detach from the current target, then attach to S.Processes (P)
   --  through the kernel: procmgr mints Debug_Observe through the
   --  Debugger's "debug-session" authority, within the user session's
   --  debug envelope; the target needs no stub.
   procedure Attach_Process (S : in out Snapshot; P : Process_Index; Result : out Attach_Result)
     with Pre => P <= S.Process_Total;
   procedure Detach;
end Debugger_Source;
