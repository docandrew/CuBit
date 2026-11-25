------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Desktop's share of the renderer report (GPU-014): how its output is
--  composed, and why when on the CPU, posted to devmgr's report
--  (CuBit.Graphics_Report), at start and at each runtime fallback.
--
--  @description
--  Desktop composes on the CPU by default (desktop.composition = CPU,
--  LAT-002): that is Software, Policy Composition_CPU, in the report but
--  never a warning (Indicator_Of: Quiet). Otherwise the first gate GPU
--  start-up missed names the reason. devmgr keeps the entry after Desktop
--  exits, marked stale, so the report still says how Desktop last
--  rendered.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Compositor_Backend_Selection;
with CuBit.Graphics_Report;

package Desktop_Renderer_Report with SPARK_Mode is
   package GR renames CuBit.Graphics_Report;
   package Selection renames Compositor_Backend_Selection;
   use type GR.Subject_Kind, GR.Path_Kind, GR.Reason;

   --  How the renderer's start-up ended (Start_Renderer).
   type Facts is record
      Full_Output : Boolean := False;    --  the compositor composes on the GPU
      CPU_Policy  : Boolean := False;    --  desktop.composition is CPU
      Abandoned   : Boolean := False;    --  start-up exceeded its budget
      Allowed     : Boolean := False;    --  the bootstrap allowed a GPU start
      Evidence    : Selection.Readiness;
      VK_Result   : GR.Detail_Value := 0; --  the pipeline's failing VkResult, if known
   end record;

   --  Desktop composes on the CPU by its policy (desktop.composition).
   function Composes_On_CPU (Policy : Selection.Composition) return Boolean is
     (Selection."=" (Policy, Selection.CPU_Composition));

   --  The output's entry. Since is when the path was taken.
   function Output_Entry (Item : Facts; Output : GR.Subject_Id; Since : GR.Milliseconds)
     return GR.Report_Entry
     with Post => Output_Entry'Result.Kind = GR.Output and then Output_Entry'Result.Id = Output;

   --  The output fell back to the CPU at runtime.
   function Fallback_Entry (Cause : GR.Fallback_Cause; Output : GR.Subject_Id; Since : GR.Milliseconds)
     return GR.Report_Entry
     with Post => Fallback_Entry'Result.Path = GR.Software and then Fallback_Entry'Result.Why = GR.Fell_Back;

   --  Post either to devmgr (Desktop's graphics-poster endpoint; nothing
   --  without one), and log one line naming the path and the reason.
   procedure Post (Item : GR.Report_Entry)
     with SPARK_Mode => Off;
end Desktop_Renderer_Report;
