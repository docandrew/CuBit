------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  `desktop.composition`, a typed CCL Desktop_Composition
--  (CCL.Interfaces.GPU_Power), e.g.
--  (Desktop_Composition path => Composition_Path.GPU): which processor
--  composes the UI (LAT-002's CPU/GPU A/B). Read once at startup; a change
--  takes effect when Desktop next starts (a live replace). Absent or
--  invalid: Compositor_Backend_Selection.Default_Composition (CPU).
--  GPU composition still needs the render session desktop.render.gpu
--  admits; without it Desktop composes on the CPU.
------------------------------------------------------------------------------
with Compositor_Backend_Selection;

package Desktop_Composition_Settings is
   subtype Composition is Compositor_Backend_Selection.Composition;
   Default_Composition : constant Composition := Compositor_Backend_Selection.Default_Composition;
   --  LAT-003: how CPU frames reach a scanout buffer Display lends. Staged:
   --  composed in a private buffer, then streamed out; Direct: composed in
   --  the lent buffer; Copied: Display lends nothing and copies each frame.
   type Scanout_Use is (Staged, Direct, Copied);
   type Value is record
      Path : Composition := Default_Composition;
      Scanout : Scanout_Use := Direct;
   end record;
   procedure Decode (Source : String; Item : out Value; Success : out Boolean);
   --  What Desktop read at startup (Direct when the setting is absent):
   --  Staged pays a copy that only wins where the lent buffer is costly to
   --  write (intel-gpu flushes its lines); the laptop A/B decides.
   Scanout : Scanout_Use := Direct;
end Desktop_Composition_Settings;
