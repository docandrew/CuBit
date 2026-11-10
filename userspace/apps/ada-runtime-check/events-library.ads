------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A library-level controlled object: Initialize runs when the package is
--  elaborated, before Main. (A configurable run time's binder emits no
--  library-level finalization, so it is never finalized: the process ends.)
------------------------------------------------------------------------------
pragma Ada_2022;
package Events.Library is

   Object : Tracked;

   Trace_At_Elaboration : constant String := Trace;

end Events.Library;
