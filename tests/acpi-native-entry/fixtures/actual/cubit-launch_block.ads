------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  This process's startup table, from its launch block (CAP-002).
--
--  @description
--  Elaborating this package copies the startup table its launcher appended
--  to the launch block (CuBit.Launch_Envelopes) once, and loads it into
--  CuBit.Startup_State; a block without one leaves the state empty, so every
--  required startup handle is the null handle, and the generated Startup
--  package's check (CuBit.Startup_Check) names it and stops the program.
--  CuBit.Handles.Startup depends on this package, so any program that looks
--  a startup capability up has loaded its table first.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Startup_State;

package CuBit.Launch_Block with Elaborate_Body is

   --  What loading found.
   function Outcome return CuBit.Startup_State.Load_Result;

   --  Whether the block carried a startup table at all.
   function Table_Present return Boolean;

end CuBit.Launch_Block;
