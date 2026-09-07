------------------------------------------------------------------------------
--  CuBit Control Language Workbench platform-link anchor
------------------------------------------------------------------------------

--  The Workbench imports a deliberately tiny C-compatible window boundary so
--  its editor/compiler/debugger body is identical on Linux and CuBit. Calling
--  Activate makes the selected platform body's exported implementation part
--  of the executable.
package CCL_Workbench_Platform is
   Open_Source_Event : constant := 34;
   Save_Source_Event : constant := 35;
   Tab_Event : constant := 36;
   Toggle_REPL_Event : constant := 37;
   procedure Activate;
   --  Optional lifecycle diagnostic. Never logs submitted source or values.
   procedure REPL_Completed;
end CCL_Workbench_Platform;
