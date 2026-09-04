------------------------------------------------------------------------------
--  CuBit Control Language Workbench platform-link anchor
------------------------------------------------------------------------------

--  The Workbench imports a deliberately tiny C-compatible window boundary so
--  its editor/compiler/debugger body is identical on Linux and CuBit. Calling
--  Activate makes the selected platform body's exported implementation part
--  of the executable.
package CCL_Workbench_Platform is
   procedure Activate;
end CCL_Workbench_Platform;
