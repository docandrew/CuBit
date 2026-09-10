------------------------------------------------------------------------------
--  Linux/SDL supplies the ccl_window_* boundary from native_window.c.
------------------------------------------------------------------------------

package body CCL_Workbench_Platform is
   procedure Live_Label_Changed (Event : Live_Label_Event) is null;
   procedure REPL_Completed is null;
   procedure Activate is
   begin
      null;
   end Activate;
end CCL_Workbench_Platform;
