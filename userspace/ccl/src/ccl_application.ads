--  Which CCL desktop application this process is ("ccl-console",
--  "ccl-workbench"), for diagnostics shared by every such application.
package CCL_Application is
   Maximum_Name : constant := 32;
   procedure Set_Name (Name : String);
   --  "ccl" until Set_Name.
   function Name return String;
end CCL_Application;
