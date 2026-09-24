with CCL.Interfaces.Config;
with CCL.Host_Values;
package CCL_Config_IO is
   function Available return Boolean;
   procedure Query
     (Op : CCL.Interfaces.Config.Operation; Key : String;
      Value : out CCL.Host_Values.Text; Success : out Boolean);
end CCL_Config_IO;
