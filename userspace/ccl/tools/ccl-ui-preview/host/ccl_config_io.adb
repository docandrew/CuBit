-- Linux preview is not a connection to a CuBit instance. Do not invent data.
package body CCL_Config_IO is
   function Available return Boolean is (False);
   procedure Query
     (Op : CCL.Interfaces.Config.Operation; Key : String;
      Value : out CCL.Host_Values.Text; Success : out Boolean)
   is
      pragma Unreferenced (Op, Key);
   begin
      Value := (others => <>); Success := False;
   end Query;
end CCL_Config_IO;
