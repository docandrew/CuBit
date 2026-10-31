with Main_Model;
package body CuBit.Handles.IPC is
   use type Main_Model.Scenario;
   function Describe (Item : Process) return Description is
   begin
      Main_Model.Check (Item = 1, "typed launcher handle inspected");
      return (Main_Model.Current /= Main_Model.Invalid_Launcher, ABI.Process,
         Main_Model.P.To_Word (Main_Model.Launcher_ID));
   end Describe;
end CuBit.Handles.IPC;
