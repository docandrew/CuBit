with Main_Model;
with Interrupt_Line_ABI;
package body CuBit.Handles.IPC is
   use type Main_Model.Scenario;
   function Describe (Item : Endpoint) return Description is
   begin
      Main_Model.Check (Item = 1, "typed launcher handle inspected");
      return (Current => Main_Model.Current /= Main_Model.Invalid_Launcher,
         Kind => (if Main_Model.Current = Main_Model.Wrong_Launcher_Kind then ABI.Process else ABI.Endpoint),
         Object => (if Main_Model.Current = Main_Model.Zero_Launcher_ID then 0
                    else Main_Model.P.To_Word (Main_Model.Launcher_ID)),
         Held => (Read => Main_Model.Current /= Main_Model.Unreadable_Launcher), Parameter => 0);
   end Describe;
   function Describe (Item : Interrupt) return Description is
   begin
      Main_Model.Check (Item = 1, "symbolic manifest parent handle inspected");
      return (Current => True, Kind => ABI.Interrupt, Held => (Read => True), Parameter => 1,
         Object => (if Main_Model.Current = Main_Model.Wrong_Parent_Vector then 0
            else Interfaces.Unsigned_64 (Interrupt_Line_ABI.Vector_Of (14))));
   end Describe;
end CuBit.Handles.IPC;
