with Main_Model;
package body CuBit.Handles.Lines is
 use type Main_Model.Scenario, Main_Model.I.Word;
 function Acknowledge (Line : Interrupt; Vector : Interrupt_Line_ABI.Line_Vector) return Acknowledgement is
   L : constant Main_Model.G.Location := Main_Model.G.Locate (160);
 begin
   Main_Model.Check (Line = 1 and Vector = Interrupt_Line_ABI.Vector_Of (14), "only admitted symbolic parent/vector acknowledged");
   Main_Model.Check ((Main_Model.Memory (L.Resource) (Natural (L.Enable_Register) / 4) and 7) = 0 and
      (Main_Model.Memory (L.Resource) (Natural (L.Status_Register) / 4) and 7) = 0,
      "parent ACK follows actual host pin mask/status clear");
   Main_Model.Parent_ACKs := Main_Model.Parent_ACKs + 1;
   return (Accepted => Main_Model.Current /= Main_Model.Initial_ACK_Refused);
 end Acknowledge;
end CuBit.Handles.Lines;
