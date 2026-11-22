with Interrupt_Line_ABI;package CuBit.Handles.Lines is
 type Acknowledgement(Accepted:Boolean:=False) is record case Accepted is when True=>Delivered:Interrupt_Line_ABI.Delivery_Count;when False=>Why:Interrupt_Line_ABI.Route_Outcome:=Interrupt_Line_ABI.No_Line_Authority;end case;end record;
 function Acknowledge(Line:Interrupt;Vector:Interrupt_Line_ABI.Line_Vector) return Acknowledgement;
end CuBit.Handles.Lines;