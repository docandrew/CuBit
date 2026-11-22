with Interrupt_Line_ABI;
package CuBit.Handles.Lines is
 type Acknowledgement is record Accepted : Boolean := False; end record;
 function Acknowledge (Line : Interrupt; Vector : Interrupt_Line_ABI.Line_Vector) return Acknowledgement;
end CuBit.Handles.Lines;
