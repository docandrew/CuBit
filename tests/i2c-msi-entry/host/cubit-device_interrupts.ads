with Interfaces; with CuBit.Handles; with Device_Vector_ABI;
package CuBit.Device_Interrupts is
   EVENT_IRQ : constant Interfaces.Unsigned_32 := 1;
   type Interrupt_Block is record Valid : Boolean := False; Head : Device_Vector_ABI.Device_Vector := 48; Count : Device_Vector_ABI.Block_Size := 1; end record;
   function Block_Of (Authority : CuBit.Handles.Interrupt) return Interrupt_Block;
end CuBit.Device_Interrupts;
