with Fixture;
package body CuBit.Device_Interrupts is
   function Block_Of (Authority : CuBit.Handles.Interrupt) return Interrupt_Block is
      pragma Unreferenced (Authority);
   begin return (Fixture.Scenario /= 11, 48, 1); end Block_Of;
end CuBit.Device_Interrupts;
