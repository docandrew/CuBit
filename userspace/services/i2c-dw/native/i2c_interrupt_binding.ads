pragma Ada_2022;
with Interfaces;
with CuBit.Messages;
with CuBit.Process_IDs;
with Launch_Configuration;
with Device_Vector_ABI;
generic
   IRQ_Label : Interfaces.Unsigned_32;
package I2C_Interrupt_Binding is
   package V renames Device_Vector_ABI;
   type Mode is (Invalid, Polling, MSI);
   type Selection is record
      Delivery : Mode := Invalid;
      Head : V.Device_Vector := V.First_Vector;
   end record;
   function Select_Mode
     (Delivery : Launch_Configuration.Interrupt_Delivery;
      Vectors : Launch_Configuration.Vector_Count;
      Authority_Present, Block_Valid : Boolean;
      Head : V.Device_Vector; Count : V.Block_Size) return Selection;
   function Notice
     (Binding : Selection; Publisher : CuBit.Process_IDs.Process_ID;
      Message : CuBit.Messages.Message) return Boolean;
   -- Block_Valid/Head/Count come only from Device_Interrupts.Block_Of of the
   -- typed optional startup authority, never message words. Publisher is the
   -- event-queue metadata; a forwarded/supplied process identity is not valid.
   -- Generic IRQ_Label is the actual runtime Device_Interrupts.EVENT_IRQ.
end I2C_Interrupt_Binding;
