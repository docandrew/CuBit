pragma Ada_2022;
package body I2C_Interrupt_Binding is
   package LC renames Launch_Configuration;
   package M renames CuBit.Messages;
   package P renames CuBit.Process_IDs;
   use type LC.Interrupt_Delivery, LC.Vector_Count, P.Process_ID;
   use type Interfaces.Unsigned_32, Interfaces.Unsigned_16, Interfaces.Unsigned_8;
   use type M.Protocol_Flags, M.IPC_Flags;
   function Select_Mode
     (Delivery : LC.Interrupt_Delivery; Vectors : LC.Vector_Count;
      Authority_Present, Block_Valid : Boolean; Head : V.Device_Vector;
      Count : V.Block_Size) return Selection is
   begin
      if Delivery = LC.Polled and then Vectors = 0 and then not Authority_Present then
         return (Polling, V.First_Vector);
      elsif Delivery = LC.MSI and then Vectors = 1 and then Authority_Present
        and then Block_Valid and then Count = 1
      then return (MSI, Head);
      else return (others => <>);
      end if;
   end Select_Mode;
   function Notice
     (Binding : Selection; Publisher : P.Process_ID; Message : M.Message) return Boolean is
   begin
      if Binding.Delivery /= MSI or else Publisher /= P.No_Process or else
        Message.tag.label /= IRQ_Label or else Message.tag.length /= V.Pending_Words
        or else Message.tag.protocol /= 0 or else Message.tag.ipc /= M.No_IPC_Flags
        or else Message.tag.reserved /= 0 or else Message.authorityTag /= 0
      then return False; end if;
      return (Message.words (V.Word_Of (Binding.Head)) and V.Mask_Of (Binding.Head)) /= 0;
   end Notice;
end I2C_Interrupt_Binding;
