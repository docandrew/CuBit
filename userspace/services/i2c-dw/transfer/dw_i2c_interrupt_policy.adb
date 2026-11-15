pragma Ada_2022;
package body DW_I2C_Interrupt_Policy with SPARK_Mode is
   function Wanted (Need : T.Wake_Requirement; Hardware_Wait : Boolean) return Mask is
   begin
      if not Hardware_Wait or else not Need.Active then return 0; end if;
      return Error_Sources or (if Need.TX_Empty then TX_Empty else 0)
         or (if Need.RX_Full then RX_Full else 0)
         or (if Need.Stop_Detected then Stop_Detected else 0);
   end Wanted;
end DW_I2C_Interrupt_Policy;
