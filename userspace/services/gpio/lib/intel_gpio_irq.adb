pragma Ada_2022;
package body Intel_GPIO_IRQ with SPARK_Mode is
   function Input_Configuration (Old : Word; Mode : Trigger) return Word is
      Selected : constant Word := (case Mode is
         when Level_High | Level_Low => 0,
         when Rising_Edge | Falling_Edge => Edge_Selection,
         when Both_Edges => Both_Selection);
      Inverted : constant Word :=
        (if Mode in Level_Low | Falling_Edge then Receive_Invert else 0);
   begin
      return (Old and not Changed_Fields) or TX_Disable or Selected or Inverted;
   end Input_Configuration;
   function Read_Offset (Plan : Intel_ADLN_GPIO.Plan_Result; ID : Read_ID)
      return Intel_ADLN_GPIO.Register_Offset is
   begin
      return (case ID is
         when Pad_Owner => Plan.Selected.Owner_Register,
         when Host_Owner => Plan.Selected.Host_Register,
         when Configuration_Lock => Plan.Selected.Lock_Register,
         when Pad_Configuration => Plan.PADCFG0,
         when Group_Enable => Plan.Selected.Enable_Register,
         when Group_Status => Plan.Selected.Status_Register);
   end Read_Offset;
   function Write_Offset (Plan : Intel_ADLN_GPIO.Plan_Result; ID : Write_ID)
      return Intel_ADLN_GPIO.Register_Offset is
   begin
      return (case ID is
         when Write_Configuration => Plan.PADCFG0,
         when Write_Enable => Plan.Selected.Enable_Register,
         when Clear_Status => Plan.Selected.Status_Register);
   end Write_Offset;
end Intel_GPIO_IRQ;
