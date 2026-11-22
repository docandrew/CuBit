pragma Ada_2022;
with Interfaces;
with Intel_ADLN_GPIO;
package Intel_GPIO_IRQ with SPARK_Mode, Pure is
   use type Intel_ADLN_GPIO.Outcome;
   subtype Word is Interfaces.Unsigned_32;
   use type Word;
   type Tick is new Interfaces.Unsigned_64;
   type Ticket is new Interfaces.Unsigned_64;
   No_Ticket : constant Ticket := 0;
   type Count is range 0 .. 65_535;
   subtype Limit is Count range 1 .. Count'Last;
   type Trigger is (Level_High, Level_Low, Rising_Edge, Falling_Edge, Both_Edges);
   type Read_ID is (Pad_Owner, Host_Owner, Configuration_Lock,
      Pad_Configuration, Group_Enable, Group_Status);
   type Write_ID is (Write_Configuration, Write_Enable, Clear_Status);
   type Access_Status is (Access_OK, Denied, Unavailable, Unknown_Effect);
   Pin_Mode_Mask : constant Word := 16#0000_3C00#;
   RX_Disable : constant Word := 16#0000_0200#;
   TX_Disable : constant Word := 16#0000_0100#;
   Route_Mask : constant Word := 16#001E_0000#;
   Event_Mask : constant Word := 16#0600_0000#;
   Receive_Invert : constant Word := 16#0080_0000#;
   Changed_Fields : constant Word := 16#069E_3F00#;
   Edge_Selection : constant Word := 16#0200_0000#;
   Both_Selection : constant Word := 16#0600_0000#;
   function Input_Configuration (Old : Word; Mode : Trigger) return Word
     with Post =>
       (Input_Configuration'Result and not Changed_Fields) = (Old and not Changed_Fields)
       and then (Input_Configuration'Result and Pin_Mode_Mask) = 0
       and then (Input_Configuration'Result and RX_Disable) = 0
       and then (Input_Configuration'Result and TX_Disable) /= 0
       and then (Input_Configuration'Result and Route_Mask) = 0
       and then (Input_Configuration'Result and Event_Mask) =
         (case Mode is when Level_High | Level_Low => 0,
          when Rising_Edge | Falling_Edge => Edge_Selection, when Both_Edges => Both_Selection)
       and then (Input_Configuration'Result and Receive_Invert) =
         (if Mode in Level_Low | Falling_Edge then Receive_Invert else 0);
   function Read_Offset (Plan : Intel_ADLN_GPIO.Plan_Result; ID : Read_ID)
      return Intel_ADLN_GPIO.Register_Offset
      with Pre => Plan.Status = Intel_ADLN_GPIO.Ready;
   function Write_Offset (Plan : Intel_ADLN_GPIO.Plan_Result; ID : Write_ID)
      return Intel_ADLN_GPIO.Register_Offset
      with Pre => Plan.Status = Intel_ADLN_GPIO.Ready;
end Intel_GPIO_IRQ;
