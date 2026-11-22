pragma Ada_2022;
with GPIO_Control_Wire;
with GPIO_Client_Controls;
with GPIO_Lease_Control;
with I2C_Transfer_Wire;
with CuBit.Process_IDs;
package HID_GPIO_Reset with SPARK_Mode is
   subtype Cleanup_Milliseconds is GPIO_Control_Wire.Millisecond
      range 1 .. GPIO_Control_Wire.Millisecond'Last;
   generic
      with package Controls is new GPIO_Client_Controls (<>);
      Expected_Sender : CuBit.Process_IDs.Process_ID;
      Cleanup_Budget : Cleanup_Milliseconds;
      with procedure Next_Token (Token : out GPIO_Control_Wire.Word; Available : out Boolean);
      with procedure Clock (Now : out GPIO_Control_Wire.Word; Available : out Boolean);
      -- Clock returns the enclosing loop's cached observation, the SAME one
      -- supplied to HID_I2C_Device.Step. It must not make an independent read.
   package Bridge is
      procedure Begin_Arm_Reset (Deadline : I2C_Transfer_Wire.Millisecond;
         Reply : out GPIO_Lease_Control.Reply);
      procedure Poll_Arm_Reset (Reply : out GPIO_Lease_Control.Reply);
      procedure Begin_Release_Reset (Ticket : I2C_Transfer_Wire.Word;
         Deadline : I2C_Transfer_Wire.Millisecond; Reply : out GPIO_Lease_Control.Reply);
      procedure Poll_Release_Reset (Reply : out GPIO_Lease_Control.Reply);
      procedure Receive (Token : GPIO_Control_Wire.Word; Sender : CuBit.Process_IDs.Process_ID;
         Delivery : Controls.Delivery_Status; Label : GPIO_Control_Wire.Operation;
         Protocol : GPIO_Control_Wire.Word; Words : GPIO_Control_Wire.Message_Words;
         Result : out Controls.Receive_Result);
      function Observe return Controls.Report;
      function Wake_Deadline (Now, Otherwise : GPIO_Control_Wire.Word)
         return GPIO_Control_Wire.Word;
      -- Timeout with no definitive reply remains Pending so HID drains rather
      -- than losing a late lease. A late success is consumed in that drain path;
      -- original HID deadline/clock failure still prevents RESET or report fetch.
      -- Release cleanup always gets its own fresh bounded window. The original
      -- deadline remains the HID operation limit, not a cleanup/cancellation proof.
      -- This is cleanup of a known lease, never replay of the timed-out arm.
   private
      State : Controls.State;
      Operation : GPIO_Lease_Control.Operation := GPIO_Lease_Control.No_Control;
      Release_Lease : GPIO_Control_Wire.Word := 0;
   end Bridge;
end HID_GPIO_Reset;
