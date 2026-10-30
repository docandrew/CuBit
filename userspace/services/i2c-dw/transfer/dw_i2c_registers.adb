pragma Ada_2022;
with Interfaces;
package body DW_I2C_Registers with SPARK_Mode is
   use type T.Tick;
   use type T.Ticket;
   use type T.Count;
   use type T.Action_Kind;
   function Offset (ID : Read_Register) return Register_Offset is
     (case ID is
         when Component_Type => 16#FC#,
         when Component_Parameters => 16#F4#,
         when Component_Version => 16#F8#,
         when Raw_Interrupts => 16#34#,
         when Abort_Source => 16#80#,
         when TX_Level => 16#74#,
         when RX_Level => 16#78#,
         when Read_Data_Command => 16#10#);
   function Offset (ID : Write_Register) return Register_Offset is
     (case ID is when Write_Data_Command => 16#10#);
   package body Port with SPARK_Mode is
      type Clock_State is record
         Seen : Boolean := False;
         Last : T.Tick := 0;
      end record;
      procedure Check_Time
        (State : in out Clock_State; Deadline : T.Tick;
         Report : in out Diagnostic; OK : out Boolean) is
         Now : T.Tick;
         Available : Boolean;
      begin
         Clock (Now, Available);
         OK := False;
         if not Available then Report.Status := Clock_Unavailable;
         elsif State.Seen and then Now < State.Last then Report.Status := Clock_Reversed;
         elsif Now >= Deadline then Report.Status := Deadline_Expired;
         else State.Seen := True; State.Last := Now; OK := True;
         end if;
      end Check_Time;
      procedure Read_Checked
        (State : in out Clock_State; ID : Read_Register; Deadline : T.Tick;
         Value : out Word; Report : out Diagnostic; OK : out Boolean) is
      begin
         Value := 0; Report := (Register_ID => ID, others => <>);
         Check_Time (State, Deadline, Report, OK);
         if not OK then return; end if;
         Report.Access_Attempted := True;
         Read (ID, Report.Value, Report.Bus_Status);
         if Report.Bus_Status /= Access_OK then
            Report.Status := (if Report.Bus_Status = Unknown_Effect then
               Register_Indeterminate else Register_Refused);
            OK := False; return;
         end if;
         Check_Time (State, Deadline, Report, OK);
         if OK then Value := Report.Value; end if;
      end Read_Checked;
      procedure Probe (Deadline : T.Tick; Result : out Capabilities;
                       Report : out Diagnostic) is
         Time : Clock_State;
         Value, Version : Word;
         OK : Boolean;
         TX_Depth, RX_Depth : T.FIFO_Depth;
         FIFO_Field_Mask : constant Word := 16#FF#;
         RX_Depth_Shift : constant := 8;
         TX_Depth_Shift : constant := 16;
      begin
         Result := (others => <>);
         Read_Checked (Time, Component_Type, Deadline, Value, Report, OK);
         if not OK then return; end if;
         if Value /= Component_Signature then
            Report.Status := Wrong_Component; return;
         end if;
         Read_Checked (Time, Component_Parameters, Deadline, Value, Report, OK);
         if not OK then return; end if;
         TX_Depth := T.FIFO_Depth
           ((Interfaces.Shift_Right (Value, TX_Depth_Shift) and FIFO_Field_Mask) + 1);
         RX_Depth := T.FIFO_Depth
           ((Interfaces.Shift_Right (Value, RX_Depth_Shift) and FIFO_Field_Mask) + 1);
         Read_Checked (Time, Component_Version, Deadline, Version, Report, OK);
         if not OK then return; end if;
         Result := (True, TX_Depth, RX_Depth, Version);
      end Probe;
      procedure Capture
        (TX_Depth, RX_Depth : T.FIFO_Depth; Deadline : T.Tick;
         Result : out Sample; Report : out Diagnostic) is
         Time : Clock_State;
         OK : Boolean;
      begin
         Result := (others => <>);
         Read_Checked (Time, Raw_Interrupts, Deadline,
                       Result.Interrupt_Value, Report, OK);
         if not OK then return; end if;
         if (Result.Interrupt_Value and Abort_Interrupt) /= 0 then
            Read_Checked (Time, Abort_Source, Deadline, Result.Abort_Value, Report, OK);
            if not OK then return; end if;
            Result.Hardware.Aborted := True; Result.Ready := True; return;
         end if;
         if (Result.Interrupt_Value and FIFO_Error_Interrupts) /= 0 then
            Report.Status := FIFO_Error; return;
         end if;
         Read_Checked (Time, TX_Level, Deadline, Result.TX_Value, Report, OK);
         if not OK then return; end if;
         if Result.TX_Value > Word (TX_Depth) then
            Report.Status := Invalid_FIFO_Level; return;
         end if;
         Read_Checked (Time, RX_Level, Deadline, Result.RX_Value, Report, OK);
         if not OK then return; end if;
         if Result.RX_Value > Word (RX_Depth) then
            Report.Status := Invalid_FIFO_Level; return;
         end if;
         Result.Hardware :=
           (TX_Free => T.FIFO_Count (Word (TX_Depth) - Result.TX_Value),
            RX_Available => T.FIFO_Count (Result.RX_Value),
            Stop_Detected => (Result.Interrupt_Value and Stop_Interrupt) /= 0,
            Aborted => False);
         Result.Ready := True;
      end Capture;
      procedure Execute
        (Owner : in out Access_Owner; Item : T.Action; Data : Byte;
         Deadline : T.Tick; Read_Value : out Byte;
         Result : out T.Access_Result; Report : out Diagnostic) is
         Time : Clock_State;
         OK : Boolean;
      begin
         Read_Value := 0; Result := T.Refused;
         Report := (Register_ID => Read_Data_Command, others => <>);
         if Item.Kind = T.No_Action or else Item.ID = T.No_Ticket or else Item.Index = 0 then
            Report.Status := Invalid_Action; return;
         end if;
         if Item.ID <= Owner.Last_Ticket then Report.Status := Stale_Action; return; end if;
         Owner.Last_Ticket := Item.ID;
         Check_Time (Time, Deadline, Report, OK);
         if not OK then return; end if;
         Report.Access_Attempted := True;
         if Item.Kind = T.Send_Command then
            Report.Value := T.Encode_Command (Item, Data);
            Write (Write_Data_Command, Report.Value, Report.Bus_Status);
         else
            Read (Read_Data_Command, Report.Value, Report.Bus_Status);
         end if;
         case Report.Bus_Status is
            when Access_OK =>
               Result := T.Succeeded;
               if Item.Kind = T.Receive_Byte then
                  Read_Value := Byte (Report.Value and 16#FF#);
               end if;
               Check_Time (Time, Deadline, Report, OK);
            when Unknown_Effect =>
               Result := T.Indeterminate; Report.Status := Register_Indeterminate;
            when Unavailable | Denied => Report.Status := Register_Refused;
         end case;
      end Execute;
   end Port;
end DW_I2C_Registers;
