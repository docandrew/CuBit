pragma Ada_2022;
package body DW_I2C_Configuration with SPARK_Mode is
   use type T.Tick;
   use type T.Count;
   function Offset (ID : Register_ID) return Register_Offset is
     (case ID is
         when Interrupt_Mask => 16#30#,
         when Enable => 16#6C#,
         when Enable_Status => 16#9C#,
         when Controller_Status => 16#70#,
         when TX_Level => 16#74#,
         when RX_Level => 16#78#,
         when Data_Command => 16#10#,
         when Standard_High => 16#14#,
         when Standard_Low => 16#18#,
         when Fast_High => 16#1C#,
         when Fast_Low => 16#20#,
         when SDA_Hold => 16#7C#,
         when Control => 16#00#,
         when Target => 16#04#,
         when TX_Threshold => 16#3C#,
         when RX_Threshold => 16#38#,
         when Abort_Source => 16#80#,
         when Clear_Interrupts => 16#40#);
   function Observe (S : State) return Report is (S.Info);
   procedure Start
     (S : in out State; Settings : Configuration; Reset_Completed : Boolean;
      Controller_Version : Word;
      Now, Deadline : T.Tick; Step_Budget : T.Limit; Result : out Admission) is
   begin
      if S.Info.At_Stage /= Idle then Result := Busy;
      elsif not Reset_Completed then Result := Reset_Not_Established;
      elsif Now >= Deadline then Result := Expired;
      elsif Settings.Program_Hold and then Controller_Version < Minimum_Hold_Version
      then Result := Unsupported_Hold;
      else
         S.Config := Settings; S.Last_Time := Now; S.Expires := Deadline;
         S.Remaining := Step_Budget; S.Info.At_Stage := Mask_IRQ;
         Result := Accepted;
      end if;
   end Start;
   procedure Retire (S : in out State; Retired : out Boolean) is
   begin
      Retired := S.Info.At_Stage = Ready;
      if Retired then
         S.Info := (others => <>); S.Last_Time := 0; S.Expires := 0;
         S.Remaining := 0;
      end if;
   end Retire;
   procedure Fail (S : in out State; Why : Failure) is
   begin
      S.Info.Reason := Why; S.Info.Failed_At := S.Info.At_Stage;
      S.Info.At_Stage := Failed;
   end Fail;
   procedure Step (S : in out State) is
      Now : T.Tick;
      Available : Boolean;
      Value : Word := 0;
      ID : Register_ID := Interrupt_Mask;
      Is_Write : Boolean := False;
      Status : Access_Status;
      Next : Stage := S.Info.At_Stage;
   begin
      S.Info.Access_Attempted := False;
      if S.Info.At_Stage in Idle | Ready | Failed then return; end if;
      if S.Remaining = 0 then Fail (S, Budget_Exhausted); return; end if;
      S.Remaining := S.Remaining - 1;
      S.Info.Steps_Used := S.Info.Steps_Used + 1;
      Clock (Now, Available);
      if not Available then Fail (S, Clock_Unavailable); return;
      elsif Now < S.Last_Time then Fail (S, Clock_Reversed); return;
      elsif Now >= S.Expires then Fail (S, Deadline_Expired); return;
      end if;
      S.Last_Time := Now;
      case S.Info.At_Stage is
         when Mask_IRQ => Is_Write := True; ID := Interrupt_Mask; Next := Disable_Controller;
         when Disable_Controller => Is_Write := True; ID := Enable; Next := Wait_Disabled;
         when Wait_Disabled => ID := Enable_Status;
         when Wait_Idle => ID := Controller_Status;
         when Check_TX => ID := TX_Level;
         when Check_RX => ID := RX_Level;
         when Drain_RX => ID := Data_Command;
         when Set_High =>
            Is_Write := True;
            ID := (if S.Config.Speed = Standard then Standard_High else Fast_High);
            Value := Word (S.Config.High_Count); Next := Set_Low;
         when Set_Low =>
            Is_Write := True;
            ID := (if S.Config.Speed = Standard then Standard_Low else Fast_Low);
            Value := Word (S.Config.Low_Count);
            Next := (if S.Config.Program_Hold then Set_Hold else Set_Control);
         when Set_Hold => Is_Write := True; ID := SDA_Hold;
            Value := Word (S.Config.Hold_Count); Next := Set_Control;
         when Set_Control => Is_Write := True; ID := Control;
            Value := Master_Bit or Restart_Enable_Bit or Slave_Disable_Bit or
              (if S.Config.Speed = Standard then Standard_Speed_Bits else Fast_Speed_Bits);
            Next := Set_Target;
         when Set_Target => Is_Write := True; ID := Target;
            Value := Word (S.Config.Device); Next := Set_TX_Threshold;
         when Set_TX_Threshold => Is_Write := True; ID := TX_Threshold;
            Value := Word (S.Config.TX_Depth) / 2; Next := Set_RX_Threshold;
         when Set_RX_Threshold => Is_Write := True; ID := RX_Threshold; Next := Copy_Abort;
         when Copy_Abort => ID := Abort_Source; Next := Clear_IRQ;
         when Clear_IRQ => ID := Clear_Interrupts; Next := Enable_Controller;
         when Enable_Controller => Is_Write := True; ID := Enable;
            Value := Enabled_Bit; Next := Wait_Enabled;
         when Wait_Enabled => ID := Enable_Status;
         when Idle | Ready | Failed => return;
      end case;
      S.Info.Last_Register := ID; S.Info.Access_Attempted := True;
      if Is_Write then Write (ID, Value, Status);
      else Read (ID, Value, Status);
      end if;
      S.Info.Last_Value := Value; S.Info.Last_Status := Status;
      if ID = Enable and then Is_Write and then Value = Enabled_Bit
        and then Status in Access_OK | Unknown_Effect
      then S.Info.May_Be_Enabled := True;
      end if;
      if Status /= Access_OK then
         Fail (S, (if Status = Unknown_Effect then Register_Indeterminate else Register_Refused));
         return;
      end if;
      if Is_Write then S.Info.Writes_Completed := S.Info.Writes_Completed + 1; end if;
      case S.Info.At_Stage is
         when Wait_Disabled =>
            if (Value and Enabled_Bit) = 0 then
               S.Info.May_Be_Enabled := False; Next := Wait_Idle;
            end if;
         when Wait_Idle => if (Value and Activity_Bits) = 0 then Next := Check_TX; end if;
         when Check_TX =>
            if Value /= 0 then Fail (S, TX_Not_Empty); return; end if;
            Next := Check_RX;
         when Check_RX =>
            if Value > Word (S.Config.RX_Depth) then Fail (S, Invalid_RX_Level); return;
            elsif Value = 0 then Next := Set_High;
            elsif S.Info.Drained_Bytes >= T.Count (S.Config.RX_Depth) then
               Fail (S, Drain_Limit_Reached); return;
            else Next := Drain_RX;
            end if;
         when Drain_RX =>
            S.Info.Drained_Bytes := S.Info.Drained_Bytes + 1; Next := Check_RX;
         when Copy_Abort => S.Info.Abort_Copied := True; S.Info.Abort_Value := Value;
         when Wait_Enabled => if (Value and Enabled_Bit) /= 0 then Next := Ready; end if;
         when others => null;
      end case;
      -- Do not advance/repeat the hardware step after a bad post-access clock.
      -- Writes/drains/abort copies above retain their real successful effects.
      Clock (Now, Available);
      if not Available then Fail (S, Clock_Unavailable); return;
      elsif Now < S.Last_Time then Fail (S, Clock_Reversed); return;
      elsif Now >= S.Expires then Fail (S, Deadline_Expired); return;
      end if;
      S.Last_Time := Now; S.Info.At_Stage := Next;
   end Step;
end DW_I2C_Configuration;
