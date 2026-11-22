pragma Ada_2022;
package body Intel_LPSS_I2C_Reset with SPARK_Mode is
   use type Word;
   use type Physical_Address;
   use type T.Tick;
   use type T.Count;
   function Offset (ID : Register_ID) return Register_Offset is
     (case ID is when Capabilities => 16#2FC#, when Reset_Control => 16#204#,
                 when Remap_Low => 16#240#, when Remap_High => 16#244#);
   function Observe (S : State) return Report is (S.Info);
   procedure Start
     (S : in out State; Vendor, Device : P.PCI_ID;
      BAR_Base, Window_Bytes : Physical_Address; Now, Deadline : T.Tick;
      Step_Budget : T.Limit; Result : out Admission) is
      Word_Bytes : constant Physical_Address := 4;
      Low_Mask : constant Physical_Address := 16#FFFF_FFFF#;
   begin
      if S.Info.At_Stage /= Idle then Result := Busy;
      elsif not P.Supported (Vendor, Device) then Result := Unsupported_Device;
      elsif BAR_Base = 0 or else BAR_Base mod Word_Bytes /= 0 or else
        Window_Bytes < Minimum_Window_Bytes or else
        BAR_Base > Physical_Address'Last - (Minimum_Window_Bytes - 1)
      then Result := Invalid_Window;
      elsif Now >= Deadline then Result := Expired;
      else
         S.Low_Address := Word (BAR_Base and Low_Mask);
         S.High_Address := Word (Interfaces.Shift_Right (BAR_Base, 32));
         S.Last_Time := Now; S.Expires := Deadline; S.Remaining := Step_Budget;
         S.Info.At_Stage := Read_Capabilities; Result := Accepted;
      end if;
   end Start;
   procedure Fail (S : in out State; Why : Failure) is
   begin
      S.Info.Reason := Why; S.Info.Failed_At := S.Info.At_Stage;
      S.Info.At_Stage := Failed;
   end Fail;
   procedure Step (S : in out State) is
      Now : T.Tick;
      Available : Boolean;
      ID : Register_ID := Capabilities;
      Value : Word := 0;
      Status : Access_Status;
      Is_Write : Boolean := False;
      Next : Stage := S.Info.At_Stage;
   begin
      S.Info.Access_Attempted := False;
      if S.Info.At_Stage in Idle | Ready | Failed then return; end if;
      if S.Remaining = 0 then Fail (S, Budget_Exhausted); return; end if;
      S.Remaining := S.Remaining - 1; S.Info.Steps_Used := S.Info.Steps_Used + 1;
      Clock (Now, Available);
      if not Available then Fail (S, Clock_Unavailable); return;
      elsif Now < S.Last_Time then Fail (S, Clock_Reversed); return;
      elsif Now >= S.Expires then Fail (S, Deadline_Expired); return;
      end if;
      S.Last_Time := Now;
      case S.Info.At_Stage is
         when Read_Capabilities => ID := Capabilities; Next := Assert_Reset;
         when Assert_Reset => ID := Reset_Control; Is_Write := True; Next := Confirm_Reset;
         when Confirm_Reset => ID := Reset_Control;
         when Release_Reset => ID := Reset_Control; Is_Write := True;
            Value := Release_Resets; Next := Confirm_Release;
         when Confirm_Release => ID := Reset_Control;
         when Set_Remap_Low => ID := Remap_Low; Is_Write := True;
            Value := S.Low_Address; Next := Set_Remap_High;
         when Set_Remap_High => ID := Remap_High; Is_Write := True;
            Value := S.High_Address; Next := Confirm_Remap_Low;
         when Confirm_Remap_Low => ID := Remap_Low; Next := Confirm_Remap_High;
         when Confirm_Remap_High => ID := Remap_High; Next := Ready;
         when Idle | Ready | Failed => return;
      end case;
      S.Info.Last_Register := ID; S.Info.Access_Attempted := True;
      if Is_Write then Write (ID, Value, Status); else Read (ID, Value, Status); end if;
      S.Info.Last_Value := Value; S.Info.Last_Status := Status;
      if Is_Write and then ID = Reset_Control and then Value = Release_Resets
        and then Status in Access_OK | Unknown_Effect
      then S.Info.Reset_May_Be_Released := True;
      end if;
      if Status /= Access_OK then
         Fail (S, (if Status = Unknown_Effect then Register_Indeterminate else Register_Refused));
         return;
      end if;
      if Is_Write then S.Info.Writes_Completed := S.Info.Writes_Completed + 1; end if;
      case S.Info.At_Stage is
         when Read_Capabilities =>
            if (Value and Controller_Type_Mask) /= 0 then
               Fail (S, Wrong_Controller_Type); return;
            end if;
         when Confirm_Reset =>
            if (Value and Function_Reset_Mask) = 0 then
               S.Info.Reset_May_Be_Released := False; Next := Release_Reset;
            end if;
         when Confirm_Release =>
            if (Value and Function_Reset_Mask) = Function_Reset_Mask then Next := Set_Remap_Low; end if;
         when Confirm_Remap_Low =>
            if Value /= S.Low_Address then Fail (S, Remap_Mismatch); return; end if;
         when Confirm_Remap_High =>
            if Value /= S.High_Address then Fail (S, Remap_Mismatch); return; end if;
         when others => null;
      end case;
      Clock (Now, Available);
      if not Available then Fail (S, Clock_Unavailable); return;
      elsif Now < S.Last_Time then Fail (S, Clock_Reversed); return;
      elsif Now >= S.Expires then Fail (S, Deadline_Expired); return;
      end if;
      S.Last_Time := Now; S.Info.At_Stage := Next;
      if Next = Ready then S.Info.Clock_Hz := P.Root_Clock; end if;
   end Step;
end Intel_LPSS_I2C_Reset;
