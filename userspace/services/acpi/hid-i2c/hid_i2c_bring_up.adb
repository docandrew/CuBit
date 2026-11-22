pragma Ada_2022;
with HID_I2C_Wire;
package body HID_I2C_Bring_Up with SPARK_Mode is
   package W renames HID_I2C_Wire;
   use type D.Capacity_Status;
   use type W.Frame_Status;
   procedure Run
     (Descriptor_Register : D.Register_Number; Deadline : Tick;
      Report_Data, Input_Scratch : out D.Bytes; Output_Capacity : D.Byte_Count;
      Result : out Report) is
      Header : D.Bytes (0 .. D.Descriptor_Bytes - 1) := [others => 0];
      Empty : D.Bytes (1 .. 0);
      Written, Read : Natural;
      Now, Previous : Tick := 0;
      Available, Seen_Clock, OK : Boolean := False;
      Armed : Boolean := False;
      Info : D.Descriptor;
      Frame : W.Frame_Result;
      procedure Check_Time (Accepted : out Boolean) is
      begin
         Accepted := False;
         Clock (Now, Available);
         if not Available then Result.Reason := Clock_Unavailable; return; end if;
         if Seen_Clock and then Now < Previous then Result.Reason := Clock_Reversed; return; end if;
         Previous := Now; Seen_Clock := True;
         if Now >= Deadline then Result.Reason := Deadline_Expired; return; end if;
         Accepted := True;
      end Check_Time;
      procedure Check_IO (Expected_Write, Expected_Read : Natural; Accepted : out Boolean) is
      begin
         Accepted := False;
         if Result.Transport /= Transferred then Result.Reason := Transport_Failed; return; end if;
         if Written > Expected_Write or Read > Expected_Read then
            Result.Reason := Bad_Completion_Count; return;
         end if;
         if Written /= Expected_Write or Read /= Expected_Read then
            Result.Reason := Incomplete_Transfer; return;
         end if;
         Check_Time (Accepted);
      end Check_IO;
      procedure Done is
      begin
         Result.Completed := Result.Completed + 1;
         Trace (Result, True);
      end Done;
      procedure Attempt is
      begin
         Check_Time (OK); if not OK then return; end if;
         Transfer (W.Select_Register (Descriptor_Register), Header, Deadline,
                   Written, Read, Result.Transport);
         Check_IO (W.Register_Selector'Length, Header'Length, OK); if not OK then return; end if;
         Result.Descriptor := D.Decode (Header);
         if Result.Descriptor.Status /= D.Decoded then Result.Reason := Descriptor_Refused; return; end if;
         Info := Result.Descriptor.Value;
         --  Clamp capacities without narrowing a potentially larger element
         --  count; never clamp the descriptor's required byte counts.
         Result.Capacity := D.Check_Capacities (Info,
           W.Buffer_Capacity (Report_Data), W.Buffer_Capacity (Input_Scratch),
           Output_Capacity);
         if Result.Capacity /= D.Fits then Result.Reason := Capacity_Refused; return; end if;
         Done;
         Result.At_Stage := Power_Command;
         Check_Time (OK); if not OK then return; end if;
         Transfer (W.Encode_Control (Info.Command_Register, W.Power_On), Empty,
                   Deadline, Written, Read, Result.Transport);
         Check_IO (W.Control_Message'Length, 0, OK); if not OK then return; end if;
         Done;
         Result.At_Stage := Reset_Command;
         Check_Time (OK); if not OK then return; end if;
         Arm_Reset (Deadline, Result.Ticket, Result.Transport);
         --  A provider returning a ticket even on failure still owns a latch
         --  that must be released. Do not hide it behind the success status.
         Armed := Result.Ticket /= No_Reset_Ticket;
         Result.Reset_Still_Armed := Armed;
         if Result.Transport /= Transferred then Result.Reason := Transport_Failed; return; end if;
         if not Armed then Result.Reason := Invalid_Reset_Ticket; return; end if;
         Check_Time (OK); if not OK then return; end if;
         Transfer (W.Encode_Control (Info.Command_Register, W.Reset), Empty,
                   Deadline, Written, Read, Result.Transport);
         Check_IO (W.Control_Message'Length, 0, OK); if not OK then return; end if;
         Done;
         Result.At_Stage := Reset_Completion;
         Check_Time (OK); if not OK then return; end if;
         Await_Interrupt (Result.Ticket, Deadline, Result.Transport);
         if Result.Transport /= Transferred then Result.Reason := Transport_Failed; return; end if;
         Check_Time (OK); if not OK then return; end if;
         Transfer ([], Input_Scratch (Input_Scratch'First ..
           Input_Scratch'First + (Natural (Info.Maximum_Input) - 1)), Deadline,
           Written, Read, Result.Transport);
         Check_IO (0, Natural (Info.Maximum_Input), OK); if not OK then return; end if;
         Frame := W.Inspect_Input (Input_Scratch (Input_Scratch'First ..
           Input_Scratch'First + (Natural (Info.Maximum_Input) - 1)), Info.Maximum_Input, True);
         if Frame.Status /= W.Reset_Completed then Result.Reason := Reset_Not_Acknowledged; return; end if;
         Release_Reset (Result.Ticket, Result.Cleanup);
         if Result.Cleanup /= Transferred then Result.Reason := Reset_Release_Failed; return; end if;
         Armed := False;
         Result.Reset_Still_Armed := False;
         Done;
         Result.At_Stage := Report_Descriptor;
         Check_Time (OK); if not OK then return; end if;
         Transfer (W.Select_Register (Info.Report_Register), Report_Data
           (Report_Data'First .. Report_Data'First + (Natural (Info.Report_Length) - 1)),
           Deadline, Written, Read, Result.Transport);
         Check_IO (W.Register_Selector'Length, Natural (Info.Report_Length), OK); if not OK then return; end if;
         Result.Descriptor_Retrieved := True; Done;
      end Attempt;
   begin
      Result := (others => <>);
      Report_Data := [others => 0]; Input_Scratch := [others => 0];
      Attempt;
      if Armed then
         --  Never repeat a failed release from the normal completion path.
         if Result.Reason /= Reset_Release_Failed then
            Release_Reset (Result.Ticket, Result.Cleanup);
         end if;
         Result.Reset_Still_Armed := Result.Cleanup /= Transferred;
      end if;
      Input_Scratch := [others => 0];
      if not Result.Descriptor_Retrieved then
         Report_Data := [others => 0];
         Trace (Result, False);
      end if;
   end Run;
end HID_I2C_Bring_Up;
