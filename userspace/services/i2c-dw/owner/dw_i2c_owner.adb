pragma Ada_2022;
package body DW_I2C_Owner with SPARK_Mode is
   use type T.Tick;
   use type T.Count;
   use type T.Ticket;
   use type T.Word;
   use type T.Phase;
   use type T.Action_Kind;
   use type T.Access_Result;
   use type T.Admission;
   use type P.Outcome;
   use type P.Mode;
   use type L.Stage;
   use type L.Admission;
   use type L.Access_Status;
   use type C.Stage;
   use type C.Admission;
   use type C.Access_Status;
   use type R.Outcome;
   use type R.Access_Status;
   Startup_Tick : constant T.Tick := 0;
   Startup_Clock_Limit : constant T.Tick := T.Tick'Last;
   procedure Startup_Clock (Now : out T.Tick; Available : out Boolean) is
   begin Now := Startup_Tick; Available := True; end Startup_Clock;
   -- Startup is explicitly work-bounded. This internal synthetic sample lets
   -- shared reset/probe primitives retain their time-domain contract without
   -- consulting GETTIME. Runtime transfers still use the actual Clock callback.
   procedure Reset_Step is new L.Step (Startup_Clock, Reset_Read, Reset_Write);
   procedure Configuration_Step is new C.Step (Clock, Config_Read, Config_Write);
   function Observe (S : State) return Report is
      Result : Report := S.Info;
   begin
      Result.Reset := L.Observe (S.Reset_State);
      Result.Configuration := C.Observe (S.Config_State);
      Result.Engine_Phase := T.Current_Phase (S.Engine);
      Result.Engine_Reason := T.Reason (S.Engine);
      return Result;
   end Observe;
   procedure Fail (S : in out State; Why : Failure) is
   begin
      S.Info.Reason := Why; S.Info.Failed_At := S.Info.At_Phase;
      S.Info.At_Phase := Faulted;
      S.Read_Buffer := [others => 0];
      if S.Request_Active then
         S.Info.Completion_Pending := True;
         S.Info.Completion_Data.Successful := False;
         S.Info.Completion_Data.Reason := Why;
      end if;
   end Fail;
   procedure Start (S : in out State; Parameters : Startup; Result : out Admission) is
      Reset_Result : L.Admission;
   begin
      if S.Info.At_Phase /= Cold then Result := Busy; return; end if;
      if not P.Supported (Parameters.Vendor, Parameters.Device) then
         Result := Invalid_Resource; return;
      end if;
      L.Start (S.Reset_State, Parameters.Vendor, Parameters.Device,
         Parameters.BAR_Base, Parameters.Window_Bytes, Startup_Tick, Startup_Clock_Limit,
         Parameters.Reset_Budget, Reset_Result);
      if Reset_Result /= L.Accepted then Result := Invalid_Resource; return; end if;
      S.Vendor := Parameters.Vendor; S.Device := Parameters.Device;
      S.Deadline := Startup_Clock_Limit;
      S.Last_Time := Startup_Tick; S.Remaining := Parameters.Owner_Budget;
      S.Config_Budget := Parameters.Configuration_Budget;
      S.Info.At_Phase := Resetting; Result := Accepted;
   end Start;
   procedure Check_Time (S : in out State; OK : out Boolean) is
      Now : T.Tick;
      Clock_OK : Boolean;
   begin
      if not S.Request_Active then OK := True; return; end if;
      Clock (Now, Clock_OK); OK := False;
      if not Clock_OK then Fail (S, Clock_Unavailable);
      elsif Now < S.Last_Time then Fail (S, Clock_Reversed);
      elsif Now >= S.Deadline then Fail (S, Deadline_Expired);
      else S.Last_Time := Now; OK := True;
      end if;
   end Check_Time;
   procedure Do_Work (S : in out State) is
      Config_Result : C.Admission;
      Settings : C.Configuration;
      Sample : R.Sample;
      Action : T.Action;
      Data, Read_Value : T.Byte := 0;
      Access_Result : T.Access_Result;
      Matched, Retired : Boolean;
      Value : T.Word := 0;
      Status : C.Access_Status;
      ID : C.Register_ID := C.Controller_Status;
      procedure Startup_Read
        (Register : R.Read_Register; Data : out R.Word; Result : out R.Access_Status) is
      begin
         Data_Read (Register, Data, Result);
         if Result in R.Access_OK | R.Unknown_Effect then
            S.Info.Startup_Value_Available := True; S.Info.Startup_Last_Value := Data;
         end if;
      end Startup_Read;
      package Startup_Port is new R.Port (Startup_Clock, Startup_Read, Data_Write);
   begin
      case S.Info.At_Phase is
         when Resetting =>
            Reset_Step (S.Reset_State);
            S.Info.Startup_Effects_Possible := S.Info.Startup_Effects_Possible or else
              L.Observe (S.Reset_State).Writes_Completed > 0 or else
              (L.Observe (S.Reset_State).Access_Attempted and then
               L.Observe (S.Reset_State).Last_Status = L.Unknown_Effect);
            if L.Observe (S.Reset_State).Access_Attempted and then
              L.Observe (S.Reset_State).Last_Status in L.Access_OK | L.Unknown_Effect
            then
               S.Info.Startup_Value_Available := True;
               S.Info.Startup_Last_Value := T.Word (L.Observe (S.Reset_State).Last_Value);
            end if;
            if L.Observe (S.Reset_State).At_Stage = L.Failed then Fail (S, Reset_Failed);
            elsif L.Observe (S.Reset_State).At_Stage = L.Ready then S.Info.At_Phase := Probing;
            end if;
         when Probing =>
            Startup_Port.Probe (S.Deadline, S.Capabilities, S.Info.Port_Status);
            if not S.Capabilities.Ready then Fail (S, Probe_Failed); return; end if;
            S.Info.At_Phase := Mask_Interrupts;
         when Prepare_Configuration =>
            Settings :=
              (Speed => (if S.Timing.Selected_Mode = P.Standard_Mode then C.Standard else C.Fast),
               Device => S.Target, High_Count => C.Counter (S.Timing.High_Count),
               Low_Count => C.Counter (S.Timing.Low_Count), Program_Hold => True,
               Hold_Count => C.Hold_Field
                 (T.Word (S.Timing.TX_Hold) or T.Word (S.Timing.RX_Hold) * 16#10000#),
               TX_Depth => S.Capabilities.TX_Depth, RX_Depth => S.Capabilities.RX_Depth);
            C.Start (S.Config_State, Settings, True, S.Capabilities.Version,
               S.Last_Time, S.Deadline, S.Config_Budget, Config_Result);
            if Config_Result /= C.Accepted then Fail (S, Configuration_Failed);
            else S.Info.At_Phase := Configuring;
            end if;
         when Configuring =>
            Configuration_Step (S.Config_State);
            S.Info.Controller_May_Be_Enabled := C.Observe (S.Config_State).May_Be_Enabled;
            S.Info.Completion_Data.Effects_Possible :=
              S.Info.Completion_Data.Effects_Possible or else
              (C.Observe (S.Config_State).Access_Attempted and then
               C.Observe (S.Config_State).Last_Status in C.Access_OK | C.Unknown_Effect);
            if C.Observe (S.Config_State).At_Stage = C.Failed then Fail (S, Configuration_Failed);
            elsif C.Observe (S.Config_State).At_Stage = C.Ready then S.Info.At_Phase := Transferring;
            end if;
         when Transferring =>
            Port.Capture (S.Capabilities.TX_Depth, S.Capabilities.RX_Depth,
                          S.Deadline, Sample, S.Info.Port_Status);
            if not Sample.Ready then Fail (S, Transfer_Failed); return; end if;
            T.Poll (S.Engine, S.Last_Time, Sample.Hardware, Action);
            if T.Current_Phase (S.Engine) = T.Failed then Fail (S, Transfer_Failed); return; end if;
            if Action.Kind /= T.No_Action then
               if Action.Kind = T.Send_Command and then not Action.Read_Request then
                  Data := S.Write_Buffer (Action.Index);
               end if;
               Port.Execute (S.Access_Owner, Action, Data, S.Deadline,
                             Read_Value, Access_Result, S.Info.Port_Status);
               S.Info.Completion_Data.Effects_Possible :=
                 S.Info.Completion_Data.Effects_Possible or
                 (S.Info.Port_Status.Access_Attempted and then
                   Access_Result in T.Succeeded | T.Indeterminate);
               T.Finish_Access (S.Engine, Action.ID, Access_Result, Matched);
               if not Matched then Fail (S, Accounting_Failed); return; end if;
               if Action.Kind = T.Receive_Byte and then Access_Result = T.Succeeded then
                  S.Read_Buffer (Action.Index) := Read_Value;
               end if;
               if S.Info.Port_Status.Status /= R.Success or else
                 T.Current_Phase (S.Engine) = T.Failed
               then Fail (S, Transfer_Failed); return; end if;
            end if;
            if T.Current_Phase (S.Engine) = T.Complete then S.Info.At_Phase := Disable_Controller; end if;
         when Mask_Interrupts | Disable_Controller | Enable_Controller | Wait_Disabled | Wait_Enabled |
              Verify_Idle | Verify_TX | Verify_RX | Copy_Abort | Clear_Status =>
            case S.Info.At_Phase is
               when Mask_Interrupts => ID := C.Interrupt_Mask; Config_Write (ID, 0, Status);
               when Disable_Controller | Enable_Controller =>
                  ID := C.Enable;
                  Value := (if S.Info.At_Phase = Enable_Controller then C.Enabled_Bit else 0);
                  Config_Write (C.Enable, Value, Status);
               when Wait_Disabled | Wait_Enabled => ID := C.Enable_Status; Config_Read (ID, Value, Status);
               when Verify_TX => ID := C.TX_Level; Config_Read (ID, Value, Status);
               when Verify_RX => ID := C.RX_Level; Config_Read (ID, Value, Status);
               when Copy_Abort => ID := C.Abort_Source; Config_Read (ID, Value, Status);
               when Clear_Status => ID := C.Clear_Interrupts; Config_Read (ID, Value, Status);
               when others => Config_Read (ID, Value, Status);
            end case;
            S.Info.Last_Owner_Register := ID; S.Info.Last_Owner_Value := Value;
            S.Info.Last_Owner_Status := Status; S.Info.Owner_Access_Attempted := True;
            if not S.Request_Active and then Status in C.Access_OK | C.Unknown_Effect then
               S.Info.Startup_Value_Available := True; S.Info.Startup_Last_Value := Value;
            end if;
            if not S.Request_Active and then
              ID in C.Interrupt_Mask | C.Enable | C.Clear_Interrupts and then
              Status in C.Access_OK | C.Unknown_Effect
            then S.Info.Startup_Effects_Possible := True;
            end if;
            if S.Request_Active and then S.Info.At_Phase in Disable_Controller | Enable_Controller
              and then Status in C.Access_OK | C.Unknown_Effect
            then S.Info.Completion_Data.Effects_Possible := True;
            end if;
            if S.Info.At_Phase = Enable_Controller and then Status in C.Access_OK | C.Unknown_Effect
            then S.Info.Controller_May_Be_Enabled := True;
            end if;
            if Status /= C.Access_OK then
               Fail (S, (if Status = C.Unknown_Effect then Register_Indeterminate else Register_Refused)); return;
            end if;
            case S.Info.At_Phase is
               when Mask_Interrupts => S.Info.At_Phase := Disable_Controller;
               when Disable_Controller => S.Info.At_Phase := Wait_Disabled;
               when Enable_Controller => S.Info.At_Phase := Wait_Enabled;
               when Wait_Disabled =>
                  if (Value and C.Enabled_Bit) = 0 then
                     S.Info.Controller_May_Be_Enabled := False; S.Info.At_Phase := Verify_Idle;
                  end if;
               when Wait_Enabled => if (Value and C.Enabled_Bit) /= 0 then S.Info.At_Phase := Transferring; end if;
               when Verify_Idle => if (Value and C.Activity_Bits) = 0 then S.Info.At_Phase := Verify_TX; end if;
               when Verify_TX =>
                  if Value /= 0 then Fail (S, Controller_Not_Quiescent); return; end if;
                  S.Info.At_Phase := Verify_RX;
               when Verify_RX =>
                  if Value /= 0 then Fail (S, Controller_Not_Quiescent); return; end if;
                  S.Info.At_Phase := Copy_Abort;
               when Copy_Abort =>
                  S.Info.Cleanup_Abort_Value := Value;
                  if Value /= 0 then Fail (S, Abort_During_Quiescence); return; end if;
                  S.Info.At_Phase := Clear_Status;
               when Clear_Status =>
                  if S.Request_Active then
                     C.Retire (S.Config_State, Retired);
                     if not Retired then Fail (S, Accounting_Failed); return; end if;
                     T.Retire (S.Engine, True, Retired);
                     if not Retired then Fail (S, Accounting_Failed); return; end if;
                     S.Info.Completion_Pending := True;
                     S.Info.Completion_Data.Successful := True;
                     S.Info.At_Phase := Completion_Ready;
                  else S.Info.At_Phase := Available;
                  end if;
               when others => null;
            end case;
         when others => null;
      end case;
   end Do_Work;
   procedure Advance (S : in out State) is
      OK : Boolean;
      Worked_At : constant Phase := S.Info.At_Phase;
   begin
      if S.Info.At_Phase in Cold | Available | Completion_Ready | Faulted then return; end if;
      S.Info.Owner_Access_Attempted := False;
      if S.Remaining = 0 then Fail (S, Budget_Exhausted); return; end if;
      S.Remaining := S.Remaining - 1;
      Check_Time (S, OK); if not OK then return; end if;
      Do_Work (S);
      if S.Info.At_Phase /= Faulted then
         Check_Time (S, OK);
         if not OK then S.Info.Failed_At := Worked_At; end if;
      end if;
   end Advance;
   procedure Submit
     (S : in out State; Target : C.Address; Speed_Limit : P.Hertz;
      Write_Data : Bytes; Read_Count : T.Count;
      Deadline : T.Tick; Step_Budget : Work_Limit;
      ID : out T.Ticket; Result : out Admission) is
      Now : T.Tick;
      Clock_OK : Boolean;
      Core_Result : T.Admission;
      Position : T.Count := 0;
      Timing : P.Timing;
   begin
      ID := T.No_Ticket;
      if S.Info.At_Phase /= Available then Result := Busy; return; end if;
      if Write_Data'Length = 0 and then Read_Count = 0 then Result := Empty_Transfer; return; end if;
      if Write_Data'Length > Capacity or else Read_Count > Capacity - T.Count (Write_Data'Length)
      then Result := Capacity_Refused; return; end if;
      Timing := P.Derive (S.Vendor, S.Device, Speed_Limit);
      if Timing.Status /= P.Calculated then Result := Invalid_Timing; return; end if;
      if S.Sequence = T.Ticket'Last then Result := Tickets_Exhausted; return; end if;
      Clock (Now, Clock_OK);
      if not Clock_OK or else Now < S.Last_Time then Result := Clock_Refused; return; end if;
      if Now >= Deadline then Result := Expired; return; end if;
      T.Submit (S.Engine, T.Count (Write_Data'Length), Read_Count, Capacity,
                S.Capabilities.TX_Depth, S.Capabilities.RX_Depth, Now, Deadline, Core_Result);
      if Core_Result /= T.Accepted then Result := Busy; return; end if;
      S.Write_Buffer := [others => 0]; S.Read_Buffer := [others => 0];
      for Byte of Write_Data loop Position := Position + 1; S.Write_Buffer (Position) := Byte; end loop;
      S.Target := Target; S.Timing := Timing;
      S.Writes := T.Count (Write_Data'Length); S.Reads := Read_Count;
      S.Deadline := Deadline; S.Last_Time := Now; S.Remaining := Step_Budget;
      S.Sequence := S.Sequence + 1; ID := S.Sequence;
      S.Info.Completion_Data := (ID, False, None, S.Writes, S.Reads, False);
      S.Request_Active := True; S.Info.At_Phase := Prepare_Configuration; Result := Accepted;
   end Submit;
   procedure Take_Completion
     (S : in out State; Read_Data : out Bytes; Item : out Completion;
      Required : out T.Count; Result : out Collection) is
      Position : T.Count := 0;
   begin
      Read_Data := [others => 0]; Item := (others => <>); Required := 0;
      if not S.Info.Completion_Pending then Result := Not_Ready; return; end if;
      Item := S.Info.Completion_Data;
      Required := (if Item.Successful then S.Reads else 0);
      if Read_Data'Length < Required then Result := Insufficient_Capacity; return; end if;
      if Required > 0 then Position := Read_Data'First; end if;
      for I in T.Count range 1 .. Required loop
         Read_Data (Position) := S.Read_Buffer (I);
         if I < Required then Position := Position + 1; end if;
      end loop;
      S.Info.Completion_Pending := False; S.Request_Active := False;
      S.Write_Buffer := [others => 0]; S.Read_Buffer := [others => 0];
      if S.Info.At_Phase = Completion_Ready then S.Info.At_Phase := Available; end if;
      Result := Collected;
   end Take_Completion;
end DW_I2C_Owner;
