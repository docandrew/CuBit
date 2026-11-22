pragma Ada_2022;
with Intel_LPSS_I2C_Profile;
package body I2C_Native_Loop is
   use type Controller.Phase;
   use type W.Reply_Status;
   function Observe (S : State) return Snapshot is
     ((S.Info, Controller.Observe (S.Device), Requests.Observe (S.Client)));
   function Ready (S : State) return Boolean is
     (not S.Busy and then S.Info.At_Phase = Serving and then S.Info.Startup_Reported
      and then S.Info.Startup_Delivered and then S.Info.Startup_Outcome.Status = W.Ready);
   function Needs_Step (S : State) return Boolean is
     (not S.Busy and then (S.Info.At_Phase = Starting or else
       (S.Info.At_Phase in Serving | Draining and then Requests.Observe (S.Client).Active)));
   function Stopped_Quiescent (S : State) return Boolean is
     (not S.Busy and then S.Info.At_Phase = Stopped and then
      S.Info.Stopped_Because = Requested_Stop and then
      not Requests.Observe (S.Client).Active and then
      Controller.Observe (S.Device).At_Phase = Controller.Available and then
      not Controller.Observe (S.Device).Controller_May_Be_Enabled and then
      not Controller.Observe (S.Device).Completion_Pending);
   procedure Finish_Drain (S : in out State) is
   begin
      if S.Info.At_Phase /= Draining or else Requests.Observe (S.Client).Active then return; end if;
      S.Info.At_Phase := Stopped;
      S.Info.Stopped_Because :=
        (if Controller.Observe (S.Device).At_Phase = Controller.Available and then
            not Controller.Observe (S.Device).Controller_May_Be_Enabled and then
            not Controller.Observe (S.Device).Completion_Pending
         then Requested_Stop else Client_Fault);
   end Finish_Drain;
   procedure Request_Stop (S : in out State; Result : out Stop_Result) is
   begin
      if S.Busy then Result := Stop_Reentry; return; end if;
      if S.Info.Stop_Requested then Result := Stop_Already_Requested; return; end if;
      if S.Info.At_Phase /= Serving then Result := Stop_Not_Ready; return; end if;
      S.Busy := True;
      S.Info.Stop_Requested := True; S.Info.At_Phase := Draining;
      Finish_Drain (S);
      S.Busy := False; Result := Stop_Started;
   end Request_Stop;
   procedure Announce (S : in out State; Outcome : W.Reply) is
   begin
      if S.Info.Startup_Reported then return; end if;
      S.Info.Startup_Outcome := Outcome; S.Info.Startup_Reported := True;
      Report_Startup (Outcome, S.Info.Startup_Delivered);
      if not S.Info.Startup_Delivered then
         S.Info.At_Phase := Stopped; S.Info.Stopped_Because := Report_Delivery_Failed;
      elsif Outcome.Status = W.Ready then S.Info.At_Phase := Serving;
      else S.Info.At_Phase := Stopped;
         S.Info.Stopped_Because := (if Outcome.Status = W.Refused then Startup_Refused else Startup_Failed);
      end if;
   end Announce;
   function Failed_Outcome (Item : Controller.Report) return W.Reply is
      Stage : W.Startup_Stage := W.Frame_Validation;
      Status : constant W.Failure_Status :=
        (if Item.Startup_Effects_Possible then W.Failed_Effects_Possible else W.Failed_No_Effects);
      Value : constant W.Register_Value :=
        (if Item.Startup_Value_Available then W.Register_Value (Item.Startup_Last_Value) else 0);
   begin
      case Item.Failed_At is
         when Controller.Resetting => Stage := W.Controller_Reset;
         when Controller.Probing => Stage := W.Component_Probe;
         when Controller.Mask_Interrupts => Stage := W.Interrupt_Mask;
         when Controller.Disable_Controller => Stage := W.Controller_Disable;
         when Controller.Wait_Disabled => Stage := W.Disabled_Confirmation;
         when Controller.Verify_Idle => Stage := W.Idle_Confirmation;
         when Controller.Verify_TX => Stage := W.TX_Confirmation;
         when Controller.Verify_RX => Stage := W.RX_Confirmation;
         when Controller.Copy_Abort => Stage := W.Abort_Check;
         when Controller.Clear_Status => Stage := W.Status_Clear;
         when others => return (W.Failed_Effects_Possible, W.Frame_Validation, Value);
      end case;
      return (Status, Stage, Value);
   end Failed_Outcome;
   procedure Stop_On_Client_Fault (S : in out State) is
   begin
      if Requests.Observe (S.Client).Faulted or else
        Controller.Observe (S.Device).At_Phase = Controller.Faulted
      then S.Info.At_Phase := Stopped; S.Info.Stopped_Because := Client_Fault;
      end if;
   end Stop_On_Client_Fault;
   procedure Begin_Startup
     (S : in out State; Registers : CuBit.Handles.Device_Memory;
      Facts : Controller.Startup; Virtual : I2C_Mapping_Policy.Address;
      Result : out Start_Result) is
      Started : Boolean;
      Stage : W.Startup_Stage := W.Mapping;
   begin
      if S.Busy then Result := Start_Reentry; return; end if;
      if S.Info.At_Phase /= Cold then Result := Already_Started; return; end if;
      S.Busy := True; S.Info.At_Phase := Starting;
      Native.Initialize (Registers, Facts, Virtual, S.Device, S.Info.Mapping,
                         S.Info.Initial_Admission, Started);
      if not Started then
         if not Intel_LPSS_I2C_Profile.Supported (Facts.Vendor, Facts.Device) then Stage := W.Launch_Validation; end if;
         Announce (S, (W.Refused, Stage, 0));
      end if;
      S.Busy := False; Result := Start_Accepted;
   end Begin_Startup;
   procedure Submit
     (S : in out State; Caller : CuBit.Process_IDs.Process_ID;
      Badge : Interfaces.Unsigned_64; Reply : CuBit.Handles.Reply;
      Item : Requests.Request; Result : out Submit_Result) is
   begin
      if S.Busy then Result := Submit_Reentry; return; end if;
      if not Ready (S) then
         S.Busy := True; S.Info.Last_Early_Reply := (others => <>);
         S.Info.Last_Early_Reply.Status := (if Authorized (Caller, Badge) then Q.Busy else Q.Unauthorized);
         Answer_Client (Reply, S.Info.Last_Early_Reply, S.Info.Last_Early_Delivered);
      else
         S.Busy := True; Requests.Submit (S.Client, S.Device, Caller, Badge, Reply, Item);
         Stop_On_Client_Fault (S);
      end if;
      S.Busy := False; Result := Reply_Taken;
   end Submit;
   procedure Step (S : in out State) is
      Item : Controller.Report;
   begin
      if S.Busy or else S.Info.At_Phase in Cold | Stopped then return; end if;
      S.Busy := True;
      if S.Info.At_Phase = Starting then
         Controller.Advance (S.Device); Item := Controller.Observe (S.Device);
         if Item.At_Phase = Controller.Available then
            if Item.Controller_May_Be_Enabled then Announce (S, (W.Failed_Effects_Possible, W.Disabled_Confirmation, 0));
            else Announce (S, (Status => W.Ready)); end if;
         elsif Item.At_Phase = Controller.Faulted then Announce (S, Failed_Outcome (Item));
         end if;
      elsif S.Info.At_Phase in Serving | Draining then
         Requests.Advance (S.Client, S.Device); Stop_On_Client_Fault (S);
         Finish_Drain (S);
      end if;
      S.Busy := False;
   end Step;
end I2C_Native_Loop;
