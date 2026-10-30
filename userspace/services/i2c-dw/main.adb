pragma Ada_2022;
with Interfaces;
with CuBit.Messages;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Process_IDs;
with CuBit.Driver_Readiness;
with CuBit.Launch_Configurations;
with Launch_Configuration;
with Startup;
with DW_I2C_Transfer;
with DW_I2C_Request_Types;
with Intel_LPSS_I2C_Profile;
with I2C_Channel_Badges;
with I2C_Configure_Replies;
with I2C_Transfer_Wire;
with I2C_Native_Controller;
with I2C_Native_Grant_Copy;
with I2C_Native_Loop;
with I2C_Native_Results;
with I2C_Mapping_Policy;
with I2C_Log;
with I2C_Work_Policy;
with I2C_Event_Policy;
with CuBit.Channel_Bells;
procedure Main is
   package M renames CuBit.Messages;
   package IPC renames CuBit.Handles.IPC;
   package PID renames CuBit.Process_IDs;
   package LC renames Launch_Configuration;
   package T renames DW_I2C_Transfer;
   package Q renames DW_I2C_Request_Types;
   package W renames I2C_Transfer_Wire;
   package R renames I2C_Configure_Replies;
   use type Interfaces.Unsigned_64;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_16;
   use type M.IPC_Flags;
   use type M.Protocol_Flags;
   use type M.MessageTag;
   use type LC.Interrupt_Delivery;
   use type W.Decode_Status;
   use type R.Reply_Status;
   use type PID.Process_ID;
   use type M.Activity_Result;
   Register_Virtual : constant I2C_Mapping_Policy.Address := 16#6000_0000_0000#;
   Register_Bytes : constant LC.Byte_Count := 4096;
   Transfer_Capacity : constant T.Limit := T.Limit'Last;
   I2C_Role : constant Interfaces.Unsigned_64 := 32;
   Registration_Refused : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
   Reset_Steps : constant T.Limit := 4096;
   Configuration_Steps : constant T.Limit := 4096;
   Startup_Steps : constant := 16_384;
   Work_Burst : constant := I2C_Work_Policy.Work_Burst;
   Poll_Milliseconds : constant Interfaces.Unsigned_64 := I2C_Work_Policy.Poll_Milliseconds;
   Health_Milliseconds : constant Interfaces.Unsigned_64 := 5000;
   package Native is new I2C_Native_Controller (Transfer_Capacity);
   package C renames Native.Controller;
   package Grants is new I2C_Native_Grant_Copy (C.Bytes);
   use type C.Phase;
   function Authorized (Caller : PID.Process_ID; Badge : Interfaces.Unsigned_64)
      return Boolean is
     (PID.Is_Process (Caller) and then I2C_Channel_Badges.Valid_Word (Badge));
   -- Caller and Badge below come only from Poll_Service_Request's kernel
   -- metadata. Payload words never substitute for either authority input.
   function Message_For (Item : W.Completion) return M.Message is
      Words : constant W.Completion_Words := W.Encode_Completion (Item);
      Msg : M.Message := M.NULL_MESSAGE;
   begin
      Msg.tag.label := W.Transfer_Reply_Operation;
      Msg.tag.protocol := M.Protocol_Flags (W.Protocol_Version);
      Msg.tag.length := Interfaces.Unsigned_8 (W.Completion_Word_Count);
      for N in Words'Range loop Msg.words (N) := Words (N); end loop;
      return Msg;
   end Message_For;
   procedure Answer_Client
     (Reply : CuBit.Handles.Reply; Item : Q.Response; Delivered : out Boolean) is
   begin
      Delivered := IPC.Answer (Reply, Message_For (I2C_Native_Results.To_Wire (Item)));
   end Answer_Client;
   procedure Report_Startup (Item : R.Reply; Delivered : out Boolean);
   package Driver is new I2C_Native_Loop
     (Native, Authorized, Grants.Copy.Read_Input, Grants.Copy.Write_Output,
      Answer_Client, Report_Startup);
   use type Driver.Phase;
   use type Driver.Submit_Result;
   use type Driver.Stop_Result;
   State : Driver.State;
   procedure Report_Startup (Item : R.Reply; Delivered : out Boolean) is
      Ready : Boolean := Item.Status = R.Ready;
      Report : M.Message := M.NULL_MESSAGE;
      Tag : M.MessageTag;
      Registration : Interfaces.Unsigned_64;
      Snapshot : constant Driver.Snapshot := Driver.Observe (State);
   begin
      if Ready and then Startup.Registration.Present then
         Registration := M.registerDriver (I2C_Role, Startup.Registration.Value);
         Ready := Registration /= Registration_Refused and then
                  Registration = PID.To_Word (M.Own_Process);
         I2C_Log.Stage ("registration", Ready, (if Ready then "primary" else "refused"));
      end if;
      if Item.Status /= R.Ready then
         I2C_Log.Stage (R.Startup_Stage'Image (Item.Stage), False,
            R.Reply_Status'Image (Item.Status),
            (if Snapshot.Hardware.Startup_Value_Available then
               I2C_Log.Hex ("val", Interfaces.Unsigned_64 (Item.Last_Register_Value))
             else " sample=unavailable"));
      end if;
      Report.tag.label := (if Ready then CuBit.Driver_Readiness.Ready_Label
                          else CuBit.Driver_Readiness.Not_Present_Label);
      -- Generic_Report replies immediately and never calls this driver back.
      Tag := M.capSend (Startup.Ready, Report, M.Wait_Forever);
      Delivered := Ready = (Item.Status = R.Ready) and then Tag = M.NULL_TAG;
      I2C_Log.Stage ("readiness-report", Delivered,
         (if Delivered then "acknowledged" else "refused"));
      I2C_Log.Summary (Ready and then Delivered);
   end Report_Startup;
   procedure Refuse_Launch (Reason : String) is
      Delivered : Boolean;
   begin
      I2C_Log.Stage ("identify", False, Reason);
      Report_Startup ((R.Refused, R.Launch_Validation, 0), Delivered);
      I2C_Log.Disable ("launch-refused");
   end Refuse_Launch;
   Config : LC.Generic_PCI_Configuration;
   Configured : Boolean;
   Start_Result : Driver.Start_Result;
   Ignored : Interfaces.Unsigned_64;
   Next_Health : Interfaces.Unsigned_64 := 0;
   Clock_Warning : Boolean := False;
   Requests_Seen : Interfaces.Unsigned_64 := 0;
   Rejected_Frames : Interfaces.Unsigned_64 := 0;
   Events_Per_Turn : constant := 8;
   Events_Seen, Ignored_Events : Interfaces.Unsigned_64 := 0;
   procedure Poll_Events (Progressed : out Boolean) is
      Event : M.Message;
      Publisher : PID.Process_ID;
      Action : I2C_Event_Policy.Action;
      Result : Driver.Stop_Result;
      Noted : Boolean;
   begin
      Progressed := False;
      for N in 1 .. Events_Per_Turn loop
         exit when not M.Poll_Event (Event, Publisher);
         Progressed := True;
         if Events_Seen < Interfaces.Unsigned_64'Last then Events_Seen := Events_Seen + 1; end if;
         Action := I2C_Event_Policy.Ignore;
         if Event.tag.protocol = 0 and then Event.tag.ipc = M.No_IPC_Flags and then
            Event.tag.reserved = 0
         then
            Action := I2C_Event_Policy.Classify
              (Publisher, Event.tag.label, Event.tag.length,
               Event.words (0), Event.words (1), Event.words (2));
         end if;
         case Action is
            when I2C_Event_Policy.Route_Channel_Wake =>
               CuBit.Channel_Bells.Note_Notice
                 (Event.tag.label, Event.tag.length,
                  [Event.words (0), Event.words (1), Event.words (2), Event.words (3)], Noted);
               if not Noted and then Ignored_Events < Interfaces.Unsigned_64'Last then
                  Ignored_Events := Ignored_Events + 1;
               end if;
            when I2C_Event_Policy.Disable_Logging =>
               -- Client acquisitions are returned inside copy callbacks; only
               -- the logger can retain a grant-backed mapping between turns.
               I2C_Log.Disable ("grant-lifetime-event");
            when I2C_Event_Policy.Stop_After_Drain =>
               Driver.Request_Stop (State, Result);
               if Result = Driver.Stop_Started then I2C_Log.Stage ("stop", True, "draining"); end if;
            when I2C_Event_Policy.Ignore =>
               if Ignored_Events < Interfaces.Unsigned_64'Last then Ignored_Events := Ignored_Events + 1; end if;
         end case;
      end loop;
   end Poll_Events;
   function Request_Budget (Item : W.Request; Badge : W.Word) return C.Work_Limit is
      package P renames Intel_LPSS_I2C_Profile;
      Channel : constant I2C_Channel_Badges.Decoding := I2C_Channel_Badges.Decode (Badge);
      use type I2C_Channel_Badges.Decode_Status;
   begin
      if Channel.Status /= I2C_Channel_Badges.Decoded or else
         Channel.Value.Maximum_Hz > I2C_Channel_Badges.Hertz (P.Fast_Plus_Maximum)
      then return I2C_Work_Policy.Base_Steps; end if;
      return C.Work_Limit (I2C_Work_Policy.Budget
        (Item.Write_Count, Item.Read_Count,
         P.Derive (P.PCI_ID (Config.Vendor), P.PCI_ID (Config.Device),
                   P.Hertz (Channel.Value.Maximum_Hz))));
   end Request_Budget;
   procedure Handle (Caller : PID.Process_ID; Msg : M.Message) is
      Words : W.Request_Words;
      Parsed : W.Decoding;
      Saved : IPC.Saved_Reply;
      Taken : Driver.Submit_Result;
      Delivered : Boolean;
      Rejection : W.Completion := (W.Invalid_Frame, 0, False, False, 0, 0);
   begin
      if Requests_Seen < Interfaces.Unsigned_64'Last then Requests_Seen := Requests_Seen + 1; end if;
      for N in Words'Range loop Words (N) := Msg.words (N); end loop;
      if Driver.Observe (State).Loop_Status.Stop_Requested then
         Rejection.Status := W.Busy;
      elsif not Authorized (Caller, Msg.authorityTag) then
         Rejection.Status := W.Unauthorized;
      elsif Msg.tag.length = Interfaces.Unsigned_8 (W.Request_Word_Count) and then
            Msg.tag.ipc = M.No_IPC_Flags
      then
         Parsed := W.Decode (Msg.tag.label, W.Word (Msg.tag.protocol), Words,
                             W.Capacity (Transfer_Capacity));
      end if;
      Saved := IPC.Save_Reply;
      if not Saved.Saved then
         if Parsed.Status = W.Decoded then Rejection.Status := W.Busy; end if;
         -- Refusal retains current reply authority; never retain Caller as
         -- a replacement saved reply. One-way messages have no reply to save.
         Ignored := M.reply (Caller, Message_For (Rejection));
         return;
      end if;
      if Parsed.Status /= W.Decoded then
         if Rejected_Frames < Interfaces.Unsigned_64'Last then Rejected_Frames := Rejected_Frames + 1; end if;
         Delivered := IPC.Answer (Saved.Handle, Message_For (Rejection));
         return;
      end if;
      declare
         Item : constant Driver.Requests.Request :=
           (Input => Parsed.Value.Input, Output => Parsed.Value.Output,
            Write_Count => T.Count (Parsed.Value.Write_Count),
            Read_Count => T.Count (Parsed.Value.Read_Count),
            Deadline => T.Tick (W.Encode_Deadline (Parsed.Value.Limit)),
            Step_Budget => Request_Budget (Parsed.Value, Msg.authorityTag));
      begin
         Driver.Submit (State, Caller, Msg.authorityTag, Saved.Handle, Item, Taken);
         if Taken = Driver.Submit_Reentry then
            -- No reply was consumed by Submit. Main is serialized; this
            -- branch still retires the newly saved authority exactly once.
            Answer_Client (Saved.Handle, (Status => Q.Busy, others => <>), Delivered);
         end if;
      end;
   end Handle;
begin
   CuBit.Launch_Configurations.Get (Config, Configured);
   if not Configured or else Config.Window_Total /= 1 or else
      Config.Windows (1).Bytes < Register_Bytes or else
      Config.Delivery /= LC.Polled or else Config.Vectors /= 0 or else
      not Intel_LPSS_I2C_Profile.Supported
        (Intel_LPSS_I2C_Profile.PCI_ID (Config.Vendor),
         Intel_LPSS_I2C_Profile.PCI_ID (Config.Device))
   then Refuse_Launch ("launch-config"); return; end if;
   I2C_Log.Stage ("identify", True, "polled",
      I2C_Log.Hex ("vendor", Config.Vendor) & I2C_Log.Hex ("device", Config.Device) &
      I2C_Log.Hex ("rev", Config.Revision) & I2C_Log.Hex ("location", Config.Location) &
      I2C_Log.Hex ("bar-bytes", Config.Windows (1).Bytes));
   Driver.Begin_Startup
     (State, Startup.Registers,
      (Vendor => Intel_LPSS_I2C_Profile.PCI_ID (Config.Vendor),
       Device => Intel_LPSS_I2C_Profile.PCI_ID (Config.Device),
       BAR_Base => Config.Windows (1).First, Window_Bytes => Config.Windows (1).Bytes,
       Reset_Budget => Reset_Steps, Configuration_Budget => Configuration_Steps,
       Owner_Budget => Startup_Steps), Register_Virtual, Start_Result);
   while Driver.Observe (State).Loop_Status.At_Phase = Driver.Starting loop
      declare Before : constant C.Phase := Driver.Observe (State).Hardware.At_Phase; begin
         Driver.Step (State);
         if Before /= Driver.Observe (State).Hardware.At_Phase and then
            Driver.Observe (State).Hardware.At_Phase /= C.Faulted
         then I2C_Log.Stage (C.Phase'Image (Before), True, "complete"); end if;
      end;
   end loop;
   if not Driver.Ready (State) then I2C_Log.Disable ("startup-refused"); return; end if;
   loop
      declare
         Caller : PID.Process_ID;
         Msg : M.Message;
         Found : Boolean;
         Event_Progressed : Boolean;
         Now : Interfaces.Unsigned_64;
         Activity : M.Activity_Result;
      begin
         Poll_Events (Event_Progressed);
         M.Poll_Service_Request (Caller, Msg, Found);
         if Found then Handle (Caller, Msg); end if;
         for N in 1 .. Work_Burst loop
            exit when not Driver.Needs_Step (State);
            Driver.Step (State);
         end loop;
         Now := M.syscall (M.SYSCALL_GETTIME);
         if Now = Interfaces.Unsigned_64'Last then
            if not Clock_Warning then
               I2C_Log.Note ("health clock=unavailable"); Clock_Warning := True;
            end if;
         elsif Now >= Next_Health then
            I2C_Log.Note ("health phase=" & Driver.Phase'Image
              (Driver.Observe (State).Loop_Status.At_Phase) &
              " requests=" & Interfaces.Unsigned_64'Image (Requests_Seen) &
              " rejected=" & Interfaces.Unsigned_64'Image (Rejected_Frames) &
              " events=" & Interfaces.Unsigned_64'Image (Events_Seen) &
              " ignored=" & Interfaces.Unsigned_64'Image (Ignored_Events));
            Next_Health := (if Now <= Interfaces.Unsigned_64'Last - 1 - Health_Milliseconds
                            then Now + Health_Milliseconds else Interfaces.Unsigned_64'Last - 1);
         end if;
         if Driver.Observe (State).Loop_Status.At_Phase = Driver.Stopped and then
            Driver.Observe (State).Loop_Status.Stop_Requested
         then
            I2C_Log.Stage ("stop", Driver.Stopped_Quiescent (State),
               (if Driver.Stopped_Quiescent (State) then "quiescent"
                else "effects-uncertain " & C.Failure'Image (Driver.Observe (State).Hardware.Reason)),
               " client=" & Q.Reason'Image (Driver.Observe (State).Client_Status.Last_Reply.Status));
            I2C_Log.Disable ("stop");
            return;
         elsif not Driver.Ready (State) and then
            Driver.Observe (State).Loop_Status.At_Phase /= Driver.Draining
         then
            I2C_Log.Stage ("runtime", False,
               C.Failure'Image (Driver.Observe (State).Hardware.Reason),
               " client=" & Q.Reason'Image (Driver.Observe (State).Client_Status.Last_Reply.Status));
            I2C_Log.Disable ("runtime-fault");
            return;
         elsif Driver.Needs_Step (State) then
            Ignored := M.syscall (M.SYSCALL_SLEEP, Poll_Milliseconds);
         elsif not Found and then not Event_Progressed then
            Activity := M.Wait_For_Activity_Until
              ((if Now = Interfaces.Unsigned_64'Last then M.Wait_Forever else Next_Health));
            if Activity = M.Unavailable then
               Ignored := M.syscall (M.SYSCALL_SLEEP, Poll_Milliseconds);
            end if;
         end if;
      end;
   end loop;
end Main;
