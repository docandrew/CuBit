pragma Ada_2022;
with I2C_Native_Channel_Payload;
with I2C_Work_Policy;
with Intel_LPSS_I2C_Profile;
package body I2C_Channel_Controller is
   package W renames I2C_Channel_Wire;
   package B renames I2C_Channel_Badges;
   package P renames Intel_LPSS_I2C_Profile;
   use type Controller.Phase, Controller.Admission, Controller.Payload_Lifetime;
   use type W.Decode_Status, B.Decode_Status, B.Address_Mode, B.Hertz, P.Outcome;
   Owner : Controller.State;
   Vendor, Device : P.PCI_ID := 0;
   function May_Release return Boolean is
     (Controller.Payload_Status (Owner) in Controller.Not_Borrowed | Controller.Returnable);
   package Payload is new I2C_Native_Channel_Payload (May_Release);
   use type Payload.Binding, Payload.Releasing;
   Buffer_State : Payload.State;
   procedure Read_Payload (Index : T.Count; Value : out T.Byte; Accepted : out Boolean) is
   begin Payload.Read_Byte (Buffer_State, Index, Value, Accepted); end Read_Payload;
   procedure Write_Payload (Index : T.Count; Value : T.Byte; Accepted : out Boolean) is
   begin Payload.Write_Byte (Buffer_State, Index, Value, Accepted); end Write_Payload;
   procedure Start (Parameters : Controller.Startup; Result : out Controller.Admission) is
   begin
      Controller.Start (Owner, Parameters, Result);
      if Result = Controller.Accepted then Vendor := Parameters.Vendor; Device := Parameters.Device; end if;
   end Start;
   procedure Advance is
   begin Controller.Advance (Owner); end Advance;
   procedure Prepare_Interrupt_Mode (Deadline : T.Tick; Result : out Controller.Interrupt_Result) is
   begin Controller.Prepare_Interrupt_Mode (Owner, Deadline, Result); end Prepare_Interrupt_Mode;
   procedure Reject_Interrupt_Wait is
   begin Controller.Reject_Interrupt_Wait (Owner); end Reject_Interrupt_Wait;
   procedure Arm_Interrupt_Wait (Result : out Controller.Interrupt_Result) is
   begin Controller.Arm_Interrupt_Wait (Owner, Result); end Arm_Interrupt_Wait;
   procedure Disarm_Interrupt_Wait (Result : out Controller.Interrupt_Result) is
   begin Controller.Disarm_Interrupt_Wait (Owner, Result); end Disarm_Interrupt_Wait;
   function Waiting_Deadline return T.Tick is (Controller.Waiting_Deadline (Owner));
   function Observe return Controller.Report is (Controller.Observe (Owner));
   function Has_Payload return Boolean is (Payload.Is_Bound (Buffer_State));
   procedure Submit
     (Admitted_Badge : B.Word; Header : W.Raw_Request;
      Input, Output : RC.Payload_View; ID : out T.Ticket;
      Result : out Submission; Admission : out Controller.Admission) is
      Raw : constant W.Decoding := W.Decode (Header, W.Maximum_Payload);
      Channel : constant B.Decoding := B.Decode (Admitted_Badge);
      Bound : Payload.Binding;
      Released_Buffer : Payload.Releasing;
      Timing : P.Timing;
   begin
      ID := T.No_Ticket; Admission := Controller.Busy;
      if Controller.Observe (Owner).At_Phase = Controller.Faulted then Result := Retained; return; end if;
      if Has_Payload or else Controller.Observe (Owner).At_Phase /= Controller.Available then
         Result := Busy; return;
      end if;
      if Raw.Status /= W.Decoded then Result := Invalid_Metadata; return; end if;
      if Channel.Status /= B.Decoded then Result := Invalid_Channel; return; end if;
      if Channel.Value.Mode /= B.Seven_Bit or else Channel.Value.Maximum_Hz > B.Hertz (P.Hertz'Last) then
         Result := Unsupported_Channel; return;
      end if;
      Timing := P.Derive (Vendor, Device, P.Hertz (Channel.Value.Maximum_Hz));
      if Timing.Status /= P.Calculated then Result := Unsupported_Channel; return; end if;
      Payload.Bind (Buffer_State, Input, Output, T.Count (Raw.Value.Write_Count),
                    T.Count (Raw.Value.Read_Count), Bound);
      if Bound /= Payload.Bound then Result := Invalid_Views; return; end if;
      Controller.Submit_Borrowed (Owner, DW_I2C_Configuration.Address (Channel.Value.Address),
         P.Hertz (Channel.Value.Maximum_Hz), T.Count (Raw.Value.Write_Count), T.Count (Raw.Value.Read_Count),
         T.Tick (Raw.Value.Limit), Controller.Work_Limit
           (I2C_Work_Policy.Budget (Raw.Value.Write_Count, Raw.Value.Read_Count, Timing)), ID, Admission);
      if Admission = Controller.Accepted then Result := Accepted; return; end if;
      Payload.Release (Buffer_State, Released_Buffer);
      Result := (if Released_Buffer = Payload.Released then Owner_Refused else Retained);
   end Submit;
   procedure Take_Completion (Item : out Controller.Completion; Result : out Controller.Collection) is
   begin Controller.Take_Borrowed_Completion (Owner, Item, Result); end Take_Completion;
   procedure Release (Result : out Releasing) is
      Released_Buffer : Payload.Releasing;
   begin
      if not Has_Payload then Result := No_Payload; return; end if;
      case Controller.Payload_Status (Owner) is
         when Controller.Retain_For_Supervision => Result := Retained; return;
         when Controller.In_Use => Result := Busy; return;
         when Controller.Not_Borrowed | Controller.Returnable => null;
      end case;
      Payload.Release (Buffer_State, Released_Buffer);
      Result := (if Released_Buffer = Payload.Released then Released else Retained);
   end Release;
end I2C_Channel_Controller;
