with Fixture;
package body I2C_Channel_Controller is
   use type T.Tick;
   procedure Read_Payload (Index : T.Count; Value : out T.Byte; Accepted : out Boolean) is
      pragma Unreferenced (Index, Value, Accepted);
   begin raise Program_Error with "unexpected Read_Payload"; end Read_Payload;
   procedure Write_Payload (Index : T.Count; Value : T.Byte; Accepted : out Boolean) is
      pragma Unreferenced (Index, Value, Accepted);
   begin raise Program_Error with "unexpected Write_Payload"; end Write_Payload;
   procedure Start (Parameters : Controller.Startup; Result : out Controller.Admission) is
      pragma Unreferenced (Parameters);
   begin Result := Controller.Accepted; end Start;
   procedure Advance is
   begin Fixture.Masked := True; end Advance;
   procedure Prepare_Interrupt_Mode (Deadline : T.Tick; Result : out Controller.Interrupt_Result) is
   begin
      Fixture.Prepares := Fixture.Prepares + 1;
      if Deadline /= 1001 then raise Program_Error with "prepare deadline"; end if;
      Fixture.Masked := Fixture.Prepare_OK;
      Result := (if Fixture.Prepare_OK then Controller.Quiet else Controller.Refused);
   end Prepare_Interrupt_Mode;
   procedure Reject_Interrupt_Wait is
   begin Fixture.Rejects := Fixture.Rejects + 1; Fixture.Retained := True; end Reject_Interrupt_Wait;
   procedure Arm_Interrupt_Wait (Result : out Controller.Interrupt_Result) is
   begin
      Fixture.Arms := Fixture.Arms + 1;
      if Fixture.Arm_OK then Fixture.Masked := False; Result := Controller.Armed;
      else Result := Controller.Refused; end if;
   end Arm_Interrupt_Wait;
   procedure Disarm_Interrupt_Wait (Result : out Controller.Interrupt_Result) is
   begin Fixture.Masked := True; Result := Controller.Quiet; end Disarm_Interrupt_Wait;
   function Waiting_Deadline return T.Tick is (1000);
   function Observe return Controller.Report is
      Info : Controller.Report;
   begin
      Info.At_Phase := (if Fixture.Retained then Controller.Faulted else Controller.Available);
      Info.Waiting_For_Hardware := Fixture.Payload and not (Fixture.Scenario = 8 and Fixture.Turns = 1);
      Info.Interrupts_May_Be_Enabled := not Fixture.Masked;
      return Info;
   end Observe;
   function Has_Payload return Boolean is (Fixture.Payload);
   procedure Submit (Admitted_Badge : I2C_Channel_Badges.Word; Header : I2C_Channel_Wire.Raw_Request; Input, Output : RC.Payload_View; ID : out T.Ticket; Result : out Submission; Admission : out Controller.Admission) is
      pragma Unreferenced (Admitted_Badge, Header, Input, Output, ID, Result, Admission);
   begin raise Program_Error with "unexpected Submit"; end Submit;
   procedure Take_Completion (Item : out Controller.Completion; Result : out Controller.Collection) is
      pragma Unreferenced (Item, Result);
   begin raise Program_Error with "unexpected Take_Completion"; end Take_Completion;
   procedure Release (Result : out Releasing) is
      pragma Unreferenced (Result);
   begin raise Program_Error with "unexpected Release"; end Release;
end I2C_Channel_Controller;
